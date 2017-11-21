// Copyright (c) 2016, Sabre developers
//
// Licensed under the Apache License, Version 2.0 (see LICENSE.Apache in the
// root directory) or MIT license (see LICENSE.MIT in the root directory),
// at your option. This file may be copied, distributed, and modified only
// in accordance with the terms specified by the chosen license.

//! Scheme syntax analyzer.
//!
//! This module contains definition of the _syntactical analyzer_ which constructs parsing trees
//! out of token streams.

use locus::diagnostics::{Span, Handler, DiagnosticKind};

use datum::{ScannedDatum, DatumValue};
use intern_pool::{Atom, InternPool};
use lexer::{Scanner, ScannedToken};
use tokens::{Token, ParenType};

/// Data parser.
///
/// The parser reconstructs data trees out of a token stream. These are not s-expressions yet
/// as cyclic references are not resolved and abbreviations are not unwrapped. Obviously, the
/// parser also does not perform any semantical analysis, does not expand macros, etc.
pub struct Parser<'a> {
    /// Our source of tokens.
    scanner: Box<Scanner + 'a>,

    /// The pool used by the scanner.
    pool: &'a InternPool,

    // Parsing state
    //
    // ----+-------------+--------------------+-------------+-------------+--------------+-----+
    // ... | Open(Paren) | Identifier("cons") | Number("1") | Number("2") | Close(Paren) | Eof |
    // ----+-------------+--------------------+-------------+-------------+--------------+-----+
    //      ^                                                                               ^
    //  cur == Open(Paren)                                                              cur == Eof

    cur: ScannedToken,

    //
    // Diagnostic reporting
    //

    /// Designated responsible for diagnostic processing.
    diagnostic: &'a Handler,
}

/// Result of a single datum parsing.
///
/// Err means fatal parsing error. The parser was unable to recover in a meaningful way and it is
/// unlikely to produce more data. Currently this happens only on unexpected EOF condition.
///
/// Ok(Some) means that the parser has successfully parsed the datum or it has fully recovered
/// from a parsing error and is able to return a result. Ok(None) means that the parser has
/// recovered from an error, but it had to disregard the currently parsed datum.
type ParseResult = Result<Option<ScannedDatum>, ()>;

impl<'a> Parser<'a> {
    /// Construct a new parser that will use the given token stream.
    pub fn new(scanner: Box<Scanner + 'a>, pool: &'a InternPool, handler: &'a Handler)
        -> Parser<'a>
    {
        let mut parser = Parser {
            scanner: scanner,
            pool: pool,
            cur: ScannedToken { tok: Token::Eof, span: Span::new(0, 0) },
            diagnostic: handler,
        };
        parser.bump();
        return parser;
    }

    /// Read in the next meaningful token.
    fn bump(&mut self) {
        loop {
            self.cur = self.scanner.next_token();

            match self.cur.tok {
                // Skip atmosphere.
                Token::Whitespace | Token::Comment | Token::Directive(_) | Token::Unrecognized
                    => { continue; }

                _ => { break; }
            }
        }
    }

    /// Consume the whole token stream and return the parsed data sequence.
    pub fn parse_all_data(&mut self) -> Vec<ScannedDatum> {
        let mut data = Vec::new();

        while self.cur.tok != Token::Eof {
            match self.next_datum() {
                Ok(Some(datum)) => {
                    data.push(datum);
                }
                Ok(None) => {
                    // ignore parser recovery
                }
                Err(_) => {
                    assert_eq!(self.cur.tok, Token::Eof,
                        "next_datum() is expected to fail only on unexpected EOF");
                }
            }
        }

        return data;
    }

    /// Parse next datum out of the token stream.
    fn next_datum(&mut self) -> ParseResult {
        let cur_span = self.cur.span;

        match self.cur.tok {
            // Handle simple data.
            Token::Boolean(value) => {
                self.bump();

                return Ok(Some(ScannedDatum {
                    value: DatumValue::Boolean(value),
                    span: cur_span,
                }));
            }
            Token::Character(value) => {
                self.bump();

                return Ok(Some(ScannedDatum {
                    value: DatumValue::Character(value),
                    span: cur_span,
                }));
            }
            Token::String(value) => {
                self.bump();

                return Ok(Some(ScannedDatum {
                    value: DatumValue::String(value),
                    span: cur_span,
                }));
            }
            Token::Number(value) => {
                self.bump();

                return Ok(Some(ScannedDatum {
                    value: DatumValue::Number(value),
                    span: cur_span,
                }));
            }
            Token::Identifier(value) => {
                self.bump();

                return Ok(Some(ScannedDatum {
                    value: DatumValue::Symbol(value),
                    span: cur_span,
                }));
            }

            // Dots are expected only in lists.
            Token::Dot => {
                self.diagnostic.report(DiagnosticKind::err_parser_misplaced_dot,
                    self.cur.span);

                self.bump();

                return Ok(None);
            }

            // Closing parentheses should be paired with opening ones.
            Token::Close(_) => {
                self.diagnostic.report(DiagnosticKind::err_parser_extra_delimiter,
                    self.cur.span);

                self.bump();

                return Ok(None);
            }

            // Handle bytevectors.
            Token::OpenBytevector(paren) => {
                self.parse_bytevector(paren)
            }

            // Handle vectors.
            Token::OpenVector(paren) => {
                self.parse_vector(paren)
            }

            // Handle lists.
            Token::Open(paren) => {
                self.parse_list(paren)
            }

            // Handle abbreviations.
            Token::Quote => {
                self.parse_abbreviation("'", "quote")
            }
            Token::Backquote => {
                self.parse_abbreviation("`", "quasiquote")
            }
            Token::Comma => {
                self.parse_abbreviation(",", "unquote")
            }
            Token::CommaSplicing => {
                self.parse_abbreviation(",@", "unquote-splicing")
            }

            // Handle label references.
            Token::LabelRef(label) => {
                self.bump();

                return Ok(Some(ScannedDatum {
                    value: DatumValue::LabelReference(label),
                    span: cur_span,
                }));
            }

            // Handle labeled data.
            Token::LabelMark(label) => {
                self.parse_labeled_datum(label)
            }

            // Handle s-expression comments.
            Token::CommentPrefix => {
                self.parse_sexpr_comment()
            }

            Token::Whitespace | Token::Comment | Token::Directive(_) | Token::Unrecognized
                => unreachable!("atmosphere not handled before calling next_datum()"),

            Token::Eof
                => unreachable!("EOF not handled before calling next_datum()"),
        }
    }

    /// Parse next datum out of the token stream, ignoring parser recovery.
    fn next_datum_required(&mut self, start_span: Span) -> ParseResult {
        loop {
            match self.cur.tok {
                // All of these mean that we won't be able to parse a datum without skipping them.
                // A closing parenthesis most likely marks the end of a list or a vector, a dot
                // should be handled by the list-parsing code, and EOF is EOF. Either way, there
                // is no data for us here, so complain and bail out.
                Token::Close(_) | Token::Dot | Token::Eof => {
                    self.diagnostic.report(DiagnosticKind::err_parser_missing_datum,
                        Span::new(start_span.to, start_span.to));

                    return Ok(None);
                }

                // If we have got Some data then return it. Otherwise keep parsing.
                _ => {
                    if let Some(datum) = self.next_datum()? {
                        return Ok(Some(datum));
                    }
                }
            }
        }
    }

    /// Parse an s-expression comment.
    fn parse_sexpr_comment(&mut self) -> ParseResult {
        assert!(match self.cur.tok {
            Token::CommentPrefix => true,
            _ => false,
        });

        // S-expression comments have a peculiar kind of parsing recursion due to the fact that
        // 1) they are considered whitespace, and 2) whitespace delimits tokens. Thus, for example,
        // in "#; #; (1) (2 3)" both s-expressions are commented out, with inner "#; (1)" being
        // treated as intertoken space between the outer "#;" and "(2 3)" which form a comment
        // of their own.

        let mut start_spans = Vec::new();

        // Remember locations of all consecutive comment prefixes.
        while self.cur.tok == Token::CommentPrefix {
            start_spans.push(self.cur.span);

            self.bump();
        }

        // Then skip the same number of data.
        while let Some(start_span) = start_spans.pop() {
            try!(self.next_datum_required(start_span));
        }

        return Ok(None);
    }

    /// Parse a bytevector literal.
    fn parse_bytevector(&mut self, expected_paren: ParenType) -> ParseResult {
        assert!(match self.cur.tok {
            Token::OpenBytevector(_) => true,
            _ => false,
        });

        let mut values = Vec::new();

        let start_span = self.cur.span;

        self.bump();

        loop {
            match self.cur.tok {
                // Bytevector literal is terminated by a closing parenthesis.
                Token::Close(paren) => {
                    if paren != expected_paren {
                        self.diagnostic.report(DiagnosticKind::err_parser_mismatched_delimiter,
                            self.cur.span);
                    }

                    break;
                }

                // End of token stream means that we will never see the closing parenthesis.
                Token::Eof => {
                    self.diagnostic.report(DiagnosticKind::fatal_parser_unterminated_delimiter,
                        start_span);

                    return Err(());
                }

                // Everything else must be bytevector constituents.
                _ => {
                    if let Some(datum) = self.next_datum()? {
                        // Only numbers are allowed in bytevectors.
                        match datum.value {
                            DatumValue::Number(value) => {
                                values.push(value);
                            }
                            _ => {
                                self.diagnostic.report(DiagnosticKind::err_parser_invalid_bytevector_element,
                                    datum.span);
                            }
                        }
                    }
                }
            }
        }

        assert!(match self.cur.tok {
            Token::Close(_) => true,
            _ => false,
        });

        let end_span = self.cur.span;

        self.bump();

        return Ok(Some(ScannedDatum {
            value: DatumValue::Bytevector(values),
            span: Span::new(start_span.from, end_span.to),
        }));
    }

    /// Parse a vector datum.
    fn parse_vector(&mut self, expected_paren: ParenType) -> ParseResult {
        assert!(match self.cur.tok {
            Token::OpenVector(_) => true,
            _ => false,
        });

        let mut elements = Vec::new();

        let start_span = self.cur.span;

        self.bump();

        loop {
            match self.cur.tok {
                // Vectors are terminated by a closing parenthesis.
                Token::Close(paren) => {
                    if paren != expected_paren {
                        self.diagnostic.report(DiagnosticKind::err_parser_mismatched_delimiter,
                            self.cur.span);
                    }

                    break;
                }

                // End of token stream means that we will never see the closing parenthesis.
                Token::Eof => {
                    self.diagnostic.report(DiagnosticKind::fatal_parser_unterminated_delimiter,
                        start_span);

                    return Err(());
                }

                // Everything else is a vector element.
                _ => {
                    if let Some(datum) = self.next_datum()? {
                        elements.push(datum);
                    }
                }
            }
        }

        assert!(match self.cur.tok {
            Token::Close(_) => true,
            _ => false,
        });

        let end_span = self.cur.span;

        self.bump();

        return Ok(Some(ScannedDatum {
            value: DatumValue::Vector(elements),
            span: Span::new(start_span.from, end_span.to),
        }));
    }

    /// Parse a list datum.
    fn parse_list(&mut self, expected_paren: ParenType) -> ParseResult {
        assert!(match self.cur.tok {
            Token::Open(_) => true,
            _ => false,
        });

        let mut elements = Vec::new();
        let mut dot_locations = Vec::new();

        let start_span = self.cur.span;

        self.bump();

        loop {
            match self.cur.tok {
                // Lists are terminated by a closing parenthesis.
                Token::Close(paren) => {
                    if paren != expected_paren {
                        self.diagnostic.report(DiagnosticKind::err_parser_mismatched_delimiter,
                            self.cur.span);
                    }

                    break;
                }

                // End of token stream means that we will never see the closing parenthesis.
                Token::Eof => {
                    self.diagnostic.report(DiagnosticKind::fatal_parser_unterminated_delimiter,
                        start_span);

                    return Err(());
                }

                // Just remember the locations of all encountered dots. We will check later
                // whether they are placed correctly to form a dotted list.
                Token::Dot => {
                    dot_locations.push(self.cur.span);

                    self.bump();
                }

                // Everything else is a list element.
                _ => {
                    if let Some(datum) = self.next_datum()? {
                        elements.push(datum);
                    }
                }
            }
        }

        assert!(match self.cur.tok {
            Token::Close(_) => true,
            _ => false,
        });

        let end_span = self.cur.span;

        self.bump();

        let dotted_list = is_dotted_list(&elements, &dot_locations);

        // Report the dots if this is not a dotted list.
        if !dotted_list {
            for location in dot_locations {
                self.diagnostic.report(DiagnosticKind::err_parser_misplaced_dot,
                    location);
            }
        }

        return Ok(Some(ScannedDatum {
            value: if dotted_list {
                DatumValue::DottedList(elements)
            } else {
                DatumValue::ProperList(elements)
            },
            span: Span::new(start_span.from, end_span.to),
        }));
    }

    /// Parse an abbreviation.
    fn parse_abbreviation(&mut self, sigil: &str, keyword: &str) -> ParseResult {
        assert!(match self.cur.tok {
            Token::Quote | Token::Backquote | Token::Comma | Token::CommaSplicing => true,
            _ => false,
        });

        let start_span = self.cur.span;

        self.bump();

        return self.next_datum_required(start_span).map(|result| result.map(|datum| {
            ScannedDatum {
                span: Span::new(start_span.from, datum.span.to),
                value: DatumValue::ProperList(vec![
                    ScannedDatum {
                        value: DatumValue::Symbol(self.pool.intern(keyword)),
                        span: Span::new(start_span.from, start_span.from + sigil.len()),
                    },
                    datum,
                ]),
            }
        }));
    }

    /// Parse a labeled datum.
    fn parse_labeled_datum(&mut self, label: Atom) -> ParseResult {
        assert!(match self.cur.tok {
            Token::LabelMark(_) => true,
            _ => false,
        });

        let start_span = self.cur.span;

        self.bump();

        return self.next_datum_required(start_span).map(|result| result.map(|datum| {
            ScannedDatum {
                span: Span::new(start_span.from, datum.span.to),
                value: DatumValue::LabeledDatum(label, Box::new(datum)),
            }
        }));
    }
}

/// Check whether given elements and dots can form a valid dotted list.
fn is_dotted_list(elements: &[ScannedDatum], dot_locations: &[Span]) -> bool {
    // Dotted lists must contain at least two elements and exactly one dot.
    if !(elements.len() >= 2 && dot_locations.len() == 1) {
        return false;
    }

    // And the dot must be located exactly between the last element and the one before it.
    let prev = elements[elements.len() - 2].span;
    let last = elements[elements.len() - 1].span;
    let dot = dot_locations[0];

    assert!(prev.to < last.from);

    return (prev.to <= dot.from) && (dot.to <= last.from);
}
