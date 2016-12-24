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

use datum::{ScannedDatum, DatumValue};
use diagnostics::{Span, Handler, DiagnosticKind};
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

impl<'a> Parser<'a> {
    /// Construct a new parser that will use the given token stream.
    pub fn new(scanner: Box<Scanner + 'a>, handler: &'a Handler) -> Parser<'a> {
        Parser {
            scanner: scanner,
            cur: ScannedToken { tok: Token::Eof, span: Span::new(0, 0) },
            diagnostic: handler,
        }
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

        loop {
            self.bump();

            if let Some(datum) = self.next_datum() {
                data.push(datum);
            }

            if self.cur.tok == Token::Eof {
                break;
            }
        }

        return data;
    }

    /// Parse next datum out of the token stream.
    fn next_datum(&mut self) -> Option<ScannedDatum> {
        match self.cur.tok {
            // No tokens -- no data.
            Token::Eof => {
                return None;
            }

            // Handle simple data.
            Token::Boolean(value) => {
                return Some(ScannedDatum {
                    value: DatumValue::Boolean(value),
                    span: self.cur.span,
                });
            }
            Token::Character(value) => {
                return Some(ScannedDatum {
                    value: DatumValue::Character(value),
                    span: self.cur.span,
                });
            }
            Token::String(value) => {
                return Some(ScannedDatum {
                    value: DatumValue::String(value),
                    span: self.cur.span,
                });
            }
            Token::Number(value) => {
                return Some(ScannedDatum {
                    value: DatumValue::Number(value),
                    span: self.cur.span,
                });
            }
            Token::Identifier(value) => {
                return Some(ScannedDatum {
                    value: DatumValue::Symbol(value),
                    span: self.cur.span,
                });
            }

            // Dots are expected only in lists.
            Token::Dot => {
                self.diagnostic.report(DiagnosticKind::err_parser_misplaced_dot,
                    self.cur.span);

                return None;
            }

            // Handle bytevectors.
            Token::OpenBytevector(paren) => {
                return self.parse_bytevector(paren);
            }

            // Handle vectors.
            Token::OpenVector(paren) => {
                return self.parse_vector(paren);
            }

            // Handle lists.
            Token::Open(paren) => {
                return self.parse_list(paren);
            }

            Token::Whitespace | Token::Comment | Token::Directive(_) | Token::Unrecognized
                => unreachable!("atmosphere not handled before calling next_datum()"),

            Token::CommentPrefix => unimplemented!(),
            Token::Quote | Token::Backquote | Token::Comma | Token::CommaSplicing => unimplemented!(),
            Token::LabelMark(_) | Token::LabelRef(_) => unimplemented!(),
            Token::Close(_) => unimplemented!(),
        }
    }

    /// Parse a bytevector literal.
    fn parse_bytevector(&mut self, expected_paren: ParenType) -> Option<ScannedDatum> {
        let mut values = Vec::new();

        let start_span = self.cur.span;

        loop {
            self.bump();

            // Bytevector literal is terminated by a closing parenthesis.
            if let Token::Close(paren) = self.cur.tok {
                if paren != expected_paren {
                    self.diagnostic.report(DiagnosticKind::err_parser_mismatched_delimiter,
                        self.cur.span);
                }

                return Some(ScannedDatum {
                    value: DatumValue::Bytevector(values),
                    span: Span::new(start_span.from, self.cur.span.to),
                });
            }

            // End of token stream means that we will never see the closing parenthesis.
            if self.cur.tok == Token::Eof {
                self.diagnostic.report(DiagnosticKind::fatal_parser_unterminated_delimiter,
                    start_span);

                return None;
            }

            // Everything else must be bytevector constituents.
            if let Some(datum) = self.next_datum() {
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

    /// Parse a vector datum.
    fn parse_vector(&mut self, expected_paren: ParenType) -> Option<ScannedDatum> {
        let mut elements = Vec::new();

        let start_span = self.cur.span;

        loop {
            self.bump();

            // Vectors are terminated by a closing parenthesis.
            if let Token::Close(paren) = self.cur.tok {
                if paren != expected_paren {
                    self.diagnostic.report(DiagnosticKind::err_parser_mismatched_delimiter,
                        self.cur.span);
                }

                return Some(ScannedDatum {
                    value: DatumValue::Vector(elements),
                    span: Span::new(start_span.from, self.cur.span.to),
                });
            }

            // End of token stream means that we will never see the closing parenthesis.
            if self.cur.tok == Token::Eof {
                self.diagnostic.report(DiagnosticKind::fatal_parser_unterminated_delimiter,
                    start_span);

                return None;
            }

            // Everything else is a vector element.
            if let Some(datum) = self.next_datum() {
                elements.push(datum);
            }
        }
    }

    /// Parse a list datum.
    fn parse_list(&mut self, expected_paren: ParenType) -> Option<ScannedDatum> {
        let mut elements = Vec::new();
        let mut dot_locations = Vec::new();

        let start_span = self.cur.span;

        loop {
            self.bump();

            // Lists are terminated by a closing parenthesis.
            if let Token::Close(paren) = self.cur.tok {
                if paren != expected_paren {
                    self.diagnostic.report(DiagnosticKind::err_parser_mismatched_delimiter,
                        self.cur.span);
                }

                break;
            }

            // Just remember the locations of all encountered dots. We will check later
            // whether they are placed correctly to form a dotted list.
            if self.cur.tok == Token::Dot {
                dot_locations.push(self.cur.span);

                continue;
            }

            // End of token stream means that we will never see the closing parenthesis.
            if self.cur.tok == Token::Eof {
                self.diagnostic.report(DiagnosticKind::fatal_parser_unterminated_delimiter,
                    start_span);

                return None;
            }

            // Everything else is a list element.
            if let Some(datum) = self.next_datum() {
                elements.push(datum);
            }
        }

        let dotted_list = is_dotted_list(&elements, &dot_locations);

        // Report the dots if this is not a dotted list.
        if !dotted_list {
            for location in dot_locations {
                self.diagnostic.report(DiagnosticKind::err_parser_misplaced_dot,
                    location);
            }
        }

        return Some(ScannedDatum {
            value: if dotted_list {
                DatumValue::DottedList(elements)
            } else {
                DatumValue::ProperList(elements)
            },
            span: Span::new(start_span.from, self.cur.span.to),
        });
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
