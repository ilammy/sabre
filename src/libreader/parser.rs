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
use tokens::{Token};

/// S-expr parser.
///
/// The parser reconstructs s-expr trees out of a token stream. It only parses s-exprs, resolves
/// cyclic references, and unwraps abbreviations. It does not perform any semantical analysis,
/// does not expand macros, etc.
pub struct Parser<'a> {
    /// Our source of tokens.
    scanner: Box<Scanner + 'a>,

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
            diagnostic: handler,
        }
    }

    /// Consume the whole token stream and return the parsed data sequence.
    pub fn parse_all_data(&mut self) -> Vec<ScannedDatum> {
        let mut data = Vec::new();
        while let Some(datum) = self.next_datum() {
            data.push(datum);
        }
        return data;
    }

    ///
    fn next_datum(&mut self) -> Option<ScannedDatum> {
        loop {
            let next = self.scanner.next_token();

            match next.tok {
                // Skip over the atmosphere.
                Token::Whitespace | Token::Comment | Token::Directive(_) => {
                    continue;
                }

                // Ignore unrecognized parts, they have no syntactical value.
                Token::Unrecognized => {
                    continue;
                }

                // Stop at EOF.
                Token::Eof => {
                    return None;
                }

                // Handle simple data.
                Token::Boolean(value) => {
                    return Some(ScannedDatum {
                        value: DatumValue::Boolean(value),
                        span: next.span,
                    });
                }
                Token::Character(value) => {
                    return Some(ScannedDatum {
                        value: DatumValue::Character(value),
                        span: next.span,
                    });
                }
                Token::String(value) => {
                    return Some(ScannedDatum {
                        value: DatumValue::String(value),
                        span: next.span,
                    });
                }
                Token::Number(value) => {
                    return Some(ScannedDatum {
                        value: DatumValue::Number(value),
                        span: next.span,
                    });
                }
                Token::Identifier(value) => {
                    return Some(ScannedDatum {
                        value: DatumValue::Symbol(value),
                        span: next.span,
                    });
                }

                _ => unimplemented!(),
            }
        }
    }
}
