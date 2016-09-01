// Copyright (c) 2016, ilammy
//
// Licensed under MIT license (see LICENSE file in the root directory).
// This file may be copied, distributed, and modified only in accordance
// with the terms specified by this license.

//! Diagnostic reporting.
//!
//! This module contains types and functions used to generate, format, and report _diagnostics_:
//! error and warning messages, notifications, etc.

// Strictly speaking, this should not be a part of libreader as diagnostics are a more broad
// subject. But for now they are here. Diagnostic processing will be moved into a better place
// as soon as we will be concerned with file handling.

use std::cell::RefCell;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Spans
//

/// Span of a token or an expression.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Span {
    /// Byte offset of the first character of the span.
    pub from: usize,

    /// Byte offset of the first character following the span (after the last character).
    pub to: usize,
}

impl Span {
    /// Make a new span with given bounds.
    pub fn new(from: usize, to: usize) -> Span {
        assert!(from <= to);
        Span { from: from, to: to }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Diagnostics
//

/// A reported diagnostic.
#[derive(Debug, PartialEq, Eq)]
pub struct Diagnostic {
    /// The kind of a diagnostic.
    pub kind: DiagnosticKind,

    /// Optional primary location of the offending code.
    pub loc: Option<Span>,
}

/// Diagnostic reporter interface.
///
/// Reporters are used to report various errors, warnings, suggestions, notes, and insults
/// (collectively known as _diagnostics_) to the user.
pub trait Reporter {
    /// Report a single diagnostic.
    fn report(&mut self, diagnostic: Diagnostic);
}

/// Kinds of repoted diagnostics.
#[allow(non_camel_case_types)]
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum DiagnosticKind {
    /// Lexer has encountered an invalid sequence of characters with unrecognized meaning.
    err_lexer_unrecognized,

    /// Lexer has recovered from an invalid spelling of bytevector opener `#u8(`.
    err_lexer_invalid_bytevector,

    /// Lexer has encountered a block comment without matching termination token.
    fatal_lexer_unterminated_comment,

    /// Lexer has found a hex-coded Unicode code point literal outside of Unicode range.
    err_lexer_invalid_unicode_range,

    /// Lexer has expected an inline character after `#\` but there wasn't any.
    err_lexer_character_missing,

    /// Lexer has scanned a named character with unknown name.
    err_lexer_unknown_character_name,

    /// Lexer has encountered a string without a closing quote.
    fatal_lexer_unterminated_string,

    /// Lexer has encountered an invalid escape sequence.
    err_lexer_invalid_escape_sequence,

    /// Lexer has encountered an invalid line escape, not followed by only whitespace.
    err_lexer_invalid_line_escape,

    /// Lexer has expected hex digits in a hexcoded character escape but there weren't any.
    err_lexer_unicode_escape_missing_digits,

    /// Lexer has expected a semicolon to terminate a hexcoded character escape
    /// but there wasn't any.
    err_lexer_unicode_escape_missing_semicolon,

    /// Lexer has encountered multiple numeric base specifiers in a number literal.
    err_lexer_multiple_number_bases,

    /// Lexer has encountered multiple exactness specifiers in a number literal.
    err_lexer_multiple_exactness,

    /// Lexer has encountered an invalid number prefix.
    err_lexer_invalid_number_prefix,

    /// Lexer has encountered a digit of invalid base.
    err_lexer_invalid_number_digit,

    /// Lexer has encountered a character which is not allowed in numbers.
    err_lexer_invalid_number_character,

    /// Lexer has encountered an escaped identifier without a closing vertical bar.
    fatal_lexer_unterminated_identifier,
}

/// Convenience wrapper for reporting diagnostics with known spans.
pub struct Handler {
    /// Internal reporter implementation.
    reporter: RefCell<Box<Reporter>>,
}

impl Handler {
    /// Construct a new Handler for a given reporter.
    pub fn with_reporter(reporter: Box<Reporter>) -> Handler {
        Handler {
            reporter: RefCell::new(reporter),
        }
    }

    /// Immediately report a single diagnostic.
    pub fn report(&self, kind: DiagnosticKind, span: Span) {
        self.reporter.borrow_mut().report(Diagnostic {
            kind: kind,
            loc: Some(span),
        });
    }
}
