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
