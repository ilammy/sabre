// Copyright (c) 2017, Sabre developers
//
// Licensed under the Apache License, Version 2.0 (see LICENSE.Apache in the
// root directory) or MIT license (see LICENSE.MIT in the root directory),
// at your option. This file may be copied, distributed, and modified only
// in accordance with the terms specified by the chosen license.

//! Diagnostic reporting.
//!
//! This module contains types and functions used to generate, format, and report _diagnostics_:
//! error and warning messages, notifications, etc.

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

pub use diagnostic_kinds::DiagnosticKind;

/// A reported diagnostic.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Diagnostic {
    /// The kind of a diagnostic.
    pub kind: DiagnosticKind,

    /// Location of the offending code.
    pub span: Span,
}

/// Diagnostic reporter interface.
///
/// Reporters are used to report various errors, warnings, suggestions, notes, and insults
/// (collectively known as _diagnostics_) to the user.
pub trait Reporter {
    /// Report a single diagnostic.
    fn report(&mut self, diagnostic: Diagnostic);
}

/// Convenience wrapper for reporting diagnostics.
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
        self.reporter.borrow_mut().report(Diagnostic { kind, span });
    }
}
