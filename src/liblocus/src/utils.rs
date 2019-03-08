// Copyright (c) 2017, Sabre developers
//
// Licensed under the Apache License, Version 2.0 (see LICENSE.Apache in the
// root directory) or MIT license (see LICENSE.MIT in the root directory),
// at your option. This file may be copied, distributed, and modified only
// in accordance with the terms specified by the chosen license.

//! Utilities.

use std::cell::RefCell;
use std::rc::Rc;

use crate::diagnostics::{Diagnostic, Handler, Reporter};

/// A simple Reporter collecting diagnostics.
struct SinkReporter {
    /// Reported diagnostics are collected into this vector.
    diagnostics: Rc<RefCell<Vec<Diagnostic>>>,
}

impl Reporter for SinkReporter {
    fn report(&mut self, diagnostic: Diagnostic) {
        self.diagnostics.borrow_mut().push(diagnostic);
    }
}

impl SinkReporter {
    /// Make a new sink reporter.
    pub fn new(diagnostics: Rc<RefCell<Vec<Diagnostic>>>) -> SinkReporter {
        SinkReporter {
            diagnostics,
        }
    }
}

/// Call a function while collecting diagnostics.
///
/// All diagnostics reported to the provided handler will be returned back together with the
/// value returned by the function.
pub fn collect_diagnostics<T, F>(body: F) -> (T, Vec<Diagnostic>)
    where F: FnOnce(&Handler) -> T
{
    let diagnostics = Rc::new(RefCell::new(Vec::new()));
    let value = {
        let reporter = SinkReporter::new(diagnostics.clone());
        let handler = Handler::with_reporter(Box::new(reporter));
        body(&handler)
    };
    (value, Rc::try_unwrap(diagnostics).unwrap().into_inner())
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::diagnostics::{Diagnostic, DiagnosticKind, Span};

    #[test]
    fn collect_none() {
        let (value, diagnostics) = collect_diagnostics(|_| 42);
        assert_eq!(value, 42);
        assert_eq!(diagnostics, vec![]);
    }

    #[test]
    fn collect_some() {
        let (value, diagnostics) = collect_diagnostics(|diagnostic| {
            diagnostic.report(DiagnosticKind::fatal_lexer_unterminated_comment, Span::new(0, 1));
            diagnostic.report(DiagnosticKind::err_parser_misplaced_dot, Span::new(34, 51));
        });
        assert_eq!(value, ());
        assert_eq!(diagnostics, vec![
            Diagnostic {
                kind: DiagnosticKind::fatal_lexer_unterminated_comment,
                span: Span::new(0, 1),
            },
            Diagnostic {
                kind: DiagnosticKind::err_parser_misplaced_dot,
                span: Span::new(34, 51),
            },
        ]);
    }
}
