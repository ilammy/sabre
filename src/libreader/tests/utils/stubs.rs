// Copyright (c) 2016, ilammy
//
// Licensed under MIT license (see LICENSE file in the root directory).
// This file may be copied, distributed, and modified only in accordance
// with the terms specified by this license.

//! Stub interface implementations.

use std::cell::RefCell;
use std::rc::Rc;

use reader::diagnostics::{Reporter, Diagnostic};

/// A simple Reporter collecting diagnostics.
pub struct SinkReporter {
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
            diagnostics: diagnostics,
        }
    }
}
