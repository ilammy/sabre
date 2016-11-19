// Copyright (c) 2016, Sabre developers
//
// Licensed under the Apache License, Version 2.0 (see LICENSE.Apache in the
// root directory) or MIT license (see LICENSE.MIT in the root directory),
// at your option. This file may be copied, distributed, and modified only
// in accordance with the terms specified by the chosen license.

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
