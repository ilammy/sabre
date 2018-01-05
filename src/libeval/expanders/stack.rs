// Copyright (c) 2017, Sabre developers
//
// Licensed under the Apache License, Version 2.0 (see LICENSE.Apache in the
// root directory) or MIT license (see LICENSE.MIT in the root directory),
// at your option. This file may be copied, distributed, and modified only
// in accordance with the terms specified by the chosen license.

//! Macro expander stack.

use std::rc::{Rc};

use locus::diagnostics::{Handler};
use reader::datum::{ScannedDatum};

use environment::{Environment};
use expanders::{Expander, ExpansionResult};

/// Macro expander stack.
///
/// This models the nested syntactical environments. You push an expander when you need to extend
/// then environment on entering a scope and then pop one when leaving the scope.
///
/// The stack is an expander as well. It tries the embedded expanders sequentially in LIFO order
/// until one of them fails or produces a value. The stack returns `Unknown` value only when all
/// subexpanders do not know what to do with the datum.
pub struct ExpanderStack {
    /// The current expander.
    head: Box<Expander>,

    /// The rest of the expanders.
    tail: Option<Box<ExpanderStack>>,
}

impl ExpanderStack {
    /// Create a new expander stack with a given expander.
    pub fn new(base: Box<Expander>) -> ExpanderStack {
        ExpanderStack {
            head: base,
            tail: None,
        }
    }

    /// Push a new expander into the stack.
    pub fn push(self, head: Box<Expander>) -> ExpanderStack {
        ExpanderStack {
            head: head,
            tail: Some(Box::new(self)),
        }
    }

    /// Pop a single expander off the stack.
    ///
    /// Returns None if that expander was the last one and you have messed up (as this should not
    /// happen if the environments are balanced).
    pub fn pop(self) -> Option<ExpanderStack> {
        self.tail.map(|v| *v)
    }
}

impl Expander for ExpanderStack {
    fn expand(&self, datum: &ScannedDatum, environment: &Rc<Environment>, diagnostic: &Handler, expander: &Expander) -> ExpansionResult {
        let mut current = self.head.as_ref();
        loop {
            let result = current.expand(datum, environment, diagnostic, expander);

            if let ExpansionResult::Unknown = result {
                if let Some(ref next) = self.tail {
                    current = next.as_ref();
                    continue;
                }
            }

            return result;
        }
    }
}
