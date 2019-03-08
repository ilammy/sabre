// Copyright (c) 2017, Sabre developers
//
// Licensed under the Apache License, Version 2.0 (see LICENSE.Apache in the
// root directory) or MIT license (see LICENSE.MIT in the root directory),
// at your option. This file may be copied, distributed, and modified only
// in accordance with the terms specified by the chosen license.

//! `begin` expander.

use std::rc::Rc;

use liblocus::diagnostics::{DiagnosticKind, Handler};
use libreader::datum::ScannedDatum;
use libreader::intern_pool::Atom;

use crate::environment::Environment;
use crate::expanders::{Expander, ExpansionResult};
use crate::expression::{Expression, ExpressionKind};

/// Expand `begin` special forms into sequences.
pub struct BeginExpander {
    /// Recognized `begin` atom.
    name: Atom,
}

impl BeginExpander {
    /// Make a new `begin` expander for a given name.
    pub fn new(name: Atom) -> BeginExpander {
        BeginExpander {
            name,
        }
    }
}

impl Expander for BeginExpander {
    fn expand(&self, datum: &ScannedDatum, environment: &Rc<Environment>, diagnostic: &Handler, expander: &dyn Expander) -> ExpansionResult {
        use crate::expanders::utils::{is_named_form, expect_list_length_at_least};

        // Filter out anything that certainly does not look as a begin form.
        let (dotted, values) = match is_named_form(datum, self.name) {
            Some(v) => v,
            None => { return ExpansionResult::Unknown; }
        };

        // The only valid form is (begin expr1 expr2 ...).
        expect_list_length_at_least(datum, dotted, values, 2,
            diagnostic, DiagnosticKind::err_expand_invalid_begin);

        // Ignore any errors when recovering. Expand empty (begin) into an empty sequence.
        let expressions = values[1..].iter()
            .filter_map(|datum| match expander.expand(datum, environment, diagnostic, expander) {
                ExpansionResult::Some(expression) => Some(expression),
                ExpansionResult::None => None,
                ExpansionResult::Unknown => None,
            })
            .collect();

        ExpansionResult::Some(Expression {
            kind: ExpressionKind::Sequence(expressions),
            span: datum.span,
            environment: environment.clone(),
        })
    }
}
