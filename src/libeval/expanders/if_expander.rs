// Copyright (c) 2017, Sabre developers
//
// Licensed under the Apache License, Version 2.0 (see LICENSE.Apache in the
// root directory) or MIT license (see LICENSE.MIT in the root directory),
// at your option. This file may be copied, distributed, and modified only
// in accordance with the terms specified by the chosen license.

//! `if` expander.

use std::rc::{Rc};

use locus::diagnostics::{Handler, DiagnosticKind};
use reader::datum::{ScannedDatum};
use reader::intern_pool::{Atom};

use environment::{Environment};
use expression::{Expression, ExpressionKind, Literal};
use expanders::{Expander, ExpansionResult};

/// Expand `if` special forms into alternatives.
pub struct IfExpander<'a> {
    /// Recognized `if` atom.
    name: Atom,

    /// Designated responsible for diagnostic processing.
    diagnostic: &'a Handler,
}

impl<'a> IfExpander<'a> {
    /// Make a new `if` expander for a given name.
    pub fn new(name: Atom, handler: &Handler) -> IfExpander {
        IfExpander {
            name: name,
            diagnostic: handler,
        }
    }
}

impl<'a> Expander for IfExpander<'a> {
    fn expand(&self, datum: &ScannedDatum, environment: &Rc<Environment>, expander: &Expander) -> ExpansionResult {
        use expanders::utils::{is_named_form, expect_list_length_fixed};

        // Filter out anything that certainly does not look as a if form.
        let (dotted, values) = match is_named_form(datum, self.name) {
            Some(v) => v,
            None => { return ExpansionResult::Unknown; }
        };

        // The only valid form is (if condition consequence alternative).
        expect_list_length_fixed(datum, dotted, values, 4,
            &self.diagnostic, DiagnosticKind::err_expand_invalid_if);

        // Recover from errors by using #f as placeholder values.
        let expand_or_recover = |datum| {
            if let Some(datum) = datum {
                let result = expander.expand(datum, environment, expander);
                if let ExpansionResult::Some(expression) = result {
                    return expression;
                }
            }
            return Expression {
                kind: ExpressionKind::Literal(Literal::Boolean(false)),
                span: None,
            };
        };
        let condition   = Box::new(expand_or_recover(values.get(1)));
        let consequence = Box::new(expand_or_recover(values.get(2)));
        let alternative = Box::new(expand_or_recover(values.get(3)));

        return ExpansionResult::Some(Expression {
            kind: ExpressionKind::Alternative(condition, consequence, alternative),
            span: Some(datum.span),
        });
    }
}
