// Copyright (c) 2017, Sabre developers
//
// Licensed under the Apache License, Version 2.0 (see LICENSE.Apache in the
// root directory) or MIT license (see LICENSE.MIT in the root directory),
// at your option. This file may be copied, distributed, and modified only
// in accordance with the terms specified by the chosen license.

//! `set!` expander.

use locus::diagnostics::{Handler, DiagnosticKind};
use reader::datum::{ScannedDatum, DatumValue};
use reader::intern_pool::{Atom};

use expression::{Expression, ExpressionKind, Literal, Variable};
use expanders::{Expander, ExpansionResult};

/// Expand `set!` special forms into assignments.
pub struct SetExpander<'a> {
    /// Recognized `set!` atom.
    name: Atom,

    /// Designated responsible for diagnostic processing.
    diagnostic: &'a Handler,
}

impl<'a> SetExpander<'a> {
    /// Make a new `set!` expander for a given name.
    pub fn new(name: Atom, handler: &Handler) -> SetExpander {
        SetExpander {
            name: name,
            diagnostic: handler,
        }
    }
}

impl<'a> Expander for SetExpander<'a> {
    fn expand(&self, datum: &ScannedDatum, expander: &Expander) -> ExpansionResult {
        use expanders::utils::{is_named_form, expect_list_length_fixed};

        // Filter out anything that certainly does not look as a set! form.
        let (dotted, values) = match is_named_form(datum, self.name) {
            Some(v) => v,
            None => { return ExpansionResult::Unknown; }
        };

        // The only valid form is (set! variable value).
        expect_list_length_fixed(datum, dotted, values, 3,
            &self.diagnostic, DiagnosticKind::err_expand_invalid_set);

        // The first element should be the variable name, followed by the new variable value.
        let variable = self.expand_variable(values.get(1));
        let value = self.expand_value(values.get(2), expander);

        // If we have a variable to assign then use that variable. Otherwise leave only the value.
        return ExpansionResult::Some(
            if let Some(variable) = variable {
                Expression {
                    kind: ExpressionKind::Assignment(variable, Box::new(value)),
                    span: Some(datum.span),
                }
            } else {
                value
            }
        );
    }
}

impl<'a> SetExpander<'a> {
    /// Expand (as a no-op) the variable name in a set! expression.
    ///
    /// It may be missing, or not be a symbol. In either case recover with None.
    fn expand_variable(&self, datum: Option<&ScannedDatum>) -> Option<Variable> {
        if let Some(datum) = datum {
            if let DatumValue::Symbol(name) = datum.value {
                return Some(Variable {
                    name: name,
                    span: Some(datum.span),
                });
            }
            self.diagnostic.report(DiagnosticKind::err_expand_invalid_set,
                datum.span
            );
        }
        return None;
    }

    /// Expand the subexpression denoting variable value in a set! expression.
    ///
    /// In case of errors return an #f literal as a placeholder.
    fn expand_value(&self, datum: Option<&ScannedDatum>, expander: &Expander) -> Expression {
        if let Some(datum) = datum {
            if let ExpansionResult::Some(expression) = expander.expand(datum, expander) {
                return expression;
            }
        }
        return Expression {
            kind: ExpressionKind::Literal(Literal::Boolean(false)),
            span: None,
        };
    }
}