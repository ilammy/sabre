// Copyright (c) 2017, Sabre developers
//
// Licensed under the Apache License, Version 2.0 (see LICENSE.Apache in the
// root directory) or MIT license (see LICENSE.MIT in the root directory),
// at your option. This file may be copied, distributed, and modified only
// in accordance with the terms specified by the chosen license.

//! `set!` expander.

use std::rc::Rc;

use liblocus::diagnostics::{DiagnosticKind, Handler};
use libreader::datum::{DatumValue, ScannedDatum};
use libreader::intern_pool::Atom;

use crate::environment::Environment;
use crate::expand::Expander;
use crate::expression::{Expression, ExpressionKind, Variable};

/// Expand `set!` special forms into assignments.
pub struct SetExpander {
    /// Recognized `set!` atom.
    name: Atom,
}

impl SetExpander {
    /// Make a new `set!` expander for a given name.
    pub fn new(name: Atom) -> SetExpander {
        SetExpander { name }
    }
}

impl Expander for SetExpander {
    fn expand_form(
        &self,
        datum: &ScannedDatum,
        environment: &Rc<Environment>,
        handler: &Handler,
    ) -> Expression {
        use crate::expanders::utils::expect_macro_use;

        // The only valid form is (set! variable value). Variable must be mentioned verbatim.
        // The value expression needs to be expanded.
        let terms = expect_macro_use(
            datum,
            self.name,
            3,
            handler,
            DiagnosticKind::err_expand_invalid_set,
        );

        let variable = expand_variable(terms.get(0), handler);
        let new_value = expand_new_value(terms.get(1), datum, environment, handler);

        // If we have a variable to assign then use that variable. Otherwise leave only the value.
        // Use the same span though.
        Expression {
            kind: if let Some(variable) = variable {
                ExpressionKind::Assignment(variable, Box::new(new_value))
            } else {
                new_value.kind
            },
            span: datum.span,
            environment: environment.clone(),
        }
    }
}

/// Expand (as a no-op) the variable name in a set! expression.
///
/// It may be missing, or not be a symbol. In either case recover with None.
fn expand_variable(datum: Option<&ScannedDatum>, handler: &Handler) -> Option<Variable> {
    if let Some(datum) = datum {
        if let DatumValue::Symbol(name) = datum.value {
            return Some(Variable {
                name,
                span: datum.span,
            });
        }
        DiagnosticKind::err_expand_invalid_set
            .report_at(datum.span)
            .report_to(handler);
    }
    None
}

/// Expand the subexpression denoting variable value in a set! expression.
///
/// In case of errors return an #f literal as a placeholder.
fn expand_new_value(
    term: Option<&ScannedDatum>,
    datum: &ScannedDatum,
    environment: &Rc<Environment>,
    handler: &Handler,
) -> Expression {
    use crate::expand::expand;
    use crate::expanders::utils::missing_last_span;

    term.map(|datum| expand(datum, environment, handler))
        .unwrap_or(Expression {
            kind: ExpressionKind::Undefined,
            span: missing_last_span(datum),
            environment: environment.clone(),
        })
}
