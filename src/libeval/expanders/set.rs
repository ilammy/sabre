// Copyright (c) 2017, Sabre developers
//
// Licensed under the Apache License, Version 2.0 (see LICENSE.Apache in the
// root directory) or MIT license (see LICENSE.MIT in the root directory),
// at your option. This file may be copied, distributed, and modified only
// in accordance with the terms specified by the chosen license.

//! `set!` expander.

use std::rc::{Rc};

use locus::diagnostics::{Handler, DiagnosticKind, Span};
use reader::datum::{ScannedDatum, DatumValue};
use reader::intern_pool::{Atom};

use environment::{Environment};
use expression::{Expression, ExpressionKind, Literal, Variable};
use expand::Expander;
use expanders::{Expander as OldExpander, ExpansionResult};

/// Expand `set!` special forms into assignments.
pub struct SetExpander {
    /// Recognized `set!` atom.
    name: Atom,
}

impl SetExpander {
    /// Make a new `set!` expander for a given name.
    pub fn new(name: Atom) -> SetExpander {
        SetExpander {
            name: name,
        }
    }
}

impl OldExpander for SetExpander {
    fn expand(&self, datum: &ScannedDatum, environment: &Rc<Environment>, diagnostic: &Handler, expander: &OldExpander) -> ExpansionResult {
        use expanders::utils::{is_named_form, expect_list_length_fixed};

        // Filter out anything that certainly does not look as a set! form.
        let (dotted, values) = match is_named_form(datum, self.name) {
            Some(v) => v,
            None => { return ExpansionResult::Unknown; }
        };

        // The only valid form is (set! variable value).
        expect_list_length_fixed(datum, dotted, values, 3,
            diagnostic, DiagnosticKind::err_expand_invalid_set);

        // The first element should be the variable name, followed by the new variable value.
        let variable = expand_variable(values.get(1), diagnostic);
        let value = expand_value(datum, values.get(2), environment, diagnostic, expander);

        // If we have a variable to assign then use that variable. Otherwise leave only the value.
        return ExpansionResult::Some(
            if let Some(variable) = variable {
                Expression {
                    kind: ExpressionKind::Assignment(variable, Box::new(value)),
                    span: datum.span,
                    environment: environment.clone(),
                }
            } else {
                value
            }
        );
    }
}

impl Expander for SetExpander {
    fn expand_form(
        &self,
        datum: &ScannedDatum,
        environment: &Rc<Environment>,
        diagnostic: &Handler,
    ) -> Expression {
        // The only valid form is (set! variable value). Variable must be mentioned verbatim.
        // The value expression needs to be expanded.
        let terms = expect_set_form(self.name, datum, diagnostic);

        let variable = expand_variable(terms.get(0), diagnostic);
        let new_value = expand_new_value(terms.get(1), datum, environment, diagnostic);

        // If we have a variable to assign then use that variable. Otherwise leave only the value.
        // Use the same span though.
        return Expression {
            kind: if let Some(variable) = variable {
                ExpressionKind::Assignment(variable, Box::new(new_value))
            } else {
                new_value.kind
            },
            span: datum.span,
            environment: environment.clone(),
        };
    }
}

fn expect_set_form<'a>(
    keyword: Atom,
    datum: &'a ScannedDatum,
    diagnostic: &Handler,
) -> &'a [ScannedDatum] {
    use expanders::utils::{expect_form, missing_last_span};

    let (dotted, terms) = expect_form(keyword, datum);
    let last = terms.len() - 1;

    if terms.len() < 3 {
        diagnostic.report(DiagnosticKind::err_expand_invalid_set, missing_last_span(datum));
    }
    if terms.len() > 3 {
        let extra_forms = Span::new(terms[3].span.from, terms[last].span.to);
        diagnostic.report(DiagnosticKind::err_expand_invalid_set, extra_forms);
    }

    if dotted {
        assert!(terms.len() >= 2);
        let around_dot = Span::new(terms[last - 1].span.to, terms[last].span.from);
        diagnostic.report(DiagnosticKind::err_expand_invalid_set, around_dot);
    }

    return &terms[1..];
}

/// Expand (as a no-op) the variable name in a set! expression.
///
/// It may be missing, or not be a symbol. In either case recover with None.
fn expand_variable(datum: Option<&ScannedDatum>, diagnostic: &Handler) -> Option<Variable> {
    if let Some(datum) = datum {
        if let DatumValue::Symbol(name) = datum.value {
            return Some(Variable {
                name: name,
                span: datum.span,
            });
        }
        diagnostic.report(DiagnosticKind::err_expand_invalid_set, datum.span);
    }
    return None;
}

/// Expand the subexpression denoting variable value in a set! expression.
///
/// In case of errors return an #f literal as a placeholder.
fn expand_value(datum: &ScannedDatum, term: Option<&ScannedDatum>, environment: &Rc<Environment>, diagnostic: &Handler, expander: &OldExpander) -> Expression {
    use expanders::utils::missing_last_span;

    if let Some(term) = term {
        if let ExpansionResult::Some(expression) = expander.expand(term, environment, diagnostic, expander) {
            return expression;
        } else {
            return Expression {
                kind: ExpressionKind::Literal(Literal::Boolean(false)),
                span: term.span,
                environment: environment.clone(),
            };
        }
    } else {
        return Expression {
            kind: ExpressionKind::Literal(Literal::Boolean(false)),
            span: missing_last_span(datum),
            environment: environment.clone(),
        };
    }
}

/// Expand the subexpression denoting variable value in a set! expression.
///
/// In case of errors return an #f literal as a placeholder.
fn expand_new_value(
    term: Option<&ScannedDatum>,
    datum: &ScannedDatum,
    environment: &Rc<Environment>,
    diagnostic: &Handler,
) -> Expression {
    use expand::expand;
    use expanders::utils::missing_last_span;

    term.map(|datum| expand(datum, environment, diagnostic))
        .unwrap_or(Expression {
            kind: ExpressionKind::Literal(Literal::Boolean(false)),
            span: missing_last_span(datum),
            environment: environment.clone(),
        })
}
