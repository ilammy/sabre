// Copyright (c) 2017, Sabre developers
//
// Licensed under the Apache License, Version 2.0 (see LICENSE.Apache in the
// root directory) or MIT license (see LICENSE.MIT in the root directory),
// at your option. This file may be copied, distributed, and modified only
// in accordance with the terms specified by the chosen license.

//! `lambda` expander.

use std::rc::Rc;

use liblocus::diagnostics::{DiagnosticKind, Handler, Span};
use libreader::datum::{DatumValue, ScannedDatum};
use libreader::intern_pool::Atom;

use crate::environment::Environment;
use crate::expanders::{Expander, ExpansionResult};
use crate::expression::{Arguments, Expression, ExpressionKind, Variable};

/// Expand `lambda` special forms into abstractions.
pub struct LambdaExpander {
    /// Recognized `lambda` atom.
    name: Atom,
}

impl LambdaExpander {
    /// Make a new `lambda` expander for a given name.
    pub fn new(name: Atom) -> LambdaExpander {
        LambdaExpander {
            name,
        }
    }
}

impl Expander for LambdaExpander {
    fn expand(&self, datum: &ScannedDatum, environment: &Rc<Environment>, diagnostic: &Handler, expander: &dyn Expander) -> ExpansionResult {
        use crate::expanders::utils::{is_named_form, expect_list_length_at_least};

        // Filter out anything that certainly does not look as a lambda form.
        let (dotted, values) = match is_named_form(datum, self.name) {
            Some(v) => v,
            None => { return ExpansionResult::Unknown; }
        };

        // The only valid form is (lambda (variable...) body1 body2...).
        expect_list_length_at_least(datum, dotted, values, 3,
            diagnostic, DiagnosticKind::err_expand_invalid_lambda);

        // The first element describes the abstraction's arguments. They form a new local
        // environment for the procedure body.
        let arguments = expand_arguments(values.get(1), diagnostic);
        let new_environment = new_local_environment(&arguments, environment);

        // All other elements (except for the first two) are the procedure body.
        // Expand them sequentially, as in the begin form.
        let expressions = values.iter()
            .skip(2)
            .filter_map(|datum| match expander.expand(datum, &new_environment, diagnostic, expander) {
                ExpansionResult::Some(expression) => Some(expression),
                ExpansionResult::None => None,
                ExpansionResult::Unknown => None,
            })
            .collect();

        ExpansionResult::Some(Expression {
            kind: ExpressionKind::Abstraction(arguments, expressions),
            span: datum.span,
            environment: environment.clone(), // note that this is *not* the new environment
        })
    }
}

/// Expand the argument list.
///
/// Simply ignore non-variables in the list, or fall back to empty argument list in case the
/// problem is really severe.
fn expand_arguments(datum: Option<&ScannedDatum>, diagnostic: &Handler) -> Arguments {
    match expect_argument_list(datum, diagnostic) {
        Some(arguments) => {
            let raw_variables: Vec<Variable> = arguments.iter()
                .filter_map(|argument| {
                    if let DatumValue::Symbol(name) = argument.value {
                        Some(Variable { name, span: Some(argument.span) })
                    } else {
                        diagnostic.report(DiagnosticKind::err_expand_invalid_lambda,
                                          argument.span);
                        None
                    }
                })
                .collect();

            Arguments::Fixed(deduplicate_variables(raw_variables, diagnostic))
        }
        None => {
            Arguments::Fixed(vec![])
        }
    }
}

/// Extract argument list from the datum (if it really looks like an argument list).
fn expect_argument_list<'b>(datum: Option<&'b ScannedDatum>, diagnostic: &Handler)
    -> Option<&'b [ScannedDatum]>
{
    if let Some(datum) = datum {
        match datum.value {
            DatumValue::ProperList(ref data) => {
                Some(&data)
            }
            DatumValue::DottedList(ref data) => {
                // Currently we do not support &rest arguments, so we unify them into
                // fixed argument list and produce a diagnostic.
                assert!(data.len() >= 2);
                let last = data.len() - 1;
                diagnostic.report(DiagnosticKind::err_expand_invalid_lambda,
                    Span::new(data[last - 1].span.to, data[last].span.from));

                Some(&data)
            }
            _ => {
                diagnostic.report(DiagnosticKind::err_expand_invalid_lambda,
                    datum.span);
                None
            }
        }
    } else {
        None
    }
}

/// Check that arguments are not duplicated, report and remove duplicates.
fn deduplicate_variables(raw_variables: Vec<Variable>, diagnostic: &Handler) -> Vec<Variable> {
    let mut variables: Vec<Variable> = Vec::with_capacity(raw_variables.len());

    // Argument lists should be short, so this O(n^2) algorithm is okay.
    'next_variable:
    for variable in raw_variables {
        for previous in &variables {
            if variable.name == previous.name {
                diagnostic.report(DiagnosticKind::err_expand_invalid_lambda,
                    variable.span.expect("all lambda args have spans"));

                continue 'next_variable;
            }
        }
        variables.push(variable);
    }

    variables
}

/// Extend the parent environment with new variables based on the argument list of a lambda form.
fn new_local_environment(arguments: &Arguments, parent: &Rc<Environment>) -> Rc<Environment> {
    match *arguments {
        Arguments::Fixed(ref variables) => {
            Environment::new_local(variables, parent)
        }
    }
}
