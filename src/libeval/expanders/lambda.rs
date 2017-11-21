// Copyright (c) 2017, Sabre developers
//
// Licensed under the Apache License, Version 2.0 (see LICENSE.Apache in the
// root directory) or MIT license (see LICENSE.MIT in the root directory),
// at your option. This file may be copied, distributed, and modified only
// in accordance with the terms specified by the chosen license.

//! `lambda` expander.

use locus::diagnostics::{Handler, DiagnosticKind, Span};
use reader::datum::{ScannedDatum, DatumValue};
use reader::intern_pool::{Atom};

use expression::{Expression, ExpressionKind, Variable, Arguments};
use expanders::{Expander, ExpansionResult};

/// Expand `lambda` special forms into abstractions.
pub struct LambdaExpander<'a> {
    /// Recognized `lambda` atom.
    name: Atom,

    /// Designated responsible for diagnostic processing.
    diagnostic: &'a Handler,
}

impl<'a> LambdaExpander<'a> {
    /// Make a new `lambda` expander for a given name.
    pub fn new(name: Atom, handler: &Handler) -> LambdaExpander {
        LambdaExpander {
            name: name,
            diagnostic: handler,
        }
    }
}

impl<'a> Expander for LambdaExpander<'a> {
    fn expand(&self, datum: &ScannedDatum, expander: &Expander) -> ExpansionResult {
        use expanders::utils::{is_named_form, expect_list_length_at_least};

        // Filter out anything that certainly does not look as a lambda form.
        let (dotted, values) = match is_named_form(datum, self.name) {
            Some(v) => v,
            None => { return ExpansionResult::Unknown; }
        };

        // The only valid form is (lambda (variable...) body1 body2...).
        expect_list_length_at_least(datum, dotted, values, 3,
            &self.diagnostic, DiagnosticKind::err_expand_invalid_lambda);

        // The first element describes the abstraction's arguments.
        let arguments = self.expand_arguments(values.get(1));

        // All other elements (except for the first two) are the procedure body.
        // Expand them sequentially, as in the begin form.
        let expressions = values.iter()
            .skip(2)
            .filter_map(|datum| match expander.expand(datum, expander) {
                ExpansionResult::Some(expression) => Some(expression),
                ExpansionResult::None => None,
                ExpansionResult::Unknown => None,
            })
            .collect();

        return ExpansionResult::Some(Expression {
            kind: ExpressionKind::Abstraction(arguments, expressions),
            span: Some(datum.span),
        });
    }
}

impl<'a> LambdaExpander<'a> {
    /// Expand the argument list.
    ///
    /// Simply ignore non-variables in the list, or fall back to empty argument list in case the
    /// problem is really severe.
    fn expand_arguments(&self, datum: Option<&ScannedDatum>) -> Arguments {
        if let Some(arguments) = self.expect_argument_list(datum) {
            let raw_variables: Vec<Variable> = arguments.iter()
                .filter_map(|argument| {
                    if let DatumValue::Symbol(name) = argument.value {
                        Some(Variable { name: name, span: Some(argument.span) })
                    } else {
                        self.diagnostic.report(DiagnosticKind::err_expand_invalid_lambda,
                            argument.span);
                        None
                    }
                })
                .collect();

            let variables = self.deduplicate_variables(raw_variables);

            return Arguments::Fixed(variables);
        }
        return Arguments::Fixed(vec![]);
    }

    /// Extract argument list from the datum (if it really looks like an argument list).
    fn expect_argument_list<'b>(&self, datum: Option<&'b ScannedDatum>)
        -> Option<&'b [ScannedDatum]>
    {
        if let Some(datum) = datum {
            match datum.value {
                DatumValue::ProperList(ref data) => {
                    return Some(&data);
                }
                DatumValue::DottedList(ref data) => {
                    // Currently we do not support &rest arguments, so we unify them into
                    // fixed argument list and produce a diagnostic.
                    assert!(data.len() >= 2);
                    let last = data.len() - 1;
                    self.diagnostic.report(DiagnosticKind::err_expand_invalid_lambda,
                        Span::new(data[last - 1].span.to, data[last].span.from));

                    return Some(&data);
                }
                _ => {
                    self.diagnostic.report(DiagnosticKind::err_expand_invalid_lambda,
                        datum.span);
                }
            }
        }
        return None;
    }

    /// Check that arguments are not duplicated, report and remove duplicates.
    fn deduplicate_variables(&self, raw_variables: Vec<Variable>) -> Vec<Variable> {
        let mut variables: Vec<Variable> = Vec::with_capacity(raw_variables.len());

        // Argument lists should be short, so this O(n^2) algorithm is okay.
        'next_variable:
        for variable in raw_variables {
            for previous in &variables {
                if variable.name == previous.name {
                    self.diagnostic.report(DiagnosticKind::err_expand_invalid_lambda,
                        variable.span.expect("all lambda args have spans"));

                    continue 'next_variable;
                }
            }
            variables.push(variable);
        }

        return variables;
    }
}
