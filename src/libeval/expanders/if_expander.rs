// Copyright (c) 2017, Sabre developers
//
// Licensed under the Apache License, Version 2.0 (see LICENSE.Apache in the
// root directory) or MIT license (see LICENSE.MIT in the root directory),
// at your option. This file may be copied, distributed, and modified only
// in accordance with the terms specified by the chosen license.

//! `if` expander.

use std::rc::{Rc};

use locus::diagnostics::{Handler, DiagnosticKind, Span};
use reader::datum::{ScannedDatum};
use reader::intern_pool::{Atom};

use environment::{Environment};
use expression::{Expression, ExpressionKind, Literal};
use expand::Expander;
use expanders::{Expander as OldExpander, ExpansionResult};

/// Expand `if` special forms into alternatives.
pub struct IfExpander {
    /// Recognized `if` atom.
    name: Atom,
}

impl IfExpander {
    /// Make a new `if` expander for a given name.
    pub fn new(name: Atom) -> IfExpander {
        IfExpander {
            name: name,
        }
    }
}

impl OldExpander for IfExpander {
    fn expand(&self, datum: &ScannedDatum, environment: &Rc<Environment>, diagnostic: &Handler, expander: &OldExpander) -> ExpansionResult {
        use expanders::utils::{is_named_form, expect_list_length_fixed, missing_last_span};

        // Filter out anything that certainly does not look as a if form.
        let (dotted, values) = match is_named_form(datum, self.name) {
            Some(v) => v,
            None => { return ExpansionResult::Unknown; }
        };

        assert!(values.len() >= 1);

        // The only valid form is (if condition consequence alternative).
        expect_list_length_fixed(datum, dotted, values, 4,
            diagnostic, DiagnosticKind::err_expand_invalid_if);

        // Recover from errors by using #f as placeholder values.
        let expand_or_recover = |index| {
            if let Some(term) = values.get(index) {
                let result = expander.expand(term, environment, diagnostic, expander);
                if let ExpansionResult::Some(expression) = result {
                    return expression;
                }
            }
            return Expression {
                kind: ExpressionKind::Literal(Literal::Boolean(false)),
                span: missing_last_span(datum),
                environment: environment.clone(),
            };
        };
        let condition   = Box::new(expand_or_recover(1));
        let consequence = Box::new(expand_or_recover(2));
        let alternative = Box::new(expand_or_recover(3));

        return ExpansionResult::Some(Expression {
            kind: ExpressionKind::Alternative(condition, consequence, alternative),
            span: datum.span,
            environment: environment.clone(),
        });
    }
}

impl Expander for IfExpander {
    fn expand_form(
        &self,
        datum: &ScannedDatum,
        environment: &Rc<Environment>,
        diagnostic: &Handler,
    ) -> Expression {
        use expand::expand;
        use expanders::utils::missing_last_span;

        // The only valid form is (if condition consequent alternative). Expand the terms before
        // gathering them up. Replace any missing terms with placeholder values.
        let terms = expect_if_form(self.name, datum, diagnostic);

        let expand_or_recover = |term: Option<&ScannedDatum>| {
            term.map(|datum| expand(datum, environment, diagnostic))
                .unwrap_or(Expression {
                    kind: ExpressionKind::Literal(Literal::Boolean(false)),
                    span: missing_last_span(datum),
                    environment: environment.clone(),
                })
        };
        let condition   = Box::new(expand_or_recover(terms.get(0)));
        let consequent  = Box::new(expand_or_recover(terms.get(1)));
        let alternative = Box::new(expand_or_recover(terms.get(2)));

        return Expression {
            kind: ExpressionKind::Alternative(condition, consequent, alternative),
            span: datum.span,
            environment: environment.clone(),
        };
    }
}

fn expect_if_form<'a>(
    keyword: Atom,
    datum: &'a ScannedDatum,
    diagnostic: &Handler,
) -> &'a [ScannedDatum] {
    use expanders::utils::{expect_form, missing_last_span};

    let (dotted, terms) = expect_form(keyword, datum);
    let last = terms.len() - 1;

    if terms.len() < 4 {
        diagnostic.report(DiagnosticKind::err_expand_invalid_if, missing_last_span(datum));
    }
    if terms.len() > 4 {
        let extra_forms = Span::new(terms[4].span.from, terms[last].span.to);
        diagnostic.report(DiagnosticKind::err_expand_invalid_if, extra_forms);
    }

    if dotted {
        assert!(terms.len() >= 2);
        let around_dot = Span::new(terms[last - 1].span.to, terms[last].span.from);
        diagnostic.report(DiagnosticKind::err_expand_invalid_if, around_dot);
    }

    return &terms[1..];
}
