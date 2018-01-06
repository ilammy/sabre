// Copyright (c) 2017, Sabre developers
//
// Licensed under the Apache License, Version 2.0 (see LICENSE.Apache in the
// root directory) or MIT license (see LICENSE.MIT in the root directory),
// at your option. This file may be copied, distributed, and modified only
// in accordance with the terms specified by the chosen license.

//! `begin` expander.

use std::rc::{Rc};

use locus::diagnostics::{Handler, DiagnosticKind, Span};
use reader::datum::{ScannedDatum};
use reader::intern_pool::{Atom};

use environment::{Environment};
use expression::{Expression, ExpressionKind};
use expand::Expander;
use expanders::{Expander as OldExpander, ExpansionResult};

/// Expand `begin` special forms into sequences.
pub struct BeginExpander {
    /// Recognized `begin` atom.
    name: Atom,
}

impl BeginExpander {
    /// Make a new `begin` expander for a given name.
    pub fn new(name: Atom) -> BeginExpander {
        BeginExpander {
            name: name,
        }
    }
}

impl OldExpander for BeginExpander {
    fn expand(&self, datum: &ScannedDatum, environment: &Rc<Environment>, diagnostic: &Handler, expander: &OldExpander) -> ExpansionResult {
        use expanders::utils::{is_named_form, expect_list_length_at_least};

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

        return ExpansionResult::Some(Expression {
            kind: ExpressionKind::Sequence(expressions),
            span: datum.span,
            environment: environment.clone(),
        });
    }
}

impl Expander for BeginExpander {
    fn expand_form(
        &self,
        datum: &ScannedDatum,
        environment: &Rc<Environment>,
        diagnostic: &Handler,
    ) -> Expression {
        use expand::expand;

        // The only valid form is (begin expr1 expr2 ...). Expand nested terms in sequence.
        // Expand anything erroneous into an empty sequence after reporting it to the handler.
        let expressions =
            expect_begin_form(self.name, datum, diagnostic)
            .iter()
            .map(|datum| expand(datum, environment, diagnostic))
            .collect();

        return Expression {
            kind: ExpressionKind::Sequence(expressions),
            span: datum.span,
            environment: environment.clone(),
        };
    }
}

fn expect_begin_form<'a>(
    keyword: Atom,
    datum: &'a ScannedDatum,
    diagnostic: &Handler,
) -> &'a [ScannedDatum] {
    use expanders::utils::{expect_form, missing_last_span};

    let (dotted, terms) = expect_form(keyword, datum);

    if terms.len() < 2 {
        diagnostic.report(DiagnosticKind::err_expand_invalid_begin, missing_last_span(datum));
    }

    if dotted && (terms.len() >= 2) {
        assert!(terms.len() >= 2);
        let last = terms.len() - 1;
        let around_dot = Span::new(terms[last - 1].span.to, terms[last].span.from);
        diagnostic.report(DiagnosticKind::err_expand_invalid_begin, around_dot);
    }

    return &terms[1..];
}
