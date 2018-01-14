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
use expression::{Expression, ExpressionKind};
use expand::Expander;

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
                    kind: ExpressionKind::Undefined,
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
