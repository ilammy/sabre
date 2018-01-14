// Copyright (c) 2017, Sabre developers
//
// Licensed under the Apache License, Version 2.0 (see LICENSE.Apache in the
// root directory) or MIT license (see LICENSE.MIT in the root directory),
// at your option. This file may be copied, distributed, and modified only
// in accordance with the terms specified by the chosen license.

//! `quote` expander.

use std::rc::{Rc};

use locus::diagnostics::{Handler, DiagnosticKind, Span};
use reader::datum::{ScannedDatum, DatumValue};
use reader::intern_pool::{Atom};

use environment::{Environment};
use expression::{Expression, ExpressionKind};
use expand::Expander;

/// Expand `quote` special forms into quotations.
pub struct QuoteExpander {
    /// Recognized `quote` atom.
    name: Atom,
}

impl QuoteExpander {
    /// Make a new `quote` expander for a given name.
    pub fn new(name: Atom) -> QuoteExpander {
        QuoteExpander {
            name: name,
        }
    }
}

impl Expander for QuoteExpander {
    fn expand_form(
        &self,
        datum: &ScannedDatum,
        environment: &Rc<Environment>,
        diagnostic: &Handler,
    ) -> Expression {
        use expanders::utils::missing_last_span;

        // The only valid form is (quote datum). Do not expand anything. Use the first datum
        // available or pull some placeholder out of thin air if this is a (quote) form.
        let data = expect_quote_form(self.name, datum, diagnostic);

        let quoted_datum = data.first().cloned().unwrap_or_else(|| ScannedDatum {
            value: DatumValue::Boolean(false),
            span: missing_last_span(datum),
        });

        return Expression {
            kind: ExpressionKind::Quotation(quoted_datum),
            span: datum.span,
            environment: environment.clone(),
        };
    }
}

fn expect_quote_form<'a>(
    keyword: Atom,
    datum: &'a ScannedDatum,
    diagnostic: &Handler,
) -> &'a [ScannedDatum] {
    use expanders::utils::{expect_form, missing_last_span};

    let (dotted, terms) = expect_form(keyword, datum);
    let last = terms.len() - 1;

    if terms.len() < 2 {
        diagnostic.report(DiagnosticKind::err_expand_invalid_quote, missing_last_span(datum));
    }
    if terms.len() > 2 {
        let extra_forms = Span::new(terms[2].span.from, terms[last].span.to);
        diagnostic.report(DiagnosticKind::err_expand_invalid_quote, extra_forms);
    }

    if dotted && (terms.len() == 2) {
        assert!(terms.len() >= 2);
        let around_dot = Span::new(terms[last - 1].span.to, terms[last].span.from);
        diagnostic.report(DiagnosticKind::err_expand_invalid_quote, around_dot);
    }

    return &terms[1..];
}
