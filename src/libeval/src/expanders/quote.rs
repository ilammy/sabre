// Copyright (c) 2017, Sabre developers
//
// Licensed under the Apache License, Version 2.0 (see LICENSE.Apache in the
// root directory) or MIT license (see LICENSE.MIT in the root directory),
// at your option. This file may be copied, distributed, and modified only
// in accordance with the terms specified by the chosen license.

//! `quote` expander.

use std::rc::{Rc};

use liblocus::diagnostics::{Handler, DiagnosticKind, Span};
use libreader::datum::{ScannedDatum, DatumValue};
use libreader::intern_pool::{Atom};

use crate::environment::{Environment};
use crate::expression::{Expression, ExpressionKind};
use crate::expanders::{Expander, ExpansionResult};

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
    fn expand(&self, datum: &ScannedDatum, environment: &Rc<Environment>, diagnostic: &Handler, _expand: &Expander) -> ExpansionResult {
        use crate::expanders::utils::{is_named_form, expect_list_length_fixed};

        // Filter out anything that certainly does not look as a quote form.
        let (dotted, values) = match is_named_form(datum, self.name) {
            Some(v) => v,
            None => { return ExpansionResult::Unknown; }
        };

        // The only valid form is (quote datum).
        expect_list_length_fixed(datum, dotted, values, 2,
            diagnostic, DiagnosticKind::err_expand_invalid_quote);

        // Well, even in error cases we can recover. If there are values then return the last one
        // (consistent with `begin`). Otherwise pull an `#f` out of thin air as a placeholder.
        let result = if values.len() > 1 {
            values.last().unwrap().clone()
        } else {
            ScannedDatum {
                value: DatumValue::Boolean(false),
                span: Span::new(values[0].span.to, datum.span.to - 1),
            }
        };

        return ExpansionResult::Some(Expression {
            kind: ExpressionKind::Quotation(result),
            span: datum.span,
            environment: environment.clone(),
        });
    }
}
