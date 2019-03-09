// Copyright (c) 2017, Sabre developers
//
// Licensed under the Apache License, Version 2.0 (see LICENSE.Apache in the
// root directory) or MIT license (see LICENSE.MIT in the root directory),
// at your option. This file may be copied, distributed, and modified only
// in accordance with the terms specified by the chosen license.

//! `quote` expander.

use std::rc::Rc;

use liblocus::diagnostics::{DiagnosticKind, Handler};
use libreader::datum::ScannedDatum;
use libreader::intern_pool::Atom;

use crate::environment::Environment;
use crate::expand::Expander;
use crate::expression::{Expression, ExpressionKind};

/// Expand `quote` special forms into quotations.
pub struct QuoteExpander {
    /// Recognized `quote` atom.
    name: Atom,
}

impl QuoteExpander {
    /// Make a new `quote` expander for a given name.
    pub fn new(name: Atom) -> QuoteExpander {
        QuoteExpander { name }
    }
}

impl Expander for QuoteExpander {
    fn expand_form(
        &self,
        datum: &ScannedDatum,
        environment: &Rc<Environment>,
        diagnostic: &Handler,
    ) -> Expression {
        use crate::expanders::utils::expect_macro_use;

        // The only valid form is (quote datum). Do not expand anything. Use the first datum
        // available or pull some placeholder out of thin air if this is a (quote) form.
        let terms = expect_macro_use(
            datum,
            self.name,
            2,
            diagnostic,
            DiagnosticKind::err_expand_invalid_quote,
        );

        Expression {
            kind: if let Some(quoted_datum) = terms.first().cloned() {
                ExpressionKind::Quotation(quoted_datum)
            } else {
                ExpressionKind::Undefined
            },
            span: datum.span,
            environment: environment.clone(),
        }
    }
}
