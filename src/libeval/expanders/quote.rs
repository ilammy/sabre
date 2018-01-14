// Copyright (c) 2017, Sabre developers
//
// Licensed under the Apache License, Version 2.0 (see LICENSE.Apache in the
// root directory) or MIT license (see LICENSE.MIT in the root directory),
// at your option. This file may be copied, distributed, and modified only
// in accordance with the terms specified by the chosen license.

//! `quote` expander.

use std::rc::{Rc};

use locus::diagnostics::{Handler, DiagnosticKind};
use reader::datum::{ScannedDatum};
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
        use expanders::utils::expect_macro_use;

        // The only valid form is (quote datum). Do not expand anything. Use the first datum
        // available or pull some placeholder out of thin air if this is a (quote) form.
        let terms = expect_macro_use(datum, self.name, 2, diagnostic,
            DiagnosticKind::err_expand_invalid_quote);

        return Expression {
            kind: if let Some(quoted_datum) = terms.first().cloned() {
                ExpressionKind::Quotation(quoted_datum)
            } else {
                ExpressionKind::Undefined
            },
            span: datum.span,
            environment: environment.clone(),
        };
    }
}
