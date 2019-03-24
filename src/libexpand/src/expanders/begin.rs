// Copyright (c) 2017, Sabre developers
//
// Licensed under the Apache License, Version 2.0 (see LICENSE.Apache in the
// root directory) or MIT license (see LICENSE.MIT in the root directory),
// at your option. This file may be copied, distributed, and modified only
// in accordance with the terms specified by the chosen license.

//! `begin` expander.

use std::rc::Rc;

use liblocus::diagnostics::{DiagnosticKind, Handler};
use libreader::datum::ScannedDatum;
use libreader::intern_pool::Atom;

use crate::environment::Environment;
use crate::expand::Expander;
use crate::expression::{Expression, ExpressionKind};

/// Expand `begin` special forms into sequences.
pub struct BeginExpander {
    /// Recognized `begin` atom.
    name: Atom,
}

impl BeginExpander {
    /// Make a new `begin` expander for a given name.
    pub fn new(name: Atom) -> BeginExpander {
        BeginExpander { name }
    }
}

impl Expander for BeginExpander {
    fn expand_form(
        &self,
        datum: &ScannedDatum,
        environment: &Rc<Environment>,
        handler: &Handler,
    ) -> Expression {
        use crate::expand::expand;
        use crate::expanders::utils::expect_macro_use;

        // The only valid form is (begin expr1 expr2 ...). Expand nested terms in sequence.
        // Expand anything erroneous into an empty sequence after reporting it to the handler.
        let terms = expect_macro_use(
            datum,
            self.name,
            2..,
            handler,
            DiagnosticKind::err_expand_invalid_begin,
        );

        let expressions = terms
            .iter()
            .map(|datum| expand(datum, environment, handler))
            .collect::<Vec<_>>();

        Expression {
            kind: if expressions.is_empty() {
                ExpressionKind::Undefined
            } else {
                ExpressionKind::Sequence(expressions)
            },
            span: datum.span,
            environment: environment.clone(),
        }
    }
}
