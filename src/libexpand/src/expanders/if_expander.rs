// Copyright (c) 2017, Sabre developers
//
// Licensed under the Apache License, Version 2.0 (see LICENSE.Apache in the
// root directory) or MIT license (see LICENSE.MIT in the root directory),
// at your option. This file may be copied, distributed, and modified only
// in accordance with the terms specified by the chosen license.

//! `if` expander.

use std::rc::Rc;

use liblocus::diagnostics::{DiagnosticKind, Handler};
use libreader::datum::ScannedDatum;
use libreader::intern_pool::Atom;

use crate::environment::Environment;
use crate::expand::Expander;
use crate::expression::{Expression, ExpressionKind};

/// Expand `if` special forms into alternatives.
pub struct IfExpander {
    /// Recognized `if` atom.
    name: Atom,
}

impl IfExpander {
    /// Make a new `if` expander for a given name.
    pub fn new(name: Atom) -> IfExpander {
        IfExpander {
            name,
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
        use crate::expand::expand;
        use crate::expanders::utils::{expect_macro_use, missing_last_span};

        // The only valid form is (if condition consequent alternative). Expand the terms before
        // gathering them up. Replace any missing terms with placeholder values.
        let terms = expect_macro_use(datum, self.name, 4, diagnostic,
            DiagnosticKind::err_expand_invalid_if);

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

        Expression {
            kind: ExpressionKind::Alternative(condition, consequent, alternative),
            span: datum.span,
            environment: environment.clone(),
        }
    }
}
