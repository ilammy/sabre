// Copyright (c) 2017, Sabre developers
//
// Licensed under the Apache License, Version 2.0 (see LICENSE.Apache in the
// root directory) or MIT license (see LICENSE.MIT in the root directory),
// at your option. This file may be copied, distributed, and modified only
// in accordance with the terms specified by the chosen license.

//! Application expander.

use std::rc::{Rc};

use liblocus::diagnostics::{Handler, DiagnosticKind};
use libreader::datum::{ScannedDatum};

use crate::environment::{Environment};
use crate::expression::{Expression, ExpressionKind};
use crate::expanders::{Expander, ExpansionResult};

/// Expand all procedure calls into applications.
///
/// This expander should follow the basic expander, but come before all specific expanders that
/// recognize special forms or macros.
pub struct ApplicationExpander;

impl ApplicationExpander {
    /// Make a new application expander.
    pub fn new() -> ApplicationExpander {
        ApplicationExpander
    }
}

impl Expander for ApplicationExpander {
    fn expand(&self, datum: &ScannedDatum, environment: &Rc<Environment>, diagnostic: &Handler, expander: &Expander) -> ExpansionResult {
        use crate::expanders::utils::{is_form, expect_list_length_at_least};

        // Filter out anything that certainly does not look as a form.
        let (dotted, values) = match is_form(datum) {
            Some(v) => v,
            None => { return ExpansionResult::Unknown; }
        };

        // The only valid form is (procedure args ...).
        expect_list_length_at_least(datum, dotted, values, 1,
            diagnostic, DiagnosticKind::err_expand_invalid_application);

        let expressions = values[..].iter()
            .filter_map(|datum| match expander.expand(datum, environment, diagnostic, expander) {
                ExpansionResult::Some(expression) => Some(expression),
                ExpansionResult::None => None,
                ExpansionResult::Unknown => None,
            })
            .collect();

        return ExpansionResult::Some(Expression {
            kind: ExpressionKind::Application(expressions),
            span: datum.span,
            environment: environment.clone(),
        });
    }
}
