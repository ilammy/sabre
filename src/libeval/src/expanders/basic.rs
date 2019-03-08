// Copyright (c) 2017, Sabre developers
//
// Licensed under the Apache License, Version 2.0 (see LICENSE.Apache in the
// root directory) or MIT license (see LICENSE.MIT in the root directory),
// at your option. This file may be copied, distributed, and modified only
// in accordance with the terms specified by the chosen license.

//! Basic expander.

use std::rc::{Rc};

use liblocus::diagnostics::{Handler, DiagnosticKind, Span};
use libreader::datum::{ScannedDatum, DatumValue};

use crate::environment::{Environment};
use crate::expression::{Expression, ExpressionKind, Literal};
use crate::expanders::{Expander, ExpansionResult};

/// Basic expander.
///
/// Expand literals and variable references into themselves. Enforce ban on the datum labels
/// in programs. Ignore all non-atomic forms.
///
/// This expander should be the root expander of the macro expander stack.
pub struct BasicExpander;

impl BasicExpander {
    /// Make a new fixed expander.
    pub fn new() -> BasicExpander {
        BasicExpander
    }
}

impl Expander for BasicExpander {
    fn expand(&self, datum: &ScannedDatum, environment: &Rc<Environment>, diagnostic: &Handler, expander: &dyn Expander) -> ExpansionResult {
        match datum.value {
            // Simple literal data.
            DatumValue::Boolean(value) =>
                literal(datum, environment, Literal::Boolean(value)),
            DatumValue::Number(value) =>
                literal(datum, environment, Literal::Number(value)),
            DatumValue::Character(value) =>
                literal(datum, environment, Literal::Character(value)),
            DatumValue::String(value) =>
                literal(datum, environment, Literal::String(value)),

            // Bare symbols mean variable references.
            DatumValue::Symbol(name) => {
                ExpansionResult::Some(Expression {
                    kind: ExpressionKind::Reference(name),
                    span: datum.span,
                    environment: environment.clone(),
                })
            }

            // Vectors and bytevectors contain literal data.
            DatumValue::Vector(ref elements) =>
                literal(datum, environment, Literal::Vector(elements.clone())),
            DatumValue::Bytevector(ref elements) =>
                literal(datum, environment, Literal::Bytevector(elements.clone())),

            // These are not literal data, so we ignore them.
            DatumValue::ProperList(_) => ExpansionResult::Unknown,
            DatumValue::DottedList(_) => ExpansionResult::Unknown,

            // Labeled data cannot be used in programs, only in literal quoted data.
            // Report the error and assume the label never existed, reinvoking the expander.
            DatumValue::LabeledDatum(_, ref labeled_datum) => {
                diagnostic.report(DiagnosticKind::err_expand_datum_label,
                    Span::new(datum.span.from, labeled_datum.span.from));

                expander.expand(&labeled_datum, environment, diagnostic, expander)
            }

            // Datum labels cannot be used in programs, only in literal quoted data.
            // Report the error and use some placeholder value instead.
            DatumValue::LabelReference(_) => {
                diagnostic.report(DiagnosticKind::err_expand_datum_label,
                    datum.span);

                literal(datum, environment, Literal::Boolean(false))
            }
        }
    }
}

fn literal(datum: &ScannedDatum, environment: &Rc<Environment>, value: Literal)
    -> ExpansionResult
{
    ExpansionResult::Some(Expression {
        kind: ExpressionKind::Literal(value),
        span: datum.span,
        environment: environment.clone(),
    })
}
