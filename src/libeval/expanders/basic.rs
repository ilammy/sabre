// Copyright (c) 2017, Sabre developers
//
// Licensed under the Apache License, Version 2.0 (see LICENSE.Apache in the
// root directory) or MIT license (see LICENSE.MIT in the root directory),
// at your option. This file may be copied, distributed, and modified only
// in accordance with the terms specified by the chosen license.

//! Basic expander.

use locus::diagnostics::{Handler, DiagnosticKind, Span};
use reader::datum::{ScannedDatum, DatumValue};

use expression::{Expression, ExpressionKind, Literal};
use expanders::{Expander, ExpansionResult};

/// Basic expander.
///
/// Expand literals and variable references into themselves. Enforce ban on the datum labels
/// in programs. Ignore all non-atomic forms.
///
/// This expander should be the root expander of the macro expander stack.
pub struct BasicExpander<'a> {
    /// Designated responsible for diagnostic processing.
    diagnostic: &'a Handler,
}

impl<'a> BasicExpander<'a> {
    /// Make a new fixed expander.
    pub fn new(handler: &Handler) -> BasicExpander {
        BasicExpander {
            diagnostic: handler,
        }
    }
}

impl<'a> Expander for BasicExpander<'a> {
    fn expand(&self, datum: &ScannedDatum, expander: &Expander) -> ExpansionResult {
        match datum.value {
            // Simple literal data.
            DatumValue::Boolean(value) => {
                ExpansionResult::Some(Expression {
                    kind: ExpressionKind::Literal(Literal::Boolean(value)),
                    span: Some(datum.span),
                })
            }
            DatumValue::Number(value) => {
                ExpansionResult::Some(Expression {
                    kind: ExpressionKind::Literal(Literal::Number(value)),
                    span: Some(datum.span),
                })
            }
            DatumValue::Character(value) => {
                ExpansionResult::Some(Expression {
                    kind: ExpressionKind::Literal(Literal::Character(value)),
                    span: Some(datum.span),
                })
            }
            DatumValue::String(value) => {
                ExpansionResult::Some(Expression {
                    kind: ExpressionKind::Literal(Literal::String(value)),
                    span: Some(datum.span),
                })
            }

            // Bare symbols mean variable references.
            DatumValue::Symbol(name) => {
                ExpansionResult::Some(Expression {
                    kind: ExpressionKind::Reference(name),
                    span: Some(datum.span),
                })
            }

            // Vectors and bytevectors contain literal data.
            DatumValue::Vector(ref elements) => {
                ExpansionResult::Some(Expression {
                    kind: ExpressionKind::Literal(Literal::Vector(elements.clone())),
                    span: Some(datum.span),
                })
            }
            DatumValue::Bytevector(ref elements) => {
                ExpansionResult::Some(Expression {
                    kind: ExpressionKind::Literal(Literal::Bytevector(elements.clone())),
                    span: Some(datum.span),
                })
            }

            // These are not literal data, so we ignore them.
            DatumValue::ProperList(_) => ExpansionResult::Unknown,
            DatumValue::DottedList(_) => ExpansionResult::Unknown,

            // Labeled data cannot be used in programs, only in literal quoted data.
            // Report the error and assume the label never existed, reinvoking the expander.
            DatumValue::LabeledDatum(_, ref labeled_datum) => {
                self.diagnostic.report(DiagnosticKind::err_expand_datum_label,
                    Span::new(datum.span.from, labeled_datum.span.from));

                expander.expand(&labeled_datum, expander)
            }

            // Datum labels cannot be used in programs, only in literal quoted data.
            // Report the error and use some placeholder value instead.
            DatumValue::LabelReference(_) => {
                self.diagnostic.report(DiagnosticKind::err_expand_datum_label,
                    datum.span);

                ExpansionResult::Some(Expression {
                    kind: ExpressionKind::Literal(Literal::Boolean(false)),
                    span: Some(datum.span),
                })
            }
        }
    }
}
