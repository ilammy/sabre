// Copyright (c) 2017, Sabre developers
//
// Licensed under the Apache License, Version 2.0 (see LICENSE.Apache in the
// root directory) or MIT license (see LICENSE.MIT in the root directory),
// at your option. This file may be copied, distributed, and modified only
// in accordance with the terms specified by the chosen license.

//! Scheme macro expander.
//!
//! This module defines the basic macro expansion engine (the `expand()` function), the macro
//! transformer interface (the `Expander` trait), and core Scheme expressions (the `*Expander`
//! structs). Core expressions are not 'macros' in a strict sense, but they do convert data
//! into code, just like user-defined macros do. Sometimes they are called  _magic keywords_.

use std::rc::Rc;

use liblocus::diagnostics::{DiagnosticKind, Handler, Span};
use libreader::datum::{DatumValue, ScannedDatum};
use libreader::intern_pool::Atom;

use crate::environment::{Environment, ReferenceKind};
use crate::expression::{Expression, ExpressionKind, Literal};

/// Trait of all macro expanders.
pub trait Expander {
    /// Expand a macro use.
    ///
    /// Macro uses are always forms, possibly dotted. `datum` represents the form itself which
    /// should be a ProperList, DottedList, or Vector (though it depends on the expander).
    ///
    /// The provided environment should be used for expanding any nested forms, it also
    /// should be stored in the resulting expressions.
    ///
    /// The expansion always succeeds, in the sense that it always returns some expression.
    /// Though, the resulting expression may be garbage or bogus due to error recovery.
    /// Any errors should be reported to the provided handler.
    fn expand_form(
        &self,
        datum: &ScannedDatum,
        environment: &Rc<Environment>,
        diagnosic: &Handler,
    ) -> Expression;
}

/// Expand macros in the provided datum.
///
/// The provided environment is used to lookup macro definitions and to store defined variables.
/// Any expansion errors are reported to the provided handler (the resulting expression may be
/// garbled due to error recovery).
pub fn expand(
    datum: &ScannedDatum,
    environment: &Rc<Environment>,
    diagnostic: &Handler,
) -> Expression {
    match datum.value {
        // Literal data does not contain any macros, it is retured as is.
        DatumValue::Boolean(value) =>
            literal(datum, environment, Literal::Boolean(value)),
        DatumValue::Number(value) =>
            literal(datum, environment, Literal::Number(value)),
        DatumValue::Character(value) =>
            literal(datum, environment, Literal::Character(value)),
        DatumValue::String(value) =>
            literal(datum, environment, Literal::String(value)),
        DatumValue::Vector(ref elements) =>
            literal(datum, environment, Literal::Vector(elements.clone())),
        DatumValue::Bytevector(ref elements) =>
            literal(datum, environment, Literal::Bytevector(elements.clone())),

        // R7RS Scheme does not support identifier macros, so symbols always mean references.
        DatumValue::Symbol(name) =>
            reference(datum, environment, name),

        // Lists denote forms, so proceed with macro expansion in them.
        DatumValue::ProperList(ref terms) | DatumValue::DottedList(ref terms) =>
            form(datum, terms, environment, diagnostic),

        // Labeled data cannot be used in programs, only in literal quoted data.
        // Report the error and assume the label never existed, reinvoking the expander.
        DatumValue::LabeledDatum(_, ref labeled_datum) => {
            diagnostic.report(DiagnosticKind::err_expand_datum_label,
                Span::new(datum.span.from, labeled_datum.span.from));

            expand(labeled_datum, environment, diagnostic)
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

/// Make a literal expansion result.
fn literal(datum: &ScannedDatum, environment: &Rc<Environment>, value: Literal) -> Expression {
    Expression {
        kind: ExpressionKind::Literal(value),
        span: datum.span,
        environment: environment.clone(),
    }
}

/// Make a reference expansion result.
fn reference(datum: &ScannedDatum, environment: &Rc<Environment>, name: Atom) -> Expression {
    Expression {
        kind: ExpressionKind::Reference(name),
        span: datum.span,
        environment: environment.clone(),
    }
}

/// Expand macros in a form.
fn form(
    datum: &ScannedDatum,
    terms: &[ScannedDatum],
    environment: &Rc<Environment>,
    diagnosic: &Handler,
) -> Expression {
    // There are three forms of forms in Scheme.
    //
    // The first one is (), an empty list. It is invalid in Scheme and has no prescribed meaning.
    // Report an error and return some placeholder value.
    if terms.is_empty() {
        diagnosic.report(DiagnosticKind::err_expand_invalid_application,
            Span::new(datum.span.from + 1, datum.span.to - 1));
        return Expression {
            kind: ExpressionKind::Application(vec![]),
            span: datum.span,
            environment: environment.clone(),
        };
    }

    // Macro uses always have a (<keyword> . <data>) form where <keyword> is an identifier bound
    // to a syntactic keyword, and <data> is a proper, improper, or empty list of data. It is
    // the binding which distinguishes a macro use from a procedure call (an application).
    if let Some(expander) = is_macro_use(terms, environment) {
        return expander.expand_form(datum, environment, diagnosic);
    }

    // Applications have a form (procedure arguments ...) which must be a proper list. Report
    // dotted lists and expand all terms before placing them into a corresponding expression.
    if let DatumValue::DottedList(..) = datum.value {
        assert!(terms.len() >= 2);
        let last = terms.len() - 1;
        diagnosic.report(DiagnosticKind::err_expand_invalid_application,
            Span::new(terms[last - 1].span.to, terms[last].span.from));
    }

    let expressions = terms.iter()
        .map(|datum| expand(datum, environment, diagnosic))
        .collect();

    Expression {
        kind: ExpressionKind::Application(expressions),
        span: datum.span,
        environment: environment.clone(),
    }
}

/// Check if terms denote a macro use. Return the corresponding expander if they are.
fn is_macro_use<'a>(terms: &[ScannedDatum], environment: &'a Environment) -> Option<&'a Expander> {
    assert!(!terms.is_empty());
    if let DatumValue::Symbol(name) = terms[0].value {
        if let Some(variable) = environment.resolve_variable(name) {
            if let ReferenceKind::Syntactic { expander } = variable.kind {
                return Some(expander);
            }
        }
    }
    None
}
