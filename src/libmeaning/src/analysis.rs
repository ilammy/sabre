// Copyright (c) 2019, Sabre developers
//
// Licensed under the Apache License, Version 2.0 (see LICENSE.Apache in the
// root directory) or MIT license (see LICENSE.MIT in the root directory),
// at your option. This file may be copied, distributed, and modified only
// in accordance with the terms specified by the chosen license.

//! Analyzing meaning of expressions.

use std::rc::Rc;

use libexpand::environment::{Environment, ReferenceKind};
use libexpand::expression::{Arguments, Expression, ExpressionKind, Literal, Variable};
use liblocus::diagnostics::{DiagnosticKind, Handler, Span};
use libreader::datum::{DatumValue, ScannedDatum};
use libreader::intern_pool::Atom;

use crate::meaning::{Meaning, MeaningKind};
use crate::sequence::splice_in_sequences;

/// Result of meaning analysis.
pub struct MeaningResult {
    /// Expression body.
    pub sequence: Meaning,
    /// Enumerated constants of the expression.
    pub constants: Vec<Value>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Value {
    // TODO: use actual Scheme values.
    Boolean(bool),
    Number(Atom),
    Character(char),
    String(Atom),
}

/// Analyzes meaning of provided expressions, report errors to provided handler.
pub fn meaning(expressions: &[Expression], diagnostic: &Handler) -> MeaningResult {
    let mut constants = Vec::new();
    let body_sequence = meaning_body(expressions, diagnostic, &mut constants);

    MeaningResult {
        sequence: body_sequence,
        constants,
    }
}

/// Analyzes expression body.
fn meaning_body(
    expressions: &[Expression],
    diagnostic: &Handler,
    constants: &mut Vec<Value>,
) -> Meaning {
    Meaning {
        kind: MeaningKind::Sequence(
            splice_in_sequences(expressions)
                .map(|e| meaning_expression(e, diagnostic, constants))
                .collect(),
        ),
        span: expressions_span(expressions),
    }
}

/// Computes total span of an expression sequence. Assumes that expressions are ordered.
fn expressions_span(expressions: &[Expression]) -> Span {
    // Well, meaning() should not be called with no expressions, but if it does get called then
    // return some bogus span. It's not really an error, but the scanner returns no tokens (and
    // thus no spans) if the file is empty or consists only of comments.
    if expressions.is_empty() {
        return Span::new(0, 0);
    }

    let first = expressions.first().unwrap().span;
    let last = expressions.last().unwrap().span;

    Span::new(first.from, last.to)
}

/// Analyzes expressions.
fn meaning_expression(
    expression: &Expression,
    diagnostic: &Handler,
    constants: &mut Vec<Value>,
) -> Meaning {
    Meaning {
        kind: match expression.kind {
            ExpressionKind::Literal(ref value) => meaning_literal(value, constants),
            ExpressionKind::Quotation(ref datum) => meaning_quote(datum, constants),
            ExpressionKind::Reference(name) => {
                meaning_reference(name, expression.span, &expression.environment, diagnostic)
            }
            ExpressionKind::Alternative(ref condition, ref consequent, ref alternate) => {
                meaning_alternative(condition, consequent, alternate, diagnostic, constants)
            }
            ExpressionKind::Assignment(ref variable, ref value) => meaning_assignment(
                variable,
                value.as_ref(),
                &expression.environment,
                diagnostic,
                constants,
            ),
            ExpressionKind::Sequence(ref expressions) => {
                meaning_sequence(expressions, diagnostic, constants)
            }
            ExpressionKind::Abstraction(ref arguments, ref body) => {
                meaning_abstraction(arguments, body, diagnostic, constants)
            }
            ExpressionKind::Application(ref terms) => {
                meaning_application(terms, diagnostic, constants)
            }
            ExpressionKind::Undefined => MeaningKind::Undefined,
        },
        span: expression.span,
    }
}

/// Analyzes literals.
fn meaning_literal(value: &Literal, constants: &mut Vec<Value>) -> MeaningKind {
    let index = constants.len();

    constants.push(match *value {
        Literal::Boolean(value) => Value::Boolean(value),
        Literal::Number(value) => Value::Number(value),
        Literal::Character(value) => Value::Character(value),
        Literal::String(value) => Value::String(value),
        _ => unimplemented!(),
    });

    MeaningKind::Constant(index)
}

/// Analyzes quotations.
fn meaning_quote(datum: &ScannedDatum, constants: &mut Vec<Value>) -> MeaningKind {
    let index = constants.len();

    constants.push(match datum.value {
        DatumValue::Boolean(value) => Value::Boolean(value),
        DatumValue::Number(value) => Value::Number(value),
        DatumValue::Character(value) => Value::Character(value),
        DatumValue::String(value) => Value::String(value),
        _ => unimplemented!(),
    });

    MeaningKind::Constant(index)
}

/// Analyzes variable references.
fn meaning_reference(
    name: Atom,
    span: Span,
    environment: &Rc<Environment>,
    diagnostic: &Handler,
) -> MeaningKind {
    if let Some(reference) = environment.resolve_variable(name) {
        match reference.kind {
            ReferenceKind::Local { depth, index } => MeaningKind::ArgumentReference(depth, index),
            ReferenceKind::Global { index } => MeaningKind::GlobalReference(index),
            ReferenceKind::Imported { index } => MeaningKind::ImportedReference(index),
            ReferenceKind::Syntactic { .. } => {
                // TODO: provide suggestions based on the environment
                diagnostic.report(DiagnosticKind::err_meaning_reference_to_syntactic_binding,
                    span);

                // Syntactic variables do not have runtime values, so return a placeholder.
                MeaningKind::Undefined
            }
        }
    } else {
        // TODO: provide suggestions based on the environment
        diagnostic.report(DiagnosticKind::err_meaning_unresolved_variable, span);

        // We cannot return an actual value or reference here, so return a poisoned value.
        MeaningKind::Undefined
    }
}

/// Analyzes alternative conditions.
fn meaning_alternative(
    condition: &Expression,
    consequent: &Expression,
    alternate: &Expression,
    diagnostic: &Handler,
    constants: &mut Vec<Value>,
) -> MeaningKind {
    MeaningKind::Alternative(
        Box::new(meaning_expression(condition, diagnostic, constants)),
        Box::new(meaning_expression(consequent, diagnostic, constants)),
        Box::new(meaning_expression(alternate, diagnostic, constants)),
    )
}

/// Analyzes variable assignments.
fn meaning_assignment(
    variable: &Variable,
    value: &Expression,
    environment: &Rc<Environment>,
    diagnostic: &Handler,
    constants: &mut Vec<Value>,
) -> MeaningKind {
    let reference = environment.resolve_variable(variable.name);

    // Report the errors before computing the meaning of the assigned value
    // so that the reported diagnostics are ordered better.
    if let Some(ref reference) = reference {
        if let ReferenceKind::Imported { .. } = reference.kind {
            // TODO: show where the variable is imported from
            diagnostic.report(DiagnosticKind::err_meaning_assign_to_imported_binding,
                variable.span);
        }
        if let ReferenceKind::Syntactic { .. } = reference.kind {
            // TODO: provide suggestions based on the environment
            // TODO: show where the variable is imported from
            diagnostic.report(DiagnosticKind::err_meaning_assign_to_syntactic_binding,
                variable.span);
        }
    } else {
        // TODO: provide suggestions based on the environment
        diagnostic.report(DiagnosticKind::err_meaning_unresolved_variable,
            variable.span);
    }

    let new_value = Box::new(meaning_expression(value, diagnostic, constants));

    if let Some(ref reference) = reference {
        match reference.kind {
            ReferenceKind::Local { depth, index } => {
                MeaningKind::ArgumentSet(depth, index, new_value)
            }
            ReferenceKind::Global { index } => MeaningKind::GlobalSet(index, new_value),
            // We really can't assign to the imported variable, so return the meaning of the new
            // value being computed in order to allow further passes to analyze it if necessary
            // (and see any side-effects and errors that the new value computation may contain).
            //
            // In the same vein, Scheme allows to reassign syntactic binding to a runtime binding
            // via a (define ...) form, but in that case the variable should resolve into Global.
            // So if we see Syntactic here then it's really (set! macro-expander ...) in source
            // which is an error.
            ReferenceKind::Imported { .. } | ReferenceKind::Syntactic { .. } => new_value.kind,
        }
    } else {
        // The same goes for assignments to unresolved variables. Instead of replacing them with
        // MeaningKind::Undefined return the computation of the new value.
        new_value.kind
    }
}

/// Analyzes sequences.
fn meaning_sequence(
    expressions: &[Expression],
    diagnostic: &Handler,
    constants: &mut Vec<Value>,
) -> MeaningKind {
    assert!(!expressions.is_empty(), "BUG: (begin) not handled");

    MeaningKind::Sequence(
        expressions
            .iter()
            .map(|e| meaning_expression(e, diagnostic, constants))
            .collect(),
    )
}

/// Analyzes lambda abstractions.
fn meaning_abstraction(
    arguments: &Arguments,
    body: &[Expression],
    diagnostic: &Handler,
    constants: &mut Vec<Value>,
) -> MeaningKind {
    match *arguments {
        Arguments::Fixed(ref variables) => MeaningKind::ClosureFixed(
            variables.len(),
            Box::new(meaning_body(body, diagnostic, constants)),
        ),
    }
}

/// Analyzes procedure applications.
fn meaning_application(
    terms: &[Expression],
    diagnostic: &Handler,
    constants: &mut Vec<Value>,
) -> MeaningKind {
    assert!(!terms.is_empty(), "BUG: empty application");

    let procedure = Box::new(meaning_expression(&terms[0], diagnostic, constants));
    let arguments = terms[1..]
        .iter()
        .map(|e| meaning_expression(e, diagnostic, constants))
        .collect();

    MeaningKind::ProcedureCall(procedure, arguments)
}
