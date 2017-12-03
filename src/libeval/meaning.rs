// Copyright (c) 2017, Sabre developers
//
// Licensed under the Apache License, Version 2.0 (see LICENSE.Apache in the
// root directory) or MIT license (see LICENSE.MIT in the root directory),
// at your option. This file may be copied, distributed, and modified only
// in accordance with the terms specified by the chosen license.

//! Scheme expression analyzer.
//!
//! Here we transform core Scheme expressions into their meaning based on the semantics of Scheme.
//! This is the finish line of the front-end.

use locus::diagnostics::{Span};
use reader::datum::{ScannedDatum, DatumValue};
use reader::intern_pool::{Atom};

use expression::{Expression, ExpressionKind, Literal, Variable};

pub trait Environment {
    fn resolve_variable(&self, name: Atom) -> VariableKind;
}

pub enum VariableKind {
    Local {
        depth: usize,
        index: usize,
    },
    Global {
        index: usize,
    },
    Imported {
        index: usize,
    },
    Unresolved,
}

pub struct Meaning {
    pub kind: MeaningKind,
    pub span: Option<Span>,
}

pub enum MeaningKind {
    Constant(Value),
    ShallowArgumentReference(usize),
    DeepArgumentReference(usize, usize),
    GlobalReference(usize),
    ImportedReference(usize),
    Alternative(Box<Meaning>, Box<Meaning>, Box<Meaning>),
    ShallowArgumentSet(usize, Box<Meaning>),
    DeepArgumentSet(usize, usize, Box<Meaning>),
    GlobalSet(usize, Box<Meaning>),
}

pub enum Value {
    Boolean(bool),
    Number(Atom),
    Character(char),
    String(Atom),
}

pub fn meaning(expression: &Expression, environment: &Environment) -> Meaning {
    match expression.kind {
        ExpressionKind::Literal(ref value) => meaning_literal(value, &expression.span),
        ExpressionKind::Quotation(ref datum) => meaning_quote(datum, &expression.span),
        ExpressionKind::Reference(name) => meaning_reference(name, environment, &expression.span),
        ExpressionKind::Alternative(ref condition, ref consequent, ref alternate) =>
            meaning_alternative(condition, consequent, alternate, environment, &expression.span),
        ExpressionKind::Assignment(ref variable, ref value) =>
            meaning_assignment(variable, value.as_ref(), environment, &expression.span),
        _ => unimplemented!(),
    }
}

fn meaning_literal(value: &Literal, span: &Option<Span>) -> Meaning {
    Meaning {
        kind: MeaningKind::Constant(
            match *value {
                Literal::Boolean(value) => Value::Boolean(value),
                Literal::Number(value) => Value::Number(value),
                Literal::Character(value) => Value::Character(value),
                Literal::String(value) => Value::String(value),
                _ => unimplemented!(),
            }
        ),
        span: span.clone(),
    }
}

fn meaning_quote(datum: &ScannedDatum, span: &Option<Span>) -> Meaning {
    Meaning {
        kind: MeaningKind::Constant(
            match datum.value {
                DatumValue::Boolean(value) => Value::Boolean(value),
                DatumValue::Number(value) => Value::Number(value),
                DatumValue::Character(value) => Value::Character(value),
                DatumValue::String(value) => Value::String(value),
                _ => unimplemented!(),
            }
        ),
        span: span.clone(),
    }
}

fn meaning_reference(name: Atom, environment: &Environment, span: &Option<Span>) -> Meaning {
    Meaning {
        kind: match environment.resolve_variable(name)  {
            VariableKind::Local { depth, index } => {
                if depth == 0 {
                    MeaningKind::ShallowArgumentReference(index)
                } else {
                    MeaningKind::DeepArgumentReference(depth, index)
                }
            }
            VariableKind::Global { index } => {
                MeaningKind::GlobalReference(index)
            }
            VariableKind::Imported { index } => {
                MeaningKind::ImportedReference(index)
            }
            VariableKind::Unresolved => {
                // report error
                // provide suggestions
                unimplemented!()
            }
        },
        span: span.clone(),
    }
}

fn meaning_alternative(condition: &Expression, consequent: &Expression, alternate: &Expression,
    environment: &Environment, span: &Option<Span>) -> Meaning
{
    Meaning {
        kind: MeaningKind::Alternative(
            Box::new(meaning(condition, environment)),
            Box::new(meaning(consequent, environment)),
            Box::new(meaning(alternate, environment)),
        ),
        span: span.clone(),
    }
}

fn meaning_assignment(variable: &Variable, value: &Expression, environment: &Environment,
    span: &Option<Span>) -> Meaning
{
    // Note that we use the same environment, not extended with the variable name.
    let new_value = Box::new(meaning(value, environment));

    Meaning {
        kind: match environment.resolve_variable(variable.name) {
            VariableKind::Local { depth, index } => {
                if depth == 0 {
                    MeaningKind::ShallowArgumentSet(index, new_value)
                } else {
                    MeaningKind::DeepArgumentSet(depth, index, new_value)
                }
            }
            VariableKind::Global { index } => {
                MeaningKind::GlobalSet(index, new_value)
            }
            VariableKind::Imported { index } => {
                // report error
                // provide suggestions?
                unimplemented!()
            }
            VariableKind::Unresolved => {
                // report error
                // provide suggestions
                unimplemented!()
            }
        },
        span: span.clone(),
    }
}
