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

use expression::{Expression, ExpressionKind, Literal};

pub trait Environment {
}

pub struct Meaning {
    pub kind: MeaningKind,
    pub span: Option<Span>,
}

pub enum MeaningKind {
    Constant(Value),
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
