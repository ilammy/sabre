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

use expression::{Expression, ExpressionKind, Literal, Variable, Arguments};

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
    Sequence(Vec<Meaning>),
    ClosureFixed(usize, Box<Meaning>),
    ProcedureCall(Box<Meaning>, Vec<Meaning>),
}

pub enum Value {
    Boolean(bool),
    Number(Atom),
    Character(char),
    String(Atom),
}

pub fn meaning(expression: &Expression, environment: &Environment) -> Meaning {
    Meaning {
        kind: match expression.kind {
            ExpressionKind::Literal(ref value) =>
                meaning_literal(value),
            ExpressionKind::Quotation(ref datum) =>
                meaning_quote(datum),
            ExpressionKind::Reference(name) =>
                meaning_reference(name, environment),
            ExpressionKind::Alternative(ref condition, ref consequent, ref alternate) =>
                meaning_alternative(condition, consequent, alternate, environment),
            ExpressionKind::Assignment(ref variable, ref value) =>
                meaning_assignment(variable, value.as_ref(), environment),
            ExpressionKind::Sequence(ref expressions) =>
                meaning_sequence(expressions, environment),
            ExpressionKind::Abstraction(ref arguments, ref body) =>
                meaning_abstraction(arguments, body, environment),
            ExpressionKind::Application(ref terms) =>
                meaning_application(terms, environment),
        },
        span: expression.span.clone(),
    }
}

fn meaning_literal(value: &Literal) -> MeaningKind {
    MeaningKind::Constant(
        match *value {
            Literal::Boolean(value) => Value::Boolean(value),
            Literal::Number(value) => Value::Number(value),
            Literal::Character(value) => Value::Character(value),
            Literal::String(value) => Value::String(value),
            _ => unimplemented!(),
        }
    )
}

fn meaning_quote(datum: &ScannedDatum) -> MeaningKind {
    MeaningKind::Constant(
        match datum.value {
            DatumValue::Boolean(value) => Value::Boolean(value),
            DatumValue::Number(value) => Value::Number(value),
            DatumValue::Character(value) => Value::Character(value),
            DatumValue::String(value) => Value::String(value),
            _ => unimplemented!(),
        }
    )
}

fn meaning_reference(name: Atom, environment: &Environment) -> MeaningKind {
    match environment.resolve_variable(name)  {
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
    }
}

fn meaning_alternative(condition: &Expression, consequent: &Expression, alternate: &Expression,
    environment: &Environment) -> MeaningKind
{
    MeaningKind::Alternative(
        Box::new(meaning(condition, environment)),
        Box::new(meaning(consequent, environment)),
        Box::new(meaning(alternate, environment)),
    )
}

fn meaning_assignment(variable: &Variable, value: &Expression,
    environment: &Environment) -> MeaningKind
{
    // Note that we use the same environment, not extended with the variable name.
    let new_value = Box::new(meaning(value, environment));

    match environment.resolve_variable(variable.name) {
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
    }
}

fn meaning_sequence(expressions: &[Expression], environment: &Environment) -> MeaningKind {
    assert!(expressions.len() >= 1, "BUG: (begin) not handled");

    MeaningKind::Sequence(
        expressions.iter()
                    .map(|e| meaning(e, environment))
                    .collect()
    )
}

fn meaning_abstraction(arguments: &Arguments, body: &[Expression],
    environment: &Environment) -> MeaningKind
{
    match *arguments {
        Arguments::Fixed(ref variables) =>
            MeaningKind::ClosureFixed(variables.len(),
                Box::new(meaning_abstraction_fixed(variables, body, environment))
            ),
    }
}

fn meaning_abstraction_fixed(arguments: &[Variable], body: &[Expression],
    environment: &Environment) -> Meaning
{
    let new_environment = FixedVariableEnvironment::new(arguments, environment);

    let body_begin = body.first().unwrap().span;
    let body_end = body.last().unwrap().span;
    let body_span =
        if let (Some(span_begin), Some(span_end)) = (body_begin, body_end) {
            Some(Span::new(span_begin.from, span_end.to))
        } else {
            None
        };

    return Meaning {
        kind: meaning_sequence(body, &new_environment),
        span: body_span,
    };
}

struct FixedVariableEnvironment<'a> {
    variables: &'a [Variable],
    upper: &'a Environment,
}

impl<'a> FixedVariableEnvironment<'a> {
    fn new(variables: &'a [Variable], upper: &'a Environment) -> FixedVariableEnvironment<'a> {
        FixedVariableEnvironment {
            variables: variables,
            upper: upper,
        }
    }
}

impl<'a> Environment for FixedVariableEnvironment<'a> {
    fn resolve_variable(&self, name: Atom) -> VariableKind {
        for (index, local) in self.variables.iter().enumerate() {
            if name == local.name {
                return VariableKind::Local { depth: 0, index: index };
            }
        }

        let upper_result = self.upper.resolve_variable(name);

        return if let VariableKind::Local { depth, index } = upper_result {
            VariableKind::Local { depth: depth + 1, index: index }
        } else {
            upper_result
        };
    }
}

fn meaning_application(terms: &[Expression], environment: &Environment) -> MeaningKind {
    assert!(terms.len() >= 1, "BUG: empty application");

    let procedure = Box::new(meaning(&terms[0], environment));
    let arguments = terms[1..].iter().map(|e| meaning(e, environment)).collect();

    return MeaningKind::ProcedureCall(procedure, arguments);
}
