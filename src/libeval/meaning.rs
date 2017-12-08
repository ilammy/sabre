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

use std::rc::{Rc};

use locus::diagnostics::{Span, Handler, DiagnosticKind};
use reader::datum::{ScannedDatum, DatumValue};
use reader::intern_pool::{Atom};

use expression::{Expression, ExpressionKind, Literal, Variable, Arguments};

pub struct Environment {
    kind: EnvironmentKind,
    variables: Vec<Variable>,
    parent: Option<Rc<Environment>>,
}

#[derive(Eq, PartialEq)]
enum EnvironmentKind {
    Local,
    Global,
    Imported,
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

impl Environment {
    pub fn new_local(variables: &[Variable], parent: &Rc<Environment>) -> Rc<Environment> {
        Rc::new(Environment {
            kind: EnvironmentKind::Local,
            variables: variables.to_vec(),
            parent: Some(parent.clone()),
        })
    }

    pub fn new_global(variables: &[Variable], parent: &Rc<Environment>) -> Rc<Environment> {
        assert!(parent.kind == EnvironmentKind::Imported);
        Rc::new(Environment {
            kind: EnvironmentKind::Global,
            variables: variables.to_vec(),
            parent: Some(parent.clone()),
        })
    }

    pub fn new_imported(variables: &[Variable]) -> Rc<Environment> {
        Rc::new(Environment {
            kind: EnvironmentKind::Imported,
            variables: variables.to_vec(),
            parent: None,
        })
    }

    pub fn resolve_variable(&self, name: Atom) -> VariableKind {
        // First, try to resolve the name locally.
        for (index, local) in self.variables.iter().enumerate() {
            if name == local.name {
                return match self.kind {
                    EnvironmentKind::Local => VariableKind::Local { index, depth: 0 },
                    EnvironmentKind::Global => VariableKind::Global { index },
                    EnvironmentKind::Imported => VariableKind::Imported { index },
                };
            }
        }

        // If that fails then look into parent environment (if it's available).
        if let Some(ref parent) = self.parent {
            let mut variable = parent.resolve_variable(name);
            // Bump the nesting depth for local variables.
            if let VariableKind::Local { ref mut depth, .. } = variable {
                *depth += 1;
            }
            return variable;
        }

        // The variable cannot be resolved if it is absent in all environments.
        return VariableKind::Unresolved;
    }
}

pub struct Meaning {
    pub kind: MeaningKind,
    pub span: Option<Span>,
}

pub enum MeaningKind {
    Undefined,
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

pub fn meaning(
    diagnostic: &Handler,
    expression: &Expression,
    environment: &Rc<Environment>) -> Meaning
{
    Meaning {
        kind: match expression.kind {
            ExpressionKind::Literal(ref value) =>
                meaning_literal(value),
            ExpressionKind::Quotation(ref datum) =>
                meaning_quote(datum),
            ExpressionKind::Reference(name) =>
                meaning_reference(diagnostic, name, &expression.span, environment),
            ExpressionKind::Alternative(ref condition, ref consequent, ref alternate) =>
                meaning_alternative(diagnostic, condition, consequent, alternate, environment),
            ExpressionKind::Assignment(ref variable, ref value) =>
                meaning_assignment(diagnostic, variable, value.as_ref(), environment),
            ExpressionKind::Sequence(ref expressions) =>
                meaning_sequence(diagnostic, expressions, environment),
            ExpressionKind::Abstraction(ref arguments, ref body) =>
                meaning_abstraction(diagnostic, arguments, body, environment),
            ExpressionKind::Application(ref terms) =>
                meaning_application(diagnostic, terms, environment),
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

fn meaning_reference(
    diagnostic: &Handler,
    name: Atom, span: &Option<Span>,
    environment: &Rc<Environment>) -> MeaningKind
{
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
            // TODO: provide suggestions based on the environment
            diagnostic.report(DiagnosticKind::err_meaning_unresolved_variable,
                span.expect("BUG: unresolved variable").clone());

            // We cannot return an actual value or reference here, so return a poisoned value.
            MeaningKind::Undefined
        }
    }
}

fn meaning_alternative(
    diagnostic: &Handler,
    condition: &Expression, consequent: &Expression, alternate: &Expression,
    environment: &Rc<Environment>) -> MeaningKind
{
    MeaningKind::Alternative(
        Box::new(meaning(diagnostic, condition, environment)),
        Box::new(meaning(diagnostic, consequent, environment)),
        Box::new(meaning(diagnostic, alternate, environment)),
    )
}

fn meaning_assignment(
    diagnostic: &Handler,
    variable: &Variable, value: &Expression,
    environment: &Rc<Environment>) -> MeaningKind
{
    let variable_kind = environment.resolve_variable(variable.name);

    // Report the errors before computing the meaning of the assigned value
    // so that the reported diagnostics are ordered better.
    if let VariableKind::Unresolved = variable_kind {
        // TODO: provide suggestions based on the environment
        diagnostic.report(DiagnosticKind::err_meaning_unresolved_variable,
            variable.span.expect("BUG: unresolved variable").clone());
    }
    if let VariableKind::Imported { .. } = variable_kind {
        diagnostic.report(DiagnosticKind::err_meaning_assign_to_imported_binding,
            variable.span.expect("BUG: unresolved variable").clone());
    }

    // Note that we use the same environment, not extended with the variable name.
    let new_value = Box::new(meaning(diagnostic, value, environment));

    match variable_kind {
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
        // Well... in these cases return the meaning of the new value being computed
        // in order to allow further passes to analyze it if necessary (and see any
        // side-effects and errors that the new value computation may contain).
        VariableKind::Imported { .. } | VariableKind::Unresolved => {
            new_value.kind
        }
    }
}

fn meaning_sequence(
    diagnostic: &Handler,
    expressions: &[Expression],
    environment: &Rc<Environment>) -> MeaningKind
{
    assert!(expressions.len() >= 1, "BUG: (begin) not handled");

    MeaningKind::Sequence(
        expressions.iter()
                    .map(|e| meaning(diagnostic, e, environment))
                    .collect()
    )
}

fn meaning_abstraction(
    diagnostic: &Handler,
    arguments: &Arguments, body: &[Expression],
    environment: &Rc<Environment>) -> MeaningKind
{
    match *arguments {
        Arguments::Fixed(ref variables) =>
            MeaningKind::ClosureFixed(variables.len(),
                Box::new(meaning_abstraction_fixed(diagnostic, variables, body, environment))
            ),
    }
}

fn meaning_abstraction_fixed(
    diagnostic: &Handler,
    arguments: &[Variable], body: &[Expression],
    environment: &Rc<Environment>) -> Meaning
{
    let new_environment = Environment::new_local(arguments, environment);

    let body_begin = body.first().unwrap().span;
    let body_end = body.last().unwrap().span;
    let body_span =
        if let (Some(span_begin), Some(span_end)) = (body_begin, body_end) {
            Some(Span::new(span_begin.from, span_end.to))
        } else {
            None
        };

    return Meaning {
        kind: meaning_sequence(diagnostic, body, &new_environment),
        span: body_span,
    };
}

fn meaning_application(
    diagnostic: &Handler,
    terms: &[Expression],
    environment: &Rc<Environment>) -> MeaningKind
{
    assert!(terms.len() >= 1, "BUG: empty application");

    let procedure = Box::new(meaning(diagnostic, &terms[0], environment));
    let arguments = terms[1..].iter().map(|e| meaning(diagnostic, e, environment)).collect();

    return MeaningKind::ProcedureCall(procedure, arguments);
}
