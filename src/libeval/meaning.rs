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

use std::fmt;
use std::rc::{Rc};
use std::slice;

use locus::diagnostics::{Span, Handler, DiagnosticKind};
use reader::datum::{ScannedDatum, DatumValue};
use reader::intern_pool::{Atom};

use expression::{Expression, ExpressionKind, Literal, Variable, Arguments};
use environment::{Environment, ReferenceKind};

pub struct Meaning {
    pub kind: MeaningKind,
    pub span: Span,
}

pub enum MeaningKind {
    Undefined,
    Constant(usize),
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

impl fmt::Debug for Meaning {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.kind {
            MeaningKind::Undefined =>
                write!(f, "(Undefined)"),
            MeaningKind::Constant(index) =>
                write!(f, "(Constant {})", index),
            MeaningKind::ShallowArgumentReference(index) =>
                write!(f, "(ShallowArgumentReference {})", index),
            MeaningKind::DeepArgumentReference(depth, index) =>
                write!(f, "(DeepArgumentReference {} {})", depth, index),
            MeaningKind::GlobalReference(index) =>
                write!(f, "(GlobalReference {})", index),
            MeaningKind::ImportedReference(index) =>
                write!(f, "(ImportedReference {})", index),
            MeaningKind::Alternative(ref condition, ref consequent, ref alternate) =>
                write!(f, "(Alternative {:?} {:?} {:?})", condition, consequent, alternate),
            MeaningKind::ShallowArgumentSet(index, ref value) =>
                write!(f, "(ShallowArgumentSet {} {:?})", index, value),
            MeaningKind::DeepArgumentSet(depth, index, ref value) =>
                write!(f, "(DeepArgumentSet {} {} {:?})", depth, index, value),
            MeaningKind::GlobalSet(index, ref value) =>
                write!(f, "(GlobalSet {} {:?})", index, value),
            MeaningKind::Sequence(ref computations) => {
                try!(write!(f, "(Sequence"));
                for c in computations {
                    try!(write!(f, " {:?}", c));
                }
                try!(write!(f, ")"));
                Ok(())
            }
            MeaningKind::ClosureFixed(arg_count, ref body) =>
                write!(f, "(ClosureFixed {} {:?})", arg_count, body),
            MeaningKind::ProcedureCall(ref procedure, ref args) => {
                try!(write!(f, "(ProcedureCall {:?}", procedure));
                for a in args {
                    try!(write!(f, " {:?}", a));
                }
                try!(write!(f, ")"));
                Ok(())
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Value {
    Boolean(bool),
    Number(Atom),
    Character(char),
    String(Atom),
}

pub struct MeaningResult {
    pub sequence: Meaning,
    pub constants: Vec<Value>, // TODO: use actual Scheme values
}

struct SequenceSplicingIterator<'a> {
    current: Option<slice::Iter<'a, Expression>>,
    postponed: Vec<slice::Iter<'a, Expression>>,
}

// I personally consider the splicing role of the (begin term ...) form to be a wart on Scheme
// syntax and semantics. It could have been better if it was named "splice". Or did not exist
// at all.
//
// The splicing "begin" form is not very consistent with the sequencing role of the (begin expr1
// exprs ...) form, the 'implicit sequence' of top-level expressions, and the 'internal
// definitions' introduced by lambda forms and syntax derived from them (e.g., letrec). It feels
// that this form was introduced and maintainined purely because of historical reasons.
//
// The main motivation for the splicing "begin" are macros. It is a desirable feature for macros
// to be able to introduce definitions that are visible outside of the immediately expanded
// expression: e.g., define-values define multiple variables which are accessible to expressions
// that come after the "define-values" form.
//
// However, this can be achieved by allowing macros to expand into multiple terms which are then
// spliced into the expansion site. There is no need for a new special form "splice" or for
// overloaded "begin" to achieve that.
//
// With this,
// - "begin" form has only one meaning
// - macros still have rewriting as expansion-time semantics
// - splicing magic is still there if you need it
// - macros do not introduce an implicit scope when they are expanded
// - something like (let () ...) can be used to explicitly delimit the lexical scope for internal
//   definitons if needed
//
// But instead we have a splicing "begin" and have to support it. It sucks to be backwards-
// compatible and to eat histerical raisins, I guess.
//
// Either way, Abathur would certainly like this function name, so that alone justifies it.
fn splice_in_sequences<'a>(expressions: &'a [Expression]) -> SequenceSplicingIterator<'a> {
    SequenceSplicingIterator {
        current: Some(expressions.iter()),
        postponed: Vec::new(),
    }
}

impl<'a> Iterator for SequenceSplicingIterator<'a> {
    type Item = &'a Expression;

    fn next(&mut self) -> Option<&'a Expression> {
        loop {
            // If there is an iterator we're currently walking through then take it.
            // Otherwise we're done and there is nothing to do anymore. Note that
            // we have to *not* borrow self.current as we may need to change it later.
            if let Some(mut iter) = self.current.take() {
                // If the current iterator still has some values in it then take them.
                // Otherwise get back to postponed iterators and try again with them.
                if let Some(expression) = iter.next() {
                    // If the next expression is a sequence then instead of returning it
                    // splice its elements in by iterating over them now and getting back
                    // to the previous iterator later.
                    if let ExpressionKind::Sequence(ref expressions) = expression.kind {
                        self.postponed.push(iter);
                        self.current = Some(expressions.iter());
                        continue;
                    }
                    self.current = Some(iter);
                    return Some(expression);
                }
                self.current = self.postponed.pop();
                continue;
            }
            return None;
        }
    }
}

pub fn meaning(diagnostic: &Handler, expressions: &[Expression]) -> MeaningResult {
    let mut constants = Vec::new();
    let body_sequence = meaning_body(diagnostic, expressions, &mut constants);

    MeaningResult {
        sequence: body_sequence,
        constants: constants,
    }
}

fn meaning_body(
    diagnostic: &Handler,
    expressions: &[Expression],
    constants: &mut Vec<Value>) -> Meaning
{
    Meaning {
        kind: MeaningKind::Sequence(
            splice_in_sequences(expressions)
                .map(|e| meaning_expression(diagnostic, e, constants))
                .collect()
        ),
        span: expressions_span(expressions),
    }
}

fn expressions_span(expressions: &[Expression]) -> Span {
    // Well, meaning() should not be called with no expressions, but if it does get called then
    // return some bogus span. It's not really an error, but the scanner returns no tokens (and
    // thus no spans) if the file is empty or consists only of comments.
    if expressions.is_empty() {
        return Span::new(0, 0);
    }

    let first = expressions.first().unwrap().span;
    let last = expressions.last().unwrap().span;

    return Span::new(first.from, last.to);
}

fn meaning_expression(
    diagnostic: &Handler,
    expression: &Expression,
    constants: &mut Vec<Value>) -> Meaning
{
    Meaning {
        kind: match expression.kind {
            ExpressionKind::Literal(ref value) =>
                meaning_literal(value, constants),
            ExpressionKind::Quotation(ref datum) =>
                meaning_quote(datum, constants),
            ExpressionKind::Reference(name) =>
                meaning_reference(diagnostic, name, expression.span, &expression.environment),
            ExpressionKind::Alternative(ref condition, ref consequent, ref alternate) =>
                meaning_alternative(diagnostic, condition, consequent, alternate, constants),
            ExpressionKind::Assignment(ref variable, ref value) =>
                meaning_assignment(diagnostic, variable, value.as_ref(), &expression.environment, constants),
            ExpressionKind::Sequence(ref expressions) =>
                meaning_sequence(diagnostic, expressions, constants),
            ExpressionKind::Abstraction(ref arguments, ref body) =>
                meaning_abstraction(diagnostic, arguments, body, constants),
            ExpressionKind::Application(ref terms) =>
                meaning_application(diagnostic, terms, constants),
        },
        span: expression.span,
    }
}

fn meaning_literal(value: &Literal, constants: &mut Vec<Value>) -> MeaningKind {
    let index = constants.len();

    constants.push(match *value {
        Literal::Boolean(value) => Value::Boolean(value),
        Literal::Number(value) => Value::Number(value),
        Literal::Character(value) => Value::Character(value),
        Literal::String(value) => Value::String(value),
        _ => unimplemented!(),
    });

    return MeaningKind::Constant(index);
}

fn meaning_quote(datum: &ScannedDatum, constants: &mut Vec<Value>) -> MeaningKind {
    let index = constants.len();

    constants.push(match datum.value {
        DatumValue::Boolean(value) => Value::Boolean(value),
        DatumValue::Number(value) => Value::Number(value),
        DatumValue::Character(value) => Value::Character(value),
        DatumValue::String(value) => Value::String(value),
        _ => unimplemented!(),
    });

    return MeaningKind::Constant(index);
}

fn meaning_reference(
    diagnostic: &Handler,
    name: Atom, span: Span,
    environment: &Rc<Environment>) -> MeaningKind
{
    match environment.resolve_variable(name)  {
        ReferenceKind::Local { depth, index } => {
            if depth == 0 {
                MeaningKind::ShallowArgumentReference(index)
            } else {
                MeaningKind::DeepArgumentReference(depth, index)
            }
        }
        ReferenceKind::Global { index } => {
            MeaningKind::GlobalReference(index)
        }
        ReferenceKind::Imported { index } => {
            MeaningKind::ImportedReference(index)
        }
        ReferenceKind::Unresolved => {
            // TODO: provide suggestions based on the environment
            diagnostic.report(DiagnosticKind::err_meaning_unresolved_variable, span);

            // We cannot return an actual value or reference here, so return a poisoned value.
            MeaningKind::Undefined
        }
    }
}

fn meaning_alternative(
    diagnostic: &Handler,
    condition: &Expression, consequent: &Expression, alternate: &Expression,
    constants: &mut Vec<Value>) -> MeaningKind
{
    MeaningKind::Alternative(
        Box::new(meaning_expression(diagnostic, condition, constants)),
        Box::new(meaning_expression(diagnostic, consequent, constants)),
        Box::new(meaning_expression(diagnostic, alternate, constants)),
    )
}

fn meaning_assignment(
    diagnostic: &Handler,
    variable: &Variable, value: &Expression,
    environment: &Rc<Environment>,
    constants: &mut Vec<Value>) -> MeaningKind
{
    let reference_kind = environment.resolve_variable(variable.name);

    // Report the errors before computing the meaning of the assigned value
    // so that the reported diagnostics are ordered better.
    if let ReferenceKind::Unresolved = reference_kind {
        // TODO: provide suggestions based on the environment
        diagnostic.report(DiagnosticKind::err_meaning_unresolved_variable,
            variable.span);
    }
    if let ReferenceKind::Imported { .. } = reference_kind {
        diagnostic.report(DiagnosticKind::err_meaning_assign_to_imported_binding,
            variable.span);
    }

    let new_value = Box::new(meaning_expression(diagnostic, value, constants));

    match reference_kind {
        ReferenceKind::Local { depth, index } => {
            if depth == 0 {
                MeaningKind::ShallowArgumentSet(index, new_value)
            } else {
                MeaningKind::DeepArgumentSet(depth, index, new_value)
            }
        }
        ReferenceKind::Global { index } => {
            MeaningKind::GlobalSet(index, new_value)
        }
        // Well... in these cases return the meaning of the new value being computed
        // in order to allow further passes to analyze it if necessary (and see any
        // side-effects and errors that the new value computation may contain).
        ReferenceKind::Imported { .. } | ReferenceKind::Unresolved => {
            new_value.kind
        }
    }
}

fn meaning_sequence(
    diagnostic: &Handler,
    expressions: &[Expression],
    constants: &mut Vec<Value>) -> MeaningKind
{
    assert!(expressions.len() >= 1, "BUG: (begin) not handled");

    MeaningKind::Sequence(
        expressions.iter()
                    .map(|e| meaning_expression(diagnostic, e, constants))
                    .collect()
    )
}

fn meaning_abstraction(
    diagnostic: &Handler,
    arguments: &Arguments, body: &[Expression],
    constants: &mut Vec<Value>) -> MeaningKind
{
    match *arguments {
        Arguments::Fixed(ref variables) =>
            MeaningKind::ClosureFixed(variables.len(),
                Box::new(meaning_body(diagnostic, body, constants))
            ),
    }
}

fn meaning_application(
    diagnostic: &Handler,
    terms: &[Expression],
    constants: &mut Vec<Value>) -> MeaningKind
{
    assert!(terms.len() >= 1, "BUG: empty application");

    let procedure = Box::new(meaning_expression(diagnostic, &terms[0], constants));
    let arguments = terms[1..].iter()
        .map(|e| meaning_expression(diagnostic, e, constants))
        .collect();

    return MeaningKind::ProcedureCall(procedure, arguments);
}
