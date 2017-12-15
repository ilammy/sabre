// Copyright (c) 2017, Sabre developers
//
// Licensed under the Apache License, Version 2.0 (see LICENSE.Apache in the
// root directory) or MIT license (see LICENSE.MIT in the root directory),
// at your option. This file may be copied, distributed, and modified only
// in accordance with the terms specified by the chosen license.

//! Core Scheme expressions.

use locus::diagnostics::{Span};
use reader::datum::{ScannedDatum};
use reader::intern_pool::{Atom};

/// Scheme core expression.
#[derive(Debug, Eq, PartialEq)]
pub struct Expression {
    /// Kind of an expression.
    pub kind: ExpressionKind,

    /// Closest known source of the expression.
    pub span: Option<Span>,
}

/// Kind of an expression.
#[derive(Debug, Eq, PartialEq)]
pub enum ExpressionKind {
    /// Variable reference.
    Reference(Atom),

    /// Self-evaluating literal value.
    Literal(Literal),

    /// `quote` special form.
    Quotation(ScannedDatum),

    /// `if` special form.
    Alternative(Box<Expression>, Box<Expression>, Box<Expression>),

    /// `begin` special form.
    Sequence(Vec<Expression>),

    /// `set!` special form.
    Assignment(Variable, Box<Expression>),

    /// `lambda` special form.
    Abstraction(Arguments, Vec<Expression>),

    /// Procedure call.
    Application(Vec<Expression>),
}

/// Literal value.
#[derive(Debug, Eq, PartialEq)]
pub enum Literal {
    /// Canonical boolean value.
    Boolean(bool),

    /// Number literal.
    Number(Atom),

    /// Character literal.
    Character(char),

    /// String literal.
    String(Atom),

    /// Vector literal.
    Vector(Vec<ScannedDatum>),

    /// Bytevector literal.
    Bytevector(Vec<Atom>),
}

/// Standalone variable reference.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Variable {
    /// Name of the variable.
    pub name: Atom,

    /// Closest known source of the reference.
    pub span: Option<Span>,
}

/// Procedure argument list.
#[derive(Debug, Eq, PartialEq)]
pub enum Arguments {
    /// Fixed number of arguments.
    Fixed(Vec<Variable>),
}
