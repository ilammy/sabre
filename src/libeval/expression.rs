// Copyright (c) 2017, Sabre developers
//
// Licensed under the Apache License, Version 2.0 (see LICENSE.Apache in the
// root directory) or MIT license (see LICENSE.MIT in the root directory),
// at your option. This file may be copied, distributed, and modified only
// in accordance with the terms specified by the chosen license.

//! Core Scheme expressions.

use std::fmt;
use std::rc::{Rc};

use locus::diagnostics::{Span};
use reader::datum::{ScannedDatum};
use reader::intern_pool::{Atom};

use environment::{Environment};

/// Scheme core expression.
pub struct Expression {
    /// Kind of an expression.
    pub kind: ExpressionKind,

    /// Environment available at this expression.
    pub environment: Rc<Environment>,

    /// Source of the expression.
    pub span: Span,
}

/// Kind of an expression.
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
#[derive(Eq, PartialEq)]
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
#[derive(Clone, Eq, PartialEq)]
pub struct Variable {
    /// Name of the variable.
    pub name: Atom,

    /// Closest known source of the reference.
    pub span: Option<Span>,
}

/// Procedure argument list.
#[derive(Eq, PartialEq)]
pub enum Arguments {
    /// Fixed number of arguments.
    Fixed(Vec<Variable>),
}

impl fmt::Debug for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.kind {
            ExpressionKind::Reference(ref variable) =>
                write!(f, "(Reference {:?})", variable),
            ExpressionKind::Literal(ref value) =>
                write!(f, "(Literal {:?})", value),
            ExpressionKind::Quotation(ref value) =>
                write!(f, "(Quotation {:?})", value),
            ExpressionKind::Alternative(ref condition, ref consequent, ref alternate) =>
                write!(f, "(Alternative {:?} {:?} {:?})", condition, consequent, alternate),
            ExpressionKind::Assignment(ref variable, ref value) =>
                write!(f, "(Assignment {:?} {:?})", variable.name, value),
            ExpressionKind::Sequence(ref body) => {
                try!(write!(f, "(Sequence"));
                for expression in body {
                    try!(write!(f, " {:?}", expression));
                }
                try!(write!(f, ")"));
                Ok(())
            }
            ExpressionKind::Abstraction(ref arguments, ref body) => {
                try!(write!(f, "(Abstraction {:?}", arguments));
                for expression in body {
                    try!(write!(f, " {:?}", expression));
                }
                try!(write!(f, ")"));
                Ok(())
            }
            ExpressionKind::Application(ref terms) => {
                try!(write!(f, "(Application"));
                for expression in terms {
                    try!(write!(f, " {:?}", expression));
                }
                try!(write!(f, ")"));
                Ok(())
            }
        }
    }
}

impl fmt::Debug for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use reader::format::write_list;

        match *self {
            Literal::Boolean(value) =>
                if value { write!(f, "#t") } else { write!(f, "#f") },
            Literal::Number(value) =>
                write!(f, "{:?}", value),
            Literal::Character(char) =>
                write!(f, "#\\x{:04X}", char as u32),
            Literal::String(value) =>
                write!(f, "\"{:?}\"", value),
            Literal::Vector(ref values) =>
                write_list(f, "#(", values, " ", ")"),
            Literal::Bytevector(ref values) =>
                write_list(f, "#u8(", values, " ", ")"),
        }
    }
}

impl fmt::Debug for Arguments {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use reader::format::write_list;

        match *self {
            Arguments::Fixed(ref variables) =>
                write_list(f, "(", variables.iter().map(|v| v.name), " ", ")")
        }
    }
}
