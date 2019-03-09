// Copyright (c) 2016, Sabre developers
//
// Licensed under the Apache License, Version 2.0 (see LICENSE.Apache in the
// root directory) or MIT license (see LICENSE.MIT in the root directory),
// at your option. This file may be copied, distributed, and modified only
// in accordance with the terms specified by the chosen license.

//! Scheme datum definitions.
//!
//! This module contains definitions of data recognized and processed by Scheme reader.

use std::fmt;

use liblocus::diagnostics::Span;

use crate::intern_pool::Atom;

/// A scanned datum with extents information. An AST node, if you with.
#[derive(PartialEq, Eq, Clone)]
pub struct ScannedDatum {
    /// The datum itself.
    pub value: DatumValue,

    /// Span of the datum.
    pub span: Span,
}

/// Types of data recognized by the parser.
#[derive(PartialEq, Eq, Clone)]
pub enum DatumValue {
    /// Boolean literal.
    Boolean(bool),

    /// Character literal.
    Character(char),

    /// Number literal.
    Number(Atom),

    /// String literal.
    String(Atom),

    /// Symbol.
    Symbol(Atom),

    /// Bytevector literal.
    Bytevector(Vec<Atom>),

    /// Vector.
    Vector(Vec<ScannedDatum>),

    /// Proper list.
    ProperList(Vec<ScannedDatum>),

    /// Dotted list.
    DottedList(Vec<ScannedDatum>),

    /// Datum with a label attached to it.
    LabeledDatum(Atom, Box<ScannedDatum>),

    /// Reference to a labeled datum.
    LabelReference(Atom),
}

impl fmt::Debug for ScannedDatum {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.value)
    }
}

impl fmt::Debug for DatumValue {
    #[rustfmt::skip]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use crate::format::{write_dotted_list, write_list};

        match *self {
            DatumValue::Boolean(value) =>
                write!(f, "{}", if value { "#t" } else { "#f" }),
            DatumValue::Character(value) =>
                write!(f, "#\\x{:04X}", value as u32),
            DatumValue::Number(value) =>
                write!(f, "{:?}", value),
            DatumValue::String(value) =>
                write!(f, "\"{:?}\"", value),
            DatumValue::Symbol(value) =>
                write!(f, "{:?}", value),
            DatumValue::Bytevector(ref values) =>
                write_list(f, "#u8(", values, " ", ")"),
            DatumValue::Vector(ref values) =>
                write_list(f, "#(", values, " ", ")"),
            DatumValue::ProperList(ref values) =>
                write_list(f, "(", values, " ", ")"),
            DatumValue::DottedList(ref values) =>
                write_dotted_list(f, "(", values, " ", ")"),
            DatumValue::LabeledDatum(label, ref value) =>
                write!(f, "#{:?}={:?}", label, value),
            DatumValue::LabelReference(label) =>
                write!(f, "#{:?}#", label),
        }
    }
}
