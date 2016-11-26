// Copyright (c) 2016, Sabre developers
//
// Licensed under the Apache License, Version 2.0 (see LICENSE.Apache in the
// root directory) or MIT license (see LICENSE.MIT in the root directory),
// at your option. This file may be copied, distributed, and modified only
// in accordance with the terms specified by the chosen license.

//! Scheme datum definitions.
//!
//! This module contains definitions of data recognized and processed by Scheme reader.

use diagnostics::{Span};
use intern_pool::Atom;
use tokens::Token;

/// A scanned datum with extents information. An AST node, if you with.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ScannedDatum {
    /// The datum itself.
    pub value: DatumValue,

    /// Span of the datum.
    pub span: Span,
}

/// Types of data recognized by the parser.
#[derive(Debug, PartialEq, Eq, Clone)]
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
}
