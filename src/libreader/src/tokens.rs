// Copyright (c) 2016, Sabre developers
//
// Licensed under the Apache License, Version 2.0 (see LICENSE.Apache in the
// root directory) or MIT license (see LICENSE.MIT in the root directory),
// at your option. This file may be copied, distributed, and modified only
// in accordance with the terms specified by the chosen license.

//! Scheme token definitions.
//!
//! This module contains definitions of tokens recognized and processed by Scheme reader.

use crate::intern_pool::Atom;

/// Types of tokens recognized by the scanner.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Token {
    /// Marker token denoting the end-of-token-stream condition.
    Eof,

    /// Non-significant whitespace.
    Whitespace,

    /// Non-significant comment.
    Comment,

    /// S-expression comment prefix `#;`.
    CommentPrefix,

    /// Apostrophe (single quote): `'`.
    Quote,

    /// Grave accent (backquote): <code>`</code>.
    Backquote,

    /// Single dot: `.`.
    Dot,

    /// Comma: `,`.
    Comma,

    /// Comma-at-sign: `,@`.
    CommaSplicing,

    /// Opening parenthesis: `(`, `[`, `{`.
    Open(ParenType),

    /// Opening parenthesis of vector literals: `#(`, `#[`, `#{`.
    OpenVector(ParenType),

    /// Opening parenthesis of bytevector literals: `#u8(`, `#u8[`, `#u8{`.
    OpenBytevector(ParenType),

    /// Closing parenthesis: `)`, `]`, `}`.
    Close(ParenType),

    /// Datum label mark.
    LabelMark(Atom),

    /// Datum label reference.
    LabelRef(Atom),

    /// Canonical boolean literal.
    Boolean(bool),

    /// Character literal.
    Character(char),

    /// String literal.
    String(Atom),

    /// Identifier (both plain or escaped).
    Identifier(Atom),

    /// Number literal.
    Number(Atom),

    /// Directive to the reader.
    Directive(Atom),

    /// Marker token denoting an invalid character sequences.
    Unrecognized,
}

/// Type of an opening or closing parenthesis.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum ParenType {
    /// Parentheses: `( )`
    Parenthesis,

    /// Brackets: `[ ]`
    Bracket,

    /// Braces: `{ }`
    Brace,
}
