// Copyright (c) 2016, ilammy
//
// Licensed under MIT license (see LICENSE file in the root directory).
// This file may be copied, distributed, and modified only in accordance
// with the terms specified by this license.

//! Scheme token definitions.
//!
//! This module contains definitions of tokens recognized and processed by Scheme reader.

/// Types of tokens recognized by the scanner.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Token {
    /// Marker token denoting the end-of-token-stream condition.
    Eof,

    /// Non-significant whitespace.
    Whitespace,

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

    /// Marker token denoting an invalid character sequences.
    Unrecognized,
}

/// Type of an opening or closing parenthesis.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ParenType {
    /// Parentheses: `( )`
    Parenthesis,

    /// Brackets: `[ ]`
    Bracket,

    /// Braces: `{ }`
    Brace,
}
