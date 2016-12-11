// Copyright (c) 2016, Sabre developers
//
// Licensed under the Apache License, Version 2.0 (see LICENSE.Apache in the
// root directory) or MIT license (see LICENSE.MIT in the root directory),
// at your option. This file may be copied, distributed, and modified only
// in accordance with the terms specified by the chosen license.

//! `ScannedDatum` test builders.
//!
//! These are organized as easily extendable _combinators_.

use reader::datum::{ScannedDatum, DatumValue};
use reader::diagnostics::{Span, Diagnostic, DiagnosticKind};
use reader::intern_pool::{Atom};

/// Parser test data set.
pub struct DataTest {
    /// A string to be fed to the scanner used by the parser.
    pub text: String,

    /// Sequence of data expected from the parser.
    pub data: Vec<ScannedDatum>,

    /// Sequence of diagnostics expected from the parser and its scanner.
    pub diagnostics: Vec<Diagnostic>,
}

/// Make a test for a string that produces no data (e.g., a comment).
pub fn ignored(text: &str) -> DataTest {
    DataTest {
        text: text.to_owned(),
        data: Vec::new(),
        diagnostics: Vec::new(),
    }
}

/// Make a test for a boolean literal.
pub fn boolean(text: &str, value: bool) -> DataTest {
    literal(text, DatumValue::Boolean(value))
}

/// Make a test for a bytevector literal.
pub fn bytevector(text: &str, values: Vec<Atom>) -> DataTest {
    literal(text, DatumValue::Bytevector(values))
}

/// Make a test for a character literal.
pub fn character(text: &str, value: char) -> DataTest {
    literal(text, DatumValue::Character(value))
}

/// Make a test for a number literal.
pub fn number(text: &str, value: Atom) -> DataTest {
    literal(text, DatumValue::Number(value))
}

/// Make a test for a string literal.
pub fn string(text: &str, value: Atom) -> DataTest {
    literal(text, DatumValue::String(value))
}

/// Make a test for a symbol.
pub fn symbol(text: &str, value: Atom) -> DataTest {
    literal(text, DatumValue::Symbol(value))
}

/// Make a test for a literal.
fn literal(text: &str, value: DatumValue) -> DataTest {
    DataTest {
        text: text.to_owned(),
        data: vec![ScannedDatum { value: value, span: Span::new(0, text.len()) }],
        diagnostics: Vec::new(),
    }
}

impl DataTest {
    /// Add an expected diagnostic to the test.
    pub fn diagnostic(mut self, from: usize, to: usize, kind: DiagnosticKind) -> Self {
        self.diagnostics.push(Diagnostic { kind: kind, loc: Some(Span::new(from, to)) });
        self
    }
}

/// Combine tests into a sequence of lines.
pub fn line_sequence(tests: Vec<DataTest>) -> DataTest {
    let mut text = String::new();
    let mut data = Vec::new();
    let mut diagnostics = Vec::new();

    for test in tests {
        let start_offset = text.len();

        text.push_str(&test.text);
        text.push_str("\n");

        data.extend(
            test.data.into_iter().map(|d| offset_scanned_datum(d, start_offset))
        );

        diagnostics.extend(
            test.diagnostics.into_iter().map(|d| offset_diagnostic(d, start_offset))
        );
    }

    return DataTest {
        text: text,
        data: data,
        diagnostics: diagnostics
    };
}

/// Offset spans in a scanned datum.
fn offset_scanned_datum(datum: ScannedDatum, offset: usize) -> ScannedDatum {
    ScannedDatum {
        value: offset_datum(datum.value, offset),
        span: offset_span(datum.span, offset),
    }
}

/// Offset spans in a datum value.
fn offset_datum(value: DatumValue, offset: usize) -> DatumValue {
    match value {
        DatumValue::Boolean(_) => value,
        DatumValue::Bytevector(_) => value,
        DatumValue::Character(_) => value,
        DatumValue::Number(_) => value,
        DatumValue::String(_) => value,
        DatumValue::Symbol(_) => value,
    }
}

/// Offset spans in a diagnostic.
fn offset_diagnostic(diagnostic: Diagnostic, offset: usize) -> Diagnostic {
    Diagnostic {
        kind: diagnostic.kind,
        loc: diagnostic.loc.map(|s| offset_span(s, offset)),
    }
}

/// Offset a span.
fn offset_span(span: Span, offset: usize) -> Span {
    Span::new(span.from + offset, span.to + offset)
}

#[cfg(test)]
mod tests {
    use super::*;
    use reader::datum::{ScannedDatum, DatumValue};
    use reader::diagnostics::{Span, Diagnostic, DiagnosticKind};
    use reader::intern_pool::{InternPool};

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    // Primitives

    #[test]
    fn primitive_ignored() {
        let test = ignored("#| comment |#");

        assert_eq!(test.text, "#| comment |#");
        assert_eq!(test.data, vec![]);
        assert_eq!(test.diagnostics, vec![]);
    }

    #[test]
    fn primitive_boolean() {
        let test = boolean("#false", false);

        assert_eq!(test.text, "#false");
        assert_eq!(test.data, vec![
            ScannedDatum { value: DatumValue::Boolean(false), span: Span::new(0, 6) },
        ]);
        assert_eq!(test.diagnostics, vec![]);
    }

    #[test]
    fn primitive_bytevector_empty() {
        let test = bytevector("#u8()", vec![]);

        assert_eq!(test.text, "#u8()");
        assert_eq!(test.data, vec![
            ScannedDatum { value: DatumValue::Bytevector(vec![]), span: Span::new(0, 5) },
        ]);
        assert_eq!(test.diagnostics, vec![]);
    }

    #[test]
    fn primitive_bytevector_values() {
        let pool = InternPool::new();
        let test = bytevector("#u8(12  34)", vec![pool.intern("12"), pool.intern("34")]);

        assert_eq!(test.text, "#u8(12  34)");
        assert_eq!(test.data, vec![
            ScannedDatum {
                value: DatumValue::Bytevector(vec![pool.intern("12"), pool.intern("34")]),
                span: Span::new(0, 11),
            },
        ]);
        assert_eq!(test.diagnostics, vec![]);
    }

    #[test]
    fn primitive_character() {
        let test = character("#\\?", '?');

        assert_eq!(test.text, "#\\?");
        assert_eq!(test.data, vec![
            ScannedDatum { value: DatumValue::Character('?'), span: Span::new(0, 3) },
        ]);
        assert_eq!(test.diagnostics, vec![]);
    }

    #[test]
    fn primitive_number() {
        let pool = InternPool::new();
        let test = number("1234", pool.intern("1234"));

        assert_eq!(test.text, "1234");
        assert_eq!(test.data, vec![
            ScannedDatum { value: DatumValue::Number(pool.intern("1234")), span: Span::new(0, 4) },
        ]);
        assert_eq!(test.diagnostics, vec![]);
    }

    #[test]
    fn primitive_string() {
        let pool = InternPool::new();
        let test = string("\"test\"", pool.intern("test"));

        assert_eq!(test.text, "\"test\"");
        assert_eq!(test.data, vec![
            ScannedDatum { value: DatumValue::String(pool.intern("test")), span: Span::new(0, 6) },
        ]);
        assert_eq!(test.diagnostics, vec![]);
    }

    #[test]
    fn primitive_symbol() {
        let pool = InternPool::new();
        let test = symbol("test", pool.intern("test"));

        assert_eq!(test.text, "test");
        assert_eq!(test.data, vec![
            ScannedDatum { value: DatumValue::Symbol(pool.intern("test")), span: Span::new(0, 4) },
        ]);
        assert_eq!(test.diagnostics, vec![]);
    }

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    // Diagnostics

    #[test]
    fn diagnostics_reporting() {
        let test =
            character("#\\unknown", '\u{FFFD}')
                .diagnostic(0, 9, DiagnosticKind::err_lexer_unknown_character_name);

        assert_eq!(test.text, "#\\unknown");
        assert_eq!(test.data, vec![
            ScannedDatum { value: DatumValue::Character('\u{FFFD}'), span: Span::new(0, 9) },
        ]);
        assert_eq!(test.diagnostics, vec![
            Diagnostic {
                kind: DiagnosticKind::err_lexer_unknown_character_name,
                loc: Some(Span::new(0, 9)),
            },
        ]);
    }

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    // Lines

    #[test]
    fn line_sequence_empty() {
        let test = line_sequence(vec![]);

        assert_eq!(test.text, "");
        assert_eq!(test.data, vec![]);
        assert_eq!(test.diagnostics, vec![]);
    }

    #[test]
    fn line_sequence_simple() {
        let pool = InternPool::new();
        let test = line_sequence(vec![
            boolean("#true", true),
            number("123x5", pool.intern("123x5"))
                .diagnostic(3, 4, DiagnosticKind::err_lexer_invalid_number_character),
            character("#\\d", 'd'),
        ]);

        assert_eq!(test.text, "#true\n123x5\n#\\d\n");
        assert_eq!(test.data, vec![
            ScannedDatum { value: DatumValue::Boolean(true), span: Span::new(0, 5) },
            ScannedDatum { value: DatumValue::Number(pool.intern("123x5")), span: Span::new(6, 11) },
            ScannedDatum { value: DatumValue::Character('d'), span: Span::new(12, 15) },
        ]);
        assert_eq!(test.diagnostics, vec![
            Diagnostic {
                kind: DiagnosticKind::err_lexer_invalid_number_character,
                loc: Some(Span::new(9, 10)),
            },
        ]);
    }
}
