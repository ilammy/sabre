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

/// Make a test for a single boolean literal.
pub fn boolean(text: &str, value: bool) -> DataTest {
    literal(text, DatumValue::Boolean(value))
}

/// Make a test for a single character literal.
pub fn character(text: &str, value: char) -> DataTest {
    literal(text, DatumValue::Character(value))
}

/// Make a test for a single number literal.
pub fn number(text: &str, value: Atom) -> DataTest {
    literal(text, DatumValue::Number(value))
}

/// Make a test for a single string literal.
pub fn string(text: &str, value: Atom) -> DataTest {
    literal(text, DatumValue::String(value))
}

/// Make a test for a single symbol.
pub fn symbol(text: &str, value: Atom) -> DataTest {
    literal(text, DatumValue::Symbol(value))
}

/// Make a literal test.
fn literal(text: &str, value: DatumValue) -> DataTest {
    DataTest {
        text: text.to_owned(),
        data: vec![ScannedDatum {
            value: value,
            span: Span::new(0, text.len()),
        }],
        diagnostics: Vec::new(),
    }
}

impl DataTest {
    /// Add an expected diagnostic at the given position.
    pub fn with_report_at(mut self, from: usize, to: usize, kind: DiagnosticKind) -> Self {
        self.diagnostics.push(Diagnostic { kind: kind, loc: Some(Span::new(from, to)) });
        self
    }
}

/// Combine tests into a sequence of lines.
pub fn line_sequence(data: Vec<DataTest>) -> DataTest {
    const LINE_ENDING: &'static str = "\n";

    let mut test = DataTest {
        text: String::new(),
        data: Vec::new(),
        diagnostics: Vec::new(),
    };

    for datum in data {
        let start_offset = test.text.len();

        test.text.push_str(&datum.text);
        test.text.push_str(LINE_ENDING);

        test.data.extend(
            datum.data.iter()
                .map(|d| offset_datum(d, start_offset))
        );

        test.diagnostics.extend(
            datum.diagnostics.iter()
                .map(|d| offset_diagnostic(d, start_offset))
        );
    }

    return test;
}

/// Offset spans in a datum.
fn offset_datum(datum: &ScannedDatum, offset: usize) -> ScannedDatum {
    ScannedDatum {
        value: datum.value.clone(),
        span: offset_span(&datum.span, offset),
    }
}

/// Offset spans in a diagnostic.
fn offset_diagnostic(diagnostic: &Diagnostic, offset: usize) -> Diagnostic {
    Diagnostic {
        kind: diagnostic.kind,
        loc: diagnostic.loc.map(|s| offset_span(&s, offset)),
    }
}

/// Offset a span.
fn offset_span(span: &Span, offset: usize) -> Span {
    Span::new(span.from + offset, span.to + offset)
}

#[cfg(test)]
mod tests {
    use super::*;
    use reader::datum::{ScannedDatum, DatumValue};
    use reader::diagnostics::{Span, Diagnostic, DiagnosticKind};
    use reader::intern_pool::{Atom, InternPool};

    #[test]
    fn simple_ignored() {
        let test = ignored("#| comment |#");

        assert_eq!(test.text, "#| comment |#");
        assert_eq!(test.data, vec![]);
        assert_eq!(test.diagnostics, vec![]);
    }

    #[test]
    fn simple_boolean() {
        let test = boolean("#false", false);

        assert_eq!(test.text, "#false");
        assert_eq!(test.data, vec![
            ScannedDatum { value: DatumValue::Boolean(false), span: Span::new(0, 6) },
        ]);
        assert_eq!(test.diagnostics, vec![]);
    }

    #[test]
    fn simple_character() {
        let test = character("#\\?", '?');

        assert_eq!(test.text, "#\\?");
        assert_eq!(test.data, vec![
            ScannedDatum { value: DatumValue::Character('?'), span: Span::new(0, 3) },
        ]);
        assert_eq!(test.diagnostics, vec![]);
    }

    #[test]
    fn simple_number() {
        let pool = InternPool::new();
        let atom = pool.intern("1234");
        let test = number("1234", atom);

        assert_eq!(test.text, "1234");
        assert_eq!(test.data, vec![
            ScannedDatum { value: DatumValue::Number(atom), span: Span::new(0, 4) },
        ]);
        assert_eq!(test.diagnostics, vec![]);
    }

    #[test]
    fn simple_string() {
        let pool = InternPool::new();
        let atom = pool.intern("test");
        let test = string("\"test\"", atom);

        assert_eq!(test.text, "\"test\"");
        assert_eq!(test.data, vec![
            ScannedDatum { value: DatumValue::String(atom), span: Span::new(0, 6) },
        ]);
        assert_eq!(test.diagnostics, vec![]);
    }

    #[test]
    fn simple_symbol() {
        let pool = InternPool::new();
        let atom = pool.intern("test");
        let test = symbol("test", atom);

        assert_eq!(test.text, "test");
        assert_eq!(test.data, vec![
            ScannedDatum { value: DatumValue::Symbol(atom), span: Span::new(0, 4) },
        ]);
        assert_eq!(test.diagnostics, vec![]);
    }

    #[test]
    fn diagnostics_reporting() {
        let test = character("#\\unknown", '\u{FFFD}')
            .with_report_at(0, 9, DiagnosticKind::err_lexer_unknown_character_name);

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
        let atom = pool.intern("123x5");
        let test = line_sequence(vec![
            boolean("#true", true),
            number("123x5", atom)
                .with_report_at(3, 4, DiagnosticKind::err_lexer_invalid_number_character),
            character("#\\d", 'd'),
        ]);

        assert_eq!(test.text, "#true\n123x5\n#\\d\n");
        assert_eq!(test.data, vec![
            ScannedDatum { value: DatumValue::Boolean(true), span: Span::new(0, 5) },
            ScannedDatum { value: DatumValue::Number(atom), span: Span::new(6, 11) },
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
