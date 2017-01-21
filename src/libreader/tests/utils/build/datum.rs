// Copyright (c) 2016, Sabre developers
//
// Licensed under the Apache License, Version 2.0 (see LICENSE.Apache in the
// root directory) or MIT license (see LICENSE.MIT in the root directory),
// at your option. This file may be copied, distributed, and modified only
// in accordance with the terms specified by the chosen license.

//! `ScannedDatum` test builders.
//!
//! These are organized as easily extendable _combinators_.

use datum::{ScannedDatum, DatumValue};
use diagnostics::{Span, Diagnostic, DiagnosticKind};
use intern_pool::{Atom, InternPool};

/// Parser test data set.
#[derive(Debug, Clone)]
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
pub fn line_sequence<I: IntoIterator<Item=DataTest>>(tests: I) -> DataTest {
    concatenate(
        tests.into_iter()
             .zip(vec![ignored("\n")].into_iter().cycle())
             .flat_map(|(d, n)| vec![d, n])
    )
}

/// Combine tests into a vector.
pub fn vector<I: IntoIterator<Item=DataTest>>(elements: I) -> DataTest {
    let DataTest { text, data, diagnostics } = concatenate(elements);

    let total_length = text.len();

    return DataTest {
        text: text,
        data: vec![ScannedDatum {
            value: DatumValue::Vector(data),
            span: Span::new(0, total_length),
        }],
        diagnostics: diagnostics,
    };
}

/// Combine tests into a proper list.
pub fn proper_list<I: IntoIterator<Item=DataTest>>(elements: I) -> DataTest {
    let DataTest { text, data, diagnostics } = concatenate(elements);

    let total_length = text.len();

    return DataTest {
        text: text,
        data: vec![ScannedDatum {
            value: DatumValue::ProperList(data),
            span: Span::new(0, total_length),
        }],
        diagnostics: diagnostics,
    };
}

/// Combine tests into a dotted list.
pub fn dotted_list<I: IntoIterator<Item=DataTest>>(elements: I) -> DataTest {
    let DataTest { text, data, diagnostics } = concatenate(elements);

    assert!(data.len() >= 2, "dotted lists must contain as least two elements");

    let total_length = text.len();

    return DataTest {
        text: text,
        data: vec![ScannedDatum {
            value: DatumValue::DottedList(data),
            span: Span::new(0, total_length),
        }],
        diagnostics: diagnostics,
    };
}

/// Make a quotation.
pub fn quote<I: IntoIterator<Item=DataTest>>(pool: &InternPool, tests: I) -> DataTest {
    abbreviation(pool, tests, "'", "quote")
}

/// Make a quasiquotation.
pub fn quasiquote<I: IntoIterator<Item=DataTest>>(pool: &InternPool, tests: I) -> DataTest {
    abbreviation(pool, tests, "`", "quasiquote")
}

/// Make an unquote.
pub fn unquote<I: IntoIterator<Item=DataTest>>(pool: &InternPool, tests: I) -> DataTest {
    abbreviation(pool, tests, ",", "unquote")
}

/// Make a splicing unquote.
pub fn unquote_splicing<I: IntoIterator<Item=DataTest>>(pool: &InternPool, tests: I) -> DataTest {
    abbreviation(pool, tests, ",@", "unquote-splicing")
}

/// Make an abbreviation of the given kind.
fn abbreviation<I>(pool: &InternPool, tests: I, sigil: &str, keyword: &str) -> DataTest
    where I: IntoIterator<Item=DataTest>
{
    let DataTest { text, mut data, diagnostics } = concatenate(tests);

    assert!(data.len() == 1, "quotes must wrap exactly one datum");

    let total_length = text.len();

    return DataTest {
        text: text,
        data: vec![ScannedDatum {
            value: DatumValue::ProperList(vec![
                ScannedDatum {
                    value: DatumValue::Symbol(pool.intern(keyword)),
                    span: Span::new(0, sigil.len()),
                },
                data.remove(0),
            ]),
            span: Span::new(0, total_length),
        }],
        diagnostics: diagnostics,
    };
}

/// Make a label reference.
pub fn label_ref(text: &str, label: Atom) -> DataTest {
    literal(text, DatumValue::LabelReference(label))
}

/// Make a labeled datum.
pub fn labeled<I: IntoIterator<Item=DataTest>>(label: Atom, tests: I) -> DataTest {
    let DataTest { text, mut data, diagnostics } = concatenate(tests);

    assert!(data.len() == 1, "label marks must wrap exactly one datum");

    let total_length = text.len();

    return DataTest {
        text: text,
        data: vec![ScannedDatum {
            value: DatumValue::LabeledDatum(label, Box::new(data.remove(0))),
            span: Span::new(0, total_length),
        }],
        diagnostics: diagnostics,
    };
}

/// Concatenate tests.
fn concatenate<I: IntoIterator<Item=DataTest>>(tests: I) -> DataTest {
    let mut text = String::new();
    let mut data = Vec::new();
    let mut diagnostics = Vec::new();

    for test in tests {
        let start_offset = text.len();

        text.push_str(&test.text);

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
        DatumValue::LabelReference(_) => value,

        DatumValue::Vector(elements) => DatumValue::Vector(offset_data_vector(elements, offset)),
        DatumValue::ProperList(elements) => DatumValue::ProperList(offset_data_vector(elements, offset)),
        DatumValue::DottedList(elements) => DatumValue::DottedList(offset_data_vector(elements, offset)),

        DatumValue::LabeledDatum(label, datum) => DatumValue::LabeledDatum(label, Box::new(offset_scanned_datum(*datum, offset))),
    }
}

/// Offset a vector of scanned data.
fn offset_data_vector<I>(data: I, offset: usize) -> Vec<ScannedDatum>
    where I: IntoIterator<Item=ScannedDatum>
{
    data.into_iter()
        .map(|d| offset_scanned_datum(d, offset))
        .collect()
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

    use datum::{ScannedDatum, DatumValue};
    use diagnostics::{Span, Diagnostic, DiagnosticKind};
    use intern_pool::{InternPool};

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

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    // Vectors

    #[test]
    fn vector_empty() {
        let test = vector(vec![ignored("#("), ignored(")")]);

        assert_eq!(test.text, "#()");
        assert_eq!(test.data, vec![
            ScannedDatum { value: DatumValue::Vector(vec![]), span: Span::new(0, 3) },
        ]);
        assert_eq!(test.diagnostics, vec![]);
    }

    #[test]
    fn vector_simple() {
        let pool = InternPool::new();
        let test = vector(vec![
            ignored("#("),
            number("1", pool.intern("1")),
            ignored(" "),
            symbol("hi", pool.intern("hi")),
            ignored(" "),
            character("#\\1", '1'),
            ignored(" "),
            string("\"test\"",  pool.intern("test")),
            ignored(" "),
            boolean("#t", true),
            ignored(")"),
        ]);

        assert_eq!(test.text, "#(1 hi #\\1 \"test\" #t)");
        assert_eq!(test.data, vec![
            ScannedDatum {
                value: DatumValue::Vector(vec![
                    ScannedDatum { value: DatumValue::Number(pool.intern("1")), span: Span::new(2, 3) },
                    ScannedDatum { value: DatumValue::Symbol(pool.intern("hi")), span: Span::new(4, 6) },
                    ScannedDatum { value: DatumValue::Character('1'), span: Span::new(7, 10) },
                    ScannedDatum { value: DatumValue::String(pool.intern("test")), span: Span::new(11, 17) },
                    ScannedDatum { value: DatumValue::Boolean(true), span: Span::new(18, 20) },
                ]),
                span: Span::new(0, 21),
            },
        ]);
        assert_eq!(test.diagnostics, vec![]);
    }

    #[test]
    fn vector_nested() {
        let pool = InternPool::new();
        let test = vector(vec![
            ignored("#("),
            number("1", pool.intern("1")),
            ignored(" "),
            vector(vec![
                ignored("#["),
                number("2", pool.intern("2")),
                ignored("\t"),
                number("3", pool.intern("3")),
                ignored("]"),
            ]),
            ignored(" "),
            number("4", pool.intern("4")),
            ignored(")"),
        ]);

        assert_eq!(test.text, "#(1 #[2\t3] 4)");
        assert_eq!(test.data, vec![
            ScannedDatum {
                value: DatumValue::Vector(vec![
                    ScannedDatum { value: DatumValue::Number(pool.intern("1")), span: Span::new(2, 3) },
                    ScannedDatum {
                        value: DatumValue::Vector(vec![
                            ScannedDatum { value: DatumValue::Number(pool.intern("2")), span: Span::new(6, 7) },
                            ScannedDatum { value: DatumValue::Number(pool.intern("3")), span: Span::new(8, 9) },
                        ]),
                        span: Span::new(4, 10),
                    },
                    ScannedDatum { value: DatumValue::Number(pool.intern("4")), span: Span::new(11, 12) },
                ]),
                span: Span::new(0, 13),
            },
        ]);
        assert_eq!(test.diagnostics, vec![]);
    }

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    // Proper lists

    #[test]
    fn proper_list_empty() {
        let test = proper_list(vec![ignored("()")]);

        assert_eq!(test.text, "()");
        assert_eq!(test.data, vec![
            ScannedDatum { value: DatumValue::ProperList(vec![]), span: Span::new(0, 2) },
        ]);
        assert_eq!(test.diagnostics, vec![]);
    }

    #[test]
    fn proper_list_simple() {
        let pool = InternPool::new();
        let test = proper_list(vec![
            ignored("("),
            number("1", pool.intern("1")),
            ignored(" "),
            symbol("hi", pool.intern("hi")),
            ignored(" "),
            character("#\\1", '1'),
            ignored(" "),
            string("\"test\"",  pool.intern("test")),
            ignored(" "),
            boolean("#t", true),
            ignored(")"),
        ]);

        assert_eq!(test.text, "(1 hi #\\1 \"test\" #t)");
        assert_eq!(test.data, vec![
            ScannedDatum {
                value: DatumValue::ProperList(vec![
                    ScannedDatum { value: DatumValue::Number(pool.intern("1")), span: Span::new(1, 2) },
                    ScannedDatum { value: DatumValue::Symbol(pool.intern("hi")), span: Span::new(3, 5) },
                    ScannedDatum { value: DatumValue::Character('1'), span: Span::new(6, 9) },
                    ScannedDatum { value: DatumValue::String(pool.intern("test")), span: Span::new(10, 16) },
                    ScannedDatum { value: DatumValue::Boolean(true), span: Span::new(17, 19) },
                ]),
                span: Span::new(0, 20),
            },
        ]);
        assert_eq!(test.diagnostics, vec![]);
    }

    #[test]
    fn proper_list_nested() {
        let pool = InternPool::new();
        let test = proper_list(vec![
            ignored("("),
            number("1", pool.intern("1")),
            ignored("\t"),
            proper_list(vec![
                ignored("["),
                number("2", pool.intern("2")),
                ignored(" "),
                number("3", pool.intern("3")),
                ignored("]"),
            ]),
            ignored("\t"),
            number("4", pool.intern("4")),
            ignored(")"),
        ]);

        assert_eq!(test.text, "(1\t[2 3]\t4)");
        assert_eq!(test.data, vec![
            ScannedDatum {
                value: DatumValue::ProperList(vec![
                    ScannedDatum { value: DatumValue::Number(pool.intern("1")), span: Span::new(1, 2) },
                    ScannedDatum {
                        value: DatumValue::ProperList(vec![
                            ScannedDatum { value: DatumValue::Number(pool.intern("2")), span: Span::new(4, 5) },
                            ScannedDatum { value: DatumValue::Number(pool.intern("3")), span: Span::new(6, 7) },
                        ]),
                        span: Span::new(3, 8),
                    },
                    ScannedDatum { value: DatumValue::Number(pool.intern("4")), span: Span::new(9, 10) },
                ]),
                span: Span::new(0, 11),
            },
        ]);
        assert_eq!(test.diagnostics, vec![]);
    }

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    // Dotted lists

    #[test]
    #[should_panic]
    fn dotted_list_empty() {
        let _ = dotted_list(vec![ignored("("), ignored(")")]);
    }

    #[test]
    #[should_panic]
    fn dotted_list_one_element() {
        let pool = InternPool::new();
        let _ = dotted_list(vec![
            ignored("("),
            number("1", pool.intern("1")),
            ignored(")"),
        ]);
    }

    #[test]
    fn dotted_list_simple() {
        let pool = InternPool::new();
        let test = dotted_list(vec![
            ignored("("),
            symbol("car", pool.intern("car")),
            ignored(" . "),
            symbol("cdr", pool.intern("cdr")),
            ignored(")"),
        ]);

        assert_eq!(test.text, "(car . cdr)");
        assert_eq!(test.data, vec![
            ScannedDatum {
                value: DatumValue::DottedList(vec![
                    ScannedDatum { value: DatumValue::Symbol(pool.intern("car")), span: Span::new(1, 4) },
                    ScannedDatum { value: DatumValue::Symbol(pool.intern("cdr")), span: Span::new(7, 10) },
                ]),
                span: Span::new(0, 11),
            },
        ]);
        assert_eq!(test.diagnostics, vec![]);
    }

    #[test]
    fn dotted_list_nested() {
        let pool = InternPool::new();
        let test = dotted_list(vec![
            ignored("("),
            number("1", pool.intern("1")),
            ignored(" . "),
            dotted_list(vec![
                ignored("["),
                number("2", pool.intern("2")),
                ignored(" . "),
                dotted_list(vec![
                    ignored("{"),
                    number("3", pool.intern("3")),
                    ignored(" . "),
                    proper_list(vec![ignored("()")]),
                    ignored("}"),
                ]),
                ignored("]"),
            ]),
            ignored(")"),
        ]);

        assert_eq!(test.text, "(1 . [2 . {3 . ()}])");
        assert_eq!(test.data, vec![
            ScannedDatum {
                value: DatumValue::DottedList(vec![
                    ScannedDatum { value: DatumValue::Number(pool.intern("1")), span: Span::new(1, 2) },
                    ScannedDatum {
                        value: DatumValue::DottedList(vec![
                            ScannedDatum { value: DatumValue::Number(pool.intern("2")), span: Span::new(6, 7) },
                            ScannedDatum {
                                value: DatumValue::DottedList(vec![
                                    ScannedDatum { value: DatumValue::Number(pool.intern("3")), span: Span::new(11, 12) },
                                    ScannedDatum { value: DatumValue::ProperList(vec![]), span: Span::new(15, 17) },
                                ]),
                                span: Span::new(10, 18),
                            },
                        ]),
                        span: Span::new(5, 19),
                    },
                ]),
                span: Span::new(0, 20),
            },
        ]);
        assert_eq!(test.diagnostics, vec![]);
    }

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    // Abbreviations

    #[test]
    #[should_panic]
    fn abbreviation_empty() {
        let pool = InternPool::new();
        let _ = quote(&pool, vec![ignored("#|comment|#")]);
    }

    #[test]
    #[should_panic]
    fn abbreviation_multiple() {
        let pool = InternPool::new();
        let _ = unquote_splicing(&pool,
            vec![character("#\\1", '1'), ignored(" "), character("#\\2", '2')]
        );
    }

    #[test]
    fn abbreviation_nested() {
        let pool = InternPool::new();
        let test =
            quasiquote(&pool, vec![
                ignored("`"),
                dotted_list(vec![
                    ignored("("),
                    symbol("x", pool.intern("x")),
                    ignored(" "),
                    unquote(&pool, vec![
                        ignored(","),
                        symbol("y", pool.intern("y")),
                    ]),
                    ignored(" "),
                    quote(&pool, vec![
                        ignored("'"),
                        dotted_list(vec![
                            ignored("("),
                            number("0", pool.intern("0")),
                            ignored(" . "),
                            quote(&pool, vec![
                                ignored("'"),
                                proper_list(vec![ignored("()")]),
                            ]),
                            ignored(")"),
                        ]),
                    ]),
                    ignored(" . "),
                    unquote_splicing(&pool, vec![
                        ignored(",@"),
                        symbol("z", pool.intern("z")),
                    ]),
                    ignored(")"),
                ]),
            ]);

        assert_eq!(test.text, "`(x ,y '(0 . '()) . ,@z)");
        assert_eq!(test.data, vec![
            ScannedDatum {
                value: DatumValue::ProperList(vec![
                    ScannedDatum { value: DatumValue::Symbol(pool.intern("quasiquote")), span: Span::new(0, 1) },
                    ScannedDatum {
                        value: DatumValue::DottedList(vec![
                            ScannedDatum { value: DatumValue::Symbol(pool.intern("x")), span: Span::new(2, 3) },
                            ScannedDatum {
                                value: DatumValue::ProperList(vec![
                                    ScannedDatum { value: DatumValue::Symbol(pool.intern("unquote")), span: Span::new(4, 5) },
                                    ScannedDatum { value: DatumValue::Symbol(pool.intern("y")), span: Span::new(5, 6) },
                                ]),
                                span: Span::new(4, 6),
                            },
                            ScannedDatum {
                                value: DatumValue::ProperList(vec![
                                    ScannedDatum { value: DatumValue::Symbol(pool.intern("quote")), span: Span::new(7, 8) },
                                    ScannedDatum {
                                        value: DatumValue::DottedList(vec![
                                            ScannedDatum { value: DatumValue::Number(pool.intern("0")), span: Span::new(9, 10) },
                                            ScannedDatum {
                                                value: DatumValue::ProperList(vec![
                                                    ScannedDatum { value: DatumValue::Symbol(pool.intern("quote")), span: Span::new(13, 14) },
                                                    ScannedDatum { value: DatumValue::ProperList(vec![]), span: Span::new(14, 16) },
                                                ]),
                                                span: Span::new(13, 16),
                                            },
                                        ]),
                                        span: Span::new(8, 17),
                                    },
                                ]),
                                span: Span::new(7, 17),
                            },
                            ScannedDatum {
                                value: DatumValue::ProperList(vec![
                                    ScannedDatum { value: DatumValue::Symbol(pool.intern("unquote-splicing")), span: Span::new(20, 22) },
                                    ScannedDatum { value: DatumValue::Symbol(pool.intern("z")), span: Span::new(22, 23) },
                                ]),
                                span: Span::new(20, 23),
                            },
                        ]),
                        span: Span::new(1, 24),
                    }
                ]),
                span: Span::new(0, 24),
            },
        ]);
        assert_eq!(test.diagnostics, vec![]);
    }

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    // Labels

    #[test]
    fn label_reference() {
        let pool = InternPool::new();
        let test = label_ref("#123#", pool.intern("123"));

        assert_eq!(test.text, "#123#");
        assert_eq!(test.data, vec![
            ScannedDatum {
                value: DatumValue::LabelReference(pool.intern("123")),
                span: Span::new(0, 5),
            },
        ]);
        assert_eq!(test.diagnostics, vec![]);
    }

    #[test]
    #[should_panic]
    fn label_datum_empty() {
        let pool = InternPool::new();
        let _ = labeled(pool.intern("123"), vec![ignored("#123=")]);
    }

    #[test]
    #[should_panic]
    fn label_datum_multiple() {
        let pool = InternPool::new();
        let _ = labeled(pool.intern("123"), vec![
            number("1", pool.intern("1")), ignored(" "), number("2", pool.intern("2")),
        ]);
    }

    #[test]
    fn label_datum_simple() {
        let pool = InternPool::new();
        let test = labeled(pool.intern("123"), vec![
            ignored("#123="),
            number("456", pool.intern("456")),
        ]);

        assert_eq!(test.text, "#123=456");
        assert_eq!(test.data, vec![
            ScannedDatum {
                value: DatumValue::LabeledDatum(pool.intern("123"), Box::new(
                    ScannedDatum {
                        value: DatumValue::Number(pool.intern("456")),
                        span: Span::new(5, 8),
                    }
                )),
                span: Span::new(0, 8),
            },
        ]);
        assert_eq!(test.diagnostics, vec![]);
    }

    #[test]
    fn label_datum_nested() {
        let pool = InternPool::new();
        let test =
            labeled(pool.intern("0"), vec![
                ignored("#0="),
                labeled(pool.intern("1"), vec![
                    ignored("#1="),
                    proper_list(vec![
                        ignored("("),
                        labeled(pool.intern("2"), vec![
                            ignored("#2="),
                            symbol("cons", pool.intern("cons")),
                        ]),
                        ignored(" "),
                        symbol("head", pool.intern("head")),
                        ignored(" "),
                        label_ref("#0#", pool.intern("0")),
                        ignored(")"),
                    ]),
                ]),
            ]);

        assert_eq!(test.text, "#0=#1=(#2=cons head #0#)");
        assert_eq!(test.data, vec![
            ScannedDatum {
                value: DatumValue::LabeledDatum(pool.intern("0"), Box::new(
                    ScannedDatum {
                        value: DatumValue::LabeledDatum(pool.intern("1"), Box::new(
                            ScannedDatum {
                                value: DatumValue::ProperList(vec![
                                    ScannedDatum {
                                        value: DatumValue::LabeledDatum(pool.intern("2"), Box::new(
                                            ScannedDatum {
                                                value: DatumValue::Symbol(pool.intern("cons")),
                                                span: Span::new(10, 14),
                                            }
                                        )),
                                        span: Span::new(7, 14),
                                    },
                                    ScannedDatum {
                                        value: DatumValue::Symbol(pool.intern("head")),
                                        span: Span::new(15, 19),
                                    },
                                    ScannedDatum {
                                        value: DatumValue::LabelReference(pool.intern("0")),
                                        span: Span::new(20, 23),
                                    },
                                ]),
                                span: Span::new(6, 24),
                            }
                        )),
                        span: Span::new(3, 24),
                    }
                )),
                span: Span::new(0, 24),
            },
        ]);
        assert_eq!(test.diagnostics, vec![]);
    }

    #[test]
    fn label_datum_self_reference() {
        let pool = InternPool::new();
        let test =
            labeled(pool.intern("0"), vec![
                ignored("#0="),
                label_ref("#0#", pool.intern("0")),
            ]);

        assert_eq!(test.text, "#0=#0#");
        assert_eq!(test.data, vec![
            ScannedDatum {
                value: DatumValue::LabeledDatum(pool.intern("0"), Box::new(
                    ScannedDatum {
                        value: DatumValue::LabelReference(pool.intern("0")),
                        span: Span::new(3, 6),
                    }
                )),
                span: Span::new(0, 6),
            },
        ]);
        assert_eq!(test.diagnostics, vec![]);
    }
}
