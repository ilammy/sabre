// Copyright (c) 2016, Sabre developers
//
// Licensed under the Apache License, Version 2.0 (see LICENSE.Apache in the
// root directory) or MIT license (see LICENSE.MIT in the root directory),
// at your option. This file may be copied, distributed, and modified only
// in accordance with the terms specified by the chosen license.

//! Parser test suite.
//!
//! This verifies that the parser recognizes all expected expressions and errors.

extern crate reader;
extern crate utils;

use reader::datum::{ScannedDatum, DatumValue};
use reader::diagnostics::{Span, Handler, Diagnostic, DiagnosticKind};
use reader::intern_pool::{InternPool, Atom};
use reader::lexer::{ScannedToken, StringScanner, Scanner};
use reader::parser::{Parser};
use reader::tokens::{Token};
use utils::build::datum::{self, DataTest};

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Smoke test of test harness

#[test]
fn smoke_test_empty_string() {
    let pool = InternPool::new();

    check(&pool, datum::line_sequence(vec![]));
}

#[test]
fn smoke_test_directives() {
    let pool = InternPool::new();

    check(&pool, datum::line_sequence(vec![
        datum::ignored("#!fold-case"),
        datum::ignored("#!no-fold-case"),
        datum::ignored("#!make-me-a-sandwitch")
            .with_report_at(0, 21, DiagnosticKind::err_lexer_unknown_directive),
    ]));
}

#[test]
fn smoke_test_comments() {
    let pool = InternPool::new();

    check(&pool, datum::line_sequence(vec![
        datum::ignored("; test comment"),
        datum::ignored("#| nested #|comments|#|#"),
    ]));
}

#[test]
fn smoke_test_comments_unterminated() {
    let pool = InternPool::new();

    check(&pool, datum::line_sequence(vec![
        datum::ignored("#| I'm too lazy")
            .with_report_at(0, 16, DiagnosticKind::fatal_lexer_unterminated_comment),
    ]));
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Simple literal data

#[test]
fn simple_boolean() {
    let pool = InternPool::new();

    check(&pool, datum::line_sequence(vec![
        datum::boolean("#false", false),
        datum::boolean("#T",     true),
    ]));
}

#[test]
fn simple_character() {
    let pool = InternPool::new();

    check(&pool, datum::line_sequence(vec![
        datum::character("#\\newline",      '\n'),
        datum::character("#\\X371A",        '\u{371A}'),
        datum::character("#\\?",            '?'),
        datum::character("#\\seventeen",    '\u{FFFD}')
            .with_report_at(0, 11, DiagnosticKind::err_lexer_unknown_character_name),
    ]));
}

#[test]
fn simple_number() {
    let pool = InternPool::new();

    check(&pool, datum::line_sequence(vec![
        datum::string("1",              pool.intern("1")),
        datum::number("23e+45",         pool.intern("23e+45")),
        datum::number("#E67-89I",       pool.intern("#E67-89I")),
        datum::number("-NaN.0",         pool.intern("-NaN.0")),
        datum::number("#m#x+butterfly", pool.intern("#m#x+butterfly"))
            .with_report_at( 0,  2, DiagnosticKind::err_lexer_invalid_number_prefix)
            .with_report_at( 6,  7, DiagnosticKind::err_lexer_invalid_number_character)
            .with_report_at( 7,  8, DiagnosticKind::err_lexer_invalid_number_character)
            .with_report_at( 8,  9, DiagnosticKind::err_lexer_invalid_number_character)
            .with_report_at(10, 11, DiagnosticKind::err_lexer_invalid_number_character)
            .with_report_at(12, 13, DiagnosticKind::err_lexer_invalid_number_character)
            .with_report_at(13, 14, DiagnosticKind::err_lexer_invalid_number_character),
    ]));
}

#[test]
fn simple_string() {
    let pool = InternPool::new();

    check(&pool, datum::line_sequence(vec![
        datum::string("\"\"",           pool.intern("")),
        datum::string("\"test\"",       pool.intern("test")),
        datum::string("\"\\x1234;\"",   pool.intern("\u{1234}")),
    ]));
}

#[test]
fn simple_string_unterminated() {
    let pool = InternPool::new();

    check(&pool, datum::line_sequence(vec![
        datum::string("\"CANDLEJA-",    pool.intern("CANDLEJA-"))
            .with_report_at(0, 11, DiagnosticKind::fatal_lexer_unterminated_string),
    ]));
}

#[test]
fn simple_symbol() {
    let pool = InternPool::new();

    check(&pool, datum::line_sequence(vec![
        datum::symbol("hello",          pool.intern("hello")),
        datum::symbol("|^_^|",          pool.intern("^_^")),
        datum::symbol("||",             pool.intern("")),
        datum::symbol("|\\x1111;|",     pool.intern("\u{1111}")),
    ]));
}

#[test]
fn simple_symbol_unterminated() {
    let pool = InternPool::new();

    check(&pool, datum::line_sequence(vec![
        datum::string("|Il1",           pool.intern("Il1"))
            .with_report_at(0, 5, DiagnosticKind::fatal_lexer_unterminated_identifier),
    ]));
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Test helpers

use std::cell::RefCell;
use std::fmt::Write;
use std::ops::Deref;
use std::rc::Rc;

use utils::diff::sequence::{self, Diff};
use utils::stubs::{SinkReporter};
use utils::tree::TreeNode;

/// Check whether the parser produces expected results and reports expected diagnostics
/// when given a sequence of tokens produced from a given string by `StringScanner`.
/// Panic if this is not true.
fn check(pool: &InternPool, test: DataTest) {
    let diagnostics = Rc::new(RefCell::new(Vec::new()));

    let data = {
        let scanner_reporter = SinkReporter::new(diagnostics.clone());
        let scanner_handler = Handler::with_reporter(Box::new(scanner_reporter));

        let scanner = Box::new(StringScanner::new(&test.text, &scanner_handler, pool));

        let parser_reporter = SinkReporter::new(diagnostics.clone());
        let parser_handler = Handler::with_reporter(Box::new(parser_reporter));

        let mut parser = Parser::new(scanner, &parser_handler);

        let all_data = parser.parse_all_data();

        assert!(parser.parse_all_data().is_empty(), "parser did not consume the whole stream");

        all_data
    };

    let diagnostics = Rc::try_unwrap(diagnostics).unwrap().into_inner();
/*
    let datum_mismatches =
        verify_tree("Data", &test.data, &data,
            |datum| print_datum(datum, &test.text, pool));
*/
let datum_mismatches: Result<(), String> = Ok(());

    let diagnostic_mismatches =
        verify_sequence("Diagnostics", &test.diagnostics, &diagnostics,
            |diagnostic| print_diagnostic(diagnostic, &test.text));

    if datum_mismatches.is_err() || diagnostic_mismatches.is_err() {
        println!("");

        if let Err(string) = datum_mismatches {
            println!("{}", string);
        }
        if let Err(string) = diagnostic_mismatches {
            println!("{}", string);
        }

        panic!("test failed");
    }
}

/// Print out and verifies produced results. Returns Ok if everything is fine,
/// otherwise an Err with a string to be shown the user is returned.
fn verify_sequence<T, F>(title: &str, expected: &[T], actual: &[T], to_string: F)
    -> Result<(), String>
    where T: Eq, F: Fn(&T) -> String
{
    let mut report = String::new();

    writeln!(&mut report, "{}", title).unwrap();

    let diff = sequence::diff(expected, actual);

    let mut okay = true;

    for entry in diff {
        match entry {
            Diff::Equal(ref actual, _) => {
                writeln!(&mut report, "  {}", to_string(actual)).unwrap();
            }
            Diff::Replace(ref actual, ref expected) => {
                okay = false;
                writeln!(&mut report, "- {}", to_string(actual)).unwrap();
                writeln!(&mut report, "+ {}", to_string(expected)).unwrap();
            }
            Diff::Left(ref actual) => {
                okay = false;
                writeln!(&mut report, "- {}", to_string(actual)).unwrap();
            }
            Diff::Right(ref expected) => {
                okay = false;
                writeln!(&mut report, "+ {}", to_string(expected)).unwrap();
            }
        }
    }

    return if okay { Ok(()) } else { Err(report) };
}

struct ScannedDatumTree(ScannedDatum);

impl Deref for ScannedDatumTree {
    type Target = ScannedDatum;

    fn deref(&self) -> &ScannedDatum {
        &self.0
    }
}

impl TreeNode for ScannedDatumTree {
    fn children<'a>(&'a self) -> Vec<&'a Self> {
        match self.value {
            // Simple data does not have child nodes
            DatumValue::Boolean(_) =>   Vec::new(),
            DatumValue::Character(_) => Vec::new(),
            DatumValue::Number(_) =>    Vec::new(),
            DatumValue::String(_) =>    Vec::new(),
            DatumValue::Symbol(_) =>    Vec::new(),
        }
    }
}

/*

/// Print out and verifies produced results. Returns Ok if everything is fine,
/// otherwise an Err with a string to be shown the user is returned.
fn verify_tree<T, F>(title: &str, expected: )

*/

/// Pretty-print a diagnostic in diffs.
fn print_diagnostic(diagnostic: &Diagnostic, buf: &str) -> String {
    if let Some(ref loc) = diagnostic.loc {
        format!("{diagnostic} @ [{from}, {to}] = {slice:?}",
            diagnostic = pretty_print_diagnostic(diagnostic),
            from       = loc.from,
            to         = loc.to,
            slice      = &buf[loc.from..loc.to],
        )
    } else {
        format!("{diagnostic} @ nowhere",
            diagnostic = pretty_print_diagnostic(diagnostic),
        )
    }
}

/// Pretty-print a diagnostic.
fn pretty_print_diagnostic(diagnostic: &Diagnostic) -> String {
    match diagnostic.kind {
        _ => format!("{:?}", diagnostic.kind)
    }
}
