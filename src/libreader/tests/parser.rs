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

use reader::diagnostics::{Handler, DiagnosticKind};
use reader::intern_pool::{InternPool};
use reader::lexer::{StringScanner};
use reader::parser::{Parser};

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
            .diagnostic(0, 21, DiagnosticKind::err_lexer_unknown_directive),
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
            .diagnostic(0, 16, DiagnosticKind::fatal_lexer_unterminated_comment),
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
            .diagnostic(0, 11, DiagnosticKind::err_lexer_unknown_character_name),
    ]));
}

#[test]
fn simple_number() {
    let pool = InternPool::new();

    check(&pool, datum::line_sequence(vec![
        datum::number("1",                  pool.intern("1")),
        datum::number("23e+45",             pool.intern("23e+45")),
        datum::number("#E67-89I",           pool.intern("#E67-89I")),
        datum::number("-NaN.0",             pool.intern("-NaN.0")),
        datum::number("#m#x+butterfly",     pool.intern("#m#x+butterfly"))
            .diagnostic( 0,  2, DiagnosticKind::err_lexer_invalid_number_prefix)
            .diagnostic( 6,  7, DiagnosticKind::err_lexer_invalid_number_character)
            .diagnostic( 7,  8, DiagnosticKind::err_lexer_invalid_number_character)
            .diagnostic( 8,  9, DiagnosticKind::err_lexer_invalid_number_character)
            .diagnostic(10, 11, DiagnosticKind::err_lexer_invalid_number_character)
            .diagnostic(12, 13, DiagnosticKind::err_lexer_invalid_number_character)
            .diagnostic(13, 14, DiagnosticKind::err_lexer_invalid_number_character),
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
        datum::ignored("\"CANDLEJA-")
            .diagnostic(0, 11, DiagnosticKind::fatal_lexer_unterminated_string),
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
        datum::ignored("|Il1")
            .diagnostic(0, 5, DiagnosticKind::fatal_lexer_unterminated_identifier),
    ]));
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Test helpers

use std::cell::RefCell;
use std::rc::Rc;

use utils::stubs::{SinkReporter};

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

    assert_eq!(data, test.data);

    assert_eq!(diagnostics, test.diagnostics);
}
