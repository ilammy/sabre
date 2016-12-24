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
// Bytevectors

#[test]
fn bytevector_empty() {
    let pool = InternPool::new();

    check(&pool, datum::line_sequence(vec![
        datum::bytevector("#U8()", vec![]),
        datum::bytevector("#u8[]", vec![]),
        datum::bytevector("#u8{}", vec![]),
    ]));
}

#[test]
fn bytevector_numbers() {
    let pool = InternPool::new();

    check(&pool, datum::line_sequence(vec![
        datum::bytevector("#u8(1 2 3 #e#x3A)", vec![
            pool.intern("1"),
            pool.intern("2"),
            pool.intern("3"),
            pool.intern("#e#x3A"),
        ]),
    ]));
}

#[test]
fn bytevector_padded() {
    let pool = InternPool::new();

    check(&pool, datum::line_sequence(vec![
        datum::bytevector("#U8( 1 2 )", vec![pool.intern("1"), pool.intern("2")]),
    ]));
}

#[test]
fn bytevector_out_of_range_numbers() {
    let pool = InternPool::new();

    check(&pool, datum::line_sequence(vec![
        datum::bytevector("#u8(100500 1e+400 3.1415 -74+i +inf.0)", vec![
            pool.intern("100500"),
            pool.intern("1e+400"),
            pool.intern("3.1415"),
            pool.intern("-74+i"),
            pool.intern("+inf.0"),
        ]),
    ]));
}

#[test]
fn bytevector_incorrect_numbers() {
    let pool = InternPool::new();

    check(&pool, datum::line_sequence(vec![
        datum::bytevector("#u8(#k9 1+1 #o9 1/.)", vec![
            pool.intern("#k9"),
            pool.intern("1+1"),
            pool.intern("#o9"),
            pool.intern("1/."),
        ])
            .diagnostic( 4,  6, DiagnosticKind::err_lexer_invalid_number_prefix)
            .diagnostic(11, 11, DiagnosticKind::err_lexer_missing_i)
            .diagnostic(14, 15, DiagnosticKind::err_lexer_invalid_number_digit)
            .diagnostic(18, 19, DiagnosticKind::err_lexer_digits_missing)
            .diagnostic(18, 19, DiagnosticKind::err_lexer_noninteger_rational),
    ]));
}

#[test]
fn bytevector_invalid_elements() {
    let pool = InternPool::new();

    check(&pool, datum::line_sequence(vec![
        // Only number literals are allowed.
        datum::bytevector("#u8(#false \"string\" value #\\x42)", vec![])
            .diagnostic( 4, 10, DiagnosticKind::err_parser_invalid_bytevector_element)
            .diagnostic(11, 19, DiagnosticKind::err_parser_invalid_bytevector_element)
            .diagnostic(20, 25, DiagnosticKind::err_parser_invalid_bytevector_element)
            .diagnostic(26, 31, DiagnosticKind::err_parser_invalid_bytevector_element),

        // No way bytevectors can be nested.
        datum::bytevector("#u8(#U8[#u8{0}])", vec![])
            .diagnostic(8, 14, DiagnosticKind::err_parser_invalid_bytevector_element)
            .diagnostic(4, 15, DiagnosticKind::err_parser_invalid_bytevector_element),

        // Vectors cannot be nested into bytevectors.
        datum::bytevector("#u8(#(1 2 3))", vec![])
            .diagnostic(4, 12, DiagnosticKind::err_parser_invalid_bytevector_element),

        // Lists cannot be nested into bytevectors.
        datum::bytevector("#u8(4 {5 6})", vec![pool.intern("4")])
            .diagnostic(6, 11, DiagnosticKind::err_parser_invalid_bytevector_element),

        datum::bytevector("#u8({6 7 . 8})", vec![])
            .diagnostic(4, 13, DiagnosticKind::err_parser_invalid_bytevector_element),

    ]));
}

#[test]
fn bytevector_invalid_dots() {
    let pool = InternPool::new();

    check(&pool, datum::line_sequence(vec![
        // Dot notation cannot be used in bytevectors.
        datum::bytevector("#u8(1 2 . 3)", vec![pool.intern("1"), pool.intern("2"), pool.intern("3")])
            .diagnostic(8, 9, DiagnosticKind::err_parser_misplaced_dot),

        datum::bytevector("#u8(. . .)", vec![])
            .diagnostic(4, 5, DiagnosticKind::err_parser_misplaced_dot)
            .diagnostic(6, 7, DiagnosticKind::err_parser_misplaced_dot)
            .diagnostic(8, 9, DiagnosticKind::err_parser_misplaced_dot),
    ]));
}

#[test]
fn bytevector_mismatched_delimiters() {
    let pool = InternPool::new();

    check(&pool, datum::line_sequence(vec![
        datum::bytevector("#u8(1 2 3]", vec![pool.intern("1"), pool.intern("2"), pool.intern("3")])
            .diagnostic(9, 10, DiagnosticKind::err_parser_mismatched_delimiter),

        // Check the (invalid) nested data as well:
        datum::bytevector("#u8(4 #u8(5 6])", vec![pool.intern("4")])
            .diagnostic(13, 14, DiagnosticKind::err_parser_mismatched_delimiter)
            .diagnostic( 6, 14, DiagnosticKind::err_parser_invalid_bytevector_element),

        datum::bytevector("#u8(#{9))", vec![])
            .diagnostic(7, 8, DiagnosticKind::err_parser_mismatched_delimiter)
            .diagnostic(4, 8, DiagnosticKind::err_parser_invalid_bytevector_element),

        datum::bytevector("#u8(1 [2} 3)", vec![pool.intern("1"), pool.intern("3")])
            .diagnostic(8, 9, DiagnosticKind::err_parser_mismatched_delimiter)
            .diagnostic(6, 9, DiagnosticKind::err_parser_invalid_bytevector_element),

        datum::bytevector("#u8[4 (car . cdr] 5]", vec![pool.intern("4"), pool.intern("5")])
            .diagnostic(16, 17, DiagnosticKind::err_parser_mismatched_delimiter)
            .diagnostic( 6, 17, DiagnosticKind::err_parser_invalid_bytevector_element),
    ]));

    // TODO: tell the user where the opening parenthesis is (and maybe its kind)
}

#[test]
fn bytevector_missing_delimiters() {
    let pool = InternPool::new();

    check(&pool, datum::line_sequence(vec![
        datum::ignored("#u8(1 2")
            .diagnostic(0, 4, DiagnosticKind::fatal_parser_unterminated_delimiter),
    ]));
}

#[test]
fn bytevector_missing_delimiters_nested() {
    let pool = InternPool::new();

    check(&pool, datum::line_sequence(vec![
        datum::ignored("#u8(1 #u8[2 #u8{3")
            .diagnostic(12, 16, DiagnosticKind::fatal_parser_unterminated_delimiter)
            .diagnostic( 6, 10, DiagnosticKind::fatal_parser_unterminated_delimiter)
            .diagnostic( 0,  4, DiagnosticKind::fatal_parser_unterminated_delimiter),
    ]));

    // TODO: emit only one fatal_parser_unterminated_delimiter and put the delim stack in it
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Vectors

#[test]
fn vector_empty() {
    let pool = InternPool::new();

    check(&pool, datum::line_sequence(vec![
        datum::vector(vec![datum::ignored("#()")]),
        datum::vector(vec![datum::ignored("#[]")]),
        datum::vector(vec![datum::ignored("#{}")]),
    ]));
}

#[test]
fn vector_elements() {
    let pool = InternPool::new();

    check(&pool, datum::line_sequence(vec![
        datum::vector(vec![
            datum::ignored("#("),
            datum::number("123", pool.intern("123")),
            datum::ignored("\t"),
            datum::string("\"test\"", pool.intern("test")),
            datum::ignored(")"),
        ]),
    ]));
}

#[test]
fn vector_nested() {
    let pool = InternPool::new();

    check(&pool, datum::line_sequence(vec![
        datum::vector(vec![
            datum::ignored("#("),
            datum::boolean("#true", true),
            datum::ignored(" "),
            datum::vector(vec![
                datum::ignored("#["),
                datum::boolean("#f", false),
                datum::ignored(" "),
                datum::boolean("#f", false),
                datum::ignored(" "),
                datum::character("#\\u", 'u'),
                datum::ignored("]"),
            ]),
            datum::ignored(" "),
            datum::ignored(")"),
        ]),
    ]));
}

#[test]
fn vector_invalid_dots() {
    let pool = InternPool::new();

    check(&pool, datum::line_sequence(vec![
        datum::vector(vec![
            datum::ignored("#("),
            datum::ignored(".").diagnostic(0, 1, DiagnosticKind::err_parser_misplaced_dot),
            datum::ignored(" "),
            datum::ignored(".").diagnostic(0, 1, DiagnosticKind::err_parser_misplaced_dot),
            datum::ignored(" "),
            datum::ignored(".").diagnostic(0, 1, DiagnosticKind::err_parser_misplaced_dot),
            datum::ignored(")"),
        ]),

        datum::vector(vec![
            datum::ignored("#("),
            datum::symbol("vector", pool.intern("vector")),
            datum::ignored(" "),
            datum::ignored(".").diagnostic(0, 1, DiagnosticKind::err_parser_misplaced_dot),
            datum::ignored(" "),
            datum::number("9", pool.intern("9")),
            datum::ignored(")"),
        ]),

        datum::vector(vec![
            datum::ignored("#["),
            datum::ignored(".").diagnostic(0, 1, DiagnosticKind::err_parser_misplaced_dot),
            datum::ignored(" "),
            datum::number("9", pool.intern("9")),
            datum::ignored("]"),
        ]),

        datum::vector(vec![
            datum::ignored("#{"),
            datum::number("9", pool.intern("9")),
            datum::ignored(" "),
            datum::ignored(".").diagnostic(0, 1, DiagnosticKind::err_parser_misplaced_dot),
            datum::ignored("}"),
        ]),
    ]));
}

#[test]
fn vector_mismatched_delimiters() {
    let pool = InternPool::new();

    check(&pool, datum::line_sequence(vec![
        datum::vector(vec![
            datum::ignored("#{"),
            datum::number("2", pool.intern("2")),
            datum::ignored(" "),
            datum::symbol("+", pool.intern("+")),
            datum::ignored(" "),
            datum::number("2", pool.intern("2")),
            datum::ignored(")").diagnostic(0, 1, DiagnosticKind::err_parser_mismatched_delimiter),
        ]),

        // Nested data:
        datum::vector(vec![
            datum::ignored("#{"),
            datum::vector(vec![
                datum::ignored("#["),
                datum::ignored("}").diagnostic(0, 1, DiagnosticKind::err_parser_mismatched_delimiter),
            ]),
            datum::ignored(")").diagnostic(0, 1, DiagnosticKind::err_parser_mismatched_delimiter),
        ]),
    ]));
}

#[test]
fn vector_missing_delimiters() {
    let pool = InternPool::new();

    check(&pool, datum::line_sequence(vec![
        datum::ignored("#(1 2")
            .diagnostic(0, 2, DiagnosticKind::fatal_parser_unterminated_delimiter),
    ]));
}

#[test]
fn vector_missing_delimiters_nested() {
    let pool = InternPool::new();

    check(&pool, datum::line_sequence(vec![
        datum::ignored("#(#[#u8{")
            .diagnostic(4, 8, DiagnosticKind::fatal_parser_unterminated_delimiter)
            .diagnostic(2, 4, DiagnosticKind::fatal_parser_unterminated_delimiter)
            .diagnostic(0, 2, DiagnosticKind::fatal_parser_unterminated_delimiter),
    ]));
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Proper lists

#[test]
fn proper_list_empty() {
    let pool = InternPool::new();

    check(&pool, datum::line_sequence(vec![
        datum::proper_list(vec![datum::ignored("()")]),
        datum::proper_list(vec![datum::ignored("[]")]),
        datum::proper_list(vec![datum::ignored("{}")]),
    ]));
}

#[test]
fn proper_list_elements() {
    let pool = InternPool::new();

    check(&pool, datum::line_sequence(vec![
        datum::proper_list(vec![
            datum::ignored("("),
            datum::number("123", pool.intern("123")),
            datum::ignored("\t"),
            datum::string("\"test\"", pool.intern("test")),
            datum::ignored(")"),
        ]),
    ]));
}

#[test]
fn proper_list_nested() {
    let pool = InternPool::new();

    check(&pool, datum::line_sequence(vec![
        datum::proper_list(vec![
            datum::ignored("["),
            datum::boolean("#true", true),
            datum::ignored(" "),
            datum::proper_list(vec![
                datum::ignored("{"),
                datum::boolean("#f", false),
                datum::ignored(" "),
                datum::boolean("#f", false),
                datum::ignored(" "),
                datum::character("#\\u", 'u'),
                datum::ignored("}"),
            ]),
            datum::ignored("]"),
        ]),
    ]));
}

#[test]
fn proper_list_mismatched_delimiters() {
    let pool = InternPool::new();

    check(&pool, datum::line_sequence(vec![
        datum::proper_list(vec![
            datum::ignored("["),
            datum::number("2", pool.intern("2")),
            datum::ignored(" "),
            datum::symbol("+", pool.intern("+")),
            datum::ignored(" "),
            datum::number("2", pool.intern("2")),
            datum::ignored("}")
                .diagnostic(0, 1, DiagnosticKind::err_parser_mismatched_delimiter),
        ]),

        // Nested data:
        datum::proper_list(vec![
            datum::ignored("("),
            datum::vector(vec![
                datum::ignored("#["),
                datum::ignored(")")
                    .diagnostic(0, 1, DiagnosticKind::err_parser_mismatched_delimiter),
            ]),
            datum::ignored("]")
                .diagnostic(0, 1, DiagnosticKind::err_parser_mismatched_delimiter),
        ]),
    ]));
}

#[test]
fn proper_list_missing_delimiters() {
    let pool = InternPool::new();

    check(&pool, datum::line_sequence(vec![
        datum::ignored("(1 2")
            .diagnostic(0, 1, DiagnosticKind::fatal_parser_unterminated_delimiter),
    ]));
}

#[test]
fn proper_list_missing_delimiters_nested() {
    let pool = InternPool::new();

    check(&pool, datum::line_sequence(vec![
        datum::ignored("(#u8{#[")
            .diagnostic(5, 7, DiagnosticKind::fatal_parser_unterminated_delimiter)
            .diagnostic(1, 5, DiagnosticKind::fatal_parser_unterminated_delimiter)
            .diagnostic(0, 1, DiagnosticKind::fatal_parser_unterminated_delimiter),
    ]));
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Dotted lists

#[test]
fn dotted_list_empty() {
    let pool = InternPool::new();

    check(&pool, datum::line_sequence(vec![
        datum::proper_list(vec![datum::ignored("(.)")])
            .diagnostic(1, 2, DiagnosticKind::err_parser_misplaced_dot),

        datum::proper_list(vec![datum::ignored("[.]")])
            .diagnostic(1, 2, DiagnosticKind::err_parser_misplaced_dot),

        datum::proper_list(vec![datum::ignored("{.}")])
            .diagnostic(1, 2, DiagnosticKind::err_parser_misplaced_dot),
    ]));
}

#[test]
fn dotted_list_elements() {
    let pool = InternPool::new();

    check(&pool, datum::line_sequence(vec![
        datum::dotted_list(vec![
            datum::ignored("["),
            datum::symbol("car", pool.intern("car")),
            datum::ignored(" . "),
            datum::symbol("cdr", pool.intern("cdr")),
            datum::ignored("]"),
        ]),
    ]));
}

#[test]
fn dotted_list_nested() {
    let pool = InternPool::new();

    check(&pool, datum::line_sequence(vec![
        datum::dotted_list(vec![
            datum::ignored("["),
            datum::number("1", pool.intern("1")),
            datum::ignored(" . "),
            datum::dotted_list(vec![
                datum::ignored("{"),
                datum::number("2", pool.intern("2")),
                datum::ignored(" . "),
                datum::dotted_list(vec![
                    datum::ignored("("),
                    datum::number("3", pool.intern("3")),
                    datum::ignored(" . "),
                    datum::proper_list(vec![datum::ignored("[#||||#]")]),
                    datum::ignored(")"),
                ]),
                datum::ignored("}"),
            ]),
            datum::ignored("]"),
        ]),
    ]));
}

#[test]
fn dotted_list_no_atmosphere() {
    let pool = InternPool::new();

    check(&pool, datum::line_sequence(vec![
        datum::dotted_list(vec![
            datum::ignored("("),
            datum::string("\"car\"", pool.intern("car")),
            datum::ignored("."),
            datum::string("\"cdr\"", pool.intern("cdr")),
            datum::ignored(")"),
        ]),

        datum::dotted_list(vec![
            datum::ignored("("),
            datum::string("\"car\"", pool.intern("car")),
            datum::ignored(". "),
            datum::string("\"cdr\"", pool.intern("cdr")),
            datum::ignored(")"),
        ]),

        datum::dotted_list(vec![
            datum::ignored("("),
            datum::string("\"car\"", pool.intern("car")),
            datum::ignored(" ."),
            datum::string("\"cdr\"", pool.intern("cdr")),
            datum::ignored(")"),
        ]),
    ]));
}

#[test]
fn dotted_list_misplaced_dots_leading() {
    let pool = InternPool::new();

    check(&pool, datum::line_sequence(vec![
        datum::proper_list(vec![
            datum::ignored("("),
            datum::ignored(".").diagnostic(0, 1, DiagnosticKind::err_parser_misplaced_dot),
            datum::ignored(" "),
            datum::number("1", pool.intern("1")),
            datum::ignored(" "),
            datum::number("2", pool.intern("2")),
            datum::ignored(")"),
        ]),

        datum::proper_list(vec![
            datum::ignored("("),
            datum::ignored(".").diagnostic(0, 1, DiagnosticKind::err_parser_misplaced_dot),
            datum::ignored(" "),
            datum::ignored(".").diagnostic(0, 1, DiagnosticKind::err_parser_misplaced_dot),
            datum::ignored(" "),
            datum::symbol("x", pool.intern("x")),
            datum::ignored(")"),
        ]),
    ]));
}

#[test]
fn dotted_list_misplaced_dots_trailing() {
    let pool = InternPool::new();

    check(&pool, datum::line_sequence(vec![
        datum::proper_list(vec![
            datum::ignored("("),
            datum::number("1", pool.intern("1")),
            datum::ignored(" "),
            datum::number("2", pool.intern("2")),
            datum::ignored(" "),
            datum::ignored(".").diagnostic(0, 1, DiagnosticKind::err_parser_misplaced_dot),
            datum::ignored(")"),
        ]),

        datum::proper_list(vec![
            datum::ignored("("),
            datum::symbol("x", pool.intern("x")),
            datum::ignored(" "),
            datum::ignored(".").diagnostic(0, 1, DiagnosticKind::err_parser_misplaced_dot),
            datum::ignored(" "),
            datum::ignored(".").diagnostic(0, 1, DiagnosticKind::err_parser_misplaced_dot),
            datum::ignored(")"),
        ]),
    ]));
}

#[test]
fn dotted_list_misplaced_dots_repeated() {
    let pool = InternPool::new();

    check(&pool, datum::line_sequence(vec![
        datum::proper_list(vec![
            datum::ignored("("),
            datum::ignored(".").diagnostic(0, 1, DiagnosticKind::err_parser_misplaced_dot),
            datum::ignored(" "),
            datum::ignored(".").diagnostic(0, 1, DiagnosticKind::err_parser_misplaced_dot),
            datum::ignored(" "),
            datum::ignored(".").diagnostic(0, 1, DiagnosticKind::err_parser_misplaced_dot),
            datum::ignored(")"),
        ]),
    ]));
}

#[test]
fn dotted_list_misplaced_dots_repeated_with_proper_cdr() {
    let pool = InternPool::new();

    check(&pool, datum::line_sequence(vec![
        datum::proper_list(vec![
            datum::ignored("("),
            datum::number("1", pool.intern("1")),
            datum::ignored(" "),
            datum::number("2", pool.intern("2")),
            datum::ignored(" "),
            datum::ignored(".").diagnostic(0, 1, DiagnosticKind::err_parser_misplaced_dot),
            datum::ignored(" "),
            datum::ignored(".").diagnostic(0, 1, DiagnosticKind::err_parser_misplaced_dot),
            datum::ignored(" "),
            datum::number("3", pool.intern("3")),
            datum::ignored(")"),
        ]),
    ]));
}

#[test]
fn dotted_list_misplaced_dots_interleaved() {
    let pool = InternPool::new();

    check(&pool, datum::line_sequence(vec![
        datum::proper_list(vec![
            datum::ignored("("),
            datum::number("1", pool.intern("1")),
            datum::ignored(" "),
            datum::ignored(".").diagnostic(0, 1, DiagnosticKind::err_parser_misplaced_dot),
            datum::ignored(" "),
            datum::number("2", pool.intern("2")),
            datum::ignored(" "),
            datum::ignored(".").diagnostic(0, 1, DiagnosticKind::err_parser_misplaced_dot),
            datum::ignored(" "),
            datum::number("3", pool.intern("3")),
            datum::ignored(")"),
        ]),
    ]));
}

#[test]
fn dotted_list_misplaced_dots_more_than_one_cdr() {
    let pool = InternPool::new();

    check(&pool, datum::line_sequence(vec![
        datum::proper_list(vec![
            datum::ignored("("),
            datum::number("1", pool.intern("1")),
            datum::ignored(" "),
            datum::number("2", pool.intern("2")),
            datum::ignored(" "),
            datum::ignored(".").diagnostic(0, 1, DiagnosticKind::err_parser_misplaced_dot),
            datum::ignored(" "),
            datum::number("3", pool.intern("3")),
            datum::ignored(" "),
            datum::number("4", pool.intern("4")),
            datum::ignored(")"),
        ]),
    ]));
}

#[test]
fn dotted_list_mismatched_delimiters() {
    let pool = InternPool::new();

    check(&pool, datum::line_sequence(vec![
        datum::dotted_list(vec![
            datum::ignored("("),
            datum::boolean("#t", true),
            datum::ignored(" . "),
            datum::boolean("#false", false),
            datum::ignored("}").diagnostic(0, 1, DiagnosticKind::err_parser_mismatched_delimiter),
        ]),

        datum::dotted_list(vec![
            datum::ignored("("),
            datum::boolean("#f", false),
            datum::ignored(" . "),
            datum::dotted_list(vec![
                datum::ignored("["),
                datum::boolean("#true", true),
                datum::ignored(" . "),
                datum::proper_list(vec![
                    datum::ignored("{"),
                    datum::ignored("]").diagnostic(0, 1, DiagnosticKind::err_parser_mismatched_delimiter),
                ]),
                datum::ignored(")").diagnostic(0, 1, DiagnosticKind::err_parser_mismatched_delimiter),
            ]),
            datum::ignored("}").diagnostic(0, 1, DiagnosticKind::err_parser_mismatched_delimiter),
        ]),
    ]));
}

#[test]
fn dotted_list_missing_delimiters() {
    let pool = InternPool::new();

    check(&pool, datum::line_sequence(vec![
        datum::ignored("(1 2 . 3")
            .diagnostic( 0,  1, DiagnosticKind::fatal_parser_unterminated_delimiter),
    ]));
}

#[test]
fn dotted_list_missing_delimiters_no_cdr() {
    let pool = InternPool::new();

    check(&pool, datum::line_sequence(vec![
        datum::ignored("(1 2 .")
            .diagnostic( 0,  1, DiagnosticKind::fatal_parser_unterminated_delimiter),
    ]));
}

#[test]
fn dotted_list_missing_delimiters_nested() {
    let pool = InternPool::new();

    check(&pool, datum::line_sequence(vec![
        datum::ignored("(1 2 . (3 . #(")
            .diagnostic(12, 14, DiagnosticKind::fatal_parser_unterminated_delimiter)
            .diagnostic( 7,  8, DiagnosticKind::fatal_parser_unterminated_delimiter)
            .diagnostic( 0,  1, DiagnosticKind::fatal_parser_unterminated_delimiter),
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
