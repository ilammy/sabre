// Copyright (c) 2016, Sabre developers
//
// Licensed under the Apache License, Version 2.0 (see LICENSE.Apache in the
// root directory) or MIT license (see LICENSE.MIT in the root directory),
// at your option. This file may be copied, distributed, and modified only
// in accordance with the terms specified by the chosen license.

//! Parser test suite.
//!
//! This verifies that the parser recognizes all expected expressions and errors.

extern crate liblocus;
extern crate libreader;

use liblocus::diagnostics::{DiagnosticKind};
use libreader::intern_pool::{InternPool};
use libreader::lexer::{StringScanner};
use libreader::parser::{Parser};

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Smoke test of test harness

#[test]
fn smoke_test_empty_string() {
    TestCase::new().input("").result("").check();
}

#[test]
fn smoke_test_directives() {
    TestCase::new().input("#!fold-case").result("").check();
    TestCase::new().input("#!no-fold-case").result("").check();
    TestCase::new().input("#!make-me-a-sandwitch").result("")
        .diagnostic(0, 21, DiagnosticKind::err_lexer_unknown_directive)
        .check();
}

#[test]
fn smoke_test_comments() {
    TestCase::new().input("; test comment").result("").check();
    TestCase::new().input("#| nested #|comments|#|#").result("").check();
}

#[test]
fn smoke_test_comments_unterminated() {
    TestCase::new()
        .input("#| I'm too lazy")
        .result("")
        .diagnostic(0, 15, DiagnosticKind::fatal_lexer_unterminated_comment)
        .check();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Simple literal data

#[test]
fn simple_boolean() {
    TestCase::new().input("#false").result("#f").check();
}

#[test]
fn simple_character() {
    TestCase::new().input("#\\newline")         .result("#\\x000A")         .check();
    TestCase::new().input("#\\X371A")           .result("#\\x371A")         .check();
    TestCase::new().input("#\\?")               .result("#\\x003F")         .check();
    TestCase::new().input("#\\seventeen")       .result("#\\xFFFD")
        .diagnostic(0, 11, DiagnosticKind::err_lexer_unknown_character_name)
        .check();
}

#[test]
fn simple_number() {
    TestCase::new().input("1")                  .result("1")                .check();
    TestCase::new().input("23e+45")             .result("23e+45")           .check();
    TestCase::new().input("#E67-89I")           .result("#E67-89I")         .check();
    TestCase::new().input("-NaN.0")             .result("-NaN.0")           .check();
    TestCase::new().input("#m#x+butterfly")     .result("#m#x+butterfly")
        .diagnostic( 0,  2, DiagnosticKind::err_lexer_invalid_number_prefix)
        .diagnostic( 6,  7, DiagnosticKind::err_lexer_invalid_number_character)
        .diagnostic( 7,  8, DiagnosticKind::err_lexer_invalid_number_character)
        .diagnostic( 8,  9, DiagnosticKind::err_lexer_invalid_number_character)
        .diagnostic(10, 11, DiagnosticKind::err_lexer_invalid_number_character)
        .diagnostic(12, 13, DiagnosticKind::err_lexer_invalid_number_character)
        .diagnostic(13, 14, DiagnosticKind::err_lexer_invalid_number_character)
        .check();
}

#[test]
fn simple_string() {
    // TODO: add escaping to debug output
    TestCase::new().input("\"\"")           .result("\"\"")         .check();
    TestCase::new().input("\"test\"")       .result("\"test\"")     .check();
    TestCase::new().input("\"\\x1234;\"")   .result("\"\u{1234}\"") .check();
}

#[test]
fn simple_string_unterminated() {
    TestCase::new()
        .input("\"CANDLEJA-")
        .result("")
        .diagnostic(0, 10, DiagnosticKind::fatal_lexer_unterminated_string)
        .check();
}

#[test]
fn simple_symbol() {
    // TODO: add escaping to debug output
    TestCase::new().input("hello")      .result("hello")    .check();
    TestCase::new().input("|^_^|")      .result("^_^")      .check();
    TestCase::new().input("||")         .result("")         .check();
    TestCase::new().input("|\\x1111;|") .result("\u{1111}") .check();
}

#[test]
fn simple_symbol_unterminated() {
    TestCase::new()
        .input("|Il1")
        .result("")
        .diagnostic(0, 4, DiagnosticKind::fatal_lexer_unterminated_identifier)
        .check();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Bytevectors

#[test]
fn bytevector_empty() {
    TestCase::new().input("#U8()").result("#u8()").check();
    TestCase::new().input("#u8[]").result("#u8()").check();
    TestCase::new().input("#u8{}").result("#u8()").check();
}

#[test]
fn bytevector_numbers() {
    TestCase::new()
        .input("#u8(1 2 3 #e#x3A)")
        .result("#u8(1 2 3 #e#x3A)")
        .check();
}

#[test]
fn bytevector_padded() {
    TestCase::new()
        .input("#U8( 1 2 )")
        .result("#u8(1 2)")
        .check();
}

#[test]
fn bytevector_out_of_range_numbers() {
    TestCase::new()
        .input("#u8(100500 1e+400 3.1415 -74+i +inf.0)")
        .result("#u8(100500 1e+400 3.1415 -74+i +inf.0)")
        .check();
}

#[test]
fn bytevector_incorrect_numbers() {
    TestCase::new()
        .input("#u8(#k9 1+1 #o9 1/.)")
        .result("#u8(#k9 1+1 #o9 1/.)")
        .diagnostic( 4,  6, DiagnosticKind::err_lexer_invalid_number_prefix)
        .diagnostic(11, 11, DiagnosticKind::err_lexer_missing_i)
        .diagnostic(14, 15, DiagnosticKind::err_lexer_invalid_number_digit)
        .diagnostic(18, 19, DiagnosticKind::err_lexer_digits_missing)
        .diagnostic(18, 19, DiagnosticKind::err_lexer_noninteger_rational)
        .check();
}

#[test]
fn bytevector_invalid_elements() {
    // Only number literals are allowed.
    TestCase::new()
        .input("#u8(#false \"string\" value #\\x42)")
        .result("#u8()")
        .diagnostic( 4, 10, DiagnosticKind::err_parser_invalid_bytevector_element)
        .diagnostic(11, 19, DiagnosticKind::err_parser_invalid_bytevector_element)
        .diagnostic(20, 25, DiagnosticKind::err_parser_invalid_bytevector_element)
        .diagnostic(26, 31, DiagnosticKind::err_parser_invalid_bytevector_element)
        .check();

    // No way bytevectors can be nested.
    TestCase::new()
        .input("#u8(#U8[#u8{0}])")
        .result("#u8()")
        .diagnostic(8, 14, DiagnosticKind::err_parser_invalid_bytevector_element)
        .diagnostic(4, 15, DiagnosticKind::err_parser_invalid_bytevector_element)
        .check();

    // Vectors cannot be nested into bytevectors.
    TestCase::new()
        .input("#u8(#(1 2 3))")
        .result("#u8()")
        .diagnostic(4, 12, DiagnosticKind::err_parser_invalid_bytevector_element)
        .check();

    // Lists cannot be nested into bytevectors.
    TestCase::new()
        .input("#u8(4 {5 6})")
        .result("#u8(4)")
        .diagnostic(6, 11, DiagnosticKind::err_parser_invalid_bytevector_element)
        .check();

    TestCase::new()
        .input("#u8({6 7 . 8})")
        .result("#u8()")
        .diagnostic(4, 13, DiagnosticKind::err_parser_invalid_bytevector_element)
        .check();

    // Abbreviations cannot be nested into bytevectors.
    TestCase::new()
        .input("#U8(1 '2 3)")
        .result("#u8(1 3)")
        .diagnostic(6, 8, DiagnosticKind::err_parser_invalid_bytevector_element)
        .check();

    // Labels cannot be used inside bytevector.
    TestCase::new()
        .input("#u8(#1=2 4 #8# 16)")
        .result("#u8(4 16)")
        .diagnostic( 4,  8, DiagnosticKind::err_parser_invalid_bytevector_element)
        .diagnostic(11, 14, DiagnosticKind::err_parser_invalid_bytevector_element)
        .check();

    // S-expr comments *can* be used inside bytevector.
    TestCase::new()
        .input("#U8(#; 1 2 #;[3 4] 5)")
        .result("#u8(2 5)")
        .check();
}

#[test]
fn bytevector_invalid_dots() {
    // Dot notation cannot be used in bytevectors.
    TestCase::new()
        .input("#u8(1 2 . 3)")
        .result("#u8(1 2 3)")
        .diagnostic(8, 9, DiagnosticKind::err_parser_misplaced_dot)
        .check();

    TestCase::new()
        .input("#u8(. . .)")
        .result("#u8()")
        .diagnostic(4, 5, DiagnosticKind::err_parser_misplaced_dot)
        .diagnostic(6, 7, DiagnosticKind::err_parser_misplaced_dot)
        .diagnostic(8, 9, DiagnosticKind::err_parser_misplaced_dot)
        .check();
}

#[test]
fn bytevector_mismatched_delimiters() {
    TestCase::new()
        .input("#u8(1 2 3]")
        .result("#u8(1 2 3)")
        .diagnostic(9, 10, DiagnosticKind::err_parser_mismatched_delimiter)
        .check();

    TestCase::new()
        .input("#u8(4 #u8(5 6])")
        .result("#u8(4)")
        .diagnostic(13, 14, DiagnosticKind::err_parser_mismatched_delimiter)
        .diagnostic( 6, 14, DiagnosticKind::err_parser_invalid_bytevector_element)
        .check();

    TestCase::new()
        .input("#u8(#{9))")
        .result("#u8()")
        .diagnostic(7, 8, DiagnosticKind::err_parser_mismatched_delimiter)
        .diagnostic(4, 8, DiagnosticKind::err_parser_invalid_bytevector_element)
        .check();

    TestCase::new()
        .input("#u8(1 [2} 3)")
        .result("#u8(1 3)")
        .diagnostic(8, 9, DiagnosticKind::err_parser_mismatched_delimiter)
        .diagnostic(6, 9, DiagnosticKind::err_parser_invalid_bytevector_element)
        .check();

    TestCase::new()
        .input("#u8[4 (car . cdr] 5]")
        .result("#u8(4 5)")
        .diagnostic(16, 17, DiagnosticKind::err_parser_mismatched_delimiter)
        .diagnostic( 6, 17, DiagnosticKind::err_parser_invalid_bytevector_element)
        .check();

    // TODO: tell the user where the opening parenthesis is (and maybe its kind)
}

#[test]
fn bytevector_missing_delimiters() {
    TestCase::new()
        .input("#u8(1 2")
        .result("")
        .diagnostic(0, 4, DiagnosticKind::fatal_parser_unterminated_delimiter)
        .check();
}

#[test]
fn bytevector_missing_delimiters_nested() {
    TestCase::new()
        .input("#u8(1 #u8[2 #u8{3")
        .result("")
        .diagnostic(12, 16, DiagnosticKind::fatal_parser_unterminated_delimiter)
        .check();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Vectors

#[test]
fn vector_empty() {
    TestCase::new().input("#()").result("#()").check();
    TestCase::new().input("#[]").result("#()").check();
    TestCase::new().input("#{}").result("#()").check();
}

#[test]
fn vector_elements() {
    TestCase::new()
        .input("#(123\t\"test\")")
        .result("#(123 \"test\")")
        .check();
}

#[test]
fn vector_nested() {
    TestCase::new()
        .input("#(#true #[#f #f #\\u])")
        .result("#(#t #(#f #f #\\x0075))")
        .check();
}

#[test]
fn vector_invalid_dots() {
    TestCase::new()
        .input("#(. . .)")
        .result("#()")
        .diagnostic(2, 3, DiagnosticKind::err_parser_misplaced_dot)
        .diagnostic(4, 5, DiagnosticKind::err_parser_misplaced_dot)
        .diagnostic(6, 7, DiagnosticKind::err_parser_misplaced_dot)
        .check();

    TestCase::new()
        .input("#(vector . 9)")
        .result("#(vector 9)")
        .diagnostic(9, 10, DiagnosticKind::err_parser_misplaced_dot)
        .check();

    TestCase::new()
        .input("#[. 9]")
        .result("#(9)")
        .diagnostic(2, 3, DiagnosticKind::err_parser_misplaced_dot)
        .check();

    TestCase::new()
        .input("#{9 .}")
        .result("#(9)")
        .diagnostic(4, 5, DiagnosticKind::err_parser_misplaced_dot)
        .check();
}

#[test]
fn vector_mismatched_delimiters() {
    TestCase::new()
        .input("#{2 + 2)")
        .result("#(2 + 2)")
        .diagnostic(7, 8, DiagnosticKind::err_parser_mismatched_delimiter)
        .check();

    TestCase::new()
        .input("#{#[})")
        .result("#(#())")
        .diagnostic(4, 5, DiagnosticKind::err_parser_mismatched_delimiter)
        .diagnostic(5, 6, DiagnosticKind::err_parser_mismatched_delimiter)
        .check();
}

#[test]
fn vector_missing_delimiters() {
    TestCase::new()
        .input("#(1 2")
        .result("")
        .diagnostic(0, 2, DiagnosticKind::fatal_parser_unterminated_delimiter)
        .check();
}

#[test]
fn vector_missing_delimiters_nested() {
    TestCase::new()
        .input("#(#[#u8{")
        .result("")
        .diagnostic(4, 8, DiagnosticKind::fatal_parser_unterminated_delimiter)
        .check();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Proper lists

#[test]
fn proper_list_empty() {
    TestCase::new().input("()").result("()").check();
    TestCase::new().input("[]").result("()").check();
    TestCase::new().input("{}").result("()").check();
}

#[test]
fn proper_list_elements() {
    TestCase::new()
        .input("(123\t\"test\")")
        .result("(123 \"test\")")
        .check();
}

#[test]
fn proper_list_nested() {
    TestCase::new()
        .input("[#true {#f #f #\\u}]")
        .result("(#t (#f #f #\\x0075))")
        .check();
}

#[test]
fn proper_list_mismatched_delimiters() {
    TestCase::new()
        .input("[2 + 2}")
        .result("(2 + 2)")
        .diagnostic(6, 7, DiagnosticKind::err_parser_mismatched_delimiter)
        .check();

    TestCase::new()
        .input("(#[)]")
        .result("(#())")
        .diagnostic(3, 4, DiagnosticKind::err_parser_mismatched_delimiter)
        .diagnostic(4, 5, DiagnosticKind::err_parser_mismatched_delimiter)
        .check();
}

#[test]
fn proper_list_missing_delimiters() {
    TestCase::new()
        .input("(1 2")
        .result("")
        .diagnostic(0, 1, DiagnosticKind::fatal_parser_unterminated_delimiter)
        .check();
}

#[test]
fn proper_list_missing_delimiters_nested() {
    TestCase::new()
        .input("(#u8{#[")
        .result("")
        .diagnostic(5, 7, DiagnosticKind::fatal_parser_unterminated_delimiter)
        .check();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Dotted lists

#[test]
fn dotted_list_empty() {
    TestCase::new()
        .input("(.)")
        .result("()")
        .diagnostic(1, 2, DiagnosticKind::err_parser_misplaced_dot)
        .check();

    TestCase::new()
        .input("[.]")
        .result("()")
        .diagnostic(1, 2, DiagnosticKind::err_parser_misplaced_dot)
        .check();

    TestCase::new()
        .input("{.}")
        .result("()")
        .diagnostic(1, 2, DiagnosticKind::err_parser_misplaced_dot)
        .check();
}

#[test]
fn dotted_list_elements() {
    TestCase::new()
        .input("[car . cdr]")
        .result("(car . cdr)")
        .check();
}

#[test]
fn dotted_list_nested() {
    TestCase::new()
        .input("[1 . {2 . (3 . [#||||#])}]")
        .result("(1 . (2 . (3 . ())))")
        .check();
}

#[test]
fn dotted_list_no_atmosphere() {
    TestCase::new()
        .input("(\"car\".\"cdr\")")
        .result("(\"car\" . \"cdr\")")
        .check();

    TestCase::new()
        .input("(\"car\". \"cdr\")")
        .result("(\"car\" . \"cdr\")")
        .check();

    TestCase::new()
        .input("(\"car\" .\"cdr\")")
        .result("(\"car\" . \"cdr\")")
        .check();
}

#[test]
fn dotted_list_misplaced_dots_leading() {
    TestCase::new()
        .input("(. 1 2)")
        .result("(1 2)")
        .diagnostic(1, 2, DiagnosticKind::err_parser_misplaced_dot)
        .check();

    TestCase::new()
        .input("(. . x)")
        .result("(x)")
        .diagnostic(1, 2, DiagnosticKind::err_parser_misplaced_dot)
        .diagnostic(3, 4, DiagnosticKind::err_parser_misplaced_dot)
        .check();
}

#[test]
fn dotted_list_misplaced_dots_trailing() {
    TestCase::new()
        .input("(1 2 .)")
        .result("(1 2)")
        .diagnostic(5, 6, DiagnosticKind::err_parser_misplaced_dot)
        .check();

    TestCase::new()
        .input("(x . .)")
        .result("(x)")
        .diagnostic(3, 4, DiagnosticKind::err_parser_misplaced_dot)
        .diagnostic(5, 6, DiagnosticKind::err_parser_misplaced_dot)
        .check();
}

#[test]
fn dotted_list_misplaced_dots_repeated() {
    TestCase::new()
        .input("(. . .)")
        .result("()")
        .diagnostic(1, 2, DiagnosticKind::err_parser_misplaced_dot)
        .diagnostic(3, 4, DiagnosticKind::err_parser_misplaced_dot)
        .diagnostic(5, 6, DiagnosticKind::err_parser_misplaced_dot)
        .check();
}

#[test]
fn dotted_list_misplaced_dots_repeated_with_proper_cdr() {
    TestCase::new()
        .input("(1 2 . . 3)")
        .result("(1 2 3)")
        .diagnostic(5, 6, DiagnosticKind::err_parser_misplaced_dot)
        .diagnostic(7, 8, DiagnosticKind::err_parser_misplaced_dot)
        .check();
}

#[test]
fn dotted_list_misplaced_dots_interleaved() {
    TestCase::new()
        .input("(1 . 2 . 3)")
        .result("(1 2 3)")
        .diagnostic(3, 4, DiagnosticKind::err_parser_misplaced_dot)
        .diagnostic(7, 8, DiagnosticKind::err_parser_misplaced_dot)
        .check();
}

#[test]
fn dotted_list_misplaced_dots_more_than_one_cdr() {
    TestCase::new()
        .input("(1 2 . 3 4)")
        .result("(1 2 3 4)")
        .diagnostic(5, 6, DiagnosticKind::err_parser_misplaced_dot)
        .check();
}

#[test]
fn dotted_list_mismatched_delimiters() {
    TestCase::new()
        .input("(#t . #false}")
        .result("(#t . #f)")
        .diagnostic(12, 13, DiagnosticKind::err_parser_mismatched_delimiter)
        .check();

    TestCase::new()
        .input("(#f . [#true . {])}")
        .result("(#f . (#t . ()))")
        .diagnostic(16, 17, DiagnosticKind::err_parser_mismatched_delimiter)
        .diagnostic(17, 18, DiagnosticKind::err_parser_mismatched_delimiter)
        .diagnostic(18, 19, DiagnosticKind::err_parser_mismatched_delimiter)
        .check();
}

#[test]
fn dotted_list_missing_delimiters() {
    TestCase::new()
        .input("(1 2 . 3")
        .result("")
        .diagnostic(0, 1, DiagnosticKind::fatal_parser_unterminated_delimiter)
        .check();
}

#[test]
fn dotted_list_missing_delimiters_no_cdr() {
    TestCase::new()
        .input("(1 2 .")
        .result("")
        .diagnostic(0, 1, DiagnosticKind::fatal_parser_unterminated_delimiter)
        .check();
}

#[test]
fn dotted_list_missing_delimiters_nested() {
    TestCase::new()
        .input("(1 2 . (3 . #(")
        .result("")
        .diagnostic(12, 14, DiagnosticKind::fatal_parser_unterminated_delimiter)
        .check();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Abbreviations

#[test]
fn abbreviation_simple() {
    TestCase::new()
        .input("'\"a string\"")
        .result("(quote \"a string\")")
        .check();

    TestCase::new()
        .input("`#u8(0 0 7)")
        .result("(quasiquote #u8(0 0 7))")
        .check();

    TestCase::new()
        .input(",var")
        .result("(unquote var)")
        .check();

    TestCase::new()
        .input(",@another")
        .result("(unquote-splicing another)")
        .check();
}

#[test]
fn abbreviation_complex() {
    TestCase::new()
        .input("`(,(+ 2 2) '#[x y] . ,@dumb-list)")
        .result("(quasiquote ((unquote (+ 2 2)) (quote #(x y)) . (unquote-splicing dumb-list)))")
        .check();
}

#[test]
fn abbreviation_nested() {
    TestCase::new()
        .input("''``,,@,,@'(9)")
        .result("(quote (quote (quasiquote \
                    (quasiquote (unquote (unquote-splicing \
                        (unquote (unquote-splicing (quote (9))))))))))")
        .check();
}

#[test]
fn abbreviation_interspersed_sexpr_comments() {
    TestCase::new()
        .input("`#;%^&*()")
        .result("(quasiquote ())")
        .check();
}

#[test]
fn abbreviation_missing_data_eof() {
    TestCase::new()
        .input("(1 '")
        .result("")
        .diagnostic(4, 4, DiagnosticKind::err_parser_missing_datum)
        .diagnostic(0, 1, DiagnosticKind::fatal_parser_unterminated_delimiter)
        .check();
}

#[test]
fn abbreviation_missing_data_complex() {
    TestCase::new()
        .input("(2 .',`)")
        .result("(2)")
        .diagnostic(7, 7, DiagnosticKind::err_parser_missing_datum)
        .diagnostic(6, 6, DiagnosticKind::err_parser_missing_datum)
        .diagnostic(5, 5, DiagnosticKind::err_parser_missing_datum)
        .diagnostic(3, 4, DiagnosticKind::err_parser_misplaced_dot)
        .check();
}

#[test]
fn abbreviation_missing_data_after_sexpr_comments() {
    TestCase::new()
        .input("(8 ',`#;)")
        .result("(8)")
        .diagnostic(8, 8, DiagnosticKind::err_parser_missing_datum)
        .diagnostic(6, 6, DiagnosticKind::err_parser_missing_datum)
        .diagnostic(5, 5, DiagnosticKind::err_parser_missing_datum)
        .diagnostic(4, 4, DiagnosticKind::err_parser_missing_datum)
        .check();
}

#[test]
fn abbreviation_invalid_dot() {
    TestCase::new()
        .input("(1 '. 2)")
        .result("(1 . 2)")
        .diagnostic(4, 4, DiagnosticKind::err_parser_missing_datum)
        .check();

    TestCase::new()
        .input("#(1 '. 2)")
        .result("#(1 2)")
        .diagnostic(5, 5, DiagnosticKind::err_parser_missing_datum)
        .diagnostic(5, 6, DiagnosticKind::err_parser_misplaced_dot)
        .check();

    TestCase::new()
        .input("(1 . ' 2)")
        .result("(1 . (quote 2))")
        .check();

    TestCase::new()
        .input("'.()")
        .result("()")
        .diagnostic(1, 1, DiagnosticKind::err_parser_missing_datum)
        .diagnostic(1, 2, DiagnosticKind::err_parser_misplaced_dot)
        .check();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Labels

#[test]
fn labels_simple() {
    TestCase::new()
        .input("#0=test")
        .result("#0=test")
        .check();

    TestCase::new()
        .input("#1=9")
        .result("#1=9")
        .check();

    TestCase::new()
        .input("#2= #U8(3 4 5)")
        .result("#2=#u8(3 4 5)")
        .check();

    TestCase::new()
        .input("#59595#")
        .result("#59595#")
        .check();

    TestCase::new()
        .input("#9=#9#")
        .result("#9=#9#")
        .check();
}

#[test]
fn labels_complex() {
    TestCase::new()
        .input("#0= #1=(1 . #1#)")
        .result("#0=#1=(1 . #1#)")
        .check();
}

#[test]
fn labels_interspersed_sexpr_comments() {
    TestCase::new()
        .input("#0= #;1 #;2 3")
        .result("#0=3")
        .check();
}

#[test]
fn labels_data_eof() {
    TestCase::new()
        .input("#1=(2")
        .result("")
        .diagnostic(3, 4, DiagnosticKind::fatal_parser_unterminated_delimiter)
        .check();
}

#[test]
fn labels_missing_data_eof() {
    TestCase::new()
        .input("#1=")
        .result("")
        .diagnostic(3, 3, DiagnosticKind::err_parser_missing_datum)
        .check();
}

#[test]
fn labels_missing_data_complex() {
    TestCase::new()
        .input("(2 #0=)")
        .result("(2)")
        .diagnostic(6, 6, DiagnosticKind::err_parser_missing_datum)
        .check();
}

#[test]
fn labels_missing_data_after_sexpr_comments() {
    TestCase::new()
        .input("(9 #0= #;1 #;2)")
        .result("(9)")
        .diagnostic(6, 6, DiagnosticKind::err_parser_missing_datum)
        .check();
}

#[test]
fn labels_invalid_dot() {
    TestCase::new()
        .input("(3 #666=. ())")
        .result("(3 . ())")
        .diagnostic(8, 8, DiagnosticKind::err_parser_missing_datum)
        .check();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// S-expression comments

#[test]
fn sexpr_comment_simple() {
    TestCase::new()
        .input("#; 1 2")
        .result("2")
        .check();
}

#[test]
fn sexpr_comment_complex() {
    TestCase::new()
        .input("#;(1 #(2) 3 . 4) 5")
        .result("5")
        .check();
}

#[test]
fn sexpr_comment_nested_data() {
    TestCase::new()
        .input("#;[1 #; #(2 #;3) 4] #(#; (5 6) var)")
        .result("#(var)")
        .check();
}

#[test]
fn sexpr_comment_nested_comments() {
    TestCase::new()
        .input("#; #; 1 2 3 #; #;4 #;5 6 7")
        .result("3 7")
        .check();
}

#[test]
fn sexpr_comment_masked_line_comments() {
    TestCase::new()
        .input(";#; ( in . valid .\n#\\x1234")
        .result("#\\x1234")
        .check();
}

#[test]
fn sexpr_comment_masked_block_comments() {
    TestCase::new()
        .input("#|#;(in . valid .|# #\\x1234")
        .result("#\\x1234")
        .check();
}

#[test]
fn sexpr_comment_interspersed_line_comments() {
    TestCase::new()
        .input("#;\n; test\n1\n2")
        .result("2")
        .check();
}

#[test]
fn sexpr_comment_interspersed_block_comments() {
    TestCase::new()
        .input("#;#||#5 10")
        .result("10")
        .check();
}

#[test]
fn sexpr_comment_data_eof() {
    TestCase::new()
        .input("#; (1 2 3")
        .result("")
        .diagnostic(3, 4, DiagnosticKind::fatal_parser_unterminated_delimiter)
        .check();
}

#[test]
fn sexpr_comment_missing_data_eof() {
    TestCase::new()
        .input("#;")
        .result("")
        .diagnostic(2, 2, DiagnosticKind::err_parser_missing_datum)
        .check();
}

#[test]
fn sexpr_comment_missing_data_complex() {
    TestCase::new()
        .input("(1 #;)")
        .result("(1)")
        .diagnostic(5, 5, DiagnosticKind::err_parser_missing_datum)
        .check();
}

#[test]
fn sexpr_comment_invalid_dots() {
    TestCase::new()
        .input("(3 #; . 4)")
        .result("(3 . 4)")
        .diagnostic(5, 5, DiagnosticKind::err_parser_missing_datum)
        .check();
}

#[test]
fn sexpr_comment_errors_in_datum() {
    TestCase::new()
        .input("#; (\"\\xDEAD;\"]")
        .result("")
        .diagnostic( 5, 12, DiagnosticKind::err_lexer_invalid_unicode_range)
        .diagnostic(13, 14, DiagnosticKind::err_parser_mismatched_delimiter)
        .check();
}

#[test]
fn sexpr_comment_srfi_62_examples() {
    TestCase::new()
        .input("(+ 1 #;(* 2 3) 4)")
        .result("(+ 1 4)")
        .check();

    TestCase::new()
        .input("(list 'x #;'y 'z)")
        .result("(list (quote x) (quote z))")
        .check();

    TestCase::new()
        .input("(* 3 #;(+ 1 2))")
        .result("(* 3)")
        .check();

    TestCase::new()
        .input("(#;sqrt abs -16)")
        .result("(abs -16)")
        .check();

    TestCase::new()
        .input("(list 'a #; #;'b 'c 'd)")
        .result("(list (quote a) (quote d))")
        .check();

    TestCase::new()
        .input("(list 'a #;(list 'b #;c 'd) 'e)")
        .result("(list (quote a) (quote e))")
        .check();

    TestCase::new()
        .input("'(a . #;b c)")
        .result("(quote (a . c))")
        .check();

    TestCase::new()
        .input("'(a . b #;c)")
        .result("(quote (a . b))")
        .check();

    TestCase::new()
        .input("(#;a . b)")
        .result("(b)")
        .diagnostic(5, 6, DiagnosticKind::err_parser_misplaced_dot)
        .check();

    TestCase::new()
        .input("(a . #;b)")
        .result("(a)")
        .diagnostic(3, 4, DiagnosticKind::err_parser_misplaced_dot)
        .check();

    TestCase::new()
        .input("(a #; . b)")
        .result("(a . b)")
        .diagnostic(5, 5, DiagnosticKind::err_parser_missing_datum)
        .check();

    TestCase::new()
        .input("(#;x #;y . z)")
        .result("(z)")
        .diagnostic(9, 10, DiagnosticKind::err_parser_misplaced_dot)
        .check();

    TestCase::new()
        .input("(#; #;x #;y . z)")
        .result("(z)")
        .diagnostic( 3,  3, DiagnosticKind::err_parser_missing_datum)
        .diagnostic(12, 13, DiagnosticKind::err_parser_misplaced_dot)
        .check();

    TestCase::new()
        .input("(#; #;x . z)")
        .result("(z)")
        .diagnostic(3, 3, DiagnosticKind::err_parser_missing_datum)
        .diagnostic(8, 9, DiagnosticKind::err_parser_misplaced_dot)
        .check();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Unmatched closing parentheses

#[test]
fn unmatched_paren_alone() {
    TestCase::new()
        .input(")")
        .result("")
        .diagnostic(0, 1, DiagnosticKind::err_parser_extra_delimiter)
        .check();
}

#[test]
fn unmatched_paren_after_normal_comments() {
    TestCase::new()
        .input("#|{|#}")
        .result("")
        .diagnostic(5, 6, DiagnosticKind::err_parser_extra_delimiter)
        .check();
}

#[test]
fn unmatched_paren_after_sexpr_comment() {
    TestCase::new()
        .input("#;(9))")
        .result("")
        .diagnostic(5, 6, DiagnosticKind::err_parser_extra_delimiter)
        .check();
}

#[test]
fn unmatched_paren_after_sexpr_comment_with_missing_data() {
    TestCase::new()
        .input("#;)")
        .result("")
        .diagnostic(2, 2, DiagnosticKind::err_parser_missing_datum)
        .diagnostic(2, 3, DiagnosticKind::err_parser_extra_delimiter)
        .check();
}

#[test]
fn unmatched_paren_after_misplaced_dot() {
    TestCase::new()
        .input(". )")
        .result("")
        .diagnostic(0, 1, DiagnosticKind::err_parser_misplaced_dot)
        .diagnostic(2, 3, DiagnosticKind::err_parser_extra_delimiter)
        .check();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Test helpers

use liblocus::diagnostics::{Diagnostic, Span};

#[derive(Default)]
struct TestCase {
    input: Option<String>,
    expected_result: Option<String>,
    expected_diagnostics: Vec<Diagnostic>,
}

impl TestCase {
    fn new() -> TestCase {
        TestCase::default()
    }

    fn input<T: Into<String>>(mut self, input: T) -> Self {
        assert!(self.input.is_none(), "don't set input twice");
        self.input = Some(input.into());
        self
    }

    fn result<T: Into<String>>(mut self, result: T) -> Self {
        assert!(self.expected_result.is_none(), "don't set result twice");
        self.expected_result = Some(result.into());
        self
    }

    fn diagnostic(mut self, from: usize, to: usize, kind: DiagnosticKind) -> Self {
        self.expected_diagnostics.push(Diagnostic {
            kind: kind,
            span: Span::new(from, to)
        });
        self
    }

    fn check(self) {
        let input = self.input.expect("input not set");
        let expected_result = self.expected_result.expect("result not set");

        check(&input, &expected_result, &self.expected_diagnostics);
    }
}

/// Check whether the parser produces expected results and reports expected diagnostics
/// when given a sequence of tokens produced from a given string by `StringScanner`.
/// Panic if this is not true.
fn check(input: &str, expected_result: &str, expected_diagnostics: &[Diagnostic]) {
    use liblocus::utils::collect_diagnostics;
    use libreader::intern_pool::with_formatting_pool;

    let pool = InternPool::new();

    let (data, diagnostics) = collect_diagnostics(|handler| {
        let scanner = Box::new(StringScanner::new(input, handler, &pool));
        let mut parser = Parser::new(scanner, &pool, handler);

        let all_data = parser.parse_all_data();
        assert!(parser.parse_all_data().is_empty(), "parser did not consume the whole stream");
        all_data
    });

    let result = with_formatting_pool(&pool,
        || join(data.iter().map(|d| format!("{:?}", d)), " "));

    assert_eq!(result, expected_result);
    assert_eq!(diagnostics, expected_diagnostics);
}

fn join<I, T>(items: I, sep: &str) -> String
    where I: IntoIterator<Item=T>,
          T: AsRef<str>
{
    let mut result = String::new();

    for item in items {
        result.push_str(item.as_ref());
        result.push_str(sep);
    }

    if !result.is_empty() {
        let new_len = result.len() - sep.len();
        result.truncate(new_len);
    }

    return result;
}
