// Copyright (c) 2017, Sabre developers
//
// Licensed under the Apache License, Version 2.0 (see LICENSE.Apache in the
// root directory) or MIT license (see LICENSE.MIT in the root directory),
// at your option. This file may be copied, distributed, and modified only
// in accordance with the terms specified by the chosen license.

//! Expander test suite.
//!
//! This verifies that the basic syntax of Scheme is handled as expected.

use std::rc::Rc;

use libeval::{
    environment::Environment,
    expanders::{
        ApplicationExpander, BasicExpander, BeginExpander, Expander, ExpanderStack,
        ExpansionResult, IfExpander, LambdaExpander, QuoteExpander, SetExpander,
    },
};
use liblocus::diagnostics::DiagnosticKind;
use libreader::intern_pool::InternPool;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Tested expanders

struct SchemeBase {
    quote: &'static str,
    begin: &'static str,
    if_: &'static str,
    set: &'static str,
    lambda: &'static str,
}

impl Default for SchemeBase {
    fn default() -> SchemeBase {
        SchemeBase {
            quote: "quote",
            begin: "begin",
            if_: "if",
            set: "set!",
            lambda: "lambda",
        }
    }
}

impl SchemeBase {
    fn make(&self, pool: &InternPool) -> ExpanderStack {
        ExpanderStack::new(Box::new(BasicExpander::new()))
            .push(Box::new(ApplicationExpander::new()))
            .push(Box::new( QuoteExpander::new(pool.intern(self.quote))))
            .push(Box::new( BeginExpander::new(pool.intern(self.begin))))
            .push(Box::new(    IfExpander::new(pool.intern(self.if_))))
            .push(Box::new(   SetExpander::new(pool.intern(self.set))))
            .push(Box::new(LambdaExpander::new(pool.intern(self.lambda))))
    }
}

fn basic_scheme_environment(pool: &InternPool) -> Rc<Environment> {
    Environment::new_imported(&[])
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Literal data

#[test]
fn basic_booleans() {
    TestCase::new()
        .input("#true")
        .result("(Literal #t)")
        .check();
}

#[test]
fn basic_numbers() {
    TestCase::new()
        .input("123.45e+6")
        .result("(Literal 123.45e+6)")
        .check();
}

#[test]
fn basic_characters() {
    TestCase::new()
        .input("#\\x")
        .result("(Literal #\\x0078)")
        .check();
}

#[test]
fn basic_strings() {
    TestCase::new()
        .input("\"test\"")
        .result("(Literal \"test\")")
        .check();
}

#[test]
fn basic_references() {
    TestCase::new()
        .input("test")
        .result("(Reference test)")
        .check();
}

#[test]
fn basic_vectors() {
    TestCase::new()
        .input("#(1 2 3 foo bar (baz))")
        .result("(Literal #(1 2 3 foo bar (baz)))")
        .check();
}

#[test]
fn basic_bytevectors() {
    // We do not yet enforce the number range here. Individual numbers are still just strings.
    TestCase::new()
        .input("#U8(0  0  1  5  255  -Nan.0)")
        .result("(Literal #u8(0 0 1 5 255 -Nan.0))")
        .check();
}

#[test]
fn basic_prohibited_labels() {
    // Datum labels cannot be used in programs. Label markers are ignored during error recovery,
    // label references are replaced with #f literals.
    TestCase::new()
        .input("#1=(lambda (n) (if (<= n 1) 1 (* n (#1# (- n 1)))))")
        .result("(Abstraction (n) \
                   (Alternative (Application (Reference <=) (Reference n) (Literal 1)) \
                     (Literal 1) \
                     (Application (Reference *) (Reference n) \
                      (Application (Literal #f) \
                       (Application (Reference -) (Reference n) (Literal 1))))))")
        .diagnostic( 0,  3, DiagnosticKind::err_expand_datum_label)
        .diagnostic(36, 39, DiagnosticKind::err_expand_datum_label)
        .check();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// `quote` form

#[test]
fn quote_normal() {
    TestCase::new()
        .input("'(1 2 3)")
        .result("(Quotation (1 2 3))")
        .check();
}

#[test]
fn quote_nested() {
    TestCase::new()
        .input("''quote")
        .result("(Quotation (quote quote))")
        .check();
}

#[test]
fn quote_empty() {
    TestCase::new()
        .input("(quote)")
        .result("(Quotation #f)")
        .diagnostic(6, 6, DiagnosticKind::err_expand_invalid_quote)
        .check();
}

#[test]
fn quote_dotted() {
    TestCase::new()
        .input("(quote . 4)")
        .result("(Quotation 4)")
        .diagnostic(6, 9, DiagnosticKind::err_expand_invalid_quote)
        .check();
}

#[test]
fn quote_extra() {
    TestCase::new()
        .input("(quote 1 2 3)")
        .result("(Quotation 3)")
        .diagnostic(9, 12, DiagnosticKind::err_expand_invalid_quote)
        .check();
}

#[test]
fn quote_extra_dotted() {
    TestCase::new()
        .input("(quote 1 2 . 3)")
        .result("(Quotation 3)")
        .diagnostic(9, 14, DiagnosticKind::err_expand_invalid_quote)
        .check();
}

#[test]
fn quote_renaming() {
    TestCase::new()
        .input("'(1 2 3)")
        .expander(SchemeBase { quote: "not-a-quote", ..Default::default() })
        .result("(Application (Reference quote) \
                  (Application (Literal 1) (Literal 2) (Literal 3)))")
        .check();

    TestCase::new()
        .input("(a-quote 5)")
        .expander(SchemeBase { quote: "a-quote", ..Default::default() })
        .result("(Quotation 5)")
        .check();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// `begin` form

#[test]
fn begin_normal() {
    TestCase::new()
        .input("(begin 1 2 3)")
        .result("(Sequence (Literal 1) (Literal 2) (Literal 3))")
        .check();
}

#[test]
fn begin_nested() {
    TestCase::new()
        .input("(begin (begin 1 2) (begin 3 4) (begin (begin 5) 6) 7)")
        .result("(Sequence \
                  (Sequence (Literal 1) (Literal 2)) \
                  (Sequence (Literal 3) (Literal 4)) \
                  (Sequence \
                   (Sequence (Literal 5)) \
                   (Literal 6)) \
                  (Literal 7))")
        .check();
}

#[test]
fn begin_empty() {
    TestCase::new()
        .input("(begin)")
        .result("(Sequence)")
        .diagnostic(6, 6, DiagnosticKind::err_expand_invalid_begin)
        .check();
}

#[test]
fn begin_dotted() {
    TestCase::new()
        .input("(begin 1 2 . 3)")
        .result("(Sequence (Literal 1) (Literal 2) (Literal 3))")
        .diagnostic(10, 13, DiagnosticKind::err_expand_invalid_begin)
        .check();

    TestCase::new()
        .input("(begin . #f)")
        .result("(Sequence (Literal #f))")
        .diagnostic(6, 9, DiagnosticKind::err_expand_invalid_begin)
        .check();
}

#[test]
fn begin_renaming() {
    TestCase::new()
        .input("(seq 1 (seq 2 3))")
        .expander(SchemeBase { begin: "seq", ..Default::default() })
        .result("(Sequence (Literal 1) (Sequence (Literal 2) (Literal 3)))")
        .check();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// `if` form

#[test]
fn if_normal() {
    TestCase::new()
        .input("(if #t 1 2)")
        .result("(Alternative (Literal #t) (Literal 1) (Literal 2))")
        .check();
}

#[test]
fn if_nested() {
    TestCase::new()
        .input("(if (if #t 1 2) 6 (if #f #t #t))")
        .result("(Alternative (Alternative (Literal #t) (Literal 1) (Literal 2)) \
                   (Literal 6) \
                   (Alternative (Literal #f) (Literal #t) (Literal #t)))")
        .check();
}

#[test]
fn if_forms_0() {
    TestCase::new()
        .input("( if )")
        .result("(Alternative (Literal #f) (Literal #f) (Literal #f))")
        .diagnostic(4, 5, DiagnosticKind::err_expand_invalid_if)
        .check();
}

#[test]
fn if_forms_1() {
    TestCase::new()
        .input("( if #false )")
        .result("(Alternative (Literal #f) (Literal #f) (Literal #f))")
        .diagnostic(11, 12, DiagnosticKind::err_expand_invalid_if)
        .check();
}

#[test]
fn if_forms_1_dotted() {
    TestCase::new()
        .input("(if . #f)")
        .result("(Alternative (Literal #f) (Literal #f) (Literal #f))")
        .diagnostic(8, 8, DiagnosticKind::err_expand_invalid_if)
        .diagnostic(3, 6, DiagnosticKind::err_expand_invalid_if)
        .check();
}

#[test]
fn if_forms_2() {
    TestCase::new()
        .input("(if #f 1)")
        .result("(Alternative (Literal #f) (Literal 1) (Literal #f))")
        .diagnostic(8, 8, DiagnosticKind::err_expand_invalid_if)
        .check();
}

#[test]
fn if_forms_2_dotted() {
    TestCase::new()
        .input("(if #f . 1)")
        .result("(Alternative (Literal #f) (Literal 1) (Literal #f))")
        .diagnostic(10, 10, DiagnosticKind::err_expand_invalid_if)
        .diagnostic( 6,  9, DiagnosticKind::err_expand_invalid_if)
        .check();
}

#[test]
fn if_forms_3_dotted() {
    TestCase::new()
        .input("(if #f 1 . 2)")
        .result("(Alternative (Literal #f) (Literal 1) (Literal 2))")
        .diagnostic(8, 11, DiagnosticKind::err_expand_invalid_if)
        .check();
}

#[test]
fn if_forms_5() {
    TestCase::new()
        .input("(if #f 1 2 3 4)")
        .result("(Alternative (Literal #f) (Literal 1) (Literal 2))")
        .diagnostic(11, 14, DiagnosticKind::err_expand_invalid_if)
        .check();
}

#[test]
fn if_forms_5_dotted() {
    TestCase::new()
        .input("(if #f 1 2 3 . 4)")
        .result("(Alternative (Literal #f) (Literal 1) (Literal 2))")
        .diagnostic(11, 16, DiagnosticKind::err_expand_invalid_if)
        .diagnostic(12, 15, DiagnosticKind::err_expand_invalid_if)
        .check();
}

#[test]
fn if_renaming() {
    TestCase::new()
        .input("(whether #false or not)")
        .expander(SchemeBase { if_: "whether", ..Default::default() })
        .result("(Alternative (Literal #f) (Reference or) (Reference not))")
        .check();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// `set!` form

#[test]
fn set_normal() {
    TestCase::new()
        .input("(set! i 0)")
        .result("(Assignment i (Literal 0))")
        .check();
}

#[test]
fn set_nested() {
    TestCase::new()
        .input("(set! n (if #true (begin 1 2) 'y))")
        .result("(Assignment n (Alternative (Literal #t) \
                                 (Sequence (Literal 1) (Literal 2)) \
                                 (Quotation y)))")
        .check();
}

#[test]
fn set_forms_0() {
    TestCase::new()
        .input("(set!)")
        .result("(Literal #f)")
        .diagnostic(5, 5, DiagnosticKind::err_expand_invalid_set)
        .check();
}

#[test]
fn set_forms_1() {
    TestCase::new()
        .input("(set! i)")
        .result("(Assignment i (Literal #f))")
        .diagnostic(7, 7, DiagnosticKind::err_expand_invalid_set)
        .check();
}

#[test]
fn set_forms_1_dotted() {
    TestCase::new()
        .input("(set! . i)")
        .result("(Assignment i (Literal #f))")
        .diagnostic(9, 9, DiagnosticKind::err_expand_invalid_set)
        .diagnostic(5, 8, DiagnosticKind::err_expand_invalid_set)
        .check();
}

#[test]
fn set_forms_2_dotted() {
    TestCase::new()
        .input("(set! i . 5)")
        .result("(Assignment i (Literal 5))")
        .diagnostic(7, 10, DiagnosticKind::err_expand_invalid_set)
        .check();
}

#[test]
fn set_forms_3() {
    TestCase::new()
        .input("(set! i 1 2)")
        .result("(Assignment i (Literal 1))")
        .diagnostic(10, 11, DiagnosticKind::err_expand_invalid_set)
        .check();
}

#[test]
fn set_forms_3_dotted() {
    TestCase::new()
        .input("(set! i 5 . 4)")
        .result("(Assignment i (Literal 5))")
        .diagnostic(12, 13, DiagnosticKind::err_expand_invalid_set)
        .diagnostic( 9, 12, DiagnosticKind::err_expand_invalid_set)
        .check();
}

#[test]
fn set_non_variable() {
    TestCase::new()
        .input("(set! 2 4)")
        .result("(Literal 4)")
        .diagnostic(6, 7, DiagnosticKind::err_expand_invalid_set)
        .check();

    TestCase::new()
        .input("(set! (test test) (if 1 2 3))")
        .result("(Alternative (Literal 1) (Literal 2) (Literal 3))")
        .diagnostic(6, 17, DiagnosticKind::err_expand_invalid_set)
        .check();

    TestCase::new()
        .input("(set! () #true)")
        .result("(Literal #t)")
        .diagnostic(6, 8, DiagnosticKind::err_expand_invalid_set)
        .check();
}

#[test]
fn set_renaming() {
    TestCase::new()
        .input("(!!!SUMMER-ASSIGNMENT!!!)")
        .expander(SchemeBase { set: "!!!SUMMER-ASSIGNMENT!!!", ..Default::default() })
        .result("(Literal #f)")
        .diagnostic(24, 24, DiagnosticKind::err_expand_invalid_set)
        .check();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// `lambda` form

#[test]
fn lambda_normal() {
    TestCase::new()
        .input("(lambda (x) x)")
        .result("(Abstraction (x) (Reference x))")
        .check();
}

#[test]
fn lambda_nested() {
    TestCase::new()
        .input("(lambda (a b) (if (> a b) a b))")
        .result("(Abstraction (a b) \
                   (Alternative (Application (Reference >) (Reference a) (Reference b)) \
                     (Reference a) \
                     (Reference b)))")
        .check();
}

#[test]
fn lambda_forms_0() {
    TestCase::new()
        .input("( lambda )")
        .result("(Abstraction ())")
        .diagnostic(8, 9, DiagnosticKind::err_expand_invalid_lambda)
        .check();
}

#[test]
fn lambda_forms_1() {
    TestCase::new()
        .input("(lambda ())")
        .result("(Abstraction ())")
        .diagnostic(10, 10, DiagnosticKind::err_expand_invalid_lambda)
        .check();
}

#[test]
fn lambda_forms_1_dotted() {
    TestCase::new()
        .input("(lambda . ())")
        .result("(Abstraction ())")
        .diagnostic(12, 12, DiagnosticKind::err_expand_invalid_lambda)
        .diagnostic( 7, 10, DiagnosticKind::err_expand_invalid_lambda)
        .check();
}

#[test]
fn lambda_forms_2_dotted() {
    TestCase::new()
        .input("(lambda () . 9)")
        .result("(Abstraction () (Literal 9))")
        .diagnostic(10, 13, DiagnosticKind::err_expand_invalid_lambda)
        .check();
}

#[test]
fn lambda_forms_3_dotted() {
    TestCase::new()
        .input("(lambda () 1 . 2)")
        .result("(Abstraction () (Literal 1) (Literal 2))")
        .diagnostic(12, 15, DiagnosticKind::err_expand_invalid_lambda)
        .check();
}

#[test]
fn lambda_args_0() {
    TestCase::new()
        .input("(lambda () #f)")
        .result("(Abstraction () (Literal #f))")
        .check();
}

#[test]
fn lambda_args_1() {
    TestCase::new()
        .input("(lambda (a) #f)")
        .result("(Abstraction (a) (Literal #f))")
        .check();
}

#[test]
fn lambda_args_2() {
    TestCase::new()
        .input("(lambda (a b) #f)")
        .result("(Abstraction (a b) (Literal #f))")
        .check();
}

#[test]
fn lambda_args_2_dotted() {
    // Currently we do not support dotted arguments.
    TestCase::new()
        .input("(lambda (a . b) #f)")
        .result("(Abstraction (a b) (Literal #f))")
        .diagnostic(10, 13, DiagnosticKind::err_expand_invalid_lambda)
        .check();
}

#[test]
fn lambda_args_non_variable() {
    TestCase::new()
        .input("(lambda (a b #f c 16 d) #f)")
        .result("(Abstraction (a b c d) (Literal #f))")
        .diagnostic(13, 15, DiagnosticKind::err_expand_invalid_lambda)
        .diagnostic(18, 20, DiagnosticKind::err_expand_invalid_lambda)
        .check();
}

#[test]
fn lambda_args_non_form() {
    // Currenty we do not support argument lists.
    TestCase::new()
        .input("(lambda x x)")
        .result("(Abstraction () (Reference x))")
        .diagnostic(8, 9, DiagnosticKind::err_expand_invalid_lambda)
        .check();

    // Only identifiers may be used for argument lists.
    TestCase::new()
        .input("(lambda 5 5)")
        .result("(Abstraction () (Literal 5))")
        .diagnostic(8, 9, DiagnosticKind::err_expand_invalid_lambda)
        .check();
}

#[test]
fn lambda_args_non_unique() {
    TestCase::new()
        .input("(lambda (x y z z y) !)")
        .result("(Abstraction (x y z) (Reference !))")
        .diagnostic(15, 16, DiagnosticKind::err_expand_invalid_lambda)
        .diagnostic(17, 18, DiagnosticKind::err_expand_invalid_lambda)
        .check();
}

#[test]
fn lambda_renaming() {
    TestCase::new()
        .input("(\u{03BB} (a) #f)")
        .expander(SchemeBase { lambda: "\u{03BB}", ..Default::default() })
        .result("(Abstraction (a) (Literal #f))")
        .check();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Procedure calls

#[test]
fn application_simple() {
    TestCase::new()
        .input("(cons 1 2)")
        .result("(Application (Reference cons) (Literal 1) (Literal 2))")
        .check();
}

#[test]
fn application_nested() {
    TestCase::new()
        .input("(cons 1 (cons 2 (cons 3 '())))")
        .result("(Application (Reference cons) (Literal 1) \
                  (Application (Reference cons) (Literal 2) \
                   (Application (Reference cons) (Literal 3) (Quotation ()))))")
        .check();
}

#[test]
fn application_forms_0() {
    TestCase::new()
        .input("()")
        .result("(Application)")
        .diagnostic(1, 1, DiagnosticKind::err_expand_invalid_application)
        .check();
}

#[test]
fn application_forms_1() {
    TestCase::new()
        .input("(foo)")
        .result("(Application (Reference foo))")
        .check();
}

#[test]
fn application_forms_2_dotted() {
    TestCase::new()
        .input("(foo . bar)")
        .result("(Application (Reference foo) (Reference bar))")
        .diagnostic(4, 7, DiagnosticKind::err_expand_invalid_application)
        .check();
}

#[test]
fn application_forms_3_dotted() {
    TestCase::new()
        .input("(foo bar . baz)")
        .result("(Application (Reference foo) (Reference bar) (Reference baz))")
        .diagnostic(8, 11, DiagnosticKind::err_expand_invalid_application)
        .check();
}

#[test]
fn application_non_reference() {
    TestCase::new()
        .input("((lambda (x) (+ x x)) 5)")
        .result("(Application \
                  (Abstraction (x) (Application (Reference +) (Reference x) (Reference x))) \
                  (Literal 5))")
        .check();

    TestCase::new()
        .input("(9)")
        .result("(Application (Literal 9))")
        .check();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Using all the basic forms

#[test]
fn altogether() {
    TestCase::new()
        .input("(lambda (a b) (if a (begin (set! a 9) (+ b c)) (print '(17 #(x)))))")
        .result(
            "(Abstraction (a b) \
             (Alternative (Reference a) \
             (Sequence \
             (Assignment a (Literal 9)) \
             (Application (Reference +) (Reference b) (Reference c))) \
             (Application (Reference print) (Quotation (17 #(x))))))",
        )
        .check();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Test helpers

use liblocus::diagnostics::{Diagnostic, Span};
use libreader::lexer::StringScanner;
use libreader::parser::Parser;

#[derive(Default)]
struct TestCase {
    input: Option<String>,
    expected_result: Option<String>,
    expected_diagnostics: Vec<Diagnostic>,
    expander_factory: Option<SchemeBase>,
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
            span: Span::new(from, to),
        });
        self
    }

    fn expander(mut self, factory: SchemeBase) -> Self {
        assert!(self.expander_factory.is_none(), "don't set expander twice");
        self.expander_factory = Some(factory);
        self
    }

    fn check(self) {
        let input = self.input.expect("input not set");
        let expected_result = self.expected_result.expect("result not set");
        let expected_diagnostics = self.expected_diagnostics;
        let expander_factory = self.expander_factory.unwrap_or_default();

        check(&expander_factory, &input, &expected_result, &expected_diagnostics);
    }
}

/// Check whether the given expander produces expected results and reports expected diagnostics.
/// Panic if this is not true.
fn check(expander_factory: &SchemeBase, input: &str, expected_result: &str, expected_diagnostics: &[Diagnostic]) {
    use liblocus::utils::collect_diagnostics;
    use libreader::intern_pool::with_formatting_pool;

    let pool = InternPool::new();

    let (datum, parsing_diagnostics) = collect_diagnostics(|handler| {
        let scanner = Box::new(StringScanner::new(input, handler, &pool));
        let mut parser = Parser::new(scanner, &pool, handler);

        let mut all_data = parser.parse_all_data();
        assert!(parser.parse_all_data().is_empty(), "parser did not consume the whole stream");
        assert!(all_data.len() == 1, "input must describe exactly one datum");
        all_data.pop().unwrap()
    });

    assert!(parsing_diagnostics.is_empty(), "parsing produced diagnostics");

    let (expand_result, expand_diagnostics) = collect_diagnostics(|handler| {
        let environment = basic_scheme_environment(&pool);

        let expander = expander_factory.make(&pool);

        expander.expand(&datum, &environment, &handler, &expander)
    });

    let expand_result = match expand_result {
        ExpansionResult::Some(expand_result) =>
            with_formatting_pool(&pool, || format!("{:?}", expand_result)),
        ExpansionResult::None => format!("None"),
        ExpansionResult::Unknown => format!("Unknown"),
    };

    assert_eq!(expand_result, expected_result);
    assert_eq!(expand_diagnostics, expected_diagnostics);
}
