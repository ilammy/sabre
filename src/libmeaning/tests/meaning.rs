// Copyright (c) 2017, Sabre developers
//
// Licensed under the Apache License, Version 2.0 (see LICENSE.Apache in the
// root directory) or MIT license (see LICENSE.MIT in the root directory),
// at your option. This file may be copied, distributed, and modified only
// in accordance with the terms specified by the chosen license.

//! Expression treater test suite.
//!
//! This verifies that the basic semantics of Scheme is handled as expected.

use libmeaning::{meaning, MeaningResult, Value};

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Tested expanders and environments

use std::rc::Rc;

use libexpand::environment::Environment;
use libexpand::expanders::{BeginExpander, IfExpander, LambdaExpander, QuoteExpander, SetExpander};
use libexpand::expression::Variable;
use libexpand::Expander;
use liblocus::diagnostics::{DiagnosticKind, Span};
use libreader::intern_pool::InternPool;

macro_rules! syntax {
    ($pool:expr, $name:expr, $type:ty) => {{
        let name = $pool.intern($name);
        (
            Variable {
                name,
                span: Span::new(0, 0),
            },
            Box::new(<$type>::new(name)),
        )
    }};
}

fn basic_scheme_environment(pool: &InternPool) -> Rc<Environment> {
    let keywords: Vec<(Variable, Box<Expander>)> = vec![
        syntax!(pool, "quote", QuoteExpander),
        syntax!(pool, "begin", BeginExpander),
        syntax!(pool, "if", IfExpander),
        syntax!(pool, "set!", SetExpander),
        syntax!(pool, "lambda", LambdaExpander),
    ];
    let imported_vars = [
        Variable {
            name: pool.intern("car"),
            span: Span::new(0, 0),
        },
        Variable {
            name: pool.intern("cdr"),
            span: Span::new(0, 0),
        },
        Variable {
            name: pool.intern("cons"),
            span: Span::new(0, 0),
        },
    ];
    let global_vars = [Variable {
        name: pool.intern("*global*"),
        span: Span::new(0, 0),
    }];

    let imported_env = Environment::new_imported(&imported_vars, keywords);
    let global_env = Environment::new_global(&global_vars, &imported_env);

    return global_env;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Edge cases

#[test]
fn empty_file() {
    TestCase::new()
        .input("")
        .meaning("(Sequence)")
        .check();
}

#[test]
fn only_comments() {
    TestCase::new()
        .input("; test test")
        .meaning("(Sequence)")
        .check();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Constants

#[test]
fn literals() {
    TestCase::new()
        .input("42 #\\x #false \"string\"")
        .meaning("(Sequence (Constant 0) (Constant 1) (Constant 2) (Constant 3))")
        .constants(|pool| {
            vec![
                Value::Number(pool.intern("42")),
                Value::Character('x'),
                Value::Boolean(false),
                Value::String(pool.intern("string")),
            ]
        })
        .check();
}

#[test]
fn quote_literals() {
    TestCase::new()
        .input("'123 (quote #\\!) '#t (quote \"\")")
        .meaning("(Sequence (Constant 0) (Constant 1) (Constant 2) (Constant 3))")
        .constants(|pool| {
            vec![
                Value::Number(pool.intern("123")),
                Value::Character('!'),
                Value::Boolean(true),
                Value::String(pool.intern("")),
            ]
        })
        .check();
}

#[test]
fn constants_are_duplicated() {
    TestCase::new()
        .input("(begin 1 2 3 1 2 3 1)")
        .meaning("(Sequence \
                    (Constant 0) (Constant 1) (Constant 2) (Constant 3) \
                    (Constant 4) (Constant 5) (Constant 6))")
        .constants(|pool| {
            vec![
                Value::Number(pool.intern("1")),
                Value::Number(pool.intern("2")),
                Value::Number(pool.intern("3")),
                Value::Number(pool.intern("1")),
                Value::Number(pool.intern("2")),
                Value::Number(pool.intern("3")),
                Value::Number(pool.intern("1")),
            ]
        })
        .check();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Global and imported variables

#[test]
fn reference_imported() {
    TestCase::new()
        .input("car")
        .meaning("(Sequence (ImportedReference 0))")
        .check();
    TestCase::new()
        .input("cdr")
        .meaning("(Sequence (ImportedReference 1))")
        .check();
    TestCase::new()
        .input("cons")
        .meaning("(Sequence (ImportedReference 2))")
        .check();
}

#[test]
fn reference_global() {
    TestCase::new()
        .input("*global*")
        .meaning("(Sequence (GlobalReference 0))")
        .check();
}

#[test]
fn reference_undefined() {
    TestCase::new()
        .input("@@UNDEFINED_VARIABLE@@")
        .meaning("(Sequence (Undefined))")
        .diagnostic(0, 22, DiagnosticKind::err_meaning_unresolved_variable)
        .check();
}

#[test]
fn reference_syntactic() {
    TestCase::new()
        .input("begin")
        .meaning("(Sequence (Undefined))")
        .diagnostic(0, 5, DiagnosticKind::err_meaning_reference_to_syntactic_binding)
        .check();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Alternative

#[test]
fn alternative() {
    TestCase::new()
        .input("(if #t 111 222)")
        .meaning("(Sequence (Alternative (Constant 0) (Constant 1) (Constant 2)))")
        .check();

    TestCase::new()
        .input("(if *global* car cdr)")
        .meaning("(Sequence \
                    (Alternative (GlobalReference 0) \
                      (ImportedReference 0) \
                      (ImportedReference 1)))")
        .check();
}

#[test]
fn alternative_nested() {
    TestCase::new()
        .input("(if (if #f 111 #t) 222 333)")
        .meaning("(Sequence \
                    (Alternative (Alternative (Constant 0) \
                                   (Constant 1) \
                                   (Constant 2)) \
                      (Constant 3) \
                      (Constant 4)))")
        .check();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Assignment

#[test]
fn assignment_global() {
    TestCase::new()
        .input("(set! *global* car)")
        .meaning("(Sequence (GlobalSet 0 (ImportedReference 0)))")
        .check();

    TestCase::new()
        .input("(set! *global* *global*)")
        .meaning("(Sequence (GlobalSet 0 (GlobalReference 0)))")
        .check();
}

#[test]
fn assignment_undefined() {
    TestCase::new()
        .input("(set! undefined 111)")
        .meaning("(Sequence (Constant 0))")
        .diagnostic(6, 15, DiagnosticKind::err_meaning_unresolved_variable)
        .check();

    TestCase::new()
        .input("(set! undefined (undefined undefined))")
        .meaning("(Sequence (ProcedureCall (Undefined) (Undefined)))")
        .diagnostic( 6, 15, DiagnosticKind::err_meaning_unresolved_variable)
        .diagnostic(17, 26, DiagnosticKind::err_meaning_unresolved_variable)
        .diagnostic(27, 36, DiagnosticKind::err_meaning_unresolved_variable)
        .check();
}

#[test]
fn assignment_imported() {
    TestCase::new()
        .input("(set! car cdr)")
        .meaning("(Sequence (ImportedReference 1))")
        .diagnostic(6, 9, DiagnosticKind::err_meaning_assign_to_imported_binding)
        .check();
}

#[test]
fn assignment_syntactic() {
    TestCase::new()
        .input("(set! set! set!)")
        .meaning("(Sequence (Undefined))")
        .diagnostic( 6, 10, DiagnosticKind::err_meaning_assign_to_syntactic_binding)
        .diagnostic(11, 15, DiagnosticKind::err_meaning_reference_to_syntactic_binding)
        .check();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Sequence

#[test]
fn sequence_simple() {
    TestCase::new()
        .input("(begin 111 222 333)")
        .meaning("(Sequence (Constant 0) (Constant 1) (Constant 2))")
        .check();
}

#[test]
fn sequence_splicing_toplevel() {
    TestCase::new()
        .input("(begin (begin #f #f #t) (if #f 1 2) (begin 9))")
        .meaning("(Sequence \
                    (Constant 0) \
                    (Constant 1) \
                    (Constant 2) \
                    (Alternative (Constant 3) \
                      (Constant 4) \
                      (Constant 5)) \
                    (Constant 6))")
        .check();
}

#[test]
fn sequence_splicing_inner() {
    TestCase::new()
        .input("(lambda () (begin #f #f #t) (begin (begin 1)))")
        .meaning("(Sequence \
                    (ClosureFixed 0 \
                     (Sequence \
                       (Constant 0) \
                       (Constant 1) \
                       (Constant 2) \
                       (Constant 3))))")
        .check();
}

#[test]
fn sequence_nonsplicing() {
    TestCase::new()
        .input("(if *global* (begin 1 2) (begin 3))")
        .meaning("(Sequence \
                    (Alternative (GlobalReference 0) \
                      (Sequence (Constant 0) (Constant 1)) \
                      (Sequence (Constant 2))))")
        .check();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Abstraction

#[test]
fn lambda_no_arguments() {
    TestCase::new()
        .input("(lambda () #t)")
        .meaning("(Sequence (ClosureFixed 0 (Sequence (Constant 0))))")
        .check();
    TestCase::new()
        .input("(lambda () 1 2 3)")
        .meaning("(Sequence (ClosureFixed 0 (Sequence (Constant 0) (Constant 1) (Constant 2))))")
        .check();
}

#[test]
fn lambda_fixed_arguments() {
    TestCase::new()
        .input("(lambda (n) (if n (begin 2 3) #f))")
        .meaning("(Sequence \
                    (ClosureFixed 1 \
                     (Sequence \
                       (Alternative (ArgumentReference 0 0) \
                         (Sequence (Constant 0) (Constant 1)) \
                         (Constant 2)))))")
        .check();
}

#[test]
fn lambda_fixed_arguments_nested() {
    TestCase::new()
        .input("(lambda (a b n)
                  (if n
                    (lambda (x) x a)
                    (lambda (x) x b) ) )")
        .meaning("(Sequence \
                    (ClosureFixed 3 \
                     (Sequence \
                       (Alternative (ArgumentReference 0 2) \
                         (ClosureFixed 1 \
                          (Sequence \
                            (ArgumentReference 0 0) \
                            (ArgumentReference 1 0))) \
                         (ClosureFixed 1 \
                          (Sequence \
                            (ArgumentReference 0 0) \
                            (ArgumentReference 1 1)))))))")
        .check();
}

#[test]
fn lambda_undefined_locals() {
    TestCase::new()
        .input("(lambda (x) y)")
        .meaning("(Sequence (ClosureFixed 1 (Sequence (Undefined))))")
        .diagnostic(12, 13, DiagnosticKind::err_meaning_unresolved_variable)
        .check();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Application

#[test]
fn application_simple() {
    TestCase::new()
        .input("(cons 111 222)")
        .meaning("(Sequence (ProcedureCall (ImportedReference 2) (Constant 0) (Constant 1)))")
        .check();
}

#[test]
fn application_nested() {
    TestCase::new()
        .input("(car (cons 111 222))")
        .meaning("(Sequence \
                    (ProcedureCall (ImportedReference 0) \
                      (ProcedureCall (ImportedReference 2) \
                        (Constant 0) \
                        (Constant 1))))")
        .check();
}

#[test]
fn application_closed() {
    TestCase::new()
        .input("((lambda (a b) (cons a b)) 111 222)")
        .meaning(
            "(Sequence \
             (ProcedureCall (ClosureFixed 2 \
             (Sequence \
             (ProcedureCall (ImportedReference 2) \
             (ArgumentReference 0 0) \
             (ArgumentReference 0 1)))) \
             (Constant 0) \
             (Constant 1)))",
        )
        .check();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Shadowing

#[test]
fn local_variables_shadow_special_forms() {
    TestCase::new()
        .input("((lambda (if) (if if if if)) (lambda (a b) (cons a b)))")
        .meaning("(Sequence \
                    (ProcedureCall (ClosureFixed 1 \
                                    (Sequence \
                                      (ProcedureCall \
                                        (ArgumentReference 0 0) \
                                        (ArgumentReference 0 0) \
                                        (ArgumentReference 0 0) \
                                        (ArgumentReference 0 0)))) \
                      (ClosureFixed 2 \
                       (Sequence \
                         (ProcedureCall (ImportedReference 2) \
                           (ArgumentReference 0 0) \
                           (ArgumentReference 0 1))))))")
        .check();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Test helpers

use libexpand::expression::Expression;
use liblocus::diagnostics::Diagnostic;
use libreader::datum::ScannedDatum;
use libreader::lexer::StringScanner;
use libreader::parser::Parser;

#[derive(Default)]
struct TestCase {
    input: Option<String>,
    expected_meaning: Option<String>,
    constant_generator: Option<Box<dyn Fn(&InternPool) -> Vec<Value>>>,
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

    fn meaning<T: Into<String>>(mut self, meaning: T) -> Self {
        assert!(self.expected_meaning.is_none(), "don't set meaning twice");
        self.expected_meaning = Some(meaning.into());
        self
    }

    fn constants<F>(mut self, generator: F) -> Self
    where
        F: Fn(&InternPool) -> Vec<Value> + 'static,
    {
        assert!(
            self.constant_generator.is_none(),
            "don't set constants twice"
        );
        self.constant_generator = Some(Box::new(generator));
        self
    }

    fn diagnostic(mut self, from: usize, to: usize, kind: DiagnosticKind) -> Self {
        self.expected_diagnostics.push(Diagnostic {
            kind: kind,
            span: Span::new(from, to),
        });
        self
    }

    fn check(self) {
        let input = self.input.expect("input not set");
        let expected_meaning = self.expected_meaning.expect("meaning not set");
        let expected_diagnostics = self.expected_diagnostics;
        let constants = self.constant_generator.as_ref().map(|g| g.as_ref());

        check(&input, &expected_meaning, &expected_diagnostics, constants);
    }
}

/// TODO
fn check(
    input: &str,
    output: &str,
    expected_diagnostics: &[Diagnostic],
    constant_generator: Option<&dyn Fn(&InternPool) -> Vec<Value>>,
) {
    let pool = InternPool::new();

    let data = parse(&pool, input);
    let expressions = expand(&pool, &data);
    let (meaning, diagnostics) = treat(&expressions);

    let actual = format!("{:?}", meaning.sequence);

    assert_eq!(actual, output);
    if let Some(generate_constants) = constant_generator {
        let expected_constants = generate_constants(&pool);
        assert_eq!(meaning.constants, expected_constants);
    }
    assert_eq!(diagnostics, expected_diagnostics);
}

fn parse(pool: &InternPool, input: &str) -> Vec<ScannedDatum> {
    use liblocus::utils::collect_diagnostics;

    let (data, parsing_diagnostics) = collect_diagnostics(|handler| {
        let scanner = Box::new(StringScanner::new(input, handler, pool));
        let mut parser = Parser::new(scanner, pool, handler);

        let all_data = parser.parse_all_data();

        assert!(
            parser.parse_all_data().is_empty(),
            "parser did not consume the whole stream"
        );

        return all_data;
    });

    assert!(
        parsing_diagnostics.is_empty(),
        "parsing produced diagnostics"
    );

    return data;
}

fn expand(pool: &InternPool, data: &[ScannedDatum]) -> Vec<Expression> {
    use libexpand::expand;
    use liblocus::utils::collect_diagnostics;

    let (expansion_result, expansion_diagnostics) = collect_diagnostics(|handler| {
        let environment = basic_scheme_environment(pool);

        return data
            .iter()
            .map(|datum| expand(datum, &environment, &handler))
            .collect();
    });

    assert!(
        expansion_diagnostics.is_empty(),
        "expander produced diagnostics"
    );

    return expansion_result;
}

fn treat(expressions: &[Expression]) -> (MeaningResult, Vec<Diagnostic>) {
    use liblocus::utils::collect_diagnostics;

    collect_diagnostics(|handler| {
        return meaning(expressions, handler);
    })
}
