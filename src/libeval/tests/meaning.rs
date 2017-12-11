// Copyright (c) 2017, Sabre developers
//
// Licensed under the Apache License, Version 2.0 (see LICENSE.Apache in the
// root directory) or MIT license (see LICENSE.MIT in the root directory),
// at your option. This file may be copied, distributed, and modified only
// in accordance with the terms specified by the chosen license.

//! Expression treater test suite.
//!
//! This verifies that the basic semantics of Scheme is handled as expected.

extern crate eval;
extern crate locus;
extern crate reader;

use eval::meaning::{meaning, MeaningResult, Value};

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Tested expanders and environments

use std::rc::{Rc};

use eval::expanders::{Expander, ExpanderStack, BasicExpander, ApplicationExpander,
    QuoteExpander, BeginExpander, IfExpander, SetExpander, LambdaExpander};
use eval::expression::{Variable};
use eval::meaning::{Environment};
use locus::diagnostics::{Diagnostic, DiagnosticKind, Handler, Span};
use reader::intern_pool::{InternPool};

fn standard_scheme<'a>(pool: &'a InternPool, handler: &'a Handler) -> Box<Expander +'a> {
    Box::new(
        ExpanderStack::new(Box::new(BasicExpander::new(handler)))
            .push(Box::new(ApplicationExpander::new(handler)))
            .push(Box::new( QuoteExpander::new(pool.intern("quote"),  handler)))
            .push(Box::new( BeginExpander::new(pool.intern("begin"),  handler)))
            .push(Box::new(    IfExpander::new(pool.intern("if"),     handler)))
            .push(Box::new(   SetExpander::new(pool.intern("set!"),   handler)))
            .push(Box::new(LambdaExpander::new(pool.intern("lambda"), handler)))
    )
}

fn basic_scheme_environment(pool: &InternPool) -> Rc<Environment> {
    let imported_vars = [
        Variable { name: pool.intern("car"), span: None },
        Variable { name: pool.intern("cdr"), span: None },
        Variable { name: pool.intern("cons"), span: None },
    ];
    let global_vars = [
        Variable { name: pool.intern("*global*"), span: None },
    ];

    let imported_env = Environment::new_imported(&imported_vars);
    let global_env = Environment::new_global(&global_vars, &imported_env);

    return global_env;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Constants

#[test]
fn literals() {
    check("42",         "(Sequence (Constant 0))", &[]);
    check("#\\x",       "(Sequence (Constant 0))", &[]);
    check("#false",     "(Sequence (Constant 0))", &[]);
    check("\"string\"", "(Sequence (Constant 0))", &[]);
}

#[test]
fn quote_literals() {
    check("'123",         "(Sequence (Constant 0))", &[]);
    check("(quote #\\!)", "(Sequence (Constant 0))", &[]);
    check("'#t",          "(Sequence (Constant 0))", &[]);
    check("(quote \"\")", "(Sequence (Constant 0))", &[]);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Global and imported variables

#[test]
fn reference_imported() {
    check("car",  "(Sequence (ImportedReference 0))", &[]);
    check("cdr",  "(Sequence (ImportedReference 1))", &[]);
    check("cons", "(Sequence (ImportedReference 2))", &[]);
}

#[test]
fn reference_global() {
    check("*global*", "(Sequence (GlobalReference 0))", &[]);
}

#[test]
fn reference_undefined() {
    check("@@UNDEFINED_VARIABLE@@", "(Sequence (Undefined))", &[
        Diagnostic {
            kind: DiagnosticKind::err_meaning_unresolved_variable,
            loc: Some(Span::new(0, 22)),
        },
    ]);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Alternative

#[test]
fn alternative() {
    check("(if #t 111 222)",
        "(Sequence (Alternative (Constant 0) (Constant 1) (Constant 2)))",
        &[]
    );
    check("(if *global* car cdr)",
        "(Sequence
            (Alternative (GlobalReference 0)
                (ImportedReference 0)
                (ImportedReference 1)))",
        &[]
    );
}

#[test]
fn alternative_nested() {
    check("(if (if #f 111 #t) 222 333)",
        "(Sequence
            (Alternative (Alternative (Constant 0)
                            (Constant 1)
                            (Constant 2))
                (Constant 3)
                (Constant 4)))",
        &[]
    );
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Assignment

#[test]
fn assignment_global() {
    check("(set! *global* car)",
        "(Sequence (GlobalSet 0 (ImportedReference 0)))",
        &[]
    );
    check("(set! *global* *global*)",
        "(Sequence (GlobalSet 0 (GlobalReference 0)))",
        &[]
    );
}

#[test]
fn assignment_undefined() {
    check("(set! undefined 111)",
        "(Sequence (Constant 0))",
        &[
            Diagnostic {
                kind: DiagnosticKind::err_meaning_unresolved_variable,
                loc: Some(Span::new(6, 15)),
            },
        ]
    );
    check("(set! undefined (undefined undefined))",
        "(Sequence (ProcedureCall (Undefined) (Undefined)))",
        &[
            Diagnostic {
                kind: DiagnosticKind::err_meaning_unresolved_variable,
                loc: Some(Span::new(6, 15)),
            },
            Diagnostic {
                kind: DiagnosticKind::err_meaning_unresolved_variable,
                loc: Some(Span::new(17, 26)),
            },
            Diagnostic {
                kind: DiagnosticKind::err_meaning_unresolved_variable,
                loc: Some(Span::new(27, 36)),
            },
        ]
    );
}

#[test]
fn assignment_imported() {
    check("(set! car cdr)",
        "(Sequence (ImportedReference 1))",
        &[
            Diagnostic {
                kind: DiagnosticKind::err_meaning_assign_to_imported_binding,
                loc: Some(Span::new(6, 9)),
            },
        ]
    );
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Sequence

#[test]
fn sequence_simple() {
    check("(begin 111 222 333)",
        "(Sequence (Constant 0) (Constant 1) (Constant 2))",
        &[]
    );
}

#[test]
fn sequence_splicing_toplevel() {
    check("(begin (begin #f #f #t) (if #f 1 2) (begin 9))",
        "(Sequence
            (Constant 0)
            (Constant 1)
            (Constant 2)
            (Alternative (Constant 3)
                (Constant 4)
                (Constant 5))
            (Constant 6))",
        &[]
    );
}

#[test]
fn sequence_splicing_inner() {
    check("(lambda () (begin #f #f #t) (begin (begin 1)))",
        "(Sequence
            (ClosureFixed 0
                (Sequence
                    (Constant 0)
                    (Constant 1)
                    (Constant 2)
                    (Constant 3))))",
        &[]
    );
}

#[test]
fn sequence_nonsplicing() {
    check("(if *global* (begin 1 2) (begin 3))",
        "(Sequence
            (Alternative (GlobalReference 0)
                (Sequence (Constant 0) (Constant 1))
                (Sequence (Constant 2))))",
        &[]
    );
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Abstraction

#[test]
fn lambda_no_arguments() {
    check("(lambda () #t)",
        "(Sequence (ClosureFixed 0 (Sequence (Constant 0))))",
        &[]
    );
    check("(lambda () 1 2 3)",
        "(Sequence (ClosureFixed 0 (Sequence (Constant 0) (Constant 1) (Constant 2))))",
        &[]
    );
}

#[test]
fn lambda_fixed_arguments() {
    check("(lambda (n) (if n (begin 2 3) #f))",
        "(Sequence
            (ClosureFixed 1
                (Sequence
                    (Alternative (ShallowArgumentReference 0)
                        (Sequence (Constant 0) (Constant 1))
                        (Constant 2)))))",
        &[]
    );
}

#[test]
fn lambda_fixed_arguments_nested() {
    check(
        "(lambda (a b n)
           (if n
             (lambda (x) x a)
             (lambda (x) x b)))",
        "(Sequence
            (ClosureFixed 3
                (Sequence
                    (Alternative (ShallowArgumentReference 2)
                        (ClosureFixed 1
                            (Sequence (ShallowArgumentReference 0)
                                      (DeepArgumentReference 1 0)))
                        (ClosureFixed 1
                            (Sequence (ShallowArgumentReference 0)
                                      (DeepArgumentReference 1 1)))))))",
        &[]
    );
}

#[test]
fn lambda_undefined_locals() {
    check("(lambda (x) y)",
        "(Sequence
            (ClosureFixed 1
                (Sequence
                    (Undefined))))",
        &[
            Diagnostic {
                kind: DiagnosticKind::err_meaning_unresolved_variable,
                loc: Some(Span::new(12, 13)),
            },
        ]
    );
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Application

#[test]
fn application_simple() {
    check("(cons 111 222)",
        "(Sequence
            (ProcedureCall (ImportedReference 2) (Constant 0) (Constant 1)))",
        &[]
    );
}

#[test]
fn application_nested() {
    check("(car (cons 111 222))",
        "(Sequence
            (ProcedureCall (ImportedReference 0)
                (ProcedureCall (ImportedReference 2) (Constant 0) (Constant 1))))",
        &[]
    );
}

#[test]
fn application_closed() {
    check("((lambda (a b) (cons a b)) 111 222)",
        "(Sequence
            (ProcedureCall
                (ClosureFixed 2
                    (Sequence
                        (ProcedureCall (ImportedReference 2)
                            (ShallowArgumentReference 0)
                            (ShallowArgumentReference 1))))
                (Constant 0)
                (Constant 1)))",
        &[]
    );
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Test helpers

use reader::datum::{ScannedDatum};
use reader::lexer::{StringScanner};
use reader::parser::{Parser};
use eval::expanders::{ExpansionResult};
use eval::expression::{Expression};

/// TODO
fn check(input: &str, output: &str, expected_diagnostics: &[Diagnostic]) {
    let pool = InternPool::new();

    let data = parse(&pool, input);
    let expressions = expand(&pool, &data);
    let (meaning, diagnostics) = treat(&pool, &expressions);

    let actual = format!("{:?}", meaning.sequence);

    assert_eq!(trim_space(&actual), trim_space(output));
    assert_eq!(diagnostics, expected_diagnostics);
}

fn parse(pool: &InternPool, input: &str) -> Vec<ScannedDatum> {
    use locus::utils::collect_diagnostics;

    let (data, parsing_diagnostics) = collect_diagnostics(|handler| {
        let scanner = Box::new(StringScanner::new(input, handler, pool));
        let mut parser = Parser::new(scanner, pool, handler);

        let all_data = parser.parse_all_data();

        assert!(parser.parse_all_data().is_empty(), "parser did not consume the whole stream");

        return all_data;
    });

    assert!(parsing_diagnostics.is_empty(), "parsing produced diagnostics");

    return data;
}

fn expand(pool: &InternPool, data: &[ScannedDatum]) -> Vec<Expression> {
    use locus::utils::collect_diagnostics;

    let (expansion_result, expansion_diagnostics) = collect_diagnostics(|handler| {
        let expander = standard_scheme(pool, handler);

        return data.iter()
            .map(|d| expander.expand(d, expander.as_ref()))
            .map(|e| match e {
                ExpansionResult::Some(e) => e,
                _ => panic!("expander did not produce an expression"),
            })
            .collect();
    });

    assert!(expansion_diagnostics.is_empty(), "expander produced diagnostics");

    return expansion_result;
}

fn treat(pool: &InternPool, expressions: &[Expression]) -> (MeaningResult, Vec<Diagnostic>) {
    use locus::utils::collect_diagnostics;

    let environment = basic_scheme_environment(pool);

    collect_diagnostics(|handler| {
        return meaning(handler, expressions, &environment);
    })
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Pretty-printing meanings

fn trim_space(sexpr: &str) -> String {
    enum State {
        InString,
        Other,
    }
    let mut state = State::Other;
    let mut previous_whitespace = false;

    let mut result = String::with_capacity(sexpr.len());

    for c in sexpr.chars() {
        match state {
            State::InString => {
                if c == '"' {
                    state = State::Other;
                }
            }
            State::Other => {
                if c == '"' {
                    state = State::InString;
                }
                if previous_whitespace && c.is_whitespace() {
                    continue;
                }
            }
        }
        previous_whitespace = c.is_whitespace();
        result.push(if previous_whitespace { ' ' } else { c });
    }

    return result;
}
