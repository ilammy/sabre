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

use eval::meaning::{meaning, Meaning, MeaningKind, Value};

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Tested expanders and environments

use eval::expanders::{Expander, ExpanderStack, BasicExpander, ApplicationExpander,
    QuoteExpander, BeginExpander, IfExpander, SetExpander, LambdaExpander};
use eval::meaning::{Environment, VariableKind};
use locus::diagnostics::{Handler};
use reader::intern_pool::{InternPool, Atom};

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

struct BasicSchemeEnvironment {
    car: Atom,
    cdr: Atom,
    cons: Atom,
    global: Atom,
}

impl BasicSchemeEnvironment {
    fn new(pool: &InternPool) -> BasicSchemeEnvironment {
        BasicSchemeEnvironment {
            car: pool.intern("car"),
            cdr: pool.intern("cdr"),
            cons: pool.intern("cons"),
            global: pool.intern("*global*"),
        }
    }
}

impl Environment for BasicSchemeEnvironment {
    fn resolve_variable(&self, name: Atom) -> VariableKind {
        // Imported variables
        if name == self.car {
            return VariableKind::Imported { index: 0 };
        }
        if name == self.cdr {
            return VariableKind::Imported { index: 1 };
        }
        if name == self.cons {
            return VariableKind::Imported { index: 2 };
        }
        // Global variables
        if name == self.global {
            return VariableKind::Global { index: 0 };
        }
        // Anything else
        return VariableKind::Unresolved;
    }
}

fn basic_scheme_environment(pool: &InternPool) -> Box<Environment> {
    Box::new(BasicSchemeEnvironment::new(pool))
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Constants

#[test]
fn literals() {
    check("42",         "(Constant 42)");
    check("#\\x",       "(Constant #\\x)");
    check("#false",     "(Constant #f)");
    check("\"string\"", "(Constant \"string\")");
}

#[test]
fn quote_literals() {
    check("'123",           "(Constant 123)");
    check("(quote #\\!)",   "(Constant #\\!)");
    check("'#t",            "(Constant #t)");
    check("(quote \"\")",   "(Constant \"\")");
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Global and imported variables

#[test]
fn reference_imported() {
    check("car",    "(ImportedReference 0)");
    check("cdr",    "(ImportedReference 1)");
    check("cons",   "(ImportedReference 2)");
}

#[test]
fn reference_global() {
    check("*global*",   "(GlobalReference 0)");
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Alternative

#[test]
fn alternative() {
    check("(if #t 1 0)",
        "(Alternative (Constant #t) (Constant 1) (Constant 0))");

    check("(if *global* car cdr)",
        "(Alternative (GlobalReference 0)
            (ImportedReference 0)
            (ImportedReference 1))");
}

#[test]
fn alternative_nested() {
    check("(if (if #f 1 #f) 1 0)",
        "(Alternative (Alternative (Constant #f)
                          (Constant 1)
                          (Constant #f))
            (Constant 1)
            (Constant 0))");
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Assignment

#[test]
fn assignment_global() {
    check("(set! *global* car)",        "(GlobalSet 0 (ImportedReference 0))");
    check("(set! *global* *global*)",   "(GlobalSet 0 (GlobalReference 0))");
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Test helpers

use reader::datum::{ScannedDatum};
use reader::lexer::{StringScanner};
use reader::parser::{Parser};
use eval::expanders::{ExpansionResult};
use eval::expression::{Expression};

/// TODO
fn check(input: &str, output: &str) {
    let pool = InternPool::new();

    let datum = parse(&pool, input);
    let expression = expand(&pool, &datum);
    let meaning = treat(&pool, &expression);

    let actual = pretty_print(&pool, &meaning);

    assert_eq!(trim_space(&actual), trim_space(output));
}

fn parse(pool: &InternPool, input: &str) -> ScannedDatum {
    use locus::utils::collect_diagnostics;

    let (datum, parsing_diagnostics) = collect_diagnostics(|handler| {
        let scanner = Box::new(StringScanner::new(input, handler, pool));
        let mut parser = Parser::new(scanner, pool, handler);

        let mut all_data = parser.parse_all_data();

        assert!(parser.parse_all_data().is_empty(), "parser did not consume the whole stream");
        assert!(all_data.len() == 1, "input must describe exactly one datum");

        return all_data.pop().unwrap();
    });

    assert!(parsing_diagnostics.is_empty(), "parsing produced diagnostics");

    return datum;
}

fn expand(pool: &InternPool, datum: &ScannedDatum) -> Expression {
    use locus::utils::collect_diagnostics;

    let (expansion_result, expansion_diagnostics) = collect_diagnostics(|handler| {
        let expander = standard_scheme(pool, handler);

        return expander.expand(&datum, expander.as_ref());
    });

    assert!(expansion_diagnostics.is_empty(), "expander produced diagnostics");

    if let ExpansionResult::Some(expression) = expansion_result {
        return expression;
    }

    panic!("expander did not produce an expression");
}

fn treat(pool: &InternPool, expression: &Expression) -> Meaning {
    let environment = basic_scheme_environment(pool);

    return meaning(expression, environment.as_ref());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Pretty-printing meanings

fn pretty_print(pool: &InternPool, meaning: &Meaning) -> String {
    match meaning.kind {
        MeaningKind::Constant(ref value) => pretty_print_constant(pool, value),
        MeaningKind::ShallowArgumentReference(index) => pretty_print_shallow_reference(index),
        MeaningKind::DeepArgumentReference(depth, index) => pretty_print_deep_reference(depth, index),
        MeaningKind::GlobalReference(index) => pretty_print_global_reference(index),
        MeaningKind::ImportedReference(index) => pretty_print_imported_reference(index),
        MeaningKind::Alternative(ref condition, ref consequent, ref alternate) =>
            pretty_print_alternative(pool, condition, consequent, alternate),
        MeaningKind::ShallowArgumentSet(index, ref value) =>
            pretty_print_shallow_set(pool, index, value.as_ref()),
        MeaningKind::DeepArgumentSet(depth, index, ref value) =>
            pretty_print_deep_set(pool, depth, index, value.as_ref()),
        MeaningKind::GlobalSet(index, ref value) =>
            pretty_print_global_set(pool, index, value.as_ref()),
    }
}

fn pretty_print_constant(pool: &InternPool, value: &Value) -> String {
    match *value {
        Value::Boolean(value) => format!("(Constant {})", if value { "#t" } else { "#f" }),
        Value::Character(value) => format!("(Constant #\\{})", value),
        Value::Number(value) => format!("(Constant {})", pool.get(value)),
        Value::String(value) => format!("(Constant \"{}\")", pool.get(value)), // TODO: escape quotes
    }
}

fn pretty_print_shallow_reference(index: usize) -> String {
    format!("(ShallowArgumentReference {})", index)
}

fn pretty_print_deep_reference(depth: usize, index: usize) -> String {
    format!("(DeepArgumentReference {} {})", depth, index)
}

fn pretty_print_global_reference(index: usize) -> String {
    format!("(GlobalReference {})", index)
}

fn pretty_print_imported_reference(index: usize) -> String {
    format!("(ImportedReference {})", index)
}

fn pretty_print_alternative(pool: &InternPool, condition: &Meaning, consequent: &Meaning,
    alternate: &Meaning) -> String
{
    format!("(Alternative {} {} {})",
        pretty_print(pool, condition),
        pretty_print(pool, consequent),
        pretty_print(pool, alternate)
    )
}

fn pretty_print_shallow_set(pool: &InternPool, index: usize, value: &Meaning) -> String {
    format!("(ShallowArgumentSet {} {})", index, pretty_print(pool, value))
}

fn pretty_print_deep_set(pool: &InternPool, depth: usize, index: usize, value: &Meaning) -> String {
    format!("(DeepArgumentSet {} {} {})", depth, index, pretty_print(pool, value))
}

fn pretty_print_global_set(pool: &InternPool, index: usize, value: &Meaning) -> String {
    format!("(GlobalSet {} {})", index, pretty_print(pool, value))
}

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
