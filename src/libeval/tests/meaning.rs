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

use eval::meaning::{meaning, Meaning, MeaningKind};

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Tested expanders and environments

use eval::expanders::{Expander, ExpanderStack, BasicExpander, ApplicationExpander,
    QuoteExpander, BeginExpander, IfExpander, SetExpander, LambdaExpander};
use eval::meaning::{Environment};
use locus::diagnostics::{Handler};
use reader::intern_pool::{InternPool};

fn standard_scheme<'a>(pool: &'a InternPool, handler: &'a Handler) -> Box<Expander +'a> {
    Box::new(
        ExpanderStack::new(Box::new(BasicExpander::new(handler)))
            .push(Box::new(ApplicationExpander::new(handler)))
            .push(Box::new( QuoteExpander::new(pool.intern("quote"),  handler)))
            .push(Box::new( BeginExpander::new(pool.intern("begin"),  handler)))
            .push(Box::new(    IfExpander::new(pool.intern("if"),     handler)))
            .push(Box::new(   SetExpander::new(pool.intern("set"),    handler)))
            .push(Box::new(LambdaExpander::new(pool.intern("lambda"), handler)))
    )
}

fn basic_scheme_environment() -> Box<Environment> {
    unimplemented!()
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
    let meaning = treat(&expression);

    assert_eq!(trim_space(&pretty_print(&meaning)), trim_space(output));
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

fn treat(expression: &Expression) -> Meaning {
    let environment = basic_scheme_environment();

    return meaning(expression, environment.as_ref());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Pretty-printing meanings

fn pretty_print(meaning: &Meaning) -> String {
    unimplemented!()
}

fn trim_space(sexpr: &str) -> String {
    unimplemented!()
}