// Copyright (c) 2017, Sabre developers
//
// Licensed under the Apache License, Version 2.0 (see LICENSE.Apache in the
// root directory) or MIT license (see LICENSE.MIT in the root directory),
// at your option. This file may be copied, distributed, and modified only
// in accordance with the terms specified by the chosen license.

//! Expander test suite.
//!
//! This verifies that the basic syntax of Scheme is handled as expected.

extern crate eval;
extern crate locus;
extern crate reader;

use eval::expanders::{Expander, ExpansionResult, ExpanderStack, BasicExpander,
    ApplicationExpander, QuoteExpander, BeginExpander, IfExpander, SetExpander, LambdaExpander};
use eval::expression::{Expression, ExpressionKind, Literal, Variable, Arguments};
use locus::diagnostics::{Span, Diagnostic, DiagnosticKind};
use reader::intern_pool::{InternPool};

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

impl<'a> ExpanderFactory<'a> for SchemeBase {
    type Instance = ExpanderStack<'a>;

    fn make(&self, pool: &InternPool, handler: &'a Handler) -> Self::Instance {
        ExpanderStack::new(Box::new(BasicExpander::new(handler)))
            .push(Box::new(ApplicationExpander::new(handler)))
            .push(Box::new( QuoteExpander::new(pool.intern(self.quote),  handler)))
            .push(Box::new( BeginExpander::new(pool.intern(self.begin),  handler)))
            .push(Box::new(    IfExpander::new(pool.intern(self.if_),    handler)))
            .push(Box::new(   SetExpander::new(pool.intern(self.set),    handler)))
            .push(Box::new(LambdaExpander::new(pool.intern(self.lambda), handler)))
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Literal data

#[test]
fn basic_booleans() {
    let pool = InternPool::new();

    check(&pool, SchemeBase::default(),
        "#true",
        ExpansionResult::Some(Expression {
            kind: ExpressionKind::Literal(Literal::Boolean(true)),
            span: Some(Span::new(0, 5)),
        }),
        &[]);
}

#[test]
fn basic_numbers() {
    let pool = InternPool::new();

    check(&pool, SchemeBase::default(),
        "123.45e+6",
        ExpansionResult::Some(Expression {
            kind: ExpressionKind::Literal(Literal::Number(pool.intern("123.45e+6"))),
            span: Some(Span::new(0, 9)),
        }),
        &[]);
}

#[test]
fn basic_characters() {
    let pool = InternPool::new();

    check(&pool, SchemeBase::default(),
        "#\\x",
        ExpansionResult::Some(Expression {
            kind: ExpressionKind::Literal(Literal::Character('x')),
            span: Some(Span::new(0, 3)),
        }),
        &[]);
}

#[test]
fn basic_strings() {
    let pool = InternPool::new();

    check(&pool, SchemeBase::default(),
        "\"test\"",
        ExpansionResult::Some(Expression {
            kind: ExpressionKind::Literal(Literal::String(pool.intern("test"))),
            span: Some(Span::new(0, 6)),
        }),
        &[]);
}

#[test]
fn basic_references() {
    let pool = InternPool::new();

    check(&pool, SchemeBase::default(),
        "test",
        ExpansionResult::Some(Expression {
            kind: ExpressionKind::Reference(pool.intern("test")),
            span: Some(Span::new(0, 4)),
        }),
        &[]);
}

#[test]
fn basic_vectors() {
    let pool = InternPool::new();

    check(&pool, SchemeBase::default(),
        "#(1 2 3 foo bar (baz))",
        ExpansionResult::Some(Expression {
            kind: ExpressionKind::Literal(Literal::Vector(vec![
                ScannedDatum {
                    value: DatumValue::Number(pool.intern("1")),
                    span: Span::new(2, 3),
                },
                ScannedDatum {
                    value: DatumValue::Number(pool.intern("2")),
                    span: Span::new(4, 5),
                },
                ScannedDatum {
                    value: DatumValue::Number(pool.intern("3")),
                    span: Span::new(6, 7),
                },
                ScannedDatum {
                    value: DatumValue::Symbol(pool.intern("foo")),
                    span: Span::new(8, 11),
                },
                ScannedDatum {
                    value: DatumValue::Symbol(pool.intern("bar")),
                    span: Span::new(12, 15),
                },
                ScannedDatum {
                    value: DatumValue::ProperList(vec![
                        ScannedDatum {
                            value: DatumValue::Symbol(pool.intern("baz")),
                            span: Span::new(17, 20),
                        },
                    ]),
                    span: Span::new(16, 21),
                },
            ])),
            span: Some(Span::new(0, 22)),
        }),
        &[]);
}

#[test]
fn basic_bytevectors() {
    let pool = InternPool::new();

    // We do not yet enforce the number range here. Individual numbers are still just strings.
    check(&pool, SchemeBase::default(),
        "#u8(0 0 1 5 255 -nan.0)",
        ExpansionResult::Some(Expression {
            kind: ExpressionKind::Literal(Literal::Bytevector(vec![
                pool.intern("0"),
                pool.intern("0"),
                pool.intern("1"),
                pool.intern("5"),
                pool.intern("255"),
                pool.intern("-nan.0"),
            ])),
            span: Some(Span::new(0, 23)),
        }),
        &[]);
}

#[test]
fn basic_prohibited_labels() {
    let pool = InternPool::new();

    // Datum label markers are ignored
    check(&pool, SchemeBase::default(),
        "#1=(lambda (n) (if (<= n 1) 1 (* n (#1# (- n 1)))))",
        ExpansionResult::Some(Expression {
            kind: ExpressionKind::Abstraction(
                Arguments::Fixed(vec![
                    Variable {
                        name: pool.intern("n"),
                        span: Some(Span::new(12, 13)),
                    },
                ]),
                vec![
                    Expression {
                        kind: ExpressionKind::Alternative(
                            Box::new(Expression {
                                kind: ExpressionKind::Application(vec![
                                    Expression {
                                        kind: ExpressionKind::Reference(pool.intern("<=")),
                                        span: Some(Span::new(20, 22)),
                                    },
                                    Expression {
                                        kind: ExpressionKind::Reference(pool.intern("n")),
                                        span: Some(Span::new(23, 24)),
                                    },
                                    Expression {
                                        kind: ExpressionKind::Literal(Literal::Number(pool.intern("1"))),
                                        span: Some(Span::new(25, 26)),
                                    },
                                ]),
                                span: Some(Span::new(19, 27)),
                            }),
                            Box::new(Expression {
                                kind: ExpressionKind::Literal(Literal::Number(pool.intern("1"))),
                                span: Some(Span::new(28, 29)),
                            }),
                            Box::new(Expression {
                                kind: ExpressionKind::Application(vec![
                                    Expression {
                                        kind: ExpressionKind::Reference(pool.intern("*")),
                                        span: Some(Span::new(31, 32)),
                                    },
                                    Expression {
                                        kind: ExpressionKind::Reference(pool.intern("n")),
                                        span: Some(Span::new(33, 34)),
                                    },
                                    Expression {
                                        kind: ExpressionKind::Application(vec![
                                            // Datum label references are replaced with #f
                                            Expression {
                                                kind: ExpressionKind::Literal(Literal::Boolean(false)),
                                                span: Some(Span::new(36, 39)),
                                            },
                                            Expression {
                                                kind: ExpressionKind::Application(vec![
                                                    Expression {
                                                        kind: ExpressionKind::Reference(pool.intern("-")),
                                                        span: Some(Span::new(41, 42)),
                                                    },
                                                    Expression {
                                                        kind: ExpressionKind::Reference(pool.intern("n")),
                                                        span: Some(Span::new(43, 44)),
                                                    },
                                                    Expression {
                                                        kind: ExpressionKind::Literal(Literal::Number(pool.intern("1"))),
                                                        span: Some(Span::new(45, 46)),
                                                    },
                                                ]),
                                                span: Some(Span::new(40, 47)),
                                            },
                                        ]),
                                        span: Some(Span::new(35, 48)),
                                    },
                                ]),
                                span: Some(Span::new(30, 49)),
                            }),
                        ),
                        span: Some(Span::new(15, 50)),
                    }
                ],
            ),
            span: Some(Span::new(3, 51)),
        }),
        &[
            Diagnostic {
                kind: DiagnosticKind::err_expand_datum_label,
                loc: Some(Span::new(0, 3)),
            },
            Diagnostic {
                kind: DiagnosticKind::err_expand_datum_label,
                loc: Some(Span::new(36, 39)),
            },
        ]);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// `quote` form

#[test]
fn quote_normal() {
    let pool = InternPool::new();

    check(&pool, SchemeBase::default(),
        "'(1 2 3)",
        ExpansionResult::Some(Expression {
            kind: ExpressionKind::Quotation(ScannedDatum {
                value: DatumValue::ProperList(vec![
                    ScannedDatum {
                        value: DatumValue::Number(pool.intern("1")),
                        span: Span::new(2, 3),
                    },
                    ScannedDatum {
                        value: DatumValue::Number(pool.intern("2")),
                        span: Span::new(4, 5),
                    },
                    ScannedDatum {
                        value: DatumValue::Number(pool.intern("3")),
                        span: Span::new(6, 7),
                    },
                ]),
                span: Span::new(1, 8),
            }),
            span: Some(Span::new(0, 8)),
        }),
        &[]);
}

#[test]
fn quote_nested() {
    let pool = InternPool::new();

    check(&pool, SchemeBase::default(),
        "''quote",
        ExpansionResult::Some(Expression {
            kind: ExpressionKind::Quotation(ScannedDatum {
                value: DatumValue::ProperList(vec![
                    ScannedDatum {
                        value: DatumValue::Symbol(pool.intern("quote")),
                        span: Span::new(1, 2),
                    },
                    ScannedDatum {
                        value: DatumValue::Symbol(pool.intern("quote")),
                        span: Span::new(2, 7),
                    },
                ]),
                span: Span::new(1, 7),
            }),
            span: Some(Span::new(0, 7)),
        }),
        &[]);
}

#[test]
fn quote_empty() {
    let pool = InternPool::new();

    check(&pool, SchemeBase::default(),
        "(quote)",
        ExpansionResult::Some(Expression {
            kind: ExpressionKind::Quotation(ScannedDatum {
                value: DatumValue::Boolean(false),
                span: Span::new(6, 6),
            }),
            span: Some(Span::new(0, 7)),
        }),
        &[
            Diagnostic {
                kind: DiagnosticKind::err_expand_invalid_quote,
                loc: Some(Span::new(6, 6)),
            },
        ]);
}

#[test]
fn quote_dotted() {
    let pool = InternPool::new();

    check(&pool, SchemeBase::default(),
        "(quote . 4)",
        ExpansionResult::Some(Expression {
            kind: ExpressionKind::Quotation(ScannedDatum {
                value: DatumValue::Number(pool.intern("4")),
                span: Span::new(9, 10),
            }),
            span: Some(Span::new(0, 11)),
        }),
        &[
            Diagnostic {
                kind: DiagnosticKind::err_expand_invalid_quote,
                loc: Some(Span::new(6, 9)),
            },
        ]);
}

#[test]
fn quote_extra() {
    let pool = InternPool::new();

    check(&pool, SchemeBase::default(),
        "(quote 1 2 3)",
        ExpansionResult::Some(Expression {
            kind: ExpressionKind::Quotation(ScannedDatum {
                value: DatumValue::Number(pool.intern("3")),
                span: Span::new(11, 12),
            }),
            span: Some(Span::new(0, 13)),
        }),
        &[
            Diagnostic {
                kind: DiagnosticKind::err_expand_invalid_quote,
                loc: Some(Span::new(9, 12)),
            },
        ]);
}

#[test]
fn quote_extra_dotted() {
    let pool = InternPool::new();

    check(&pool, SchemeBase::default(),
        "(quote 1 2 . 3)",
        ExpansionResult::Some(Expression {
            kind: ExpressionKind::Quotation(ScannedDatum {
                value: DatumValue::Number(pool.intern("3")),
                span: Span::new(13, 14),
            }),
            span: Some(Span::new(0, 15)),
        }),
        &[
            Diagnostic {
                kind: DiagnosticKind::err_expand_invalid_quote,
                loc: Some(Span::new(9, 14)),
            },
        ]);
}

#[test]
fn quote_renaming() {
    let pool = InternPool::new();

    check(&pool, SchemeBase { quote: "not-a-quote", ..Default::default() },
        "'(1 2 3)",
        ExpansionResult::Some(Expression {
            kind: ExpressionKind::Application(vec![
                Expression {
                    kind: ExpressionKind::Reference(pool.intern("quote")),
                    span: Some(Span::new(0, 1)),
                },
                Expression {
                    kind: ExpressionKind::Application(vec![
                        Expression {
                            kind: ExpressionKind::Literal(Literal::Number(pool.intern("1"))),
                            span: Some(Span::new(2, 3)),
                        },
                        Expression {
                            kind: ExpressionKind::Literal(Literal::Number(pool.intern("2"))),
                            span: Some(Span::new(4, 5)),
                        },
                        Expression {
                            kind: ExpressionKind::Literal(Literal::Number(pool.intern("3"))),
                            span: Some(Span::new(6, 7)),
                        },
                    ]),
                    span: Some(Span::new(1, 8)),
                },
            ]),
            span: Some(Span::new(0, 8)),
        }),
        &[]);

    check(&pool, SchemeBase { quote: "a-quote", ..Default::default() },
        "(a-quote 5)",
        ExpansionResult::Some(Expression {
            kind: ExpressionKind::Quotation(ScannedDatum {
                value: DatumValue::Number(pool.intern("5")),
                span: Span::new(9, 10),
            }),
            span: Some(Span::new(0, 11)),
        }),
        &[]);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// `begin` form

#[test]
fn begin_normal() {
    let pool = InternPool::new();

    check(&pool, SchemeBase::default(),
        "(begin 1 2 3)",
        ExpansionResult::Some(Expression {
            kind: ExpressionKind::Sequence(vec![
                Expression {
                    kind: ExpressionKind::Literal(Literal::Number(pool.intern("1"))),
                    span: Some(Span::new(7, 8)),
                },
                Expression {
                    kind: ExpressionKind::Literal(Literal::Number(pool.intern("2"))),
                    span: Some(Span::new(9, 10)),
                },
                Expression {
                    kind: ExpressionKind::Literal(Literal::Number(pool.intern("3"))),
                    span: Some(Span::new(11, 12)),
                },
            ]),
            span: Some(Span::new(0, 13)),
        }),
        &[]);
}

#[test]
fn begin_nested() {
    let pool = InternPool::new();

    check(&pool, SchemeBase::default(),
        "(begin (begin 1 2) (begin 3 4) (begin (begin 5) 6) 7)",
        ExpansionResult::Some(Expression {
            kind: ExpressionKind::Sequence(vec![
                Expression {
                    kind: ExpressionKind::Sequence(vec![
                        Expression {
                            kind: ExpressionKind::Literal(Literal::Number(pool.intern("1"))),
                            span: Some(Span::new(14, 15)),
                        },
                        Expression {
                            kind: ExpressionKind::Literal(Literal::Number(pool.intern("2"))),
                            span: Some(Span::new(16, 17)),
                        },
                    ]),
                    span: Some(Span::new(7, 18)),
                },
                Expression {
                    kind: ExpressionKind::Sequence(vec![
                        Expression {
                            kind: ExpressionKind::Literal(Literal::Number(pool.intern("3"))),
                            span: Some(Span::new(26, 27)),
                        },
                        Expression {
                            kind: ExpressionKind::Literal(Literal::Number(pool.intern("4"))),
                            span: Some(Span::new(28, 29)),
                        },
                    ]),
                    span: Some(Span::new(19, 30)),
                },
                Expression {
                    kind: ExpressionKind::Sequence(vec![
                        Expression {
                            kind: ExpressionKind::Sequence(vec![
                                Expression {
                                    kind: ExpressionKind::Literal(Literal::Number(pool.intern("5"))),
                                    span: Some(Span::new(45, 46)),
                                },
                            ]),
                            span: Some(Span::new(38, 47)),
                        },
                        Expression {
                            kind: ExpressionKind::Literal(Literal::Number(pool.intern("6"))),
                            span: Some(Span::new(48, 49)),
                        },
                    ]),
                    span: Some(Span::new(31, 50)),
                },
                Expression {
                    kind: ExpressionKind::Literal(Literal::Number(pool.intern("7"))),
                    span: Some(Span::new(51, 52)),
                },
            ]),
            span: Some(Span::new(0, 53)),
        }),
        &[]);
}

#[test]
fn begin_empty() {
    let pool = InternPool::new();

    check(&pool, SchemeBase::default(),
        "(begin)",
        ExpansionResult::Some(Expression {
            kind: ExpressionKind::Sequence(vec![]),
            span: Some(Span::new(0, 7)),
        }),
        &[
            Diagnostic {
                kind: DiagnosticKind::err_expand_invalid_begin,
                loc: Some(Span::new(6, 6)),
            }
        ]);
}

#[test]
fn begin_dotted() {
    let pool = InternPool::new();

    check(&pool, SchemeBase::default(),
        "(begin 1 2 . 3)",
        ExpansionResult::Some(Expression {
            kind: ExpressionKind::Sequence(vec![
                Expression {
                    kind: ExpressionKind::Literal(Literal::Number(pool.intern("1"))),
                    span: Some(Span::new(7, 8)),
                },
                Expression {
                    kind: ExpressionKind::Literal(Literal::Number(pool.intern("2"))),
                    span: Some(Span::new(9, 10)),
                },
                Expression {
                    kind: ExpressionKind::Literal(Literal::Number(pool.intern("3"))),
                    span: Some(Span::new(13, 14)),
                },
            ]),
            span: Some(Span::new(0, 15)),
        }),
        &[
            Diagnostic {
                kind: DiagnosticKind::err_expand_invalid_begin,
                loc: Some(Span::new(10, 13)),
            }
        ]);

    check(&pool, SchemeBase::default(),
        "(begin . #f)",
        ExpansionResult::Some(Expression {
            kind: ExpressionKind::Sequence(vec![
               Expression {
                    kind: ExpressionKind::Literal(Literal::Boolean(false)),
                    span: Some(Span::new(9, 11)),
                },
            ]),
            span: Some(Span::new(0, 12)),
        }),
        &[
            Diagnostic {
                kind: DiagnosticKind::err_expand_invalid_begin,
                loc: Some(Span::new(6, 9)),
            }
        ]);
}

#[test]
fn begin_renaming() {
    let pool = InternPool::new();

    check(&pool, SchemeBase { begin: "seq", ..Default::default() },
        "(seq 1 (seq 2 3))",
        ExpansionResult::Some(Expression {
            kind: ExpressionKind::Sequence(vec![
                Expression {
                    kind: ExpressionKind::Literal(Literal::Number(pool.intern("1"))),
                    span: Some(Span::new(5, 6)),
                },
                Expression {
                    kind: ExpressionKind::Sequence(vec![
                        Expression {
                            kind: ExpressionKind::Literal(Literal::Number(pool.intern("2"))),
                            span: Some(Span::new(12, 13)),
                        },
                        Expression {
                            kind: ExpressionKind::Literal(Literal::Number(pool.intern("3"))),
                            span: Some(Span::new(14, 15)),
                        },
                    ]),
                    span: Some(Span::new(7, 16)),
                },
            ]),
            span: Some(Span::new(0, 17)),
        }),
        &[]);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// `if` form

#[test]
fn if_normal() {
    let pool = InternPool::new();

    check(&pool, SchemeBase::default(),
        "(if #t 1 2)",
        ExpansionResult::Some(Expression {
            kind: ExpressionKind::Alternative(
                Box::new(Expression {
                    kind: ExpressionKind::Literal(Literal::Boolean(true)),
                    span: Some(Span::new(4, 6)),
                }),
                Box::new(Expression {
                    kind: ExpressionKind::Literal(Literal::Number(pool.intern("1"))),
                    span: Some(Span::new(7, 8)),
                }),
                Box::new(Expression {
                    kind: ExpressionKind::Literal(Literal::Number(pool.intern("2"))),
                    span: Some(Span::new(9, 10)),
                }),
            ),
            span: Some(Span::new(0, 11)),
        }),
        &[]);
}

#[test]
fn if_nested() {
    let pool = InternPool::new();

    check(&pool, SchemeBase::default(),
        "(if (if #t 1 2) 6 (if #f #t #t))",
        ExpansionResult::Some(Expression {
            kind: ExpressionKind::Alternative(
                Box::new(Expression {
                    kind: ExpressionKind::Alternative(
                        Box::new(Expression {
                            kind: ExpressionKind::Literal(Literal::Boolean(true)),
                            span: Some(Span::new(8, 10)),
                        }),
                        Box::new(Expression {
                            kind: ExpressionKind::Literal(Literal::Number(pool.intern("1"))),
                            span: Some(Span::new(11, 12)),
                        }),
                        Box::new(Expression {
                            kind: ExpressionKind::Literal(Literal::Number(pool.intern("2"))),
                            span: Some(Span::new(13, 14)),
                        }),
                    ),
                    span: Some(Span::new(4, 15)),
                }),
                Box::new(Expression {
                    kind: ExpressionKind::Literal(Literal::Number(pool.intern("6"))),
                    span: Some(Span::new(16, 17)),
                }),
                Box::new(Expression {
                    kind: ExpressionKind::Alternative(
                        Box::new(Expression {
                            kind: ExpressionKind::Literal(Literal::Boolean(false)),
                            span: Some(Span::new(22, 24)),
                        }),
                        Box::new(Expression {
                            kind: ExpressionKind::Literal(Literal::Boolean(true)),
                            span: Some(Span::new(25, 27)),
                        }),
                        Box::new(Expression {
                            kind: ExpressionKind::Literal(Literal::Boolean(true)),
                            span: Some(Span::new(28, 30)),
                        }),
                    ),
                    span: Some(Span::new(18, 31)),
                }),
            ),
            span: Some(Span::new(0, 32)),
        }),
        &[]);
}

#[test]
fn if_forms_0() {
    let pool = InternPool::new();

    check(&pool, SchemeBase::default(),
        "( if )",
        ExpansionResult::Some(Expression {
            kind: ExpressionKind::Alternative(
                Box::new(Expression {
                    kind: ExpressionKind::Literal(Literal::Boolean(false)),
                    span: None,
                }),
                Box::new(Expression {
                    kind: ExpressionKind::Literal(Literal::Boolean(false)),
                    span: None,
                }),
                Box::new(Expression {
                    kind: ExpressionKind::Literal(Literal::Boolean(false)),
                    span: None,
                }),
            ),
            span: Some(Span::new(0, 6)),
        }),
        &[
            Diagnostic {
                kind: DiagnosticKind::err_expand_invalid_if,
                loc: Some(Span::new(4, 5)),
            },
        ]);
}

#[test]
fn if_forms_1() {
    let pool = InternPool::new();

    check(&pool, SchemeBase::default(),
        "( if #false )",
        ExpansionResult::Some(Expression {
            kind: ExpressionKind::Alternative(
                Box::new(Expression {
                    kind: ExpressionKind::Literal(Literal::Boolean(false)),
                    span: Some(Span::new(5, 11)),
                }),
                Box::new(Expression {
                    kind: ExpressionKind::Literal(Literal::Boolean(false)),
                    span: None,
                }),
                Box::new(Expression {
                    kind: ExpressionKind::Literal(Literal::Boolean(false)),
                    span: None,
                }),
            ),
            span: Some(Span::new(0, 13)),
        }),
        &[
            Diagnostic {
                kind: DiagnosticKind::err_expand_invalid_if,
                loc: Some(Span::new(11, 12)),
            },
        ]);
}

#[test]
fn if_forms_1_dotted() {
    let pool = InternPool::new();

    check(&pool, SchemeBase::default(),
        "(if . #f)",
        ExpansionResult::Some(Expression {
            kind: ExpressionKind::Alternative(
                Box::new(Expression {
                    kind: ExpressionKind::Literal(Literal::Boolean(false)),
                    span: Some(Span::new(6, 8)),
                }),
                Box::new(Expression {
                    kind: ExpressionKind::Literal(Literal::Boolean(false)),
                    span: None,
                }),
                Box::new(Expression {
                    kind: ExpressionKind::Literal(Literal::Boolean(false)),
                    span: None,
                }),
            ),
            span: Some(Span::new(0, 9)),
        }),
        &[
            Diagnostic {
                kind: DiagnosticKind::err_expand_invalid_if,
                loc: Some(Span::new(8, 8)),
            },
            Diagnostic {
                kind: DiagnosticKind::err_expand_invalid_if,
                loc: Some(Span::new(3, 6)),
            },
        ]);
}

#[test]
fn if_forms_2() {
    let pool = InternPool::new();

    check(&pool, SchemeBase::default(),
        "(if #f 1)",
        ExpansionResult::Some(Expression {
            kind: ExpressionKind::Alternative(
                Box::new(Expression {
                    kind: ExpressionKind::Literal(Literal::Boolean(false)),
                    span: Some(Span::new(4, 6)),
                }),
                Box::new(Expression {
                    kind: ExpressionKind::Literal(Literal::Number(pool.intern("1"))),
                    span: Some(Span::new(7, 8)),
                }),
                Box::new(Expression {
                    kind: ExpressionKind::Literal(Literal::Boolean(false)),
                    span: None,
                }),
            ),
            span: Some(Span::new(0, 9)),
        }),
        &[
            Diagnostic {
                kind: DiagnosticKind::err_expand_invalid_if,
                loc: Some(Span::new(8, 8)),
            },
        ]);
}

#[test]
fn if_forms_2_dotted() {
    let pool = InternPool::new();

    check(&pool, SchemeBase::default(),
        "(if #f . 1)",
        ExpansionResult::Some(Expression {
            kind: ExpressionKind::Alternative(
                Box::new(Expression {
                    kind: ExpressionKind::Literal(Literal::Boolean(false)),
                    span: Some(Span::new(4, 6)),
                }),
                Box::new(Expression {
                    kind: ExpressionKind::Literal(Literal::Number(pool.intern("1"))),
                    span: Some(Span::new(9, 10)),
                }),
                Box::new(Expression {
                    kind: ExpressionKind::Literal(Literal::Boolean(false)),
                    span: None,
                }),
            ),
            span: Some(Span::new(0, 11)),
        }),
        &[
            Diagnostic {
                kind: DiagnosticKind::err_expand_invalid_if,
                loc: Some(Span::new(10, 10)),
            },
            Diagnostic {
                kind: DiagnosticKind::err_expand_invalid_if,
                loc: Some(Span::new(6, 9)),
            },
        ]);
}

#[test]
fn if_forms_3_dotted() {
    let pool = InternPool::new();

    check(&pool, SchemeBase::default(),
        "(if #f 1 . 2)",
        ExpansionResult::Some(Expression {
            kind: ExpressionKind::Alternative(
                Box::new(Expression {
                    kind: ExpressionKind::Literal(Literal::Boolean(false)),
                    span: Some(Span::new(4, 6)),
                }),
                Box::new(Expression {
                    kind: ExpressionKind::Literal(Literal::Number(pool.intern("1"))),
                    span: Some(Span::new(7, 8)),
                }),
                Box::new(Expression {
                    kind: ExpressionKind::Literal(Literal::Number(pool.intern("2"))),
                    span: Some(Span::new(11, 12)),
                }),
            ),
            span: Some(Span::new(0, 13)),
        }),
        &[
            Diagnostic {
                kind: DiagnosticKind::err_expand_invalid_if,
                loc: Some(Span::new(8, 11)),
            },
        ]);
}

#[test]
fn if_forms_5() {
    let pool = InternPool::new();

    check(&pool, SchemeBase::default(),
        "(if #f 1 2 3 4)",
        ExpansionResult::Some(Expression {
            kind: ExpressionKind::Alternative(
                Box::new(Expression {
                    kind: ExpressionKind::Literal(Literal::Boolean(false)),
                    span: Some(Span::new(4, 6)),
                }),
                Box::new(Expression {
                    kind: ExpressionKind::Literal(Literal::Number(pool.intern("1"))),
                    span: Some(Span::new(7, 8)),
                }),
                Box::new(Expression {
                    kind: ExpressionKind::Literal(Literal::Number(pool.intern("2"))),
                    span: Some(Span::new(9, 10)),
                }),
            ),
            span: Some(Span::new(0, 15)),
        }),
        &[
            Diagnostic {
                kind: DiagnosticKind::err_expand_invalid_if,
                loc: Some(Span::new(11, 14)),
            },
        ]);
}

#[test]
fn if_forms_5_dotted() {
    let pool = InternPool::new();

    check(&pool, SchemeBase::default(),
        "(if #f 1 2 3 . 4)",
        ExpansionResult::Some(Expression {
            kind: ExpressionKind::Alternative(
                Box::new(Expression {
                    kind: ExpressionKind::Literal(Literal::Boolean(false)),
                    span: Some(Span::new(4, 6)),
                }),
                Box::new(Expression {
                    kind: ExpressionKind::Literal(Literal::Number(pool.intern("1"))),
                    span: Some(Span::new(7, 8)),
                }),
                Box::new(Expression {
                    kind: ExpressionKind::Literal(Literal::Number(pool.intern("2"))),
                    span: Some(Span::new(9, 10)),
                }),
            ),
            span: Some(Span::new(0, 17)),
        }),
        &[
            Diagnostic {
                kind: DiagnosticKind::err_expand_invalid_if,
                loc: Some(Span::new(11, 16)),
            },
            Diagnostic {
                kind: DiagnosticKind::err_expand_invalid_if,
                loc: Some(Span::new(12, 15)),
            },
        ]);
}

#[test]
fn if_renaming() {
    let pool = InternPool::new();

    check(&pool, SchemeBase { if_: "whether", ..Default::default() },
        "(whether #false or not)",
        ExpansionResult::Some(Expression {
            kind: ExpressionKind::Alternative(
                Box::new(Expression {
                    kind: ExpressionKind::Literal(Literal::Boolean(false)),
                    span: Some(Span::new(9, 15)),
                }),
                Box::new(Expression {
                    kind: ExpressionKind::Reference(pool.intern("or")),
                    span: Some(Span::new(16, 18)),
                }),
                Box::new(Expression {
                    kind: ExpressionKind::Reference(pool.intern("not")),
                    span: Some(Span::new(19, 22)),
                }),
            ),
            span: Some(Span::new(0, 23)),
        }),
        &[]);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// `set!` form

#[test]
fn set_normal() {
    let pool = InternPool::new();

    check(&pool, SchemeBase::default(),
        "(set! i 0)",
        ExpansionResult::Some(Expression {
            kind: ExpressionKind::Assignment(
                Variable {
                    name: pool.intern("i"),
                    span: Some(Span::new(6, 7)),
                },
                Box::new(Expression {
                    kind: ExpressionKind::Literal(Literal::Number(pool.intern("0"))),
                    span: Some(Span::new(8, 9)),
                }),
            ),
            span: Some(Span::new(0, 10)),
        }),
        &[]);
}

#[test]
fn set_nested() {
    let pool = InternPool::new();

    check(&pool, SchemeBase::default(),
        "(set! n (if #true (begin 1 2) 'y))",
        ExpansionResult::Some(Expression {
            kind: ExpressionKind::Assignment(
                Variable {
                    name: pool.intern("n"),
                    span: Some(Span::new(6, 7)),
                },
                Box::new(Expression {
                    kind: ExpressionKind::Alternative(
                        Box::new(Expression {
                            kind: ExpressionKind::Literal(Literal::Boolean(true)),
                            span: Some(Span::new(12, 17)),
                        }),
                        Box::new(Expression {
                            kind: ExpressionKind::Sequence(vec![
                                Expression {
                                    kind: ExpressionKind::Literal(Literal::Number(pool.intern("1"))),
                                    span: Some(Span::new(25, 26)),
                                },
                                Expression {
                                    kind: ExpressionKind::Literal(Literal::Number(pool.intern("2"))),
                                    span: Some(Span::new(27, 28)),
                                },
                            ]),
                            span: Some(Span::new(18, 29)),
                        }),
                        Box::new(Expression {
                            kind: ExpressionKind::Quotation(ScannedDatum {
                                value: DatumValue::Symbol(pool.intern("y")),
                                span: Span::new(31, 32),
                            }),
                            span: Some(Span::new(30, 32)),
                        }),
                    ),
                    span: Some(Span::new(8, 33)),
                }),
            ),
            span: Some(Span::new(0, 34)),
        }),
        &[]);
}

#[test]
fn set_forms_0() {
    let pool = InternPool::new();

    check(&pool, SchemeBase::default(),
        "(set!)",
        ExpansionResult::Some(Expression {
            kind: ExpressionKind::Literal(Literal::Boolean(false)),
            span: None,
        }),
        &[
            Diagnostic {
                kind: DiagnosticKind::err_expand_invalid_set,
                loc: Some(Span::new(5, 5)),
            },
        ]);
}

#[test]
fn set_forms_1() {
    let pool = InternPool::new();

    check(&pool, SchemeBase::default(),
        "(set! i)",
        ExpansionResult::Some(Expression {
            kind: ExpressionKind::Assignment(
                Variable {
                    name: pool.intern("i"),
                    span: Some(Span::new(6, 7)),
                },
                Box::new(Expression {
                    kind: ExpressionKind::Literal(Literal::Boolean(false)),
                    span: None,
                }),
            ),
            span: Some(Span::new(0, 8)),
        }),
        &[
            Diagnostic {
                kind: DiagnosticKind::err_expand_invalid_set,
                loc: Some(Span::new(7, 7)),
            },
        ]);
}

#[test]
fn set_forms_1_dotted() {
    let pool = InternPool::new();

    check(&pool, SchemeBase::default(),
        "(set! . i)",
        ExpansionResult::Some(Expression {
            kind: ExpressionKind::Assignment(
                Variable {
                    name: pool.intern("i"),
                    span: Some(Span::new(8, 9)),
                },
                Box::new(Expression {
                    kind: ExpressionKind::Literal(Literal::Boolean(false)),
                    span: None,
                }),
            ),
            span: Some(Span::new(0, 10)),
        }),
        &[
            Diagnostic {
                kind: DiagnosticKind::err_expand_invalid_set,
                loc: Some(Span::new(9, 9)),
            },
            Diagnostic {
                kind: DiagnosticKind::err_expand_invalid_set,
                loc: Some(Span::new(5, 8)),
            },
        ]);
}

#[test]
fn set_forms_2_dotted() {
    let pool = InternPool::new();

    check(&pool, SchemeBase::default(),
        "(set! i . 5)",
        ExpansionResult::Some(Expression {
            kind: ExpressionKind::Assignment(
                Variable {
                    name: pool.intern("i"),
                    span: Some(Span::new(6, 7)),
                },
                Box::new(Expression {
                    kind: ExpressionKind::Literal(Literal::Number(pool.intern("5"))),
                    span: Some(Span::new(10, 11)),
                }),
            ),
            span: Some(Span::new(0, 12)),
        }),
        &[
            Diagnostic {
                kind: DiagnosticKind::err_expand_invalid_set,
                loc: Some(Span::new(7, 10)),
            },
        ]);
}

#[test]
fn set_forms_3() {
    let pool = InternPool::new();

    check(&pool, SchemeBase::default(),
        "(set! i 1 2)",
        ExpansionResult::Some(Expression {
            kind: ExpressionKind::Assignment(
                Variable {
                    name: pool.intern("i"),
                    span: Some(Span::new(6, 7)),
                },
                Box::new(Expression {
                    kind: ExpressionKind::Literal(Literal::Number(pool.intern("1"))),
                    span: Some(Span::new(8, 9)),
                }),
            ),
            span: Some(Span::new(0, 12)),
        }),
        &[
            Diagnostic {
                kind: DiagnosticKind::err_expand_invalid_set,
                loc: Some(Span::new(10, 11)),
            },
        ]);
}

#[test]
fn set_forms_3_dotted() {
    let pool = InternPool::new();

    check(&pool, SchemeBase::default(),
        "(set! i 5 . 4)",
        ExpansionResult::Some(Expression {
            kind: ExpressionKind::Assignment(
                Variable {
                    name: pool.intern("i"),
                    span: Some(Span::new(6, 7)),
                },
                Box::new(Expression {
                    kind: ExpressionKind::Literal(Literal::Number(pool.intern("5"))),
                    span: Some(Span::new(8, 9)),
                }),
            ),
            span: Some(Span::new(0, 14)),
        }),
        &[
            Diagnostic {
                kind: DiagnosticKind::err_expand_invalid_set,
                loc: Some(Span::new(12, 13)),
            },
            Diagnostic {
                kind: DiagnosticKind::err_expand_invalid_set,
                loc: Some(Span::new(9, 12)),
            },
        ]);
}

#[test]
fn set_non_variable() {
    let pool = InternPool::new();

    check(&pool, SchemeBase::default(),
        "(set! 2 4)",
        ExpansionResult::Some(Expression {
            kind: ExpressionKind::Literal(Literal::Number(pool.intern("4"))),
            span: Some(Span::new(8, 9)),
        }),
        &[
            Diagnostic {
                kind: DiagnosticKind::err_expand_invalid_set,
                loc: Some(Span::new(6, 7)),
            },
        ]);

    check(&pool, SchemeBase::default(),
        "(set! (test test) (if 1 2 3))",
        ExpansionResult::Some(Expression {
            kind: ExpressionKind::Alternative(
                Box::new(Expression {
                    kind: ExpressionKind::Literal(Literal::Number(pool.intern("1"))),
                    span: Some(Span::new(22, 23)),
                }),
                Box::new(Expression {
                    kind: ExpressionKind::Literal(Literal::Number(pool.intern("2"))),
                    span: Some(Span::new(24, 25)),
                }),
                Box::new(Expression {
                    kind: ExpressionKind::Literal(Literal::Number(pool.intern("3"))),
                    span: Some(Span::new(26, 27)),
                }),
            ),
            span: Some(Span::new(18, 28)),
        }),
        &[
            Diagnostic {
                kind: DiagnosticKind::err_expand_invalid_set,
                loc: Some(Span::new(6, 17)),
            },
        ]);

    check(&pool, SchemeBase::default(),
        "(set! () #true)",
        ExpansionResult::Some(Expression {
            kind: ExpressionKind::Literal(Literal::Boolean(true)),
            span: Some(Span::new(9, 14)),
        }),
        &[
            Diagnostic {
                kind: DiagnosticKind::err_expand_invalid_set,
                loc: Some(Span::new(6, 8)),
            },
        ]);
}

#[test]
fn set_renaming() {
    let pool = InternPool::new();

    check(&pool, SchemeBase { set: "!!!SUMMER-ASSIGNMENT!!!", ..Default::default() },
        "(!!!SUMMER-ASSIGNMENT!!!)",
        ExpansionResult::Some(Expression {
            kind: ExpressionKind::Literal(Literal::Boolean(false)),
            span: None,
        }),
        &[
            Diagnostic {
                kind: DiagnosticKind::err_expand_invalid_set,
                loc: Some(Span::new(24, 24)),
            },
        ]);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// `lambda` form

#[test]
fn lambda_normal() {
    let pool = InternPool::new();

    check(&pool, SchemeBase::default(),
        "(lambda (x) x)",
        ExpansionResult::Some(Expression {
            kind: ExpressionKind::Abstraction(
                Arguments::Fixed(vec![
                    Variable {
                        name: pool.intern("x"),
                        span: Some(Span::new(9, 10)),
                    },
                ]),
                vec![
                    Expression {
                        kind: ExpressionKind::Reference(pool.intern("x")),
                        span: Some(Span::new(12, 13)),
                    }
                ],
            ),
            span: Some(Span::new(0, 14)),
        }),
        &[]);
}

#[test]
fn lambda_nested() {
    let pool = InternPool::new();

    check(&pool, SchemeBase::default(),
        "(lambda (a b) (if (> a b) a b))",
        ExpansionResult::Some(Expression {
            kind: ExpressionKind::Abstraction(
                Arguments::Fixed(vec![
                    Variable {
                        name: pool.intern("a"),
                        span: Some(Span::new(9, 10)),
                    },
                    Variable {
                        name: pool.intern("b"),
                        span: Some(Span::new(11, 12)),
                    },
                ]),
                vec![
                    Expression {
                        kind: ExpressionKind::Alternative(
                            Box::new(Expression {
                                kind: ExpressionKind::Application(vec![
                                    Expression {
                                        kind: ExpressionKind::Reference(pool.intern(">")),
                                        span: Some(Span::new(19, 20)),
                                    },
                                    Expression {
                                        kind: ExpressionKind::Reference(pool.intern("a")),
                                        span: Some(Span::new(21, 22)),
                                    },
                                    Expression {
                                        kind: ExpressionKind::Reference(pool.intern("b")),
                                        span: Some(Span::new(23, 24)),
                                    },
                                ]),
                                span: Some(Span::new(18, 25)),
                            }),
                            Box::new(Expression {
                                kind: ExpressionKind::Reference(pool.intern("a")),
                                span: Some(Span::new(26, 27)),
                            }),
                            Box::new(Expression {
                                kind: ExpressionKind::Reference(pool.intern("b")),
                                span: Some(Span::new(28, 29)),
                            }),
                        ),
                        span: Some(Span::new(14, 30)),
                    }
                ],
            ),
            span: Some(Span::new(0, 31)),
        }),
        &[]);
}

#[test]
fn lambda_forms_0() {
    let pool = InternPool::new();

    check(&pool, SchemeBase::default(),
        "( lambda )",
        ExpansionResult::Some(Expression {
            kind: ExpressionKind::Abstraction(
                Arguments::Fixed(vec![]),
                vec![],
            ),
            span: Some(Span::new(0, 10)),
        }),
        &[
            Diagnostic {
                kind: DiagnosticKind::err_expand_invalid_lambda,
                loc: Some(Span::new(8, 9)),
            },
        ]);
}

#[test]
fn lambda_forms_1() {
    let pool = InternPool::new();

    check(&pool, SchemeBase::default(),
        "(lambda ())",
        ExpansionResult::Some(Expression {
            kind: ExpressionKind::Abstraction(
                Arguments::Fixed(vec![]),
                vec![],
            ),
            span: Some(Span::new(0, 11)),
        }),
        &[
            Diagnostic {
                kind: DiagnosticKind::err_expand_invalid_lambda,
                loc: Some(Span::new(10, 10)),
            },
        ]);
}

#[test]
fn lambda_forms_1_dotted() {
    let pool = InternPool::new();

    check(&pool, SchemeBase::default(),
        "(lambda . ())",
        ExpansionResult::Some(Expression {
            kind: ExpressionKind::Abstraction(
                Arguments::Fixed(vec![]),
                vec![],
            ),
            span: Some(Span::new(0, 13)),
        }),
        &[
            Diagnostic {
                kind: DiagnosticKind::err_expand_invalid_lambda,
                loc: Some(Span::new(12, 12)),
            },
            Diagnostic {
                kind: DiagnosticKind::err_expand_invalid_lambda,
                loc: Some(Span::new(7, 10)),
            },
        ]);
}

#[test]
fn lambda_forms_2_dotted() {
    let pool = InternPool::new();

    check(&pool, SchemeBase::default(),
        "(lambda () . 9)",
        ExpansionResult::Some(Expression {
            kind: ExpressionKind::Abstraction(
                Arguments::Fixed(vec![]),
                vec![
                    Expression {
                        kind: ExpressionKind::Literal(Literal::Number(pool.intern("9"))),
                        span: Some(Span::new(13, 14)),
                    },
                ],
            ),
            span: Some(Span::new(0, 15)),
        }),
        &[
            Diagnostic {
                kind: DiagnosticKind::err_expand_invalid_lambda,
                loc: Some(Span::new(10, 13)),
            },
        ]);
}

#[test]
fn lambda_forms_3_dotted() {
    let pool = InternPool::new();

    check(&pool, SchemeBase::default(),
        "(lambda () 1 . 2)",
        ExpansionResult::Some(Expression {
            kind: ExpressionKind::Abstraction(
                Arguments::Fixed(vec![]),
                vec![
                    Expression {
                        kind: ExpressionKind::Literal(Literal::Number(pool.intern("1"))),
                        span: Some(Span::new(11, 12)),
                    },
                    Expression {
                        kind: ExpressionKind::Literal(Literal::Number(pool.intern("2"))),
                        span: Some(Span::new(15, 16)),
                    },
                ],
            ),
            span: Some(Span::new(0, 17)),
        }),
        &[
            Diagnostic {
                kind: DiagnosticKind::err_expand_invalid_lambda,
                loc: Some(Span::new(12, 15)),
            },
        ]);
}

#[test]
fn lambda_args_0() {
    let pool = InternPool::new();

    check(&pool, SchemeBase::default(),
        "(lambda () #f)",
        ExpansionResult::Some(Expression {
            kind: ExpressionKind::Abstraction(
                Arguments::Fixed(vec![]),
                vec![
                    Expression {
                        kind: ExpressionKind::Literal(Literal::Boolean(false)),
                        span: Some(Span::new(11, 13)),
                    }
                ],
            ),
            span: Some(Span::new(0, 14)),
        }),
        &[]);
}

#[test]
fn lambda_args_1() {
    let pool = InternPool::new();

    check(&pool, SchemeBase::default(),
        "(lambda (a) #f)",
        ExpansionResult::Some(Expression {
            kind: ExpressionKind::Abstraction(
                Arguments::Fixed(vec![
                    Variable {
                        name: pool.intern("a"),
                        span: Some(Span::new(9, 10)),
                    },
                ]),
                vec![
                    Expression {
                        kind: ExpressionKind::Literal(Literal::Boolean(false)),
                        span: Some(Span::new(12, 14)),
                    }
                ],
            ),
            span: Some(Span::new(0, 15)),
        }),
        &[]);
}

#[test]
fn lambda_args_2() {
    let pool = InternPool::new();

    check(&pool, SchemeBase::default(),
        "(lambda (a b) #f)",
        ExpansionResult::Some(Expression {
            kind: ExpressionKind::Abstraction(
                Arguments::Fixed(vec![
                    Variable {
                        name: pool.intern("a"),
                        span: Some(Span::new(9, 10)),
                    },
                    Variable {
                        name: pool.intern("b"),
                        span: Some(Span::new(11, 12)),
                    },
                ]),
                vec![
                    Expression {
                        kind: ExpressionKind::Literal(Literal::Boolean(false)),
                        span: Some(Span::new(14, 16)),
                    }
                ],
            ),
            span: Some(Span::new(0, 17)),
        }),
        &[]);
}

#[test]
fn lambda_args_2_dotted() {
    let pool = InternPool::new();

    check(&pool, SchemeBase::default(),
        "(lambda (a . b) #f)",
        ExpansionResult::Some(Expression {
            kind: ExpressionKind::Abstraction(
                Arguments::Fixed(vec![
                    Variable {
                        name: pool.intern("a"),
                        span: Some(Span::new(9, 10)),
                    },
                    Variable {
                        name: pool.intern("b"),
                        span: Some(Span::new(13, 14)),
                    },
                ]),
                vec![
                    Expression {
                        kind: ExpressionKind::Literal(Literal::Boolean(false)),
                        span: Some(Span::new(16, 18)),
                    }
                ],
            ),
            span: Some(Span::new(0, 19)),
        }),
        &[
            Diagnostic {
                kind: DiagnosticKind::err_expand_invalid_lambda,
                loc: Some(Span::new(10, 13)),
            },
        ]);
}

#[test]
fn lambda_args_non_variable() {
    let pool = InternPool::new();

    check(&pool, SchemeBase::default(),
        "(lambda (a b #f c 16 d) #f)",
        ExpansionResult::Some(Expression {
            kind: ExpressionKind::Abstraction(
                Arguments::Fixed(vec![
                    Variable {
                        name: pool.intern("a"),
                        span: Some(Span::new(9, 10)),
                    },
                    Variable {
                        name: pool.intern("b"),
                        span: Some(Span::new(11, 12)),
                    },
                    Variable {
                        name: pool.intern("c"),
                        span: Some(Span::new(16, 17)),
                    },
                    Variable {
                        name: pool.intern("d"),
                        span: Some(Span::new(21, 22)),
                    },
                ]),
                vec![
                    Expression {
                        kind: ExpressionKind::Literal(Literal::Boolean(false)),
                        span: Some(Span::new(24, 26)),
                    }
                ],
            ),
            span: Some(Span::new(0, 27)),
        }),
        &[
            Diagnostic {
                kind: DiagnosticKind::err_expand_invalid_lambda,
                loc: Some(Span::new(13, 15)),
            },
            Diagnostic {
                kind: DiagnosticKind::err_expand_invalid_lambda,
                loc: Some(Span::new(18, 20)),
            },
        ]);
}

#[test]
fn lambda_args_non_form() {
    let pool = InternPool::new();

    check(&pool, SchemeBase::default(),
        "(lambda x x)",
        ExpansionResult::Some(Expression {
            kind: ExpressionKind::Abstraction(
                Arguments::Fixed(vec![]),
                vec![
                    Expression {
                        kind: ExpressionKind::Reference(pool.intern("x")),
                        span: Some(Span::new(10, 11)),
                    }
                ],
            ),
            span: Some(Span::new(0, 12)),
        }),
        &[
            Diagnostic {
                kind: DiagnosticKind::err_expand_invalid_lambda,
                loc: Some(Span::new(8, 9)),
            },
        ]);

    check(&pool, SchemeBase::default(),
        "(lambda 5 5)",
        ExpansionResult::Some(Expression {
            kind: ExpressionKind::Abstraction(
                Arguments::Fixed(vec![]),
                vec![
                    Expression {
                        kind: ExpressionKind::Literal(Literal::Number(pool.intern("5"))),
                        span: Some(Span::new(10, 11)),
                    }
                ],
            ),
            span: Some(Span::new(0, 12)),
        }),
        &[
            Diagnostic {
                kind: DiagnosticKind::err_expand_invalid_lambda,
                loc: Some(Span::new(8, 9)),
            },
        ]);
}

#[test]
fn lambda_args_non_unique() {
    let pool = InternPool::new();

    check(&pool, SchemeBase::default(),
        "(lambda (x y z z y) !)",
        ExpansionResult::Some(Expression {
            kind: ExpressionKind::Abstraction(
                Arguments::Fixed(vec![
                    Variable {
                        name: pool.intern("x"),
                        span: Some(Span::new(9, 10)),
                    },
                    Variable {
                        name: pool.intern("y"),
                        span: Some(Span::new(11, 12)),
                    },
                    Variable {
                        name: pool.intern("z"),
                        span: Some(Span::new(13, 14)),
                    },
                ]),
                vec![
                    Expression {
                        kind: ExpressionKind::Reference(pool.intern("!")),
                        span: Some(Span::new(20, 21)),
                    }
                ],
            ),
            span: Some(Span::new(0, 22)),
        }),
        &[
            Diagnostic {
                kind: DiagnosticKind::err_expand_invalid_lambda,
                loc: Some(Span::new(15, 16)),
            },
            Diagnostic {
                kind: DiagnosticKind::err_expand_invalid_lambda,
                loc: Some(Span::new(17, 18)),
            },
        ]);
}

#[test]
fn lambda_renaming() {
    let pool = InternPool::new();

    check(&pool, SchemeBase { lambda: "\u{03BB}", ..Default::default() },
        "(\u{03BB} (a) #f)",
        ExpansionResult::Some(Expression {
            kind: ExpressionKind::Abstraction(
                Arguments::Fixed(vec![
                    Variable {
                        name: pool.intern("a"),
                        span: Some(Span::new(5, 6)),
                    },
                ]),
                vec![
                    Expression {
                        kind: ExpressionKind::Literal(Literal::Boolean(false)),
                        span: Some(Span::new(8, 10)),
                    }
                ],
            ),
            span: Some(Span::new(0, 11)),
        }),
        &[]);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Procedure calls

#[test]
fn application_simple() {
    let pool = InternPool::new();

    check(&pool, SchemeBase::default(),
        "(cons 1 2)",
        ExpansionResult::Some(Expression {
            kind: ExpressionKind::Application(
                vec![
                    Expression {
                        kind: ExpressionKind::Reference(pool.intern("cons")),
                        span: Some(Span::new(1, 5)),
                    },
                    Expression {
                        kind: ExpressionKind::Literal(Literal::Number(pool.intern("1"))),
                        span: Some(Span::new(6, 7)),
                    },
                    Expression {
                        kind: ExpressionKind::Literal(Literal::Number(pool.intern("2"))),
                        span: Some(Span::new(8, 9)),
                    },
                ],
            ),
            span: Some(Span::new(0, 10)),
        }),
        &[]);
}

#[test]
fn application_nested() {
    let pool = InternPool::new();

    check(&pool, SchemeBase::default(),
        "(cons 1 (cons 2 (cons 3 '())))",
        ExpansionResult::Some(Expression {
            kind: ExpressionKind::Application(
                vec![
                    Expression {
                        kind: ExpressionKind::Reference(pool.intern("cons")),
                        span: Some(Span::new(1, 5)),
                    },
                    Expression {
                        kind: ExpressionKind::Literal(Literal::Number(pool.intern("1"))),
                        span: Some(Span::new(6, 7)),
                    },
                    Expression {
                        kind: ExpressionKind::Application(
                            vec![
                                Expression {
                                    kind: ExpressionKind::Reference(pool.intern("cons")),
                                    span: Some(Span::new(9, 13)),
                                },
                                Expression {
                                    kind: ExpressionKind::Literal(Literal::Number(pool.intern("2"))),
                                    span: Some(Span::new(14, 15)),
                                },
                                Expression {
                                    kind: ExpressionKind::Application(
                                        vec![
                                            Expression {
                                                kind: ExpressionKind::Reference(pool.intern("cons")),
                                                span: Some(Span::new(17, 21)),
                                            },
                                            Expression {
                                                kind: ExpressionKind::Literal(Literal::Number(pool.intern("3"))),
                                                span: Some(Span::new(22, 23)),
                                            },
                                            Expression {
                                                kind: ExpressionKind::Quotation(ScannedDatum {
                                                    value: DatumValue::ProperList(vec![]),
                                                    span: Span::new(25, 27),
                                                }),
                                                span: Some(Span::new(24, 27)),
                                            },
                                        ],
                                    ),
                                    span: Some(Span::new(16, 28)),
                                },
                            ],
                        ),
                        span: Some(Span::new(8, 29)),
                    },
                ],
            ),
            span: Some(Span::new(0, 30)),
        }),
        &[]);
}

#[test]
fn application_forms_0() {
    let pool = InternPool::new();

    check(&pool, SchemeBase::default(),
        "()",
        ExpansionResult::Some(Expression {
            kind: ExpressionKind::Application(
                vec![],
            ),
            span: Some(Span::new(0, 2)),
        }),
        &[
            Diagnostic {
                kind: DiagnosticKind::err_expand_invalid_application,
                loc: Some(Span::new(1, 1)),
            },
        ]);
}

#[test]
fn application_forms_1() {
    let pool = InternPool::new();

    check(&pool, SchemeBase::default(),
        "(foo)",
        ExpansionResult::Some(Expression {
            kind: ExpressionKind::Application(
                vec![
                    Expression {
                        kind: ExpressionKind::Reference(pool.intern("foo")),
                        span: Some(Span::new(1, 4)),
                    },
                ],
            ),
            span: Some(Span::new(0, 5)),
        }),
        &[]);
}

#[test]
fn application_forms_2_dotted() {
    let pool = InternPool::new();

    check(&pool, SchemeBase::default(),
        "(foo . bar)",
        ExpansionResult::Some(Expression {
            kind: ExpressionKind::Application(
                vec![
                    Expression {
                        kind: ExpressionKind::Reference(pool.intern("foo")),
                        span: Some(Span::new(1, 4)),
                    },
                    Expression {
                        kind: ExpressionKind::Reference(pool.intern("bar")),
                        span: Some(Span::new(7, 10)),
                    },
                ],
            ),
            span: Some(Span::new(0, 11)),
        }),
        &[
            Diagnostic {
                kind: DiagnosticKind::err_expand_invalid_application,
                loc: Some(Span::new(4, 7)),
            },
        ]);
}

#[test]
fn application_forms_3_dotted() {
    let pool = InternPool::new();

    check(&pool, SchemeBase::default(),
        "(foo bar . baz)",
        ExpansionResult::Some(Expression {
            kind: ExpressionKind::Application(
                vec![
                    Expression {
                        kind: ExpressionKind::Reference(pool.intern("foo")),
                        span: Some(Span::new(1, 4)),
                    },
                    Expression {
                        kind: ExpressionKind::Reference(pool.intern("bar")),
                        span: Some(Span::new(5, 8)),
                    },
                    Expression {
                        kind: ExpressionKind::Reference(pool.intern("baz")),
                        span: Some(Span::new(11, 14)),
                    },
                ],
            ),
            span: Some(Span::new(0, 15)),
        }),
        &[
            Diagnostic {
                kind: DiagnosticKind::err_expand_invalid_application,
                loc: Some(Span::new(8, 11)),
            },
        ]);
}

#[test]
fn application_non_reference() {
    let pool = InternPool::new();

    check(&pool, SchemeBase::default(),
        "((lambda (x) (+ x x)) 5)",
        ExpansionResult::Some(Expression {
            kind: ExpressionKind::Application(
                vec![
                    Expression {
                        kind: ExpressionKind::Abstraction(
                            Arguments::Fixed(vec![
                                Variable {
                                    name: pool.intern("x"),
                                    span: Some(Span::new(10, 11)),
                                },
                            ]),
                            vec![
                                Expression {
                                    kind: ExpressionKind::Application(vec![
                                        Expression {
                                            kind: ExpressionKind::Reference(pool.intern("+")),
                                            span: Some(Span::new(14, 15)),
                                        },
                                        Expression {
                                            kind: ExpressionKind::Reference(pool.intern("x")),
                                            span: Some(Span::new(16, 17)),
                                        },
                                        Expression {
                                            kind: ExpressionKind::Reference(pool.intern("x")),
                                            span: Some(Span::new(18, 19)),
                                        },
                                    ]),
                                    span: Some(Span::new(13, 20)),
                                },
                            ],
                        ),
                        span: Some(Span::new(1, 21)),
                    },
                    Expression {
                        kind: ExpressionKind::Literal(Literal::Number(pool.intern("5"))),
                        span: Some(Span::new(22, 23)),
                    },
                ],
            ),
            span: Some(Span::new(0, 24)),
        }),
        &[]);

    check(&pool, SchemeBase::default(),
        "(9)",
        ExpansionResult::Some(Expression {
            kind: ExpressionKind::Application(
                vec![
                    Expression {
                        kind: ExpressionKind::Literal(Literal::Number(pool.intern("9"))),
                        span: Some(Span::new(1, 2)),
                    },
                ],
            ),
            span: Some(Span::new(0, 3)),
        }),
        &[]);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Using all the basic forms

#[test]
fn altogether() {
    let pool = InternPool::new();

    check(&pool, SchemeBase::default(),
        "(lambda (a b) (if a (begin (set! a 9) (+ b c)) (print '(17 #(x)))))",
        ExpansionResult::Some(Expression {
            kind: ExpressionKind::Abstraction(
                Arguments::Fixed(vec![
                    Variable {
                        name: pool.intern("a"),
                        span: Some(Span::new(9, 10)),
                    },
                    Variable {
                        name: pool.intern("b"),
                        span: Some(Span::new(11, 12)),
                    },
                ]),
                vec![
                    Expression {
                        kind: ExpressionKind::Alternative(
                            Box::new(Expression {
                                kind: ExpressionKind::Reference(pool.intern("a")),
                                span: Some(Span::new(18, 19)),
                            }),
                            Box::new(Expression {
                                kind: ExpressionKind::Sequence(vec![
                                    Expression {
                                        kind: ExpressionKind::Assignment(
                                            Variable {
                                                name: pool.intern("a"),
                                                span: Some(Span::new(33, 34)),
                                            },
                                            Box::new(Expression {
                                                kind: ExpressionKind::Literal(Literal::Number(pool.intern("9"))),
                                                span: Some(Span::new(35, 36)),
                                            }),
                                        ),
                                        span: Some(Span::new(27, 37)),
                                    },
                                    Expression {
                                        kind: ExpressionKind::Application(vec![
                                            Expression {
                                                kind: ExpressionKind::Reference(pool.intern("+")),
                                                span: Some(Span::new(39, 40)),
                                            },
                                            Expression {
                                                kind: ExpressionKind::Reference(pool.intern("b")),
                                                span: Some(Span::new(41, 42)),
                                            },
                                            Expression {
                                                kind: ExpressionKind::Reference(pool.intern("c")),
                                                span: Some(Span::new(43, 44)),
                                            },
                                        ]),
                                        span: Some(Span::new(38, 45)),
                                    },
                                ]),
                                span: Some(Span::new(20, 46)),
                            }),
                            Box::new(Expression {
                                kind: ExpressionKind::Application(vec![
                                    Expression {
                                        kind: ExpressionKind::Reference(pool.intern("print")),
                                        span: Some(Span::new(48, 53)),
                                    },
                                    Expression {
                                        kind: ExpressionKind::Quotation(ScannedDatum {
                                            value: DatumValue::ProperList(vec![
                                                ScannedDatum {
                                                    value: DatumValue::Number(pool.intern("17")),
                                                    span: Span::new(56, 58),
                                                },
                                                ScannedDatum {
                                                    value: DatumValue::Vector(vec![
                                                        ScannedDatum {
                                                            value: DatumValue::Symbol(pool.intern("x")),
                                                            span: Span::new(61, 62),
                                                        },
                                                    ]),
                                                    span: Span::new(59, 63),
                                                },
                                            ]),
                                            span: Span::new(55, 64),
                                        }),
                                        span: Some(Span::new(54, 64)),
                                    },
                                ]),
                                span: Some(Span::new(47, 65)),
                            }),
                        ),
                        span: Some(Span::new(14, 66)),
                    },
                ],
            ),
            span: Some(Span::new(0, 67)),
        }),
        &[]);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Test helpers

use locus::diagnostics::{Handler};
use reader::datum::{ScannedDatum, DatumValue};
use reader::lexer::{StringScanner};
use reader::parser::{Parser};

trait ExpanderFactory<'a> {
    type Instance: Expander;
    fn make(&self, &InternPool, &'a Handler) -> Self::Instance;
}

/// Check whether the given expander produces expected results and reports expected diagnostics.
/// Panic if this is not true.
fn check<F>(pool: &InternPool, expander_factory: F, input: &str,
        expected_result: ExpansionResult,
        expected_diagnostics: &[Diagnostic])
    where F: for<'a> ExpanderFactory<'a>
{
    use locus::utils::collect_diagnostics;

    let (datum, parsing_diagnostics) = collect_diagnostics(|handler| {
        let scanner = Box::new(StringScanner::new(input, handler, pool));
        let mut parser = Parser::new(scanner, pool, handler);

        let mut all_data = parser.parse_all_data();
        assert!(parser.parse_all_data().is_empty(), "parser did not consume the whole stream");
        assert!(all_data.len() == 1, "input must describe exactly one datum");
        all_data.pop().unwrap()
    });

    assert!(parsing_diagnostics.is_empty(), "parsing produced diagnostics");

    let (expand_result, expand_diagnostics) = collect_diagnostics(|handler| {
        let expander = expander_factory.make(pool, handler);

        expander.expand(&datum, &expander)
    });

    assert_eq!(expand_result, expected_result);
    assert_eq!(expand_diagnostics, expected_diagnostics);
}

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
            loc: Some(Span::new(from, to))
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

        check_2(&expander_factory, &input, &expected_result, &expected_diagnostics);
    }
}

/// Check whether the given expander produces expected results and reports expected diagnostics.
/// Panic if this is not true.
fn check_2(expander_factory: &SchemeBase, input: &str, expected_result: &str, expected_diagnostics: &[Diagnostic]) {
    use locus::utils::collect_diagnostics;
    use reader::intern_pool::with_formatting_pool;

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
        let expander = expander_factory.make(&pool, handler);

        expander.expand(&datum, &expander)
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
