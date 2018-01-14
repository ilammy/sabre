// Copyright (c) 2017, Sabre developers
//
// Licensed under the Apache License, Version 2.0 (see LICENSE.Apache in the
// root directory) or MIT license (see LICENSE.MIT in the root directory),
// at your option. This file may be copied, distributed, and modified only
// in accordance with the terms specified by the chosen license.

//! Miscellaneous expander utilities.

use locus::diagnostics::{Span};
use reader::datum::{ScannedDatum, DatumValue};
use reader::intern_pool::{Atom};

/// Return a span between the last term and the closing parenthesis.
///
/// `datum` must represent a non-empty list or vector (otherwise the function panics).
pub fn missing_last_span(datum: &ScannedDatum) -> Span {
    match datum.value {
        DatumValue::ProperList(ref terms) |
        DatumValue::DottedList(ref terms) |
        DatumValue::Vector(ref terms) => {
            assert!(terms.len() >= 1, "the list must be non-empty");
            let last_span = terms.last().unwrap().span;
            Span::new(last_span.to, datum.span.to - 1)
        }
        _ => panic!("datum must be a list or a vector")
    }
}

/// Unwrap an expected form with expected keyword.
///
/// Expanders should be invoked only on valid forms. This function checks this and panics if the
/// datum is not an expected form. It returns all the terms of the form and a flag whether the
/// form is dotted.
pub fn expect_form(keyword: Atom, datum: &ScannedDatum) -> (bool, &[ScannedDatum]) {
    let (dotted, terms) = match datum.value {
        DatumValue::ProperList(ref terms) => (false, terms),
        DatumValue::DottedList(ref terms) => (true, terms),
        _ => panic!("the expanded datum is not a list: {:?}", datum),
    };

    assert!(!terms.is_empty());

    match terms[0].value {
        DatumValue::Symbol(name) => {
            if name != keyword {
                panic!("the first term is not the expected keyword: {:?} (expected {:?})",
                    name, keyword);
            }
        }
        _ => panic!("the first term is not a symbol: {:?}", terms[0]),
    }

    return (dotted, terms);
}
