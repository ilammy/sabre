// Copyright (c) 2017, Sabre developers
//
// Licensed under the Apache License, Version 2.0 (see LICENSE.Apache in the
// root directory) or MIT license (see LICENSE.MIT in the root directory),
// at your option. This file may be copied, distributed, and modified only
// in accordance with the terms specified by the chosen license.

//! Miscellaneous expander utilities.

use std::ops::RangeFrom;

use liblocus::diagnostics::{DiagnosticKind, Handler, Span};
use libreader::datum::{DatumValue, ScannedDatum};
use libreader::intern_pool::Atom;

/// Return a span between the last term and the closing parenthesis.
///
/// `datum` must represent a non-empty list or vector (otherwise the function panics).
pub fn missing_last_span(datum: &ScannedDatum) -> Span {
    match datum.value {
        DatumValue::ProperList(ref terms)
        | DatumValue::DottedList(ref terms)
        | DatumValue::Vector(ref terms) => {
            assert!(!terms.is_empty());
            let last_span = terms.last().unwrap().span;
            Span::new(last_span.to, datum.span.to - 1)
        }
        _ => panic!("datum must be a list or a vector"),
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
                panic!(
                    "the first term is not the expected keyword: {:?} (expected {:?})",
                    name, keyword
                );
            }
        }
        _ => panic!("the first term is not a symbol: {:?}", terms[0]),
    }

    (dotted, terms)
}

/// Expected bounds for the term count in a macro use.
pub struct MacroUseBounds {
    /// Minimum required number of terms, inclusive.
    pub min: usize,
    /// Maximum allowed number of terms, inclusive.
    pub max: usize,
}

/// Diagnostics reported by `expect_macro_use()`.
pub struct MacroUseErrors {
    /// Reported when the form does not have enough terms.
    pub not_enough_terms: Option<DiagnosticKind>,
    /// Reported when the form has too many terms.
    pub too_many_terms: Option<DiagnosticKind>,
    /// Reported when the form is not a proper list (i.e., is a dotted form).
    pub dotted_form: Option<DiagnosticKind>,
}

/// Verify and unwrap a macro use.
///
/// This function first verifies that `datum` is a macro use for the `keyword`. It panics if
/// the datum is not an expected form. This enforces correctness of the expander operation
/// which should not invoke expanders for unrelated forms.
///
/// After that the function verifies that the macro is a proper form which has enought terms
/// for the specified `bounds` (which can be either a fixed number or a closed or open range).
/// Corresponding `errors` are reported to the provided `diagnostic` handler. The errors may
/// be all specified explicitly via `MacroUseErrors` struct, or a single diagnostic kind may
/// be used for all conditions. Note that you may omit some diagnostics if you are sure that
/// they will never be reported (if they are actually reported then you'll get a panic).
///
/// The function returns the slice of the terms of the macro use, without the leading keyword.
/// Note that the length of the slice may not be in `bounds`.
pub fn expect_macro_use<'a, B: Into<MacroUseBounds>, E: Into<MacroUseErrors>>(
    datum: &'a ScannedDatum,
    keyword: Atom,
    bounds: B,
    diagnostic: &Handler,
    errors: E,
) -> &'a [ScannedDatum] {
    let bounds = bounds.into();
    let errors = errors.into();

    let (dotted, terms) = expect_form(keyword, datum);
    let last = terms.len() - 1;

    if terms.len() < bounds.min {
        let missing_terms = Span::new(terms[last].span.to, datum.span.to - 1);
        diagnostic.report(errors.not_enough_terms.unwrap(), missing_terms);
    }
    if terms.len() > bounds.max {
        let extra_terms = Span::new(terms[bounds.max - 1].span.to, terms[last].span.to);
        diagnostic.report(errors.too_many_terms.unwrap(), extra_terms);
    }

    // Don't report an error for the dot if its falls into the range
    // previously reported for extra unexpected terms.
    if dotted && !((terms.len() > bounds.max) && (bounds.max > 1)) {
        assert!(terms.len() >= 2);
        let around_dot = Span::new(terms[last - 1].span.to, terms[last].span.from);
        diagnostic.report(errors.dotted_form.unwrap(), around_dot);
    }

    &terms[1..]
}

// Provide some convenience conversions for expect_macro_use() configuration,
// based on what the actual expanders use most.

impl From<usize> for MacroUseBounds {
    fn from(value: usize) -> MacroUseBounds {
        MacroUseBounds {
            min: value,
            max: value,
        }
    }
}

impl From<RangeFrom<usize>> for MacroUseBounds {
    fn from(value: RangeFrom<usize>) -> MacroUseBounds {
        MacroUseBounds {
            min: value.start,
            max: <usize>::max_value(),
        }
    }
}

impl From<DiagnosticKind> for MacroUseErrors {
    fn from(value: DiagnosticKind) -> MacroUseErrors {
        MacroUseErrors {
            not_enough_terms: Some(value),
            too_many_terms: Some(value),
            dotted_form: Some(value),
        }
    }
}
