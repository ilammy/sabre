// Copyright (c) 2017, Sabre developers
//
// Licensed under the Apache License, Version 2.0 (see LICENSE.Apache in the
// root directory) or MIT license (see LICENSE.MIT in the root directory),
// at your option. This file may be copied, distributed, and modified only
// in accordance with the terms specified by the chosen license.

//! Miscellaneous expander utilities.

use locus::diagnostics::{Handler, DiagnosticKind, Span};
use reader::datum::{ScannedDatum, DatumValue};
use reader::intern_pool::{Atom};

/// Check if `datum` is a form (possibly empty).
/// Return `Some((dotted, values))` if it is, `None` otherwise.
pub fn is_form(datum: &ScannedDatum) -> Option<(bool, &[ScannedDatum])> {
    match datum.value {
        DatumValue::ProperList(ref values) => Some((false, values)),
        DatumValue::DottedList(ref values) => Some((true, values)),
        _ => None,
    }
}

/// Check if `datum` is a form with `expected_name` as its car.
/// Return `Some((dotted, values))` if it is, `None` otherwise.
pub fn is_named_form(datum: &ScannedDatum, expected_name: Atom)
    -> Option<(bool, &[ScannedDatum])>
{
    let (dotted, values) = match is_form(datum) {
        Some(v) => v,
        None => { return None; }
    };

    if values.is_empty() {
        return None;
    }
    match values[0].value {
        DatumValue::Symbol(actual_name) => {
            if actual_name != expected_name {
                return None;
            }
        }
        _ => {
            return None;
        }
    }

    return Some((dotted, values));
}

/// Check that a proper list has given number of elements.
///
/// `datum` is a the datum to be tested. It must be a proper or dotted list (as specified by
/// `dotted`), with `elements` being its elements. The list must have exactly `expected_length`.
/// If any of the above is not true then diagnostic `kind` is reported to provided `diagnostic`
/// handler with the offending range.
pub fn expect_list_length_fixed(datum: &ScannedDatum, dotted: bool, elements: &[ScannedDatum],
    expected_length: usize, diagnostic: &Handler, kind: DiagnosticKind)
{
    if elements.is_empty() {
        diagnostic.report(kind,
            Span::new(datum.span.from + 1, datum.span.to - 1));

        return;
    }

    let last = elements.len() - 1;

    if elements.len() < expected_length {
        diagnostic.report(kind,
            Span::new(elements[last].span.to, datum.span.to - 1));
    }

    if elements.len() > expected_length {
        diagnostic.report(kind,
            Span::new(elements[expected_length].span.from, elements[last].span.to));
    }

    if dotted && (elements.len() == expected_length || expected_length > 2) {
        assert!(elements.len() >= 2);
        diagnostic.report(kind,
            Span::new(elements[last - 1].span.to, elements[last].span.from));
    }
}

/// Check that a proper list has at least given number of elements.
///
/// `datum` is a the datum to be tested. It must be a proper or dotted list (as specified by
/// `dotted`), with `elements` being its elements. The list must have at least `expected_length`
/// elements or more. If any of the above is not true then diagnostic `kind` is reported to
/// the provided `diagnostic` handler with the offending range.
pub fn expect_list_length_at_least(datum: &ScannedDatum, dotted: bool, elements: &[ScannedDatum],
    expected_length: usize, diagnostic: &Handler, kind: DiagnosticKind)
{
    if elements.is_empty() {
        diagnostic.report(kind,
            Span::new(datum.span.from + 1, datum.span.to - 1));

        return;
    }

    let last = elements.len() - 1;

    if elements.len() < expected_length {
        diagnostic.report(kind,
            Span::new(elements[last].span.to, datum.span.to - 1));
    }

    if dotted && (elements.len() >= expected_length || expected_length > 2) {
        assert!(elements.len() >= 2);
        diagnostic.report(kind,
            Span::new(elements[last - 1].span.to, elements[last].span.from));
    }
}
