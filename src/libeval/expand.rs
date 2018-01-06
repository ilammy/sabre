// Copyright (c) 2017, Sabre developers
//
// Licensed under the Apache License, Version 2.0 (see LICENSE.Apache in the
// root directory) or MIT license (see LICENSE.MIT in the root directory),
// at your option. This file may be copied, distributed, and modified only
// in accordance with the terms specified by the chosen license.

//! Scheme macro expander.
//!
//! This module defines the basic macro expansion engine (the `expand()` function), the macro
//! transformer interface (the `Expander` trait), and core Scheme expressions (the `*Expander`
//! structs). Core expressions are not 'macros' in a strict sense, but they do convert data
//! into code, just like user-defined macros do. Sometimes they are called  _magic keywords_.

use std::rc::Rc;

use locus::diagnostics::{Handler};
use reader::datum::{ScannedDatum};

use environment::{Environment};
use expression::{Expression};

/// Trait of all macro expanders.
pub trait Expander {
    /// Expand a macro use.
    ///
    /// Macro uses are always forms, possibly dotted. `datum` represents the form itself which
    /// should be a ProperList, DottedList, or Vector (though it depends on the expander).
    ///
    /// The provided environment should be used for expanding any nested forms, it also
    /// should be stored in the resulting expressions.
    ///
    /// The expansion always succeeds, in the sense that it always returns some expression.
    /// Though, the resulting expression may be garbage or bogus due to error recovery.
    /// Any errors should be reported to the provided handler.
    fn expand_form(
        &self,
        datum: &ScannedDatum,
        environment: &Rc<Environment>,
        diagnosic: &Handler,
    ) -> Expression;
}
