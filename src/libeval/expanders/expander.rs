// Copyright (c) 2017, Sabre developers
//
// Licensed under the Apache License, Version 2.0 (see LICENSE.Apache in the
// root directory) or MIT license (see LICENSE.MIT in the root directory),
// at your option. This file may be copied, distributed, and modified only
// in accordance with the terms specified by the chosen license.

//! Macro expander interface.

use reader::datum::{ScannedDatum};

use expression::{Expression};

/// Trait of all macro expanders.
pub trait Expander {
    /// Expand macros in the provided datum.
    ///
    /// Normally the provided expander will be the same as `self`, but syntactic binding forms
    /// may replace it with an extended version.
    fn expand(&self, datum: &ScannedDatum, expand: &Expander) -> ExpansionResult;
}

/// Result of macro expansion.
#[derive(Debug, Eq, PartialEq)]
pub enum ExpansionResult {
    /// Successful expansion or error recovery.
    Some(Expression),

    /// Unsucessful error recovery, no expression can be provided.
    None,

    /// The form is not known to the expander.
    Unknown,
}