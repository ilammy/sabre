// Copyright (c) 2019, Sabre developers
//
// Licensed under the Apache License, Version 2.0 (see LICENSE.Apache in the
// root directory) or MIT license (see LICENSE.MIT in the root directory),
// at your option. This file may be copied, distributed, and modified only
// in accordance with the terms specified by the chosen license.

//! Scheme semantic analysis.
//!
//! This crate defines meaning of core Scheme expressions.

mod analysis;
mod meaning;
mod sequence;

pub use analysis::{meaning, MeaningResult, Value};
pub use meaning::{Meaning, MeaningKind};
