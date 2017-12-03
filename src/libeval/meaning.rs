// Copyright (c) 2017, Sabre developers
//
// Licensed under the Apache License, Version 2.0 (see LICENSE.Apache in the
// root directory) or MIT license (see LICENSE.MIT in the root directory),
// at your option. This file may be copied, distributed, and modified only
// in accordance with the terms specified by the chosen license.

//! Scheme expression analyzer.
//!
//! Here we transform core Scheme expressions into their meaning based on the semantics of Scheme.
//! This is the finish line of the front-end.

use locus::diagnostics::{Span};

use expression::{Expression};

pub trait Environment {
}

pub struct Meaning {
    kind: MeaningKind,
    span: Option<Span>,
}

pub enum MeaningKind {
}

pub fn meaning(expression: &Expression, environment: &Environment) -> Meaning {
    unimplemented!()
}
