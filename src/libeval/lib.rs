// Copyright (c) 2017, Sabre developers
//
// Licensed under the Apache License, Version 2.0 (see LICENSE.Apache in the
// root directory) or MIT license (see LICENSE.MIT in the root directory),
// at your option. This file may be copied, distributed, and modified only
// in accordance with the terms specified by the chosen license.

//! Scheme evaluator.
//!
//! This crate contains definitions of core Scheme expressions and a crude evaluator of them.

extern crate locus;
extern crate reader;

pub mod expression;
pub mod expand;
pub mod expanders;
pub mod meaning;
pub mod environment;
