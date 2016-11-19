// Copyright (c) 2016, Sabre developers
//
// Licensed under the Apache License, Version 2.0 (see LICENSE.Apache in the
// root directory) or MIT license (see LICENSE.MIT in the root directory),
// at your option. This file may be copied, distributed, and modified only
// in accordance with the terms specified by the chosen license.

//! Scheme reader.
//!
//! This crate contains modules implementing basic syntax analysis of Scheme source code.

#[cfg(feature = "unicode")]
extern crate unicode;

pub mod diagnostics;
pub mod intern_pool;
pub mod lexer;
pub mod tokens;
