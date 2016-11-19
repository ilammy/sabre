// Copyright (c) 2016, ilammy
//
// Licensed under MIT license (see LICENSE file in the root directory).
// This file may be copied, distributed, and modified only in accordance
// with the terms specified by this license.

//! Scheme reader.
//!
//! This crate contains modules implementing basic syntax analysis of Scheme source code.

#[cfg(feature = "unicode")]
extern crate unicode;

pub mod diagnostics;
pub mod intern_pool;
pub mod lexer;
pub mod tokens;
