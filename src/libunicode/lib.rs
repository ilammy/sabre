// Copyright (c) 2016, Sabre developers
//
// Licensed under the Apache License, Version 2.0 (see LICENSE.Apache in the
// root directory) or MIT license (see LICENSE.MIT in the root directory),
// at your option. This file may be copied, distributed, and modified only
// in accordance with the terms specified by the chosen license.

//! Unicode support for Sabre.
//!
//! This crate contains definitions of various Unicode character properties, algorithms, etc.
//! which are used internally by the implementation. It is intended to be a minimal sufficient
//! subset of some proper full Unicode support library, so as to consume minimum amount of space.
//! Full Unicode support requires multiple megabyte-sized lookup tables which are not actually
//! needed in limited contexts such as programming language implementations.

mod tables;

pub use tables::UNICODE_VERSION;
pub use tables::scheme_identifiers;

pub mod case_algorithms;
pub mod normalization;

mod util;
