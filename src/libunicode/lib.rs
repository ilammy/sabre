// Copyright (c) 2016, ilammy
//
// Licensed under MIT license (see LICENSE file in the root directory).
// This file may be copied, distributed, and modified only in accordance
// with the terms specified by this license.

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

pub mod normalization;

mod util;
