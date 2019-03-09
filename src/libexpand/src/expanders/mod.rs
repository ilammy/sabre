// Copyright (c) 2017, Sabre developers
//
// Licensed under the Apache License, Version 2.0 (see LICENSE.Apache in the
// root directory) or MIT license (see LICENSE.MIT in the root directory),
// at your option. This file may be copied, distributed, and modified only
// in accordance with the terms specified by the chosen license.

//! Scheme expression expanders.
//!
//! Here we gather the basis of Scheme extensible expressions: the macro expander interface
//! and expanders for primitive form. They are not 'macros' in the strict sense, but they do
//! convert data into code, just like user-defined macros do. Sometimes they are referred to
//! as _magic keywords_.

mod begin;
mod if_expander;
mod lambda;
mod quote;
mod set;
mod utils;

pub use self::begin::BeginExpander;
pub use self::if_expander::IfExpander;
pub use self::lambda::LambdaExpander;
pub use self::quote::QuoteExpander;
pub use self::set::SetExpander;
