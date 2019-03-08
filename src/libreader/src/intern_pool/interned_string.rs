// Copyright (c) 2017, Sabre developers
//
// Licensed under the Apache License, Version 2.0 (see LICENSE.Apache in the
// root directory) or MIT license (see LICENSE.MIT in the root directory),
// at your option. This file may be copied, distributed, and modified only
// in accordance with the terms specified by the chosen license.

use std::borrow::Borrow;
use std::cmp::{Ord, Ordering};
use std::fmt;
use std::ops::Deref;
use std::rc::Rc;

/// Value of an interned string.
#[derive(Clone, PartialEq, Eq, Hash, PartialOrd)]
pub struct InternedString {
    /// Ref-counted pointer to the actual immutable data of this string.
    data: Rc<String>,
}

impl InternedString {
    /// Make a fresh unique interned string value.
    pub fn new(s: &str) -> InternedString {
        InternedString {
            data: Rc::new(s.to_string()),
        }
    }

    /// Wrap an existing String into InternedString.
    pub fn from_string(s: String) -> InternedString {
        InternedString {
            data: Rc::new(s),
        }
    }
}

impl Ord for InternedString {
    fn cmp(&self, other: &InternedString) -> Ordering {
        self[..].cmp(&other[..])
    }
}

impl fmt::Debug for InternedString {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self[..].fmt(f)
    }
}

impl fmt::Display for InternedString {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self[..].fmt(f)
    }
}

impl Borrow<str> for InternedString {
    fn borrow(&self) -> &str {
        &self.data[..]
    }
}

impl Deref for InternedString {
    type Target = str;

    fn deref(&self) -> &str { &self.data[..] }
}
