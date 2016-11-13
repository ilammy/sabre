// Copyright (c) 2016, ilammy
//
// Licensed under MIT license (see LICENSE file in the root directory).
// This file may be copied, distributed, and modified only in accordance
// with the terms specified by this license.

//! Unicode default case algorithms.
//!
//! This module implements _Default Case Algorithms_ as defined by [Unicode Standard 3.13][US-3].
//! Case algorithms are used to transform characters of bicameral scripts between their lowercase
//! and uppercase forms.
//!
//! The algorithms are called _default_ because they are not tailored for a specific locale,
//! language, or purpose. In general, case mappings are context-dependent, but these algorithms
//! are context-free.
//!
//! [US-3]: http://www.unicode.org/versions/latest/ch03.pdf

use normalization;
use tables::case_mappings;

//
// Default Case Folding
//

/// Fold case and normalize a string according to **toNFKC_Casefold** operation (_R5_).
pub fn to_nfkc_casefold(s: &str) -> String {
    let mut folded = String::with_capacity(s.len());

    for c in s.chars() {
        match case_mappings::nfkc_casefold(c) {
            Some(slice) => { folded.push_str(slice); }
            None        => { folded.push(c); }
        }
    }

    return normalization::nfc(&folded);
}
