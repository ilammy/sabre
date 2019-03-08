// Copyright (c) 2016, Sabre developers
//
// Licensed under the Apache License, Version 2.0 (see LICENSE.Apache in the
// root directory) or MIT license (see LICENSE.MIT in the root directory),
// at your option. This file may be copied, distributed, and modified only
// in accordance with the terms specified by the chosen license.

//! Unicode utilities.
//!
//! This module contains miscellaneous minor utilities used in Unicode processing.

/// A `char` with emebedded canonical combining class.
///
/// This is an optimized layout of `(char, u8)` values. Unicode codepoints need only 21 bits while
/// char type can has 32 bits available. This means we can technically use the high octet for the
/// value of canonical combining class of a codepoint. However, the compiler does not know that we
/// do not need the high bits to contain zeros so it cannot do this optimization for us, thus all
/// `(char, u8)` tuples end up taking 8 bytes of space rather than just 4.
#[allow(non_camel_case_types)]
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct charcc(u32);

const CODEPOINT_MASK: u32 = 0x00_FF_FF_FF;

impl charcc {
    /// Make a new charcc from a char.
    ///
    /// This is for cases where you know only the codepoint. The function will lookup correct
    /// canonical combining class for you.
    pub fn from_char(c: char) -> charcc {
        use crate::tables::character_properties::canonical_combining_class as ccc;

        Self::from_char_with_ccc(c, ccc(c))
    }

    /// Make a new charcc from a char and a precomputed canonical combining class.
    ///
    /// This is for cases when you know canonical combining class somehow (e.g., it is guaranteed
    /// to be zero). The function will validate canonical combining class value for you.
    pub fn from_char_with_ccc(c: char, ccc: u8) -> charcc {
        Self::from_u32(u32::from(c) | (u32::from(ccc) << 24))
    }

    /// Make a new charcc from its raw u32 form.
    ///
    /// This is for data tables. Stable rustc does not include proper support for compile-time
    /// functions and we would like to have static tables of charccs. The function will validate
    /// the layout and values of both codepoint and canonical combining class parts of charcc.
    pub fn from_u32(value: u32) -> charcc {
        debug_assert!(charcc::valid_charcc(value));

        charcc(value)
    }

    /// Cast a u32 slice into a charcc slice.
    ///
    /// This is also for data tables, like applying `from_u32` to a whole slice.
    #[allow(clippy::transmute_ptr_to_ptr)]
    pub fn from_u32_slice(slice: &[u32]) -> &[charcc] {
        debug_assert!(slice.iter().all(|&v| charcc::valid_charcc(v)));

        // This is safe as 1) charcc and u32 have the same layout, 2) we have validated the slice.
        unsafe { std::mem::transmute(slice) }
    }

    fn valid_charcc(value: u32) -> bool {
        use crate::tables::character_properties::canonical_combining_class as compute_ccc;

        let ccc = value >> 24;
        let codepoint = value & CODEPOINT_MASK;

        let valid_ccc = ccc < 256;
        let valid_codepoint = std::char::from_u32(codepoint).is_some();

        let actual_ccc = compute_ccc(std::char::from_u32(codepoint).unwrap());

        valid_ccc && valid_codepoint && (ccc as u8) == actual_ccc
    }

    /// Extract char value of charcc.
    pub fn to_char(self) -> char {
        // This is safe as we validate character values when constructing charccs.
        unsafe { std::char::from_u32_unchecked(self.0 & CODEPOINT_MASK) }
    }

    /// Extract canonical combining class of charcc.
    pub fn ccc(self) -> u8 {
        (self.0 >> 24) as u8
    }
}
