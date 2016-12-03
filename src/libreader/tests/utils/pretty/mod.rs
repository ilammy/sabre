// Copyright (c) 2016, Sabre developers
//
// Licensed under the Apache License, Version 2.0 (see LICENSE.Apache in the
// root directory) or MIT license (see LICENSE.MIT in the root directory),
// at your option. This file may be copied, distributed, and modified only
// in accordance with the terms specified by the chosen license.

//! Pretty-printing.
//!
//! TODO

use std::fmt;

pub mod tree;

/// Trait of pretty-printable things.
///
/// TODO
///
/// You must implement only the most generic method [`write_with()`](#tymethod.write_with) which
/// is analogous to the `fmt()` method of the standard `Display` trait. Other methods are more or
/// less efficiently defined in terms of this method, but you are free, of course, to override
/// them with your implementations. You can also implement your formatting logic in any one of
/// of these methods and redefine the other three in terms of it.
///
/// Note that the actual printed element may be different from `Self`. For example, for `Vec<T>`
/// you would like to define `Element` as `T`, not the whole `Vec<T>`, to be able to reuse and
/// override formatting of individual vector elements while maintaining consistent formatting
/// of various vectors.
pub trait Pretty {
    /// The thing that actually gets formatted. Usually `Self`, but not always.
    type Element;

    /// Format self into a string.
    fn format(&self) -> String
        where Self::Element: fmt::Display
    {
        let mut s = String::new();
        let _ignore = self.write(&mut s);
        return s;
    }

    /// Format self into a string with elements formatted in the specified way.
    fn format_with<F>(&self, format: F) -> String
        where F: Fn(&Self::Element) -> String
    {
        let mut s = String::new();
        let _ignore = self.write_with(&mut s, |element, buf| buf.write_str(&format(element)));
        return s;
    }

    /// Write self into the provided sink.
    #[must_use]
    fn write(&self, buf: &mut fmt::Write) -> fmt::Result
        where Self::Element: fmt::Display
    {
        self.write_with(buf, |element, buf| write!(buf, "{}", element))
    }

    /// Write self into the provided sink with elements formatted in the specified way.
    #[must_use]
    fn write_with<F>(&self, buf: &mut fmt::Write, write: F) -> fmt::Result
        where F: Fn(&Self::Element, &mut fmt::Write) -> fmt::Result;
}
