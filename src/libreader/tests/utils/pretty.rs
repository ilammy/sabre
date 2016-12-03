// Copyright (c) 2016, Sabre developers
//
// Licensed under the Apache License, Version 2.0 (see LICENSE.Apache in the
// root directory) or MIT license (see LICENSE.MIT in the root directory),
// at your option. This file may be copied, distributed, and modified only
// in accordance with the terms specified by the chosen license.

//! Pretty-printing.
//!
//! This module defines the [`Pretty`](#trait.Pretty.html) trait for pretty-printing things.
//! The reason for a separate trait instead of just using `Display` is that sometimes you want
//! to format elements of a container differently, but maintain the overall format of the
//! container structure. `Display` does not provide an ability to use a custom dynamic formatter
//! so it's impossible to express this behavior.
//!
//! `Pretty` requires you to specify the actual `Element` that may be formatted differently.
//! It may be `Self`, in which case `Pretty` is pretty much a `Display` with a custom format
//! override.
//!
//! `Pretty` provides two ways for formatting things:
//!
//!   - [`format()`][format] — format `Self` into a string.
//!
//!     This method may use temporary strings and has no way to report formatting errors.
//!     Use it when you just need a string and do not care about the details.
//!
//!   - [`write()`][write] — write `Self` into an `fmt::Write`.
//!
//!     This method is more efficient in that it writes into a streaming buffer and tries
//!     to avoid unnecessary allocations. It is also more robust as it requires proper
//!     error handling.
//!
//! Both these methods require `Element` to implement `fmt::Display` and use it for formatting
//! the elements. If you need something different then you should use the following alternates:
//!
//!   - [`format_with(closure)`][format_with] — like `format()`, but with an explicit format.
//!
//!     It accepts an additional closure which will be used to format individual elements.
//!
//!   - [`write_with(closure)`][write_with] — like `write()`, but with an explicit format.
//!
//!     It accepts an additional closure which will be used to write individual elements
//!     into the format buffer.
//!
//! [format]:      trait.Pretty.html#method.format
//! [format_with]: trait.Pretty.html#method.format_with
//! [write]:       trait.Pretty.html#method.write
//! [write_with]:  trait.Pretty.html#tymethod.write_with

use std::fmt;

/// Trait of pretty-printable things.
///
/// You must implement only the most generic method [`write_with()`](#tymethod.write_with) which
/// is analogous to the `fmt()` method of the standard `Display` trait. Other methods are more or
/// less efficiently defined in terms of this method, but you are free, of course, to override
/// them with your implementations. You can also implement your formatting logic in any one of
/// of these methods and redefine the other three in terms of it.
pub trait Pretty<'a> where Self: 'a {
    /// The thing that actually gets formatted. Usually `Self`, but not always.
    type Element;

    /// Format self into a string.
    fn format(&'a self) -> String
        where Self::Element: fmt::Display
    {
        let mut s = String::new();
        let _ignore = self.write(&mut s);
        return s;
    }

    /// Format self into a string with elements formatted in the specified way.
    fn format_with<F>(&'a self, format: F) -> String
        where F: Fn(&Self::Element) -> String
    {
        let mut s = String::new();
        let _ignore = self.write_with(&mut s, |element, buf| buf.write_str(&format(element)));
        return s;
    }

    /// Write self into the provided sink.
    #[must_use]
    fn write(&'a self, buf: &mut fmt::Write) -> fmt::Result
        where Self::Element: fmt::Display
    {
        self.write_with(buf, |element, buf| write!(buf, "{}", element))
    }

    /// Write self into the provided sink with elements formatted in the specified way.
    #[must_use]
    fn write_with<F>(&'a self, buf: &mut fmt::Write, write: F) -> fmt::Result
        where F: Fn(&Self::Element, &mut fmt::Write) -> fmt::Result;
}
