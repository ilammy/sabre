// Copyright (c) 2016, Sabre developers
//
// Licensed under the Apache License, Version 2.0 (see LICENSE.Apache in the
// root directory) or MIT license (see LICENSE.MIT in the root directory),
// at your option. This file may be copied, distributed, and modified only
// in accordance with the terms specified by the chosen license.

//! Pretty-printing trees.

use std::fmt;

/// Displayable wrapper over `Iterator<Item=Dfs<T>>` for displayable types.
#[doc(hidden)]
pub struct ClangTreeDisplay<I> {
    iter: I,
}

/// Displayable wrapper over `Iterator<Item=Dfs<T>>` with custom formatter.
#[doc(hidden)]
pub struct ClangTree<I, F> {
    iter: I,
    f: F,
}

/// TODO
pub struct Dfs<T> {
    pub depth: u16,
    pub value: T,
}

///
pub trait ClangFormat where Self: Sized {
    ///
    fn format_clang(self) -> ClangTreeDisplay<Self>;

    ///
    fn format_clang_with<F>(self, f: F) -> ClangTree<Self, F>;
}

impl<I, T> ClangFormat for I where I: Iterator<Item=Dfs<T>> {
    fn format_clang(self) -> ClangTreeDisplay<Self> {
        ClangTreeDisplay { iter: self }
    }

    fn format_clang_with<F>(self, f: F) -> ClangTree<Self, F> {
        ClangTree { iter: self, f: f }
    }
}

impl<I, T> fmt::Display for ClangTreeDisplay<I>
    where I: Iterator<Item=Dfs<T>>, T: fmt::Display
{
    fn fmt(&self, buf: &mut fmt::Formatter) -> fmt::Result {
        Ok(())
    }
}
