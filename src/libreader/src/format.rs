// Copyright (c) 2017, Sabre developers
//
// Licensed under the Apache License, Version 2.0 (see LICENSE.Apache in the
// root directory) or MIT license (see LICENSE.MIT in the root directory),
// at your option. This file may be copied, distributed, and modified only
// in accordance with the terms specified by the chosen license.

//! Formatting utilities.
//!
//! This module exports some utilities for formatting s-expressions and Scheme values.

use std::fmt;

/// Format an s-expression list.
///
/// Format items for debugging as an s-expression list in `fmt::Debug` implementation.
pub fn write_list<I, T>(f: &mut fmt::Formatter, prefix: &str, items: I, sep: &str, suffix: &str)
    -> fmt::Result
    where I: IntoIterator<Item=T>,
          T: fmt::Debug,
{
    let mut first = true;

    f.write_str(prefix)?;
    for item in items {
        if first {
            first = false;
        } else {
            f.write_str(sep)?;
        }
        write!(f, "{:?}", item)?;
    }
    f.write_str(suffix)?;

    return Ok(());
}

/// Format a dotted s-expression list.
///
/// Format items for debugging as a dotted s-expression list in `fmt::Debug` implementation.
pub fn write_dotted_list<I, T>(f: &mut fmt::Formatter, prefix: &str, items: I, sep: &str, suffix: &str)
    -> fmt::Result
    where I: IntoIterator<Item=T>,
          T: fmt::Debug,
{
    let mut iter = items.into_iter();
    let mut curr = iter.next();
    let mut first = true;

    f.write_str(prefix)?;
    while let Some(item) = curr {
        let next = iter.next();

        if first {
            first = false;
        } else {
            if next.is_some() {
                f.write_str(sep)?;
            } else {
                f.write_str(sep)?;
                f.write_str(".")?;
                f.write_str(sep)?;
            }
        }
        write!(f, "{:?}", item)?;

        curr = next;
    }
    f.write_str(suffix)?;

    return Ok(());
}

#[cfg(test)]
mod tests {
    use super::*;

    use std::fmt;

    struct ProperList<I>(I);
    struct DottedList<I>(I);

    impl<I, T> fmt::Debug for ProperList<I>
        where I: IntoIterator<Item=T> + Clone, T: fmt::Debug
    {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            write_list(f, "<", self.0.clone(), ", ", ">")
        }
    }

    impl<I, T> fmt::Debug for DottedList<I>
        where I: IntoIterator<Item=T> + Clone, T: fmt::Debug
    {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            write_dotted_list(f, "#u8(", self.0.clone(), " ", ")")
        }
    }

    #[test]
    fn format_proper_list_zero() {
        assert_eq!(format!("{:?}", ProperList(&[] as &[i32])), "<>");
    }

    #[test]
    fn format_proper_list_one() {
        assert_eq!(format!("{:?}", ProperList(&[1])), "<1>");
    }

    #[test]
    fn format_proper_list_many() {
        assert_eq!(format!("{:?}", ProperList(&[1, 2, 3])), "<1, 2, 3>");
    }

    #[test]
    fn format_dotted_list_zero() {
        assert_eq!(format!("{:?}", DottedList(&[] as &[i32])), "#u8()");
    }

    #[test]
    fn format_dotted_list_one() {
        assert_eq!(format!("{:?}", DottedList(&[1])), "#u8(1)");
    }

    #[test]
    fn format_dotted_list_two() {
        assert_eq!(format!("{:?}", DottedList(&[1, 2])), "#u8(1 . 2)");
    }

    #[test]
    fn format_dotted_list_many() {
        assert_eq!(format!("{:?}", DottedList(&[1, 2, 3, 4])), "#u8(1 2 3 . 4)");
    }
}
