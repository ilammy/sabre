// Copyright (c) 2016, Sabre developers
//
// Licensed under the Apache License, Version 2.0 (see LICENSE.Apache in the
// root directory) or MIT license (see LICENSE.MIT in the root directory),
// at your option. This file may be copied, distributed, and modified only
// in accordance with the terms specified by the chosen license.

//! Pretty-printing sequential diffs.
//!
//! This module provides a facility for pretty-printing diffs produced by the
//! [`diff::sequential`](../../../diff/sequence/index.html) module in usual textual formats:
//!
//!   - unified diffs
//!
//!     ```diff
//!      context
//!     -old value
//!     +new value
//!      more context
//!     ```
//!
//! All formatting functions exist in two flavors:
//!
//!   - `format_*()` functions return ready-made strings
//!
//!   - `write_*()` functions write into some `fmt::Write`
//!
//! Formatting of individual elements may be performed in several ways:
//!
//!   - Implement the standard `fmt::Display` trait.
//!
//!     The `format_*()` and `write_*()` functions use this trait for formatting.
//!
//!   - Provide a formatter closure for the elements.
//!
//!     This is used by `format_*_with()` and `write_*_with()` functions.
//!
//! Pretty-printers can handle multiline element representations, but they will normalize all
//! newlines into just `\n`.

use std::fmt;

use diff::sequence::Diff;

/// Format a unified diff into a string.
pub fn format_unified<'a, T>(diff: &[Diff<'a, T>]) -> String
    where T: fmt::Display
{
    do_format_unified_with(diff, |elt| format!("{}", elt))
}

/// Write a unified diff into the provided sink.
pub fn write_unified<'a, T>(diff: &[Diff<'a, T>], output: &mut fmt::Write) -> fmt::Result
    where T: fmt::Display
{
    do_write_unified_with(diff, output, |buf, elt| write!(buf, "{}", elt))
}

/// Format a unified diff into a string, formatting elements in a specified way.
pub fn format_unified_with<'a, T, F>(diff: &[Diff<'a, T>], format: F) -> String
    where F: Fn(&T) -> String
{
    do_format_unified_with(diff, format)
}

/// Write a unified diff into the provided sink, formatting elements in a specified way.
pub fn write_unified_with<'a, T, F>(diff: &[Diff<'a, T>], output: &mut fmt::Write, format: F)
    -> fmt::Result
    where F: Fn(&mut fmt::Write, &T) -> fmt::Result
{
    do_write_unified_with(diff, output, format)
}

/// Format a diff in unified format into a string while formatting elements using the given
/// formatter.
fn do_format_unified_with<'a, T, F>(diff: &[Diff<'a, T>], format: F) -> String
    where F: Fn(&T) -> String
{
    let mut string = String::new();
    let _ = do_write_unified_with(diff, &mut string, |buf, elt| buf.write_str(&format(elt)));
    return string;
}

/// Write a diff in unified format into the provided writer while formatting elements using
/// the given formatter.
#[must_use]
fn do_write_unified_with<'a, T, F>(diff: &[Diff<'a, T>], output: &mut fmt::Write, format: F)
    -> fmt::Result
    where F: Fn(&mut fmt::Write, &T) -> fmt::Result
{
    // Reuse this buffer for repackaging multiline elements.
    let mut line_buffer = String::new();
    let mut write_diff = |prefix, element| {
        line_buffer.clear();
        format(&mut line_buffer, element)?;

        for line in line_buffer.lines() {
            output.write_str(prefix)?;
            output.write_str(line)?;
            output.write_str("\n")?;
        }

        Ok(())
    };

    for item in diff {
        match *item {
            Diff::Left(element) => {
                write_diff("-", element)?;
            }
            Diff::Right(element) => {
                write_diff("+", element)?;
            }
            Diff::Equal(element, _) => {
                write_diff(" ", element)?;
            }
            Diff::Replace(left_element, right_element) => {
                write_diff("-", left_element)?;
                write_diff("+", right_element)?;
            }
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use diff::sequence;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    // Unified diffs

    #[test]
    fn unified_simple_formatting() {
        let a = vec!["context", "old value", "more context"];
        let b = vec!["context", "new value", "more context"];

        let diff = sequence::diff(&a, &b);

        assert_eq!(format_unified(&diff),
" context
-old value
+new value
 more context
");
    }

    #[test]
    fn unified_unfolded_formatting() {
        let a = vec!["line 1", "line 2"];
        let b = vec!["line A", "line B"];

        let diff = sequence::unfold_replacements(sequence::diff(&a, &b));

        println!("");
        println!("{:?}", &diff);
        println!("");

        assert_eq!(format_unified(&diff), "\
-line 1
-line 2
+line A
+line B
");
    }

    #[test]
    fn unified_multiline_formatting() {
        let a = vec!["line 1.1\nline 1.2", "line 2.1\nline 2.2"];
        let b = vec!["line A.A\nline A.B", "line B.A\nline B.B"];

        let diff = sequence::diff(&a, &b);

        assert_eq!(format_unified(&diff), "\
-line 1.1
-line 1.2
+line A.A
+line A.B
-line 2.1
-line 2.2
+line B.A
+line B.B
");
    }

    #[test]
    fn unified_custom_formatting() {
        let a = vec!["thing"];
        let b = vec!["thing", "with", "stuff"];

        let diff = sequence::diff(&a, &b);

        assert_eq!(format_unified_with(&diff, |string| format!("<{}>", string)),
" <thing>
+<with>
+<stuff>
");
    }
}
