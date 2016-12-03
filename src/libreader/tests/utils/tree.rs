// Copyright (c) 2016, Sabre developers
//
// Licensed under the Apache License, Version 2.0 (see LICENSE.Apache in the
// root directory) or MIT license (see LICENSE.MIT in the root directory),
// at your option. This file may be copied, distributed, and modified only
// in accordance with the terms specified by the chosen license.

//! Tree utilities.
//!
//! TODO describe pp format

use std::fmt;

use pretty::Pretty;

/// Trait of tree nodes.
pub trait TreeNode {
    /// Returns references to child nodes of this node.
    fn children<'a>(&'a self) -> Vec<&'a Self>;
}

impl<T> Pretty for T where T: TreeNode {
    type Element = Self;

    fn write_with<F>(&self, buf: &mut fmt::Write, write: F) -> fmt::Result
        where F: Fn(&Self, &mut fmt::Write) -> fmt::Result
    {
        unimplemented!()
    }
}

/*

/// Write a given tree into the provided sink while formatting nodes using the given formatter
/// and prefixing each line with the given prefix.
fn write_with_prefix<T, F>(root: &T, buf: &mut fmt::Write, write: &F, prefix: &str) -> fmt::Result
    where T: TreeNode, F: Fn(&T, &mut fmt::Write) -> fmt::Result
{
    // Use this buffer to repackage multiline nodes.
    let mut line_buffer = String::new();
    try!(write(root, &mut line_buffer));

    let mut line = line_buffer.lines();
    if let Some(current) = line.next() {
        try!(buf.write_str(current));
        while let Some(current) = line.next() {
            try!(buf.write_str("\n"));
            try!(buf.write_str(prefix));
            try!(buf.write_str(current));
        }
    }

    let before_next = format!("\n{}|- ", prefix);
    let continue_next = format!("{}|  ", prefix);
    let before_last = format!("\n{}`- ", prefix);
    let continue_last = format!("{}   ", prefix);

    let children = root.children();
    let mut iter = children.iter();

    if let Some(mut current) = iter.next() {
        while let Some(next) = iter.next() {
            try!(output.write_str(&before_next));
            try!(write_with_prefix(*current, output, format, &continue_next));
            current = next;
        }
        try!(output.write_str(&before_last));
        try!(write_with_prefix(*current, output, format, &continue_last));
    }

    Ok(())
}

*/
