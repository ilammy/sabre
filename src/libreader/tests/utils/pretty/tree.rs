// Copyright (c) 2016, Sabre developers
//
// Licensed under the Apache License, Version 2.0 (see LICENSE.Apache in the
// root directory) or MIT license (see LICENSE.MIT in the root directory),
// at your option. This file may be copied, distributed, and modified only
// in accordance with the terms specified by the chosen license.

//! Pretty-printing trees.
//!
//! This module provides a facility for pretty-printing trees in a format similar to Clang:
//!
//! ```plaintext
//! root
//! |- child node
//! `- more children
//!    |- which can have
//!    |  `- their own children
//!    `- and so on
//! ```
//!
//! Users must implement the `TreeNode` trait for their data to describe the tree structure.
//! Actual formatting of individual nodes may be performed in several ways:
//!
//!   - Implement the `DisplayTreeNode` trait (exported by this module).
//!
//!     This trait is similar to the usual `fmt::Display`. It requires identical interface
//!     based on `fmt::Formatter`, and is suitable when your nodes have some canonical
//!     representation which you will be using 99% of time.
//!
//!   - Explicitly specify the way to format your nodes.
//!
//!     This is convenient when you need to use multiple representations of tree nodes.
//!     Just write a formatting function for each of your representations.
//!
//! The pretty-printer can handle multiline node representations, but it will normalize all
//! newlines into just `\n`.

use std::fmt;

use tree::{TreeNode};

/// Formatting trait for trees.
///
/// This trait is similar to `fmt::Display` and its friends from `std::fmt`.
pub trait DisplayTreeNode {
    /// Formats this node using the given formatter.
    fn fmt(&self, &mut fmt::Formatter) -> fmt::Result;
}

/// Format a tree into a string.
pub fn format<T>(tree: &T) -> String
    where T: DisplayTreeNode + TreeNode
{
    let mut string = String::new();
    let _ = write(tree, &mut string);
    return string;
}

/// Write a tree into the provided sink.
pub fn write<T>(tree: &T, output: &mut fmt::Write) -> fmt::Result
    where T: DisplayTreeNode + TreeNode
{
    write_with_prefix(tree, output, &|node| format!("{}", DisplayProxy { value: node }), "")
}

/// Format a tree into a string, formatting nodes in a specified way.
pub fn format_with<T, F>(tree: &T, format: F) -> String
    where T: TreeNode, F: Fn(&T) -> String
{
    let mut string = String::new();
    let _ = write_with(tree, &mut string, &format);
    return string;
}

/// Write a tree into the provided sink, formatting nodes in a specified way.
pub fn write_with<T, F>(tree: &T, output: &mut fmt::Write, format: F) -> fmt::Result
    where T: TreeNode, F: Fn(&T) -> String
{
    write_with_prefix(tree, output, &format, "")
}

/// A wrapper for `Display` trait.
///
/// We *do want* to provide the user with access to `fmt::Formatter` in `DisplayTreeNode` which
/// enables formatting code reuse. However, Rust does not allow custom formatting traits or
/// multiple implementations of Display for a single type. Thus we introduce this proxy type
/// that wraps a reference to `DisplayTreeNode` and provides an implementation of `Display` trait
/// so that we can use the `{}` format string to output our format.
struct DisplayProxy<'a, T> where T: 'a + DisplayTreeNode { value: &'a T }

impl<'a, T> fmt::Display for DisplayProxy<'a, T> where T: DisplayTreeNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.value.fmt(f)
    }
}

/// Write a given tree into the provided writer while formatting nodes using the given formatter
/// and prefixing each line with a given prefix.
fn write_with_prefix<T, F>(root: &T, output: &mut fmt::Write, format: &F, prefix: &str)
    -> fmt::Result
    where T: TreeNode, F: Fn(&T) -> String
{
    let root_str = format(root);
    let mut line = root_str.lines();

    if let Some(current) = line.next() {
        try!(output.write_str(current));
        while let Some(current) = line.next() {
            try!(output.write_char('\n'));
            try!(output.write_str(prefix));
            try!(output.write_str(current));
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

#[cfg(test)]
mod tests {
    use super::*;
    use std::fmt;
    use tree::TreeNode;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    // Example tree implementation

    struct Tree<T> {
        value: T,
        children: Vec<Tree<T>>,
    }

    impl<T> DisplayTreeNode for Tree<T> where T: fmt::Display {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            write!(f, "{}", self.value)
        }
    }

    impl<T> TreeNode for Tree<T> {
        fn children<'a>(&'a self) -> Vec<&'a Self> {
            self.children.iter().collect()
        }
    }

    impl<T> Tree<T> {
        fn new(value: T, children: Vec<Tree<T>>) -> Tree<T> {
            Tree { value: value, children: children }
        }
    }

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    // Simple formatting

    #[test]
    fn only_root_element() {
        let tree = Tree::new(1, vec![]);
        let expected = "1";

        assert_eq!(expected, format(&tree));
    }

    #[test]
    fn sibling_elements() {
        let tree =
            Tree::new(1, vec![
                Tree::new(2, vec![]),
                Tree::new(3, vec![]),
                Tree::new(4, vec![]),
            ]);
        let expected = "\
1
|- 2
|- 3
`- 4";
        assert_eq!(expected, format(&tree));
    }

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    // More complex formatting

    #[test]
    fn nested_elements() {
        let tree =
            Tree::new(1, vec![
                Tree::new(2, vec![
                    Tree::new(3, vec![]),
                    Tree::new(4, vec![]),
                ]),
                Tree::new(5, vec![
                    Tree::new(6, vec![
                        Tree::new(7, vec![]),
                    ]),
                ]),
                Tree::new(8, vec![
                    Tree::new(9, vec![
                        Tree::new(10, vec![]),
                        Tree::new(11, vec![]),
                    ]),
                ]),
                Tree::new(12, vec![
                    Tree::new(13, vec![]),
                ]),
            ]);
        let expected = "\
1
|- 2
|  |- 3
|  `- 4
|- 5
|  `- 6
|     `- 7
|- 8
|  `- 9
|     |- 10
|     `- 11
`- 12
   `- 13";

        assert_eq!(expected, format(&tree));
    }

    #[test]
    fn multiline_nodes() {
        let tree =
            Tree::new("Node\na", vec![
                Tree::new("Node\nb\nb\n\nb", vec![]),
                Tree::new("Node\nc", vec![
                    Tree::new("Node\r\nd", vec![]),
                ]),
                Tree::new("Node\ne", vec![]),
            ]);
        let expected = "\
Node
a
|- Node
|  b
|  b
|  \n\
|  b
|- Node
|  c
|  `- Node
|     d
`- Node
   e";

        assert_eq!(expected, format(&tree));
    }

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    // Ad-hoc formatting function

    #[test]
    fn formatting_function() {
        let tree =
            Tree::new(1, vec![
                Tree::new(2, vec![]),
                Tree::new(3, vec![])
            ]);
        let expected = "\
<1>
|- <2>
`- <3>";

        assert_eq!(expected, format_with(&tree, |node| format!("<{}>", node.value)));
    }
}
