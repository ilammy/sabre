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

use tree::TreeNodeEx;

/// Format a tree into a string.
pub fn format<T>(tree: T) -> String
    where T: TreeNodeEx, T::Value: fmt::Display
{
    let mut string = String::new();
    let _ = write(tree, &mut string);
    return string;
}

/// Write a tree into the provided sink.
pub fn write<T>(tree: T, output: &mut fmt::Write) -> fmt::Result
    where T: TreeNodeEx, T::Value: fmt::Display
{
    write_with_prefix(&tree, output, &|node| format!("{}", node), "")
}

/// Format a tree into a string, formatting nodes in a specified way.
pub fn format_with<T>(tree: T, format: &Fn(&T::Value) -> String) -> String
    where T: TreeNodeEx
{
    let mut string = String::new();
    let _ = write_with(tree, &mut string, format);
    return string;
}

/// Write a tree into the provided sink, formatting nodes in a specified way.
pub fn write_with<T>(tree: T, output: &mut fmt::Write, format: &Fn(&T::Value) -> String)
    -> fmt::Result
    where T: TreeNodeEx
{
    write_with_prefix(&tree, output, format, "")
}

/// Write a given tree into the provided writer while formatting nodes using the given formatter
/// and prefixing each line with a given prefix.
fn write_with_prefix<T>(root: &T, output: &mut fmt::Write,
                            format: &Fn(&T::Value) -> String, prefix: &str)
    -> fmt::Result
    where T: TreeNodeEx
{
    let root_str = format(root.value());
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

    let mut children = root.children();

    if let Some(mut current) = children.next() {
        while let Some(next) = children.next() {
            try!(output.write_str(&before_next));
            try!(write_with_prefix(&current, output, format, &continue_next));
            current = next;
        }
        try!(output.write_str(&before_last));
        try!(write_with_prefix(&current, output, format, &continue_last));
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::slice;
    use tree::TreeNodeEx;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    // Example tree implementation

    struct Tree<T> {
        value: T,
        children: Vec<Tree<T>>,
    }

    impl<'a, T> TreeNodeEx for &'a Tree<T> where T: 'a {
        type Value = T;
        type ChildIter = slice::Iter<'a, Tree<T>>;

        fn value(&self) -> &Self::Value {
            &self.value
        }

        fn children(&self) -> Self::ChildIter {
            self.children.iter()
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

        assert_eq!(expected, format_with(&tree, &|value| format!("<{}>", value)));
    }
}
