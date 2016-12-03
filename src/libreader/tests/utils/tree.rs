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
    /// Value of this node.
    type Value;

    /// Returns reference to the value of this node.
    fn value(&self) -> &Self::Value;

    /// Returns references to child nodes of this node.
    fn children<'a>(&'a self) -> Vec<&'a Self>;
}

impl<T> Pretty for T where T: TreeNode {
    type Element = T::Value;

    fn write_with<F>(&self, buf: &mut fmt::Write, write: F) -> fmt::Result
        where F: Fn(&Self::Element, &mut fmt::Write) -> fmt::Result
    {
        write_with_prefix(self, buf, &write, "")
    }
}

/// Write a given tree into the provided sink while formatting nodes using the given formatter
/// and prefixing each line with the given prefix.
fn write_with_prefix<T, F>(root: &T, buf: &mut fmt::Write, write: &F, prefix: &str) -> fmt::Result
    where T: TreeNode, F: Fn(&T::Value, &mut fmt::Write) -> fmt::Result
{
    // Repackage the representation of the root node and write it out.
    let mut line_buffer = String::new();
    try!(write(root.value(), &mut line_buffer));

    for line in line_buffer.lines() {
        try!(buf.write_str(prefix));
        try!(buf.write_str(line));
        try!(buf.write_str("\n"));
    }

    // Write out the child nodes, prefixing them with branch indicators. We use iterators
    // directly here as we need to do special things with the last element.
    let children = root.children();
    let mut iter = children.iter();

    if let Some(mut current) = iter.next() {
        while let Some(next) = iter.next() {
            try!(buf.write_str(prefix));
            try!(buf.write_str("|- "));
            try!(write_with_prefix(*current, buf, write, &format!("{}|  ", prefix)));
            current = next;
        }
        try!(buf.write_str(prefix));
        try!(buf.write_str("`- "));
        try!(write_with_prefix(*current, buf, write, &format!("{}   ", prefix)));
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fmt;
    use pretty::Pretty;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    // Example tree implementation

    struct Tree<T> {
        value: T,
        children: Vec<Tree<T>>,
    }

    impl<T> Tree<T> {
        fn new(value: T, children: Vec<Tree<T>>) -> Tree<T> {
            Tree { value: value, children: children }
        }
    }

    impl<T> TreeNode for Tree<T> {
        type Value = T;

        fn value(&self) -> &T {
            &self.value
        }

        fn children<'a>(&'a self) -> Vec<&'a Self> {
            self.children.iter().collect()
        }
    }

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    // Pretty-printing

    #[test]
    fn pretty_lonely_root() {
        let tree = Tree::new(1, vec![]);

        assert_eq!(tree.format(), "\
1\n");
    }

    #[test]
    fn pretty_siblings() {
        let tree =
            Tree::new(1, vec![
                Tree::new(2, vec![]),
                Tree::new(3, vec![]),
                Tree::new(4, vec![]),
            ]);
        assert_eq!(tree.format(), "\
1
|- 2
|- 3
`- 4\n");
    }

/*
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

        assert_eq!(expected, format_with(&tree, &|node| format!("<{}>", node.value)));
    }
*/
}
