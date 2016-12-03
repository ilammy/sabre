// Copyright (c) 2016, Sabre developers
//
// Licensed under the Apache License, Version 2.0 (see LICENSE.Apache in the
// root directory) or MIT license (see LICENSE.MIT in the root directory),
// at your option. This file may be copied, distributed, and modified only
// in accordance with the terms specified by the chosen license.

//! Tree utilities.
//!
//! The core definition here is the [`TreeNode`](trait.TreeNode.html) trait which describes
//! recursive tree-like structures. For example, suppose you have the following `Tree` type
//! which is obviously tree-like:
//!
//! ```
//! struct Tree<T> {
//!     value: T,
//!     children: Vec<Tree<T>>,
//! }
//! ```
//!
//! Then you can straightforwardly decribe nodes of this tree in the following way:
//!
//! ```
//! # struct Tree<T> {
//! #     value: T,
//! #     children: Vec<Tree<T>>,
//! # }
//! #
//! use std::slice;
//! use utils::tree::TreeNode;
//!
//! impl<'a, T> TreeNode<'a> for Tree<T> where T: 'a {
//!     /// `Self` is a _node_ of a tree while `Self::Value` is the actual payload
//!     /// that gets acted upon during tree traversal.
//!     ///
//!     /// In our case this is the type of the `value` field of `Tree<T>`.
//!     type Value = T;
//!
//!     /// `Self::ChildIter` is the type of the iterator over the node's chilren.
//!     /// It should be an `Iterator<Item=&'a Self>`.
//!     ///
//!     /// In our case this is the type of an iterator over the `children` field
//!     /// of `Tree<T>`.
//!     type ChildIter = slice::Iter<'a, Self>;
//!
//!     /// Describe how to get a reference to the value of a node.
//!     fn value(&'a self) -> &'a Self::Value {
//!         &self.value
//!     }
//!
//!     /// Describe how to iterate over references to the children of a node.
//!     fn children(&'a self) -> Self::ChildIter {
//!         self.children.iter()
//!     }
//! }
//! ```
//!
//! And here is how a generic tree traversal may look like:
//!
//! ```
//! # use std::slice;
//! # use utils::tree::TreeNode;
//! #
//! # struct Tree<T> {
//! #     value: T,
//! #     children: Vec<Tree<T>>,
//! # }
//! #
//! # impl<'a, T> TreeNode<'a> for Tree<T> where T: 'a {
//! #     type Value = T;
//! #     type ChildIter = slice::Iter<'a, Self>;
//! #     fn value(&'a self) -> &'a Self::Value { &self.value }
//! #     fn children(&'a self) -> Self::ChildIter { self.children.iter() }
//! # }
//! #
//! struct DfsIterator<'a, T> where T: 'a {
//!     unseen: Vec<&'a T>,
//! }
//!
//! fn dfs<'a, T>(root: &'a T) -> DfsIterator<'a, T> {
//!     DfsIterator {
//!         unseen: vec![root],
//!     }
//! }
//!
//! impl<'a, Node> Iterator for DfsIterator<'a, Node> where Node: TreeNode<'a> {
//!     type Item = &'a Node::Value;
//!
//!     fn next(&mut self) -> Option<Self::Item> {
//!         if let Some(next) = self.unseen.pop() {
//!             self.unseen.extend(next.children());
//!             Some(next.value())
//!         } else {
//!             None
//!         }
//!     }
//! }
//!
//! let tree =
//!     Tree { value: 1, children: vec![
//!         Tree { value: 2, children: vec![
//!             Tree { value: 3, children: vec![]},
//!             Tree { value: 4, children: vec![]},
//!         ]},
//!         Tree { value: 5, children: vec![
//!             Tree { value: 6, children: vec![
//!                 Tree { value: 7, children: vec![]},
//!             ]},
//!             Tree { value: 8, children: vec![]},
//!         ]},
//!     ]};
//!
//! assert_eq!(vec![1, 5, 8, 6, 7, 2, 4, 3], dfs(&tree).cloned().collect::<Vec<_>>());
//! ```
//!
//! Explicit lifetime specifications are a bit clunky, but that's the price of generality.
//! Unfortunately, the compiler cannot infer them for us at the moment.
//!
//! # Pretty-printing
//!
//! `TreeNode` is [`Pretty`][pretty] so it can be pretty-printed. The format is similar to Clang:
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
//! [pretty]: ../pretty.html

use std::fmt;

use pretty::Pretty;

/// Trait of tree nodes.
pub trait TreeNode<'a> where Self: 'a {
    /// Value of this node.
    type Value;

    /// Iterator over child nodes.
    type ChildIter: Iterator<Item=&'a Self>;

    /// Returns value of this node.
    fn value(&'a self) -> &'a Self::Value;

    /// Returns iterator over child nodes of this node.
    fn children(&'a self) -> Self::ChildIter;
}

impl<'a, T> Pretty<'a> for T where T: TreeNode<'a> {
    type Element = T::Value;

    fn write_with<F>(&'a self, buf: &mut fmt::Write, write: F) -> fmt::Result
        where F: Fn(&Self::Element, &mut fmt::Write) -> fmt::Result
    {
        write_with_prefix(self, buf, &write, "")
    }
}

/// Write a given tree into the provided sink while formatting node values using the given
/// formatter and prefixing each line with the given prefix.
fn write_with_prefix<'a, T, F>
    (root: &'a T, buf: &mut fmt::Write, write: &F, prefix: &str) -> fmt::Result
    where T: TreeNode<'a>,
          F: Fn(&T::Value, &mut fmt::Write) -> fmt::Result
{
    // Render the root node, writing its first line as is and prefixing the extra ones
    // with the given prefix.
    let mut line_buffer = String::new();
    try!(write(root.value(), &mut line_buffer));

    let mut lines = line_buffer.lines();
    if let Some(current) = lines.next() {
        try!(buf.write_str(current));
        try!(buf.write_str("\n"));
        while let Some(current) = lines.next() {
            try!(buf.write_str(prefix));
            try!(buf.write_str(current));
            try!(buf.write_str("\n"));
        }
    }

    // Render the child nodes, prefixing them with branching indicators. The last node is a bit
    // special.
    let mut children = root.children();
    if let Some(mut current) = children.next() {
        while let Some(next) = children.next() {
            try!(buf.write_str(prefix));
            try!(buf.write_str("|- "));
            try!(write_with_prefix(current, buf, write, &format!("{}|  ", prefix)));
            current = next;
        }
        try!(buf.write_str(prefix));
        try!(buf.write_str("`- "));
        try!(write_with_prefix(current, buf, write, &format!("{}   ", prefix)));
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::slice;
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

    impl<'a, T> TreeNode<'a> for Tree<T> where T: 'a {
        type Value = T;
        type ChildIter = slice::Iter<'a, Tree<T>>;

        fn value(&'a self) -> &'a Self::Value {
            &self.value
        }

        fn children(&'a self) -> Self::ChildIter {
            self.children.iter()
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

    #[test]
    fn pretty_nested_tree() {
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

        assert_eq!(tree.format(), "\
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
   `- 13\n");
    }

    #[test]
    fn pretty_multiline_nodes() {
        let tree =
            Tree::new("Node\na", vec![
                Tree::new("Node\nb\nb\n\nb", vec![]),
                Tree::new("Node\nc", vec![
                    Tree::new("Node\r\nd", vec![]),
                ]),
                Tree::new("Node\ne", vec![]),
            ]);

        assert_eq!(tree.format(), "\
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
   e\n");
    }

    #[test]
    fn pretty_custom_node_value_format() {
        let tree =
            Tree::new(1, vec![
                Tree::new(2, vec![]),
                Tree::new(3, vec![])
            ]);

        assert_eq!(tree.format_with(|value| format!("<{}>", value)), "\
<1>
|- <2>
`- <3>\n");
    }
}
