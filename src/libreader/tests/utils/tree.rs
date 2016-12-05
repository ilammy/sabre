// Copyright (c) 2016, Sabre developers
//
// Licensed under the Apache License, Version 2.0 (see LICENSE.Apache in the
// root directory) or MIT license (see LICENSE.MIT in the root directory),
// at your option. This file may be copied, distributed, and modified only
// in accordance with the terms specified by the chosen license.

//! Tree utilities.
//!
//! The core definition here is the [`TreeNode`](trait.TreeNode.html) trait which describes
//! recursive tree-like structures. It is somewhat similar to the standard `IntoIterator`
//! trait in that it _consumes_ `Self`, thus you would probably like to implement it mostly
//! for _references_ to your types. The reason for this is that such approach enables easy
//! implementation of wrapper types in the same manner as various `Iterators` are wrapped
//! by its methods.
//!
//! # Example usage
//!
//! Suppose you have the following `Tree` type which is obviously tree-like:
//!
//! ```
//! struct Tree<T> {
//!     value: T,
//!     children: Vec<Tree<T>>,
//! }
//! ```
//!
//! Then you can straightforwardly decribe references to its nodes in the following way:
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
//! /// Implementing TreeNode for _a reference_ to Tree<T>.
//! impl<'a, T> TreeNode for &'a Tree<T> {
//!     /// `Self` is a _node_ of a tree while `Self::Value` is the actual payload
//!     /// that gets acted upon during tree traversal.
//!     ///
//!     /// In our case this is a reference to the `value` field of `Tree<T>`.
//!     type Value = &'a T;
//!
//!     /// `Self::ChildIter` is the type of the iterator over the node's chilren.
//!     /// It should be an `Iterator<Item=Self>`.
//!     ///
//!     /// In our case this is the type of an iterator over the `children` field
//!     /// of `Tree<T>`.
//!     type ChildIter = slice::Iter<'a, Tree<T>>;
//!
//!     /// Describe how to get the node's value.
//!     fn value(self) -> Self::Value {
//!         &self.value
//!     }
//!
//!     /// Describe how to iterate over the child nodes of this node.
//!     fn children(self) -> Self::ChildIter {
//!         self.children.iter()
//!     }
//! }
//! ```
//!
//! And here is how a generic tree traversal may look like:
//!
//! ```
//! # struct Tree<T> {
//! #     value: T,
//! #     children: Vec<Tree<T>>,
//! # }
//! #
//! # use std::slice;
//! # use utils::tree::TreeNode;
//! #
//! # impl<'a, T> TreeNode for &'a Tree<T> {
//! #     type Value = &'a T;
//! #     type ChildIter = slice::Iter<'a, Tree<T>>;
//! #     fn value(self) -> Self::Value { &self.value }
//! #     fn children(self) -> Self::ChildIter { self.children.iter() }
//! # }
//! #
//! struct DfsIterator<'a, T> where T: 'a, &'a T: TreeNode {
//!     unseen: Vec<&'a T>,
//! }
//!
//! fn dfs<'a, T>(root: &'a T) -> DfsIterator<'a, T>
//!     where &'a T: TreeNode
//! {
//!     DfsIterator {
//!         unseen: vec![root],
//!     }
//! }
//!
//! impl<'a, Node> Iterator for DfsIterator<'a, Node> where &'a Node: TreeNode {
//!     type Item = <&'a Node as TreeNode>::Value;
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
//! Explicit lifetime specifications and trait casts are a bit clunky, but that's the price
//! of generality. Unfortunately, the compiler cannot infer these for us at the moment.
//!
//! # Pretty-printing
//!
//! TODO

/// Trait of tree nodes.
///
/// See [module documenetation](index.html) for more details.
pub trait TreeNode where Self: Sized {
    /// Value of this node.
    type Value;

    /// Iterator over child nodes.
    type ChildIter: Iterator<Item=Self>;

    /// Returns the value of this node.
    fn value(self) -> Self::Value;

    /// Returns an iterator over child nodes of this node.
    fn children(self) -> Self::ChildIter;
}

use std::fmt;

use pretty::ClangStyleTree;

impl<'a, T> fmt::Display for ClangStyleTree<'a, T>
    where &'a T: TreeNode, <&'a T as TreeNode>::Value: fmt::Display
{
    fn fmt(&self, buf: &mut fmt::Formatter) -> fmt::Result
    {
        fn do_fmt<'a, T>(root: &'a T, buf: &mut fmt::Formatter, prefix: &str) -> fmt::Result
            where &'a T: TreeNode, <&'a T as TreeNode>::Value: fmt::Display
        {
            // Render the root node, writing its first line as is and prefixing the extra ones
            // with the given prefix.
            let line_buffer = format!("{}", root.value());

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

            // Render the child nodes, prefixing them with branching indicators.
            // The last node is a bit special.
            let mut children = root.children();
            if let Some(mut current) = children.next() {
                while let Some(next) = children.next() {
                    try!(buf.write_str(prefix));
                    try!(buf.write_str("|- "));
                    try!(do_fmt(current, buf, &format!("{}|  ", prefix)));
                    current = next;
                }
                try!(buf.write_str(prefix));
                try!(buf.write_str("`- "));
                try!(do_fmt(current, buf, &format!("{}   ", prefix)));
            }

            Ok(())
        }

        do_fmt(self.root, buf, "")

        // TODO: try replacing recursion with an explicit stack and keeping a single string buffer
        //       for repackaging multiline strings
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::slice;
    use pretty::ClangStyleTree;

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

    impl<'a, T> TreeNode for &'a Tree<T> {
        type Value = &'a T;
        type ChildIter = slice::Iter<'a, Tree<T>>;

        fn value(self) -> Self::Value {
            &self.value
        }

        fn children(self) -> Self::ChildIter {
            self.children.iter()
        }
    }

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    // ClangStyleTree pretty-printing

    #[test]
    fn pretty_clang_lonely_root() {
        let tree = Tree::new(1, vec![]);

        assert_eq!(format!("{}", ClangStyleTree { root: &tree }), "\
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

        assert_eq!(format!("{}", ClangStyleTree { root: &tree }), "\
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

        assert_eq!(format!("{}", ClangStyleTree { root: &tree }), "\
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

        assert_eq!(format!("{}", ClangStyleTree { root: &tree }), "\
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
/*
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
*/
}
