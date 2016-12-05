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
//! ```x
//! struct Tree<T> {
//!     value: T,
//!     children: Vec<Tree<T>>,
//! }
//! ```
//!
//! Then you can straightforwardly decribe nodes of this tree in the following way:
//!
//! ```x
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
//! ```x
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
