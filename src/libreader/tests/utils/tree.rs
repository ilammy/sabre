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
//! use utils::tree::TreeNodeEx;
//!
//! /// Implementing TreeNode for _a reference_ to Tree<T>.
//! impl<'a, T> TreeNodeEx for &'a Tree<T> {
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
//! # use utils::tree::TreeNodeEx;
//! #
//! # impl<'a, T> TreeNodeEx for &'a Tree<T> {
//! #     type Value = &'a T;
//! #     type ChildIter = slice::Iter<'a, Tree<T>>;
//! #     fn value(self) -> Self::Value { &self.value }
//! #     fn children(self) -> Self::ChildIter { self.children.iter() }
//! # }
//! #
//! struct DfsIterator<'a, T> where T: 'a, &'a T: TreeNodeEx {
//!     unseen: Vec<&'a T>,
//! }
//!
//! fn dfs<'a, T>(root: &'a T) -> DfsIterator<'a, T>
//!     where &'a T: TreeNodeEx
//! {
//!     DfsIterator {
//!         unseen: vec![root],
//!     }
//! }
//!
//! impl<'a, Node> Iterator for DfsIterator<'a, Node> where &'a Node: TreeNodeEx {
//!     type Item = <&'a Node as TreeNodeEx>::Value;
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

/// Trait of tree nodes.
pub trait TreeNodeEx where Self: Sized {
    /// Value of this node.
    type Value;

    /// Iterator over child nodes.
    type ChildIter: Iterator<Item=Self>;

    /// Returns the value of this node.
    fn value(self) -> Self::Value;

    /// Returns an iterator over child nodes of this node.
    fn children(self) -> Self::ChildIter;
}
