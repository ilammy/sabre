// Copyright (c) 2016, Sabre developers
//
// Licensed under the Apache License, Version 2.0 (see LICENSE.Apache in the
// root directory) or MIT license (see LICENSE.MIT in the root directory),
// at your option. This file may be copied, distributed, and modified only
// in accordance with the terms specified by the chosen license.

//! Tree utilities.
//!
//! The core definition here is the [`TreeNode`](trait.TreeNode.html) trait which describes
//! recursive tree-like structures. It is somewhat similar to the standard `Iterator` trait
//! in that it actually represents an iteration state. However, iterators describe sequential
//! structures while trees have hierarchy. There is no a single right way to traverse a tree,
//! so `TreeNode` does not provide one.
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
//! /// We are implementing `TreeNode` for _a reference_ to `Tree<T>` because
//! /// the reference _is_ a representation of traversal state.
//! impl<'a, T> TreeNodeEx for &'a Tree<T> {
//!     /// `Self::Value` is the payload type of the node referenced by `Self`.
//!     /// This is the return type of the `value()` method.
//!     ///
//!     /// In our case this is simply the reference to the `value` field, but
//!     /// it can be effectively anything. In fact, the value may have nothing
//!     /// to do with the traversed tree at all.
//!     type Value = &'a T;
//!
//!     /// `Self::ChildIter` is the type of an iterator over the node's children.
//!     /// Trees are homogenous, so this has to be an `Iterator<Item=Self>`.
//!     /// This is the return type of the `chilren()` method.
//!     ///
//!     /// Again, in our case this is an iterator over the slice owned by the
//!     /// `children` vector field, but it can by any iterator type.
//!     type ChildIter = slice::Iter<'a, Tree<T>>;
//!
//!     /// Describe how to get the value of this node.
//!     fn value(&self) -> Self::Value {
//!         &self.value
//!     }
//!
//!     /// Describe how to iterate over children of this node.
//!     fn children(&self) -> Self::ChildIter {
//!         self.children.iter()
//!     }
//! }
//! ```
//!
//! And here is how a generic tree traversal may look like:
//!
//! ```
//! # use std::slice;
//! # use utils::tree::TreeNodeEx;
//! #
//! # struct Tree<T> {
//! #     value: T,
//! #     children: Vec<Tree<T>>,
//! # }
//! #
//! # impl<'a, T> TreeNodeEx for &'a Tree<T> {
//! #     type Value = &'a T;
//! #     type ChildIter = slice::Iter<'a, Tree<T>>;
//! #     fn value(&self) -> Self::Value { &self.value }
//! #     fn children(&self) -> Self::ChildIter { self.children.iter() }
//! # }
//! #
//! struct DfsIterator<Node> {
//!     unseen: Vec<Node>,
//! }
//!
//! fn dfs<Node>(root: Node) -> DfsIterator<Node> {
//!     DfsIterator {
//!         unseen: vec![root],
//!     }
//! }
//!
//! impl<Node: TreeNodeEx> Iterator for DfsIterator<Node> {
//!     type Item = Node::Value;
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

/// Trait of tree nodes.
///
/// See [module documenetation](index.html) for more details.
pub trait TreeNodeEx where Self: Sized {
    /// Value of this node.
    type Value;

    /// Iterator over child nodes.
    type ChildIter: Iterator<Item=Self>;

    /// Returns value of this node.
    fn value(&self) -> Self::Value;

    /// Returns an iterator over child nodes of this node.
    fn children(&self) -> Self::ChildIter;
}
