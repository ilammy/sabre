// Copyright (c) 2016, ilammy
//
// Licensed under MIT license (see LICENSE file in the root directory).
// This file may be copied, distributed, and modified only in accordance
// with the terms specified by this license.

//! Tree utilities.

/// Trait of tree nodes.
pub trait TreeNode {
    /// Returns references to child nodes of this node.
    fn children<'a>(&'a self) -> Vec<&'a Self>;
}
