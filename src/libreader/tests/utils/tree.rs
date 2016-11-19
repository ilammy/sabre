// Copyright (c) 2016, Sabre developers
//
// Licensed under the Apache License, Version 2.0 (see LICENSE.Apache in the
// root directory) or MIT license (see LICENSE.MIT in the root directory),
// at your option. This file may be copied, distributed, and modified only
// in accordance with the terms specified by the chosen license.

//! Tree utilities.

/// Trait of tree nodes.
pub trait TreeNode {
    /// Returns references to child nodes of this node.
    fn children<'a>(&'a self) -> Vec<&'a Self>;
}
