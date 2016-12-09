// Copyright (c) 2016, Sabre developers
//
// Licensed under the Apache License, Version 2.0 (see LICENSE.Apache in the
// root directory) or MIT license (see LICENSE.MIT in the root directory),
// at your option. This file may be copied, distributed, and modified only
// in accordance with the terms specified by the chosen license.

//! Pretty-printing trees.

use std::cell::RefCell;
use std::fmt;

/// Displayable wrapper over `Iterator<Item=Dfs<T>>` for displayable types.
#[doc(hidden)]
pub struct ClangTreeDisplay<I> {
    iter: RefCell<I>,
}

/// Displayable wrapper over `Iterator<Item=Dfs<T>>` with custom formatter.
#[doc(hidden)]
pub struct ClangTree<I, F> {
    iter: RefCell<I>,
    f: F,
}

/// TODO
pub struct Dfs<T> {
    pub depth: u16, // TODO: parameterize this ?
    pub value: T,
}

pub trait IntoDfsIterator {
    type Item;
    type IntoIter: Iterator<Item=Dfs<Self::Item>>;

    fn into_dfs_iter(self) -> Self::IntoIter;
}

///
pub fn clang_tree<T, V>(tree: T) -> ClangTreeDisplay<T::IntoIter>
    where T: IntoDfsIterator<Item=V>, V: fmt::Display
{
    ClangTreeDisplay { iter: RefCell::new(tree.into_dfs_iter()) }
}

///
pub fn clang_tree_format<T, V, F>(tree: T, f: F) -> ClangTree<T::IntoIter, F>
    where T: IntoDfsIterator<Item=V>, F: Fn(V, &mut fmt::Write) -> fmt::Result
{
    ClangTree { iter: RefCell::new(tree.into_dfs_iter()), f: f }
}

use std::ops::DerefMut;

impl<I, T> fmt::Display for ClangTreeDisplay<I>
    where I: Iterator<Item=Dfs<T>>, T: fmt::Display
{
    fn fmt(&self, buf: &mut fmt::Formatter) -> fmt::Result {
        let mut iter = self.iter.borrow_mut();
        write_clang_tree(buf, iter.deref_mut(), &|v, buf| write!(buf, "{}", v))
    }
}

impl<I, T, F> fmt::Display for ClangTree<I, F>
    where I: Iterator<Item=Dfs<T>>, F: Fn(T, &mut fmt::Write) -> fmt::Result
{
    fn fmt(&self, buf: &mut fmt::Formatter) -> fmt::Result {
        let mut iter = self.iter.borrow_mut();
        write_clang_tree(buf, iter.deref_mut(), &self.f)
    }
}

///
fn write_clang_tree<I, T, F>(buf: &mut fmt::Write, iter: &mut I, format: &F) -> fmt::Result
    where I: Iterator<Item=Dfs<T>>, F: Fn(T, &mut fmt::Write) -> fmt::Result
{
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

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

    struct TreeDfsIter<'a, T> where T: 'a {
        not_seen: Vec<&'a Tree<T>>,
    }

    impl<'a, T> IntoDfsIterator for &'a Tree<T> {
        type Item = &'a T;
        type IntoIter = TreeDfsIter<'a, T>;

        fn into_dfs_iter(self) -> Self::IntoIter {
            TreeDfsIter { not_seen: vec![self] }
        }
    }

    impl<'a, T> Iterator for TreeDfsIter<'a, T> where T: 'a {
        type Item = Dfs<&'a T>;

        fn next(&mut self) -> Option<Self::Item> {
            if let Some(next) = self.not_seen.pop() {
                let depth = self.not_seen.len() as u16;
                self.not_seen.extend(next.children.iter().rev());
                Some(Dfs { depth: depth, value: &next.value })
            } else {
                None
            }
        }
    }

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    // Simple formatting

    #[test]
    fn only_root_element() {
        let tree = Tree::new(1, vec![]);
        let expected = "1\n";

        assert_eq!(expected, format!("{}", clang_tree(&tree)));
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
`- 4
";
        assert_eq!(expected, format!("{}", clang_tree(&tree)));
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
   `- 13
";
        assert_eq!(expected, format!("{}", clang_tree(&tree)));
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
   e
";
        assert_eq!(expected, format!("{}", clang_tree(&tree)));
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
`- <3>
";
        assert_eq!(expected,
            format!("{}", clang_tree_format(&tree,
                |value, buf| write!(buf, "<{}>", value))));
    }
}
