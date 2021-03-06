// Copyright (c) 2017, Sabre developers
//
// Licensed under the Apache License, Version 2.0 (see LICENSE.Apache in the
// root directory) or MIT license (see LICENSE.MIT in the root directory),
// at your option. This file may be copied, distributed, and modified only
// in accordance with the terms specified by the chosen license.

//! String intern pool.
//!
//! Interned strings (aka _atoms_ or _quarks_) provide an optimized way of handling lots of
//! copies of equivalent strings which all have almost the same lifetime. This is a typical
//! situation in parsers which have to deal with lots of strings (like identifier names and
//! other tokens), pass them around, and compare them for equality.
//!
//! The strings are represented as integer indices (atoms) while their actual values are kept
//! in the interner table. Thus string comparison is reduced to a more simple integer comparison.
//! Also, atoms may be freely copied around as well as do not have lifetimes associated with them
//! so there is little syntactic and runtime overhead in handling interned strings.
//!
//! The only downside of this approach is that the atoms are not tied in any way to the interner
//! which produced them. Thus it is possible to have dangling atoms, and to use a wrong interner
//! to get the string value associated with the atom. However, this is not a problem in practice
//! because usually there is only one intern pool in scope.

use std::cell::RefCell;
use std::collections::HashMap;

pub use self::atom::{with_formatting_pool, Atom};
pub use self::interned_string::InternedString;

mod atom;
mod interned_string;

/// A string intern pool.
///
/// This is the pool that keeps references to all interned strings and provides bidirectional
/// lookup of strings by atoms and atoms by strings.
pub struct InternPool {
    /// A map keeping string -> atom associations, guaranteeing their uniqueness.
    pool: RefCell<HashMap<InternedString, Atom>>,

    /// A cache vector providing fast atom -> string lookup and generation of new atoms.
    backrefs: RefCell<Vec<InternedString>>,
}

impl InternPool {
    /// Create a new empty string intern pool.
    #[allow(clippy::new_without_default)]
    pub fn new() -> InternPool {
        InternPool {
            pool: RefCell::new(HashMap::new()),
            backrefs: RefCell::new(Vec::new()),
        }
    }

    /// Return the atom corresponding to a given string. If the string is not yet in the pool
    /// then it is interned and its brand new atom is returned.
    pub fn intern(&self, s: &str) -> Atom {
        if let Some(atom) = self.have_interned(s) {
            return atom;
        }
        self.insert(InternedString::new(s))
    }

    /// Intern the given string into the pool and returns its atom. This method can be used to
    /// avoid a string copy made by `intern()` if you can transfer ownership of the string.
    pub fn intern_string(&self, s: String) -> Atom {
        if let Some(atom) = self.have_interned(&s) {
            return atom;
        }
        self.insert(InternedString::from_string(s))
    }

    /// Check whether `s` has been already interned, returning the corresponding atom.
    fn have_interned(&self, s: &str) -> Option<Atom> {
        self.pool.borrow().get(s).cloned()
    }

    /// Insert an interned string into the pool and return the corresponding atom.
    fn insert(&self, interned: InternedString) -> Atom {
        let mut pool = self.pool.borrow_mut();
        let mut backrefs = self.backrefs.borrow_mut();

        let new_atom = Atom(backrefs.len() as u32);

        backrefs.push(interned.clone());
        pool.insert(interned, new_atom);

        new_atom
    }

    /// Retrieve the string value associated with a given atom.
    ///
    /// # Panics
    ///
    /// Panics if the atom is not present in this pool.
    pub fn get(&self, atom: Atom) -> InternedString {
        let backrefs = self.backrefs.borrow();
        backrefs[atom.0 as usize].clone()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn intern_pool_sequential_atoms() {
        let p = InternPool::new();

        assert_eq!(p.intern("foo"),                         Atom(0));
        assert_eq!(p.intern("bar"),                         Atom(1));
        assert_eq!(p.intern_string("foo".to_string()),      Atom(0));
        assert_eq!(p.intern_string("example".to_string()),  Atom(2));
        assert_eq!(p.intern("bar"),                         Atom(1));
        assert_eq!(p.intern_string("foo".to_string()),      Atom(0));
    }

    #[test]
    fn intern_pool_getter() {
        let p = InternPool::new();

        let atom_123 = p.intern("123");
        let atom_foo = p.intern("foo");
        let atom_bar = p.intern("bar");
        let atom_empty = p.intern("");

        assert_eq!(&p.get(atom_123)[..], "123");
        assert_eq!(&p.get(atom_foo)[..], "foo");
        assert_eq!(&p.get(atom_bar)[..], "bar");
        assert_eq!(&p.get(atom_empty)[..], "");
    }

    #[test]
    #[should_panic]
    fn intern_pool_invalid_atom() {
        let p = InternPool::new();
        p.get(Atom(9));
    }
}
