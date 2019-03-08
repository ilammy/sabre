// Copyright (c) 2017, Sabre developers
//
// Licensed under the Apache License, Version 2.0 (see LICENSE.Apache in the
// root directory) or MIT license (see LICENSE.MIT in the root directory),
// at your option. This file may be copied, distributed, and modified only
// in accordance with the terms specified by the chosen license.

use std::cell::UnsafeCell;
use std::fmt;

use crate::intern_pool::InternPool;

/// An interned string representation.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub struct Atom(pub(super) u32);

impl fmt::Debug for Atom {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        get_formatting_pool(|p|
            match p {
                Some(pool) => write!(f, "{}", pool.get(*self)),
                None       => write!(f, "Atom({})", self.0),
            }
        )
    }
}

// I spent a few days trying to generalize this pattern (dynamically-scoped variables),
// but my Rust skills are still too low for this, so we handle only a specific case now.
// This should expand into a single thread-local pointer without any additonal overhead.

type DynamicCell<T> = UnsafeCell<Option<*const T>>;

thread_local! {
    static FORMATTING_POOL: DynamicCell<InternPool> = DynamicCell::new(None);
}

/// Get a reference to current intern pool used for atom formatting (if any).
///
/// The reference is valid only during the closure call. The reference is None if the outer
/// dynamic scope does not contain any `with_formatting_pool()` invocations. Otherwise the
/// Some value set by the (dynamically) closest invocation will be returned.
fn get_formatting_pool<R, F: FnOnce(Option<&InternPool>) -> R>(f: F) -> R {
    FORMATTING_POOL.with(|current_pool| {
        f(get_dynamic_cell(current_pool))
    })
}

/// Set a current intern pool for Atom formatting.
///
/// The specified pool will be used for `Debug` (`{:?}`) formatting of Atom instances performed
/// during the dynamic extent (execution time) of the provided closure.
///
/// If no formatting pool is set then atoms will be formatted like `Atom(42)`. If a pool is set
/// then atoms will be formatted as corresponding strings from the pool with `Display` format.
///
/// Please note that if you use a wrong pool (i.e., not the one previously used to obtain the
/// atoms) then formatting will panic.
pub fn with_formatting_pool<R, F: FnOnce() -> R>(new_pool: &InternPool, f: F) -> R {
    FORMATTING_POOL.with(|current_pool| {
        let assignment = set_dynamic_cell(current_pool, new_pool);
        let result = f();
        drop(assignment);
        return result;
    })
}

fn get_dynamic_cell<'a, T>(cell: &'a DynamicCell<T>) -> Option<&'a T> {
    // It is safe to get a read-only reference to the cell content during the borrow duration
    // of the dynamic cell. The DynamicSetGuard ensures that the value is not dropped during
    // the dynamic extent of an assignment.
    let pointer = unsafe { &*cell.get() };
    // The pointers we store as Some are guaranteed to be valid for the borrow duration of
    // the dynamic cell which is established by the LocalKey::with() call.
    return pointer.map(|p| unsafe { &*p });
}

fn set_dynamic_cell<'a, T>(cell: &'a DynamicCell<T>, new_value: &'a T) -> DynamicSetGuard<'a, T> {
    DynamicSetGuard {
        // Dynamic variables are not shareable between threads. This means that if this function
        // is now active then other active function calls cannot access the current value in the
        // cell. We can now safely swap the current value with the new one (to be used by the
        // closure and its descendants) and then replace the old value back so that the outer
        // active calls continue to see the previous value when we return.
        old_value: std::mem::replace(unsafe { &mut *cell.get() }, Some(new_value)),
        cell: cell,
    }
}

struct DynamicSetGuard<'a, T> {
    old_value: Option<*const T>,
    cell: &'a DynamicCell<T>,
}

impl<'a, T> Drop for DynamicSetGuard<'a, T> {
    fn drop(&mut self) {
        // When this destructor is invoked the closure that can access the current cell value
        // is no longer active, so there are no users of that value. We can safely drop it and
        // replace the old value back now.
        std::mem::replace(unsafe { &mut *self.cell.get() }, self.old_value.take());
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::intern_pool::InternPool;

    #[test]
    fn debug_format_default() {
        let pool = InternPool::new();

        assert_eq!(format!("{:?}", pool.intern("foo")), "Atom(0)");
        assert_eq!(format!("{:?}", pool.intern("bar")), "Atom(1)");
        assert_eq!(format!("{:?}", pool.intern("baz")), "Atom(2)");
        assert_eq!(format!("{:?}", pool.intern("foo")), "Atom(0)");
    }

    #[test]
    fn debug_format_with_pool() {
        let pool = InternPool::new();

        with_formatting_pool(&pool, || {
            assert_eq!(format!("{:?}", pool.intern("foo")), "foo");
            assert_eq!(format!("{:?}", pool.intern("bar")), "bar");
            assert_eq!(format!("{:?}", pool.intern("baz")), "baz");
            assert_eq!(format!("{:?}", pool.intern("foo")), "foo");
        });
    }

    #[test]
    fn debug_format_with_nested_pools() {
        let pool1 = InternPool::new();
        let pool2 = InternPool::new();

        let atom1 = pool1.intern("foo");
        let atom2 = pool2.intern("bar");

        assert_eq!(format!("{:?}", atom1), "Atom(0)");
        assert_eq!(format!("{:?}", atom2), "Atom(0)");

        with_formatting_pool(&pool1, || {
            assert_eq!(format!("{:?}", atom1), "foo");
            assert_eq!(format!("{:?}", atom2), "foo");

            with_formatting_pool(&pool2, || {
                assert_eq!(format!("{:?}", atom1), "bar");
                assert_eq!(format!("{:?}", atom2), "bar");
            });

            assert_eq!(format!("{:?}", atom1), "foo");
            assert_eq!(format!("{:?}", atom2), "foo");
        });

        assert_eq!(format!("{:?}", atom1), "Atom(0)");
        assert_eq!(format!("{:?}", atom2), "Atom(0)");

        with_formatting_pool(&pool2, || {
            assert_eq!(format!("{:?}", atom1), "bar");
            assert_eq!(format!("{:?}", atom2), "bar");

            with_formatting_pool(&pool1, || {
                assert_eq!(format!("{:?}", atom1), "foo");
                assert_eq!(format!("{:?}", atom2), "foo");
            });

            assert_eq!(format!("{:?}", atom1), "bar");
            assert_eq!(format!("{:?}", atom2), "bar");
        });

        assert_eq!(format!("{:?}", atom1), "Atom(0)");
        assert_eq!(format!("{:?}", atom2), "Atom(0)");
    }

    #[test]
    #[should_panic(expected = "not-atom")]
    fn debug_format_with_pool_panic_friendly() {
        let pool = InternPool::new();
        let atom = pool.intern("atom");

        with_formatting_pool(&pool, || {
            assert_eq!(format!("{:?}", atom), "not-atom");
        });
    }
}
