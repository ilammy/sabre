// Copyright (c) 2016, ilammy
//
// Licensed under MIT license (see LICENSE file in the root directory).
// This file may be copied, distributed, and modified only in accordance
// with the terms specified by this license.

//! Diffing sequences.

/// Result of sequence element comparison.
#[derive(Debug, PartialEq)]
pub enum Diff<'a, T> where T: 'a {
    /// The element is present only in the left sequence.
    Left(&'a T),

    /// The element is present only in the right sequence.
    Right(&'a T),

    /// Matching elements in the left and right sequences are equal.
    Equal(&'a T, &'a T),

    /// Matching elements in the left and right sequences are not equal.
    Replace(&'a T, &'a T),
}

/// Compute the difference between two sequences of comparable elements.
pub fn diff<'a, T: Eq>(lhs: &'a [T], rhs: &'a [T]) -> Vec<Diff<'a, T>> {
    diff_with(lhs, rhs, &|lhs, rhs| lhs == rhs)
}

// Here we use the classical approach to computing diffs. First we find the longest common
// subsequences of all sequence prefixes using dynamic programming. Then we derive the diff
// from this data. The algorithm has O(n^2) complexity by its nature, and element comparisons
// may be expensive, so we take care to reduce the amount of work to be done.

/// Compute the difference between two sequences using the provided comparator.
pub fn diff_with<'a, T>(lhs: &'a[T], rhs: &'a[T], equal: &Fn(&T, &T) -> bool)
    -> Vec<Diff<'a, T>>
{
    let fwd = lhs.iter().zip(rhs.iter());
    let rev = lhs.iter().rev().zip(rhs.iter().rev());

    // It is common for sequences being diffed to have equal prefixes and/or suffixes. We use
    // an O(n^2) algorithm to actually compute the diff, so we would like to minimize the size
    // of input data. One way to do this is to strip the equal prefix and suffix from sequences.

    let prefix = fwd.take_while(|&(lhs, rhs)| equal(lhs, rhs))
                    .map(|(lhs, rhs)| Diff::Equal(lhs, rhs))
                    .collect::<Vec<_>>();

    // We may even get lucky and find out that the sequences are in fact equal.
    if (lhs.len() == rhs.len()) && (prefix.len() == lhs.len()) {
        return prefix;
    }

    let suffix = rev.take_while(|&(lhs, rhs)| equal(lhs, rhs))
                    .map(|(lhs, rhs)| Diff::Equal(lhs, rhs))
                    .collect::<Vec<_>>();

    // Otherwise, we now have the prefix and the (reversed) suffix. Compute the actual diff for
    // the sequence slices that are different.

    let lhs = &lhs[prefix.len()..(lhs.len() - suffix.len())];
    let rhs = &rhs[prefix.len()..(rhs.len() - suffix.len())];

    let diff = compute_diff(lhs, rhs, equal);

    // Finally, compute the resulting full diff by concatenating prefix, diff, and suffix.
    // Note that the diff and suffix are stored in reverse order so we must reverse them back.
    // Also not that drain() requires mutable references so we redeclare all stuff as mutable.

    let mut prefix = prefix;
    let mut suffix = suffix;
    let mut diff   = diff;

    return prefix.drain(..)
                 .chain(diff.drain(..).rev())
                 .chain(suffix.drain(..).rev())
                 .collect();
}

/// Actually compute the diff between the sequences using the provided comparator.
fn compute_diff<'a, T>(lhs: &'a[T], rhs: &'a[T], equal: &Fn(&T, &T) -> bool)
    -> Vec<Diff<'a, T>>
{
    let (lcs, eqv) = compute_lcs_lookup(lhs, rhs, equal);

    return backtrace_diff(lhs, rhs, &lcs, &eqv);
}

/// Compute the longest common subsequence lookup matrix.
///
/// Returns an `(lhs.len() + 1) * (rhs.len() + 1)` matrix C where C[i, j] contains
/// the length of the longest common subsequence of `lhs[0..i]` and `rhs[0..j]`.
///
/// Also returns the matrix E of the same size which contains results of performed
/// element comparisons. The first row and column are filled with `false`.
///
/// The matrices are represented with just Vecs. You have to compute offsets manually.
/// Thanks for having built-in array support, Rust!
fn compute_lcs_lookup<T>(lhs: &[T], rhs: &[T], equal: &Fn(&T, &T) -> bool)
    -> (Vec<usize>, Vec<bool>)
{
    use std::cmp::max;

    let h = lhs.len() + 1;
    let w = rhs.len() + 1;

    let mut c = Vec::with_capacity(w * h);
    let mut e = Vec::with_capacity(w * h);

    c.resize(w * h, 0);
    e.resize(w * h, false);

    for i in 0..lhs.len() {
        for j in 0..rhs.len() {
            if equal(&lhs[i], &rhs[j]) {
                e[w * (i + 1) + (j + 1)] = true;
                c[w * (i + 1) + (j + 1)] = c[w * i + j] + 1;
            } else {
                e[w * (i + 1) + (j + 1)] = false;
                c[w * (i + 1) + (j + 1)] = max(c[w * (i + 1) + j], c[w * i + (j + 1)]);
            }
        }
    }

    return (c, e);
}

/// Backtrace the longest common subsequence lookup matrix, computing a diff.
///
/// This is rather convoluted backtracking. The idea behind it is that every path in the LCS
/// matrix from the top-left corner to the bottom-right corner corresponds to a certain diff.
/// We are interested in the shortest diff. We can use a greedy algorithm to find the shortest
/// path by exploiting the fact that the length of the longest common subsequence does not
/// increase when elements are removed from the end of the sequences. This allows us to trace
/// the optimal path in reverse order.
///
/// Refer to any book on dynamic programming for a better explanation of how this works.
fn backtrace_diff<'a, T>(lhs: &'a [T], rhs: &'a [T],
                         lcs: &[usize], eqv: &[bool]) -> Vec<Diff<'a, T>>
{
    let h = lhs.len() + 1;
    let w = rhs.len() + 1;

    assert_eq!(lcs.len(), w * h);
    assert_eq!(eqv.len(), w * h);

    let mut diff = Vec::with_capacity(lhs.len() + rhs.len());

    let mut i = lhs.len();
    let mut j = rhs.len();

    while (i != 0) || (j != 0) {
        if (i > 0) && (j > 0) {
            let len_a = lcs[w * (i - 1) + j];
            let len_b = lcs[w * i + (j - 1)];

            if len_a == len_b {
                if eqv[w * i + j] {
                    diff.push(Diff::Equal(&lhs[i - 1], &rhs[j - 1]));
                } else {
                    diff.push(Diff::Replace(&lhs[i - 1], &rhs[j - 1]));
                }
                i -= 1;
                j -= 1;
            } else if len_a < len_b {
                diff.push(Diff::Right(&rhs[j - 1]));
                j -= 1;
            } else {
                diff.push(Diff::Left(&lhs[i - 1]));
                i -= 1;
            }
        } else if i == 0 {
            diff.push(Diff::Right(&rhs[j - 1]));
            j -= 1;
        } else if j == 0 {
            diff.push(Diff::Left(&lhs[i - 1]));
            i -= 1;
        }
    }

    return diff;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty() {
        let a: Vec<i32> = vec![];
        let b: Vec<i32> = vec![];

        let diff = diff(&a, &b);

        assert!(diff.is_empty());
    }

    #[test]
    fn partially_empty_1() {
        let a: Vec<i32> = vec![1, 2, 3];
        let b: Vec<i32> = vec![];

        let diff = diff(&a, &b);

        assert_eq!(diff, vec![
            Diff::Left (&a[0]), // -1
            Diff::Left (&a[1]), // -2
            Diff::Left (&a[2]), // -3
        ]);
    }

    #[test]
    fn partially_empty_2() {
        let a: Vec<i32> = vec![];
        let b: Vec<i32> = vec![1, 2, 3];

        let diff = diff(&a, &b);

        assert_eq!(diff, vec![
            Diff::Right (&b[0]), // +1
            Diff::Right (&b[1]), // +2
            Diff::Right (&b[2]), // +3
        ]);
    }

    #[test]
    fn equal() {
        let a = vec![1, 2, 3];
        let b = vec![1, 2, 3];

        let diff = diff(&a, &b);

        assert_eq!(diff, vec![
            Diff::Equal (&a[0], &b[0]), // 1
            Diff::Equal (&a[1], &b[1]), // 2
            Diff::Equal (&a[2], &b[2]), // 3
        ]);
    }

    #[test]
    fn different_1() {
        let a = vec![1, 2,    3, 4, 5, 6, 7, 8, 2   ];
        let b = vec![8, 2, 4, 3, 1, 1, 6, 7, 5, 2, 3];
        //           |     +     |  |        |     +

        let diff = diff(&a, &b);

        assert_eq!(diff, vec![
            Diff::Replace (&a[0], &b[0]),  // -1  +8
            Diff::Equal   (&a[1], &b[1]),  //  2   2
            Diff::Right   (       &b[2]),  //     +4
            Diff::Equal   (&a[2], &b[3]),  //  3   3
            Diff::Replace (&a[3], &b[4]),  // -4  +1
            Diff::Replace (&a[4], &b[5]),  // -5  +1
            Diff::Equal   (&a[5], &b[6]),  //  6   6
            Diff::Equal   (&a[6], &b[7]),  //  7   7
            Diff::Replace (&a[7], &b[8]),  // -8  +5
            Diff::Equal   (&a[8], &b[9]),  //  2   2
            Diff::Right   (       &b[10]), //     +3
        ]);
    }

    #[test]
    fn different_2() {
        let a = vec!['O','h','a','i',',',' ',    'I',' ','a',    'm',' ','B','o','b'    ];
        let b = vec![        'H','i',',',' ','w','e',' ','a','r','e',' ','B','o','b','!'];
        //            -   -   |               +   |           +   |                   +

        let diff = diff(&a, &b);

        assert_eq!(diff, vec![
            Diff::Left    (&a[0]         ), // -O
            Diff::Left    (&a[1]         ), // -h
            Diff::Replace (&a[2],  &b[0] ), // -a  +H
            Diff::Equal   (&a[3],  &b[1] ), //  i   i
            Diff::Equal   (&a[4],  &b[2] ), //  ,   ,
            Diff::Equal   (&a[5],  &b[3] ), //
            Diff::Right   (        &b[4] ), //     +w
            Diff::Replace (&a[6],  &b[5] ), // -I  +e
            Diff::Equal   (&a[7],  &b[6] ), //
            Diff::Equal   (&a[8],  &b[7] ), //  a   a
            Diff::Right   (        &b[8] ), //     +r
            Diff::Replace (&a[9],  &b[9] ), // -m  +e
            Diff::Equal   (&a[10], &b[10]), //
            Diff::Equal   (&a[11], &b[11]), //  B   B
            Diff::Equal   (&a[12], &b[12]), //  o   o
            Diff::Equal   (&a[13], &b[13]), //  b   b
            Diff::Right   (        &b[14]), //     +!
        ]);
    }

    #[test]
    fn different_3() {
        let a = vec!["foo", "bar", "baz"];
        let b = vec!["foo", "BAR", "baz"];
        //                    |

        let diff = diff(&a, &b);

        assert_eq!(diff, vec![
            Diff::Equal   (&a[0],  &b[0] ), //  "foo"  "foo"
            Diff::Replace (&a[1],  &b[1] ), // -"bar" +"BAR"
            Diff::Equal   (&a[2],  &b[2] ), //  "baz"  "baz"
        ]);
    }

    #[test]
    fn custom_comparator() {
        let a: Vec<i32> = vec![0, -1,  2, -3    ];
        let b: Vec<i32> = vec![    1, -2,  3, -4];
        //                     -               +

        let diff = diff_with(&a, &b, &|a, b| a.abs() == b.abs());

        assert_eq!(diff, vec![
            Diff::Left  (&a[0]       ), // -[ 0]
            Diff::Equal (&a[1], &b[0]), //  [-1]  [ 1]
            Diff::Equal (&a[2], &b[1]), //  [ 2]  [-2]
            Diff::Equal (&a[3], &b[2]), //  [-3]  [ 3]
            Diff::Right (       &b[3]), //       +[-4]
        ]);
    }
}
