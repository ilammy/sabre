// Copyright (c) 2016, Sabre developers
//
// Licensed under the Apache License, Version 2.0 (see LICENSE.Apache in the
// root directory) or MIT license (see LICENSE.MIT in the root directory),
// at your option. This file may be copied, distributed, and modified only
// in accordance with the terms specified by the chosen license.

//! Unicode normalization algorithms.
//!
//! This module implements _Unicode Normalization Forms_ as defined by [Unicode Standard
//! Annex #15][UAX-15]. Unicode normalization is used to ensure that visually equivalent
//! strings have equivalent binary representations.
//!
//! [UAX-15]: http://www.unicode.org/reports/tr15/

use std::iter::{FromIterator, IntoIterator};

use crate::tables::{composition_mappings, decomposition_mappings};
use crate::util::charcc;

//
// Definitions of Normalization Forms
//

/// Normalize a string according to **Normalization Form D** (_D118_).
pub fn nfd(s: &str) -> String {
    if already_normalized(s, NormalizationForm::D) {
        return s.to_owned();
    }
    let v = canonical_decomposition(s.chars());
    cleaned_up_string(v)
}

/// Normalize a string according to **Normalization Form KD** (_D119_).
pub fn nfkd(s: &str) -> String {
    if already_normalized(s, NormalizationForm::KD) {
        return s.to_owned();
    }
    let v = compatibility_decomposition(s.chars());
    cleaned_up_string(v)
}

/// Normalize a string according to **Normalization Form C** (_D120_).
pub fn nfc(s: &str) -> String {
    if already_normalized(s, NormalizationForm::C) {
        return s.to_owned();
    }
    let mut v = canonical_decomposition(s.chars());
    compose_canonically(&mut v);
    cleaned_up_string(v)
}

/// Normalize a string according to **Normalization Form KC** (_D121_).
pub fn nfkc(s: &str) -> String {
    if already_normalized(s, NormalizationForm::KC) {
        return s.to_owned();
    }
    let mut v = compatibility_decomposition(s.chars());
    compose_canonically(&mut v);
    cleaned_up_string(v)
}

fn cleaned_up_string(normalized: Vec<charcc>) -> String {
    String::from_iter(normalized.iter().map(|cc| cc.to_char()))
}

//
// Quick Check
//

enum NormalizationForm { C, D, KC, KD }

/// Check whether a string is already in the specified normalization form.
fn already_normalized(s: &str, form: NormalizationForm) -> bool {
    use crate::tables::character_properties::canonical_combining_class as ccc;
    use crate::tables::quick_check;

    let mut last_ccc = 0;

    for c in s.chars() {
        // ASCII text is always normalized in any form.
        if c <= '\u{7F}' {
            last_ccc = 0;
            continue;
        }

        let this_ccc = ccc(c);

        // The string is not normalized if canonical ordering is not observed.
        if (last_ccc > this_ccc) && (this_ccc != 0) {
            return false;
        }

        // Finally check for explicit exceptions.
        let not_allowed = match form {
            NormalizationForm::C  => quick_check::not_allowed_in_nfc(c),
            NormalizationForm::D  => quick_check::not_allowed_in_nfd(c),
            NormalizationForm::KC => quick_check::not_allowed_in_nfkc(c),
            NormalizationForm::KD => quick_check::not_allowed_in_nfkd(c),
        };

        if not_allowed {
            return false;
        }

        last_ccc = this_ccc;
    }

    true
}

//
// Decomposition
//

/// Produce a Compatibility decomposition (D65) of a character sequence.
fn compatibility_decomposition<I>(chars: I) -> Vec<charcc>
    where I: IntoIterator<Item=char>
{
    let mut buffer = Vec::new();

    for c in chars {
        push_compatibility_decomposition(c, &mut buffer);
    }

    reorder_canonically(&mut buffer[..]);

    buffer
}

/// Produce a Canonical decomposition (D68) of a character sequence.
fn canonical_decomposition<I>(chars: I) -> Vec<charcc>
    where I: IntoIterator<Item=char>
{
    let mut buffer = Vec::new();

    for c in chars {
        push_canonical_decomposition(c, &mut buffer);
    }

    reorder_canonically(&mut buffer[..]);

    buffer
}

/// Push a Compatibility decomposition (D65) of a single character into the given buffer.
fn push_compatibility_decomposition(c: char, vec: &mut Vec<charcc>) {
    if push_hangul_decomposition(c, vec) {
        return;
    }

    match decomposition_mappings::compatibility_mapping(c) {
        Some(decomposition) => {
            vec.extend_from_slice(decomposition);
        }
        None => {
            vec.push(charcc::from_char(c));
        }
    }
}

/// Push a Canonical decomposition (D68) of a single character into the given buffer.
fn push_canonical_decomposition(c: char, vec: &mut Vec<charcc>) {
    if push_hangul_decomposition(c, vec) {
        return;
    }

    match decomposition_mappings::canonical_mapping(c) {
        Some(decomposition) => {
            vec.extend_from_slice(decomposition);
        }
        None => {
            vec.push(charcc::from_char(c));
        }
    }
}

//
// Conjoining Jamo Behavior
//

const S_BASE: u32 = 0xAC00;
const L_BASE: u32 = 0x1100;
const V_BASE: u32 = 0x1161;
const T_BASE: u32 = 0x11A7;
const L_COUNT: u32 = 19;
const V_COUNT: u32 = 21;
const T_COUNT: u32 = 28;
const N_COUNT: u32 = V_COUNT * T_COUNT;
const S_COUNT: u32 = L_COUNT * N_COUNT;

/// If a character is a Precomposed Hangul syllable (D132) then push its full decomposition into
/// the given buffer and return true. Otherwise do not modify the buffer and return false.
fn push_hangul_decomposition(c: char, vec: &mut Vec<charcc>) -> bool {
    use std::char;

    if ((c as u32) < S_BASE) || ((S_BASE + S_COUNT) <= (c as u32)) {
        return false;
    }

    let s_index = (c as u32) - S_BASE;

    let l_index = s_index / N_COUNT;
    let v_index = (s_index % N_COUNT) / T_COUNT;
    let t_index = s_index % T_COUNT;

    // Computed Hangul syllables are guaranteed to be valid Unicode codepoints (cf. ranges).
    // They also have been assigned canonical combining class zero and canonical combining
    // class is guaranteed to not change in Unicode, so we can save a table lookup on that.

    if t_index > 0 {
        // These are safe as codepoints are guaranteed to have correct values.
        let l = unsafe { char::from_u32_unchecked(L_BASE + l_index) };
        let v = unsafe { char::from_u32_unchecked(V_BASE + v_index) };
        let t = unsafe { char::from_u32_unchecked(T_BASE + t_index) };

        vec.push(charcc::from_char_with_ccc(l, 0));
        vec.push(charcc::from_char_with_ccc(v, 0));
        vec.push(charcc::from_char_with_ccc(t, 0));
    } else {
        // These are safe as codepoints are guaranteed to have correct values.
        let l = unsafe { char::from_u32_unchecked(L_BASE + l_index) };
        let v = unsafe { char::from_u32_unchecked(V_BASE + v_index) };

        vec.push(charcc::from_char_with_ccc(l, 0));
        vec.push(charcc::from_char_with_ccc(v, 0));
    }

    true
}

/// If a character pair forms a Precomposed Hangul syllable (D132) when it is canonically composed
/// then return this Some composition. Otherwise return None.
fn compose_hangul(c1: char, c2: char) -> Option<charcc> {
    use std::char;

    // In the same way as with decomposition, arithmetics and character ranges guarantee codepoint
    // validity here, and precomposed Hangul syllables also have canonical combining class zero.

    // <L, V> pair
    if ((L_BASE <= (c1 as u32)) && ((c1 as u32) < L_BASE + L_COUNT)) &&
       ((V_BASE <= (c2 as u32)) && ((c2 as u32) < V_BASE + V_COUNT))
    {
        let l_index = (c1 as u32) - L_BASE;
        let v_index = (c2 as u32) - V_BASE;

        let lv_index = l_index * N_COUNT + v_index * T_COUNT;

        // This is safe as the codepoint is guaranteed to have correct value.
        let lv = unsafe { char::from_u32_unchecked(S_BASE + lv_index) };

        return Some(charcc::from_char_with_ccc(lv, 0));
    }

    // <LV, T> pair
    if ((S_BASE <= (c1 as u32)) && ((c1 as u32) < S_BASE + S_COUNT)) &&
       (((c1 as u32) - S_BASE) % T_COUNT == 0) &&
       (((T_BASE + 1) <= (c2 as u32)) && ((c2 as u32) < T_BASE + T_COUNT))
    {
        let t_index = (c2 as u32) - T_BASE;

        // This is safe as the codepoint is guaranteed to have correct value.
        let lvt = unsafe { char::from_u32_unchecked((c1 as u32) + t_index) };

        return Some(charcc::from_char_with_ccc(lvt, 0));
    }

    // Anything else
    None
}

//
// Canonical Ordering Algorithm
//

/// Apply the Canonical Ordering Algorithm (D109) to a character slice.
fn reorder_canonically(slice: &mut [charcc]) {
    // Actually, this is a bubble sort, but with one important detail: starter characters are never
    // reordered. That is, we must sort only the grapheme clusters. Therefore we can replace O(n^2)
    // bubble sort of the entire string with an O(n) pass over the clusters. Each grapheme cluster
    // still takes O(n^2) time to sort. However, in this case 'n' is the length of the cluster,
    // which is a small number in real-world texts (less than 10 in sane texts, usually 2..5).
    // And usually the combining marks are almost sorted, so the bubble sort works just fine.

    let len = slice.len();

    let mut cur = 0;
    while cur < len {
        // Skip over sequences of starters. We are looking for non-starters.
        if slice[cur].ccc() == 0 {
            cur += 1;
            continue;
        }

        // Now find the next starter so we know where to stop.
        let mut next = cur + 1;
        while next < len {
            if slice[next].ccc() == 0 {
                break;
            }
            next += 1;
        }

        // Apply bubble sort to the cluster.
        for limit in (cur..next).rev() {
            for i in cur..limit {
                if slice[i].ccc() > slice[i + 1].ccc() {
                    slice.swap(i, i + 1);
                }
            }
        }

        // We're done with this cluster, move on to the next one.
        cur = next;
    }
}

//
// Canonical Composition Algorithm
//

/// Apply the Canonical Composition Algorithm (D117) to a character buffer.
fn compose_canonically(buffer: &mut Vec<charcc>) {
    let mut ci = 1;
    while ci < buffer.len() {
        if let Some(li) = find_starter(&buffer[..], ci) {
            if !blocked(&buffer[..], li, ci) {
                let (l, c) = (buffer[li].to_char(), buffer[ci].to_char());
                if let Some(p) = primary_composite(l, c) {
                    buffer[li] = p;
                    buffer.remove(ci);
                    continue;
                }
            }
        }
        ci += 1;
    }
}

/// Find the last Starter (D107) preceding C in a character slice.
/// This is step R1 of the Canonical Composition Algorithm (D117).
fn find_starter(slice: &[charcc], ci: usize) -> Option<usize> {
    for li in (0..ci).rev() {
        if slice[li].ccc() == 0 {
            return Some(li);
        }
    }
    None
}

/// Verify that A is not blocked (D115) from C in a character slice.
/// This is the first part of step R2 of the Canonical Composition Algorithm (D117).
#[allow(clippy::needless_range_loop)]
fn blocked(slice: &[charcc], ai: usize, ci: usize) -> bool {
    assert!(ai < ci);

    let ccc_a = slice[ai].ccc();
    let ccc_c = slice[ci].ccc();

    if ccc_a == 0 {
        for bi in (ai + 1)..ci {
            let ccc_b = slice[bi].ccc();

            if (ccc_b == 0) || (ccc_b >= ccc_c) {
                return true;
            }
        }
    }

    false
}

/// Check for a Primary Composite (D114) equivalent to the given pair of characters.
/// This is the second part of step R2 of the Canonical Composition Algorithm (D117).
fn primary_composite(c1: char, c2: char) -> Option<charcc> {
    compose_hangul(c1, c2).or_else(|| composition_mappings::primary(c1, c2))
}
