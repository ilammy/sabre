// Copyright (c) 2016, Sabre developers
//
// Licensed under the Apache License, Version 2.0 (see LICENSE.Apache in the
// root directory) or MIT license (see LICENSE.MIT in the root directory),
// at your option. This file may be copied, distributed, and modified only
// in accordance with the terms specified by the chosen license.

//! Unicode default case algorithm tests.
//!
//! This module contains tests for _Default Case Algorithms_.

use libunicode::case_algorithms::{to_nfkc_casefold};

//
// toNFKC_Casefold
//

#[test]
fn nfkc_casefold_ascii() {
    assert_eq!(to_nfkc_casefold("Hello World!"), "hello world!");
    assert_eq!(to_nfkc_casefold("CAPS LOCK111"), "caps lock111");
}

#[test]
fn nfkc_casefold_bicameral_scripts() {
    // Latin
    assert_eq!(to_nfkc_casefold("\u{00C6}\u{0043}\u{0301}\u{0126}\u{0131}\u{00DF}\u{01C6}\u{1E4D}"),
                                "\u{00E6}\u{0107}\u{0127}\u{0131}\u{0073}\u{0073}\u{0064}\u{017E}\u{1E4D}");
    // Greek
    assert_eq!(to_nfkc_casefold("\u{0391}\u{0314}\u{0300}\u{0345}\u{03A4}\u{0395}\u{03C3}\u{03C4}\u{03C5}\u{0306}\u{03CE}"),
                                "\u{1F03}\u{03B9}\u{03C4}\u{03B5}\u{03C3}\u{03C4}\u{1FE0}\u{03CE}");
    // Cyrillic
    assert_eq!(to_nfkc_casefold("\u{04F9}\u{040C}\u{0405}\u{0443}\u{045A}\u{04D5}\u{0494}\u{0438}\u{0306}"),
                                "\u{04F9}\u{045C}\u{0455}\u{0443}\u{045A}\u{04D5}\u{0495}\u{0439}");
    // Armenian
    assert_eq!(to_nfkc_casefold("\u{0550}\u{0575}\u{056B}\u{057E}\u{0580}\u{0544}\u{0545}\u{0531}\u{057E}\u{0550}\u{053B}\u{0580}"),
                                "\u{0580}\u{0575}\u{056B}\u{057E}\u{0580}\u{0574}\u{0575}\u{0561}\u{057E}\u{0580}\u{056B}\u{0580}");
    // Cherokee
    assert_eq!(to_nfkc_casefold("\u{13F3}\u{13F4}\u{13F5}\u{13F8}\u{13F9}\u{13FA}\u{AB70}\u{AB71}\u{AB72}\u{ABB9}\u{ABBA}\u{ABBB}"),
                                "\u{13F3}\u{13F4}\u{13F5}\u{13F0}\u{13F1}\u{13F2}\u{13A0}\u{13A1}\u{13A2}\u{13E9}\u{13EA}\u{13EB}");
    // Adlam
    assert_eq!(to_nfkc_casefold("\u{1E900}\u{1E901}\u{1E902}\u{1E922}\u{1E923}\u{1E924}\u{1E950}\u{1E951}\u{1E952}"),
                                "\u{1E922}\u{1E923}\u{1E924}\u{1E922}\u{1E923}\u{1E924}\u{1E950}\u{1E951}\u{1E952}");
}

#[test]
fn nfkc_casefold_unicameral_scripts() {
    // Arabic
    assert_eq!(to_nfkc_casefold("\u{0645}\u{06CC}\u{200C}\u{062E}\u{0648}\u{0627}\u{0647}\u{0645}"),
                                "\u{0645}\u{06CC}\u{062E}\u{0648}\u{0627}\u{0647}\u{0645}");
    // Hebrew
    assert_eq!(to_nfkc_casefold("\u{05EA}\u{05E4}\u{05D5}\u{05D7}\u{05D0}\u{05D3}\u{05D5}\u{05DD}"),
                                "\u{05EA}\u{05E4}\u{05D5}\u{05D7}\u{05D0}\u{05D3}\u{05D5}\u{05DD}");
    // Kannada
    assert_eq!(to_nfkc_casefold("\u{0CB0}\u{200D}\u{0CCD}\u{0C95}"),
                                "\u{0CB0}\u{0CCD}\u{0C95}");
    // Chinese
    assert_eq!(to_nfkc_casefold("\u{8457}\u{7A7A}\u{6C23}\u{95A5}"),
                                "\u{8457}\u{7A7A}\u{6C23}\u{95A5}");
    // Japanese
    assert_eq!(to_nfkc_casefold("\u{FF77}\u{FF82}\u{FF82}\u{FF77}\u{30B3}\u{30F3}\u{FF8A}\u{FF9F}\u{FF72}\u{FF97}"),
                                "\u{30AD}\u{30C4}\u{30C4}\u{30AD}\u{30B3}\u{30F3}\u{30D1}\u{30A4}\u{30E9}");
    // Korean
    assert_eq!(to_nfkc_casefold("\u{1106}\u{1169}\u{1111}\u{1175}\u{1107}\u{116E}\u{110E}\u{1173}"),
                                "\u{BAA8}\u{D53C}\u{BD80}\u{CE20}");
}

#[test]
fn nfkc_casefold_misc_characters() {
    // Math symbols
    assert_eq!(to_nfkc_casefold("\u{2150}\u{212B}\u{2106}\u{3391}"),
                                "\u{0031}\u{2044}\u{0037}\u{00E5}\u{0063}\u{002F}\u{0075}\u{006B}\u{0068}\u{007A}");
    // \u{E}nclosed symbols
    assert_eq!(to_nfkc_casefold("\u{2150}\u{212B}\u{2106}\u{24B6}\u{24D3}\u{247D}\u{321D}"),
                                "\u{0031}\u{2044}\u{0037}\u{00E5}\u{0063}\u{002F}\u{0075}\u{0061}\u{0064}\u{0028}\u{0031}\u{0030}\u{0029}\u{0028}\u{C624}\u{C804}\u{0029}");
    // Mathematical letters
    assert_eq!(to_nfkc_casefold("\u{1D4D0}\u{1D4D1}\u{1D4D2}\u{1D747}\u{1D748}"),
                                "\u{0061}\u{0062}\u{0063}\u{03C3}\u{03C3}");
}
