// Copyright (c) 2016, Sabre developers
//
// Licensed under the Apache License, Version 2.0 (see LICENSE.Apache in the
// root directory) or MIT license (see LICENSE.MIT in the root directory),
// at your option. This file may be copied, distributed, and modified only
// in accordance with the terms specified by the chosen license.

//! Lexer test suite.
//!
//! This verifies that the lexer recognizes all expected tokens and errors.

extern crate reader;
extern crate utils;

use reader::diagnostics::{Span, Handler, Diagnostic, DiagnosticKind};
use reader::intern_pool::{InternPool};
use reader::lexer::{ScannedToken, StringScanner, Scanner};
use reader::tokens::{Token, ParenType};

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Test macro helpers

macro_rules! check {
    { $( ($str:expr => $($token:tt)+) $(, ($from:expr, $to:expr) => $kind:ident)* ; )* } => {{
        let pool = InternPool::new();
        let slices = &[
            $(ScannerTestSlice {
                slice: $str,
                token: token!(pool, $($token)+),
                diagnostics: &[
                    $(ScannerTestDiagnostic {
                        kind: DiagnosticKind::$kind,
                        from: $from, to: $to,
                    },)*
                ],
            },)*
        ];
        check(&pool, slices);
    }}
}

macro_rules! token {
    { $pool:expr, $tok:ident }                      => { Token::$tok };
    { $pool:expr, Open($ptype:ident) }              => { Token::Open(ParenType::$ptype) };
    { $pool:expr, OpenVector($ptype:ident) }        => { Token::OpenVector(ParenType::$ptype) };
    { $pool:expr, OpenBytevector($ptype:ident) }    => { Token::OpenBytevector(ParenType::$ptype) };
    { $pool:expr, Close($ptype:ident) }             => { Token::Close(ParenType::$ptype) };
    { $pool:expr, LabelMark($value:expr) }          => { Token::LabelMark($pool.intern($value)) };
    { $pool:expr, LabelRef($value:expr) }           => { Token::LabelRef($pool.intern($value)) };
    { $pool:expr, Boolean($value:expr) }            => { Token::Boolean($value) };
    { $pool:expr, Character($value:expr) }          => { Token::Character($value) };
    { $pool:expr, String($value:expr) }             => { Token::String($pool.intern($value)) };
    { $pool:expr, Identifier($value:expr) }         => { Token::Identifier($pool.intern($value)) };
    { $pool:expr, Number($value:expr) }             => { Token::Number($pool.intern($value)) };
    { $pool:expr, Directive($value:expr) }          => { Token::Directive($pool.intern($value)) };
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Smoke test of test harness

#[test]
fn smoke_test_empty_string() {
    check! { }
}

#[test]
fn smoke_test_whitespace() {
    check! {
        ("   \t\t\r\n  \t \t\n" => Whitespace);
    }
}

#[test]
fn smoke_test_garbage() {
    check! {
        ("\x01\x02\x03\x04" => Identifier("\x01\x02\x03\x04")),
            (0, 1) => err_lexer_invalid_identifier_character,
            (1, 2) => err_lexer_invalid_identifier_character,
            (2, 3) => err_lexer_invalid_identifier_character,
            (3, 4) => err_lexer_invalid_identifier_character;
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Comments

#[test]
fn line_comments() {
    check! {
        ("; test comment please ignore\n"   => Comment);
        (";;;;;;;;;;;;;;;;;;;;;;;;;;;;\n"   => Comment);
        ("\n\n"                             => Whitespace);
        (";;; \r\n"                         => Comment);
        (";\r\n"                            => Comment);
        ("\n"                               => Whitespace);
        (";\r"                              => Comment);
        (";\r\n"                            => Comment);
        (";\u{042E}\u{043D}\u{0438}\u{043A}\u{043E}\u{0434}\n" => Comment);
        ("\n"                               => Whitespace);
        ("; #|\r\n"                         => Comment);
    }
}

#[test]
fn line_comments_eof() {
    check! {
        ("; EOF is okay" => Comment);
    }
}

#[test]
fn block_comments() {
    check! {
        ("#|basic block comment|#"          => Comment);
        ("\n\n"                             => Whitespace);
        ("#|spanning\nmultiple\r\nlines|#"  => Comment);
        ("\n\n"                             => Whitespace);
        ("#|\n|#"                           => Comment);
        ("\n\n"                             => Whitespace);
        ("#|;\n'123|#"                      => Comment);
        ("\n\n"                             => Whitespace);
        ("#||#"                             => Comment);
        ("#|||#"                            => Comment);
        ("#||||#"                           => Comment);
        ("\n\n"                             => Whitespace);
        ("#| # |#"                          => Comment);
        ("\n\n"                             => Whitespace);
        ("#|bare\rCR\rcharacters\rare\rok|#" => Comment);
        ("\n\n"                             => Whitespace);
        ("#| nested #|comments|# |#"        => Comment);
        ("\n\n"                             => Whitespace);
        ("#|#||#|#"                         => Comment);
        ("\n\n"                             => Whitespace);
        ("#|#|#|||###|||#|#|#"              => Comment);
        ("\n\n"                             => Whitespace);
        ("#|\u{042E}\u{043D}\u{0438}\u{043A}\u{043E}\u{0434}|#" => Comment);
        ("\n\n"                             => Whitespace);
        ("#|\\x7C;\\x23;|#"                 => Comment);
        ("\n\n"                             => Whitespace);
        ("#|#\\|#"                          => Comment);
        ("#|\\#||#|#"                       => Comment);
    }
}

#[test]
fn block_comments_eof() {
    check! {
        ("#| EOF is not okay" => Unrecognized),
            (0, 18) => fatal_lexer_unterminated_comment;
    }
}

#[test]
fn block_comments_eof_nested() {
    check! {
        ("#| nested #|comments #| are |#also handled| #" => Unrecognized),
            (0, 45) => fatal_lexer_unterminated_comment;
    }
}

#[test]
fn sexpr_comments() {
    check! {
        ("#;" => CommentPrefix);
        ("#;" => CommentPrefix);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Fixed-syntax tokens

#[test]
fn fixed_tokens_quotations() {
    check! {
        ("'"    => Quote);
        ("`"    => Backquote);
        (","    => Comma);
        (",@"   => CommaSplicing);
        ("."    => Dot);
        ("'"    => Quote);
        ("'"    => Quote);
        ("'"    => Quote);
        ("`"    => Backquote);
        ("`"    => Backquote);
        ("`"    => Backquote);
        (","    => Comma);
        (","    => Comma);
        (",@"   => CommaSplicing);
        (","    => Comma);
        ("`"    => Backquote);
        (","    => Comma);
        ("."    => Dot);
        ("'"    => Quote);
        ("."    => Dot);
    }
}

#[test]
fn fixed_tokens_parentheses() {
    check! {
        ("("    => Open(Parenthesis));
        (")"    => Close(Parenthesis));
        (" "    => Whitespace);
        ("("    => Open(Parenthesis));
        (" "    => Whitespace);
        (")"    => Close(Parenthesis));
        (" "    => Whitespace);
        (")"    => Close(Parenthesis));
        (")"    => Close(Parenthesis));
        (")"    => Close(Parenthesis));
        (")"    => Close(Parenthesis));
        ("\t"   => Whitespace);
        ("#("   => OpenVector(Parenthesis));
        ("#("   => OpenVector(Parenthesis));
        (")"    => Close(Parenthesis));
        ("#u8(" => OpenBytevector(Parenthesis));
        ("#U8(" => OpenBytevector(Parenthesis));
    }
}

#[test]
fn fixed_tokens_brackets_and_braces() {
    check! {
        ("["    => Open(Bracket));
        ("]"    => Close(Bracket));
        (" "    => Whitespace);
        ("{"    => Open(Brace));
        ("}"    => Close(Brace));
        (" "    => Whitespace);
        ("#["   => OpenVector(Bracket));
        ("#{"   => OpenVector(Brace));
        (" "    => Whitespace);
        ("#u8[" => OpenBytevector(Bracket));
        ("#U8[" => OpenBytevector(Bracket));
        ("#u8{" => OpenBytevector(Brace));
        ("#U8{" => OpenBytevector(Brace));
    }
}

#[test]
fn recover_fixed_tokens_open_vector() {
    check! {
        ("#ahaha-oh-wow"    => Identifier("haha-oh-wow")),
                    (0, 2)  => err_lexer_invalid_number_prefix,
                    (0, 2)  => err_lexer_prefixed_identifier;
        ("("                => Open(Parenthesis));
        ("#:"               => Identifier("")),
                    (0, 2)  => err_lexer_invalid_number_prefix,
                    (0, 2)  => err_lexer_prefixed_identifier;
        ("["                => Open(Bracket));
        ("#"                => Identifier("")),
                    (0, 1)  => err_lexer_invalid_number_prefix,
                    (0, 1)  => err_lexer_prefixed_identifier;
        (","                => Comma);
        ("{"                => Open(Brace));
        ("#"                => Identifier("")),
                    (0, 1)  => err_lexer_invalid_number_prefix,
                    (0, 1)  => err_lexer_prefixed_identifier;
        (" "                => Whitespace);
        ("#"                => Identifier("")),
                    (0, 1)  => err_lexer_invalid_number_prefix,
                    (0, 1)  => err_lexer_prefixed_identifier;
        (",@"               => CommaSplicing);
        (" "                => Whitespace);
        ("#"                => Identifier("")),
                    (0, 1)  => err_lexer_invalid_number_prefix,
                    (0, 1)  => err_lexer_prefixed_identifier;
        ("`"                => Backquote);
        ("["                => Open(Bracket));
        (" "                => Whitespace);
        ("#####"            => Identifier("")),
                    (0, 1)  => err_lexer_invalid_number_prefix,
                    (1, 2)  => err_lexer_invalid_number_prefix,
                    (2, 3)  => err_lexer_invalid_number_prefix,
                    (3, 4)  => err_lexer_invalid_number_prefix,
                    (4, 5)  => err_lexer_invalid_number_prefix,
                    (0, 5)  => err_lexer_prefixed_identifier;
        ("("                => Open(Parenthesis));
        (" "                => Whitespace);
        ("#."               => Number("#.")),
                    (0, 1)  => err_lexer_invalid_number_prefix,
                    (1, 2)  => err_lexer_digits_missing;
        ("{"                => Open(Brace));
    }
}

#[test]
fn recover_fixed_tokens_open_bytevector() {
    check! {
        ("#u"               => Identifier("")),
                     (0, 2) => err_lexer_invalid_number_prefix,
                     (0, 2) => err_lexer_prefixed_identifier;
        ("("                => Open(Parenthesis));
        (" "                => Whitespace);
        ("#U"               => Identifier("")),
                     (0, 2) => err_lexer_invalid_number_prefix,
                     (0, 2) => err_lexer_prefixed_identifier;
        ("["                => Open(Bracket));
        (" "                => Whitespace);
        ("#ufo"             => Identifier("fo")),
                     (0, 2) => err_lexer_invalid_number_prefix,
                     (0, 2) => err_lexer_prefixed_identifier;
        ("("                => Open(Parenthesis));
        (" "                => Whitespace);
        ("#u16"             => Number("#u16")),
                     (0, 2) => err_lexer_invalid_number_prefix;
        ("{"                => Open(Brace));
        (" "                => Whitespace);
        ("#U571"            => Number("#U571")),
                     (0, 2) => err_lexer_invalid_number_prefix;
        ("["                => Open(Bracket));
        (" "                => Whitespace);
        ("#u90_ex.a@mp!le1" => Number("#u90_ex.a@mp!le1")),
                     (0, 2) => err_lexer_invalid_number_prefix,
                     (4, 5) => err_lexer_invalid_number_character,
                     (6, 7) => err_lexer_invalid_number_character,
                     (7, 8) => err_lexer_invalid_number_character,
                     (8, 9) => err_lexer_invalid_number_character,
                     (6, 9) => err_lexer_digits_missing,
                   (10, 11) => err_lexer_invalid_number_character,
                   (11, 12) => err_lexer_invalid_number_character,
                   (12, 13) => err_lexer_invalid_number_character,
                   (10, 13) => err_lexer_digits_missing,
                   (14, 15) => err_lexer_invalid_number_character;
        ("("                => Open(Parenthesis));
        (" "                => Whitespace);
        ("#8"               => LabelMark("8")),
                    (2, 2)  => err_lexer_missing_datum_label_terminator;
        ("["                => Open(Bracket));
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Datum labels

#[test]
fn label_mark() {
    check! {
        ("#1="              => LabelMark("1"));
        (" "                => Whitespace);
        ("#2345="           => LabelMark("2345"));
        ("#78="             => LabelMark("78"));
        ("("                => Open(Parenthesis));
        ("#9="              => LabelMark("9"));
        (" "                => Whitespace);
        ("#0000000000="     => LabelMark("0000000000"));
        ("#\\x"             => Character('x'));
        (" "                => Whitespace);
        ("#1="              => LabelMark("1"));
    }
}

#[test]
fn label_ref() {
    check! {
        ("#1#"              => LabelRef("1"));
        (" "                => Whitespace);
        ("#2#"              => LabelRef("2"));
        ("#3#"              => LabelRef("3"));
        ("#4="              => LabelMark("4"));
        (" "                => Whitespace);
        ("#0000000000#"     => LabelRef("0000000000"));
        (","                => Comma);
        ("#1#"              => LabelRef("1"));
        (" "                => Whitespace);
        ("#2#"              => LabelRef("2"));
        ("|test|"           => Identifier("test"));
    }
}

#[test]
fn recover_label_invalid_characters() {
    check! {
        ("#agaga=#9#"       => Identifier("gaga=#9#")),
                     (0, 2) => err_lexer_invalid_number_prefix,
                     (0, 2) => err_lexer_prefixed_identifier,
                     (7, 8) => err_lexer_invalid_identifier_character,
                    (9, 10) => err_lexer_invalid_identifier_character;
        (" "                => Whitespace);
        ("#1agagaga="       => LabelMark("1agagaga")),
                     (2, 3) => err_lexer_invalid_number_character,
                     (3, 4) => err_lexer_invalid_number_character,
                     (4, 5) => err_lexer_invalid_number_character,
                     (5, 6) => err_lexer_invalid_number_character,
                     (6, 7) => err_lexer_invalid_number_character,
                     (7, 8) => err_lexer_invalid_number_character,
                     (8, 9) => err_lexer_invalid_number_character;
        (" "                => Whitespace);
        ("#123hello#"       => LabelRef("123hello")),
                     (4, 5) => err_lexer_invalid_number_character,
                     (5, 6) => err_lexer_invalid_number_character,
                     (6, 7) => err_lexer_invalid_number_character,
                     (7, 8) => err_lexer_invalid_number_character,
                     (8, 9) => err_lexer_invalid_number_character;
        (" "                => Whitespace);
        ("#17.23e+14#"      => LabelRef("17.23e+14")),
                     (3, 4) => err_lexer_invalid_number_character,
                     (6, 7) => err_lexer_invalid_number_character,
                     (7, 8) => err_lexer_invalid_number_character;
        ("#14/7i="          => LabelMark("14/7i")),
                     (3, 4) => err_lexer_invalid_number_character,
                     (5, 6) => err_lexer_invalid_number_character;
        ("#6@6#"            => LabelRef("6@6")),
                     (2, 3) => err_lexer_invalid_number_character;
        (" "                => Whitespace);
        ("#+123#"           => Number("#+123#")),
                     (0, 1) => err_lexer_invalid_number_prefix,
                     (5, 6) => err_lexer_invalid_number_character;
    }
}

#[test]
fn recover_label_missing_terminators() {
    check! {
        ("#123"             => LabelMark("123")),
                     (4, 4) => err_lexer_missing_datum_label_terminator;
        (" "                => Whitespace);
        ("#4#"              => LabelRef("4"));
        ("5"                => Number("5"));
        (" "                => Whitespace);
        ("#678"             => LabelMark("678")),
                     (4, 4) => err_lexer_missing_datum_label_terminator;
        ("\"9\""            => String("9"));
        ("#10="             => LabelMark("10"));
        ("="                => Identifier("="));
        (" "                => Whitespace);
        ("#5" /* EOF */     => LabelMark("5")),
                     (2, 2) => err_lexer_missing_datum_label_terminator;
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Booleans

#[test]
fn boolean_short() {
    check! {
        ("#t"               => Boolean(true));
        ("("                => Open(Parenthesis));
        ("#f"               => Boolean(false));
        (")"                => Close(Parenthesis));
        ("#T"               => Boolean(true));
        (" "                => Whitespace);
        ("#F"               => Boolean(false));
    }
}

#[test]
fn boolean_long() {
    check! {
        ("#true"            => Boolean(true));
        (","                => Comma);
        ("#false"           => Boolean(false));
        (";\n"              => Comment);
        ("#TRUE"            => Boolean(true));
        (" "                => Whitespace);
        ("#FaLse"           => Boolean(false));
    }
}

#[test]
fn recover_boolean_garbage() {
    check! {
        ("#tr"              => Identifier("r")),
                     (0, 2) => err_lexer_invalid_number_prefix,
                     (0, 2) => err_lexer_prefixed_identifier;
        (" "                => Whitespace);
        ("#truuuue"         => Identifier("ruuuue")),
                     (0, 2) => err_lexer_invalid_number_prefix,
                     (0, 2) => err_lexer_prefixed_identifier;
        (" "                => Whitespace);
        ("#false!"          => Identifier("alse!")),
                     (0, 2) => err_lexer_invalid_number_prefix,
                     (0, 2) => err_lexer_prefixed_identifier;
        (" "                => Whitespace);
        ("#t#f#t#f"         => Identifier("")),
                     (0, 2) => err_lexer_invalid_number_prefix,
                     (2, 4) => err_lexer_invalid_number_prefix,
                     (4, 6) => err_lexer_invalid_number_prefix,
                     (6, 8) => err_lexer_invalid_number_prefix,
                     (0, 8) => err_lexer_prefixed_identifier;
        (" "                => Whitespace);
        ("#fal" /* EOF */   => Identifier("al")),
                     (0, 2) => err_lexer_invalid_number_prefix,
                     (0, 2) => err_lexer_prefixed_identifier;
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Characters

#[test]
fn characters_immediate() {
    check! {
        ("#\\t"         => Character('t'));
        (" "            => Whitespace);
        ("#\\e"         => Character('e'));
        (" "            => Whitespace);
        ("#\\s"         => Character('s'));
        (" "            => Whitespace);
        ("#\\t"         => Character('t'));
        (" "            => Whitespace);
        ("#\\X"         => Character('X'));
        (" "            => Whitespace);
        ("#\\\u{1234}"  => Character('\u{1234}'));
        (" "            => Whitespace);
        ("#\\("         => Character('('));
        (" "            => Whitespace);
        ("#\\)"         => Character(')'));
        (" "            => Whitespace);
        ("#\\."         => Character('.'));
        (" "            => Whitespace);
        ("#\\,"         => Character(','));
    }
}

#[test]
fn characters_named() {
    check! {
        ("#\\alarm"     => Character('\u{0007}'));
        ("\n"           => Whitespace);
        ("#\\escape"    => Character('\u{0008}'));
        ("\n"           => Whitespace);
        ("#\\delete"    => Character('\u{007F}'));
        ("\n"           => Whitespace);
        ("#\\newline"   => Character('\u{000A}'));
        ("\n"           => Whitespace);
        ("#\\null"      => Character('\u{0000}'));
        ("\n"           => Whitespace);
        ("#\\return"    => Character('\u{000D}'));
        ("\n"           => Whitespace);
        ("#\\space"     => Character('\u{0020}'));
        ("\n"           => Whitespace);
        ("#\\tab"       => Character('\u{0009}'));
    }
}

#[test]
fn characters_hexcoded() {
    check! {
        ("#\\x0000"     => Character('\u{0000}'));
        ("  "           => Whitespace);
        ("#\\xF00F"     => Character('\u{F00F}'));
        ("  "           => Whitespace);
        ("#\\X200C"     => Character('\u{200C}'));
        ("  "           => Whitespace);
        ("#\\x200c"     => Character('\u{200C}'));
        ("  "           => Whitespace);
        ("#\\xd"        => Character('\u{000D}'));
        ("  "           => Whitespace);
        ("#\\XFF"       => Character('\u{00FF}'));
        ("  "           => Whitespace);
        ("#\\x000000000000000000000000000000000000001" => Character('\u{0001}'));
        ("  "           => Whitespace);
        ("#\\xD7FF"     => Character('\u{D7FF}'));
        ("  "           => Whitespace);
        ("#\\xD800"     => Character('\u{FFFD}')),
                 (0, 7) => err_lexer_invalid_unicode_range;
        ("  "           => Whitespace);
        ("#\\xDBFF"     => Character('\u{FFFD}')),
                 (0, 7) => err_lexer_invalid_unicode_range;
        ("  "           => Whitespace);
        ("#\\xdC00"     => Character('\u{FFFD}')),
                 (0, 7) => err_lexer_invalid_unicode_range;
        ("  "           => Whitespace);
        ("#\\xDffF"     => Character('\u{FFFD}')),
                 (0, 7) => err_lexer_invalid_unicode_range;
        ("  "           => Whitespace);
        ("#\\xE000"     => Character('\u{E000}'));
        ("  "           => Whitespace);
        ("#\\X110000"   => Character('\u{FFFD}')),
                 (0, 9) => err_lexer_invalid_unicode_range;
        ("  "           => Whitespace);
        ("#\\xFffFDECBbDFFFFfFFFF1238126318Faaaa" => Character('\u{FFFD}')),
                (0, 37) => err_lexer_invalid_unicode_range;
    }
}

#[test]
fn characters_edge_cases() {
    check! {
        ("#\\\u{0000}"  => Character('\u{0000}'));
        (" "            => Whitespace);
        ("#\\\\"        => Character('\\'));
        (" "            => Whitespace);
        ("#\\#"         => Character('#'));
        (" "            => Whitespace);
        ("#\\\""        => Character('\"'));
        (" "            => Whitespace);
        ("#\\("         => Character('('));
        ("("            => Open(Parenthesis));
        (" "            => Whitespace);
        ("#\\;"         => Character(';'));
        ("; \n"         => Comment);
        ("#\\|"         => Character('|'));
    }
}

#[test]
fn characters_whitespace_eof_special() {
    check! {
        ("#\\"      => Character('\u{FFFD}')),
             (2, 2) => err_lexer_character_missing;
        (" "        => Whitespace);
        ("#\\"      => Character('\u{FFFD}')),
             (2, 2) => err_lexer_character_missing;
        ("\t"       => Whitespace);
        ("#\\"      => Character('\u{FFFD}')),
             (2, 2) => err_lexer_character_missing;
        ("\r"       => Whitespace);
        ("#\\"      => Character('\u{FFFD}')),
             (2, 2) => err_lexer_character_missing;
        ("\n"       => Whitespace);
        ("#\\"      => Character('\u{FFFD}')),
             (2, 2) => err_lexer_character_missing;
        ("\r\n"     => Whitespace);
        ("#\\"      => Character('\u{FFFD}')),
             (2, 2) => err_lexer_character_missing;
    }
}

#[test]
fn recover_characters_no_separator() {
    check! {
        ("#\\t#\\e#\\s#\\t" => Character('\u{FFFD}')),
                    (0, 12) => err_lexer_unknown_character_name;
        (" "                => Whitespace);
        ("#\\("             => Character('('));
        (")"                => Close(Parenthesis));
        (" "                => Whitespace);
        ("#\\(#\\"          => Character('\u{FFFD}')),
                     (0, 5) => err_lexer_unknown_character_name;
        ("("                => Open(Parenthesis));
        (" "                => Whitespace);
        ("#\\)"             => Character(')'));
        ("]"                => Close(Bracket));
        (" "                => Whitespace);
        ("#\\)#\\"          => Character('\u{FFFD}')),
                     (0, 5) => err_lexer_unknown_character_name;
        ("]"                => Close(Bracket));
        (" "                => Whitespace);
        ("#\\.."            => Character('\u{FFFD}')),
                     (0, 4) => err_lexer_unknown_character_name;
        (" "                => Whitespace);
        ("#\\..#\\."        => Character('\u{FFFD}')),
                     (0, 7) => err_lexer_unknown_character_name;
        (" "                => Whitespace);
        ("#\\,"             => Character(','));
        (","                => Comma);
        (","                => Comma);
        ("#\\,"             => Character(','));
        (" "                => Whitespace);
        ("#\\."             => Character('.'));
        ("'"                => Quote);
        ("#\\'"             => Character('\''));
        (" "                => Whitespace);
        ("#\\\\#\\#"        => Character('\u{FFFD}')),
                     (0, 6) => err_lexer_unknown_character_name;
        (" "                => Whitespace);
        ("#\\;#\\"          => Character('\u{FFFD}')),
                     (0, 5) => err_lexer_unknown_character_name;
        (";"                => Comment);
    }
}

#[test]
fn recover_characters_names() {
    check! {
        ("#\\Space"                         => Character('\u{FFFD}')),
                                     (0, 7) => err_lexer_unknown_character_name;
        (" "                                => Whitespace);
        ("#\\NEWLINE"                       => Character('\u{FFFD}')),
                                     (0, 9) => err_lexer_unknown_character_name;
        (" "                                => Whitespace);
        ("#\\1234"                          => Character('\u{FFFD}')),
                                     (0, 6) => err_lexer_unknown_character_name;
        (" "                                => Whitespace);
        ("#\\desu-desu"                     => Character('\u{FFFD}')),
                (                    0, 11) => err_lexer_unknown_character_name;
        (" "                                => Whitespace);
        ("#\\..."                           => Character('\u{FFFD}')),
                                     (0, 5) => err_lexer_unknown_character_name;
        (" "                                => Whitespace);
        ("#\\%%%"                           => Character('\u{FFFD}')),
                                     (0, 5) => err_lexer_unknown_character_name;
        (" "                                => Whitespace);
        ("#\\^_^"                           => Character('\u{FFFD}')),
                                     (0, 5) => err_lexer_unknown_character_name;
        (" "                                => Whitespace);
        ("#\\\\\\"                          => Character('\u{FFFD}')),
                                     (0, 4) => err_lexer_unknown_character_name;
        (" "                                => Whitespace);
        ("#\\+inf.0"                        => Character('\u{FFFD}')),
                                     (0, 8) => err_lexer_unknown_character_name;
        (" "                                => Whitespace);
        ("#\\-NaN.0"                        => Character('\u{FFFD}')),
                                     (0, 8) => err_lexer_unknown_character_name;
        (" "                                => Whitespace);
        ("#\\-1.@.1"                        => Character('\u{FFFD}')),
                                     (0, 8) => err_lexer_unknown_character_name;
        (" "                                => Whitespace);
        ("#\\Xample"                        => Character('\u{FFFD}')),
                                     (0, 8) => err_lexer_unknown_character_name;
        (" "                                => Whitespace);
        ("#\\\\x1232"                       => Character('\u{FFFD}')),
                                     (0, 8) => err_lexer_unknown_character_name;
        (" "                                => Whitespace);
        ("#\\\u{0}\u{1}#\\\u{2}\u{3}"       => Character('\u{FFFD}')),
                                     (0, 8) => err_lexer_unknown_character_name;
        (";\n"                              => Comment);
        ("#\\\u{1111}\u{2222}\u{3333}"      => Character('\u{FFFD}')),
                                    (0, 11) => err_lexer_unknown_character_name;
        ("("                                => Open(Parenthesis));
        ("#\\\u{e801}xx\u{102323}_\u{7F}"   => Character('\u{FFFD}')),
                                    (0, 13) => err_lexer_unknown_character_name;
        ("]"                                => Close(Bracket));
        (" "                                => Whitespace);
        ("#\\\u{00EB}"                      => Character('\u{00EB}'));
        (" "                                => Whitespace);
        ("#\\\u{0451}"                      => Character('\u{0451}'));
        (" "                                => Whitespace);
        ("#\\\u{0435}\u{0308}"              => Character('\u{FFFD}')),
                                     (0, 6) => err_lexer_unknown_character_name;
        (" "                                => Whitespace);
        ("#\\\u{0065}\u{0308}"              => Character('\u{FFFD}')),
                                     (0, 5) => err_lexer_unknown_character_name;
        (" "                                => Whitespace);
        ("#\\\u{CE68}"                      => Character('\u{CE68}'));
        (" "                                => Whitespace);
        ("#\\\u{110E}\u{1175}\u{11B7}"      => Character('\u{FFFD}')),
                                    (0, 11) => err_lexer_unknown_character_name;
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Strings

#[test]
fn strings_basic() {
    check! {
        ("\"test\""     => String("test"));
        (" "            => Whitespace);
        ("\"\""         => String(""));
        (" "            => Whitespace);
        ("\"TesT\""     => String("TesT"));
        (" "            => Whitespace);
        ("\"12312\""    => String("12312"));
        (" "            => Whitespace);
        ("\" \""        => String(" "));
        ("\"\t\""       => String("\t"));
        ("\".\""        => String("."));
        (" "            => Whitespace);
        ("\"\u{0}\""    => String("\u{0000}"));
        (" "            => Whitespace);
        ("\"\u{044E}\u{043D}\u{0438}\u{043A}\u{043E}\u{0434}\""
                        => String("\u{044E}\u{043D}\u{0438}\u{043A}\u{043E}\u{0434}"));
        (" "            => Whitespace);
        ("\"\u{1112}\u{1161}\u{11AB}\u{1100}\u{116E}\u{11A8}\u{110B}\u{1165}\""
                        => String("\u{1112}\u{1161}\u{11AB}\u{1100}\u{116E}\u{11A8}\u{110B}\u{1165}"));
    }
}

#[test]
fn strings_escape_sequences() {
    check! {
        (r#""\a""#      => String("\u{0007}"));
        (r#""\b""#      => String("\u{0008}"));
        (r#""\t""#      => String("\u{0009}"));
        (r#""\n""#      => String("\u{000A}"));
        (r#""\r""#      => String("\u{000D}"));
        (r#""\"""#      => String("\u{0022}"));
        (r#""\\""#      => String("\u{005C}"));
        (r#""\|""#      => String("\u{007C}"));
        (r#""\r\n""#    => String("\u{000D}\u{000A}"));
    }
}

#[test]
fn strings_unicode_escapes() {
    check! {
        ("\"\\x0000;\""     => String("\u{0000}"));
        (" "                => Whitespace);
        ("\"\\X1234;\""     => String("\u{1234}"));
        (" "                => Whitespace);
        ("\"\\xBeeb;\""     => String("\u{BEEB}"));
        (" "                => Whitespace);
        ("\"\\Xf0F0C;\""    => String("\u{0F0F0C}"));
        (" "                => Whitespace);
        ("\"\\x00000001;\"" => String("\u{0001}"));
        (" "                => Whitespace);
        ("\"\\x10FFFF;\""   => String("\u{10FFFF}"));
    }
}

#[test]
fn strings_newlines() {
    check! {
        ("\"one\nline\""        => String("one\nline"));
        ("\n"                   => Whitespace);
        ("\"other\r\nline\""    => String("other\nline"));
        ("\n"                   => Whitespace);
        ("\"third\n\nline\""    => String("third\n\nline"));
        ("\n"                   => Whitespace);
        ("\"bare\rCR\""         => String("bare\nCR"));
        ("\n"                   => Whitespace);
        ("\"\n\""               => String("\n"));
        ("\"\r\""               => String("\n"));
        ("\"\r\n\""             => String("\n"));
        ("\"\r\n\r\""           => String("\n\n"));
        ("\"\n\r\n\""           => String("\n\n"));
    }
}

#[test]
fn strings_line_escape() {
    check! {
        ("\"text with \\  \t  \r\n \t one line\""   => String("text with one line"));
        ("\n"                                       => Whitespace);
        ("\"another\\\nline\""                      => String("anotherline"));
        ("\r\n"                                     => Whitespace);
        ("\"one\\\r\nmore\""                        => String("onemore"));
        ("\r"                                       => Whitespace);
        ("\"as\\\rwell\""                           => String("aswell"));
        ("\n"                                       => Whitespace);
        ("\"\\\n\""                                 => String(""));
        ("\n"                                       => Whitespace);
        ("\"\\\r\n\""                               => String(""));
        ("\n"                                       => Whitespace);
        ("\"\\\r\""                                 => String(""));
        ("\n"                                       => Whitespace);
        ("\"\\ \n \""                               => String(""));
        ("\n"                                       => Whitespace);
        ("\"\\ \r\n \""                             => String(""));
        ("\n"                                       => Whitespace);
        ("\"\\ \r \""                               => String(""));
        ("\n"                                       => Whitespace);
        ("\"\\\r\r\""                               => String("\n"));
        ("\n"                                       => Whitespace);
        ("\"<\\\r\n\r\t\r\n>\""                     => String("<\n\t\n>"));
    }
}

#[test]
fn recover_strings_eof_1() {
    check! {
        ("\"endless string" => Unrecognized),
            (0, 15) => fatal_lexer_unterminated_string;
    }
}

#[test]
fn recover_strings_eof_2() {
    check! {
        ("\"" => Unrecognized),
            (0, 1) => fatal_lexer_unterminated_string;
    }
}

#[test]
fn recover_strings_eof_3() {
    check! {
        ("\"\\" => Unrecognized),
            (0, 2) => fatal_lexer_unterminated_string;
    }
}

#[test]
fn recover_strings_eof_4() {
    check! {
        ("\"\\ " => Unrecognized),
            (1, 3) => err_lexer_invalid_line_escape,
            (0, 3) => fatal_lexer_unterminated_string;
    }
}

#[test]
fn recover_strings_eof_5() {
    check! {
        ("\"\\x1" => Unrecognized),
            (4, 4) => err_lexer_unicode_escape_missing_semicolon,
            (0, 4) => fatal_lexer_unterminated_string;
    }
}

#[test]
fn recover_strings_escape_sequences() {
    check! {
        ("\"\\m\""          => String("m")),
                     (1, 3) => err_lexer_invalid_escape_sequence;
        (" "                => Whitespace);
        ("\"\\1\\2\\3\""    => String("123")),
                     (1, 3) => err_lexer_invalid_escape_sequence,
                     (3, 5) => err_lexer_invalid_escape_sequence,
                     (5, 7) => err_lexer_invalid_escape_sequence;
        (" "                => Whitespace);
        ("\"\\\u{0}\""      => String("\u{0000}")),
                     (1, 3) => err_lexer_invalid_escape_sequence;
        (" "                => Whitespace);
        ("\"\\\r\""         => String("")); // line escape
    }
}

#[test]
fn recover_strings_line_escapes() {
    check! {
        ("\"xxx \\   !\""           => String("xxx !")),
                             (5, 9) => err_lexer_invalid_line_escape;
        ("\n"                       => Whitespace);
        ("\"xxx \\\t\t\u{0000}\""   => String("xxx \u{0000}")),
                             (5, 8) => err_lexer_invalid_line_escape;
    }
}

#[test]
fn recover_strings_unicode_escapes() {
    check! {
        ("\"\\xD7FF;\\xD800;\\xDFFF;\\xC000;\""     => String("\u{D7FF}\u{FFFD}\u{FFFD}\u{C000}")),
                                            (8, 15) => err_lexer_invalid_unicode_range,
                                           (15, 22) => err_lexer_invalid_unicode_range;
        ("\n"                                       => Whitespace);
        ("\"\\XfaBBCbCBDb9BCdeeeAa2123987005;\""    => String("\u{FFFD}")),
                                            (1, 33) => err_lexer_invalid_unicode_range;
        ("\n"                                       => Whitespace);
        ("\"\\x110000;\\x0F00000;\""                => String("\u{FFFD}\u{FFFD}")),
                                            (1, 10) => err_lexer_invalid_unicode_range,
                                           (10, 20) => err_lexer_invalid_unicode_range;
        ("\"\\x\""                                  => String("x")),
                                             (1, 3) => err_lexer_invalid_escape_sequence;
        ("\n"                                       => Whitespace);
        ("\"\\X\""                                  => String("X")),
                                             (1, 3) => err_lexer_invalid_escape_sequence;
        ("\n"                                       => Whitespace);
        ("\"\\x!\""                                 => String("x!")),
                                             (1, 3) => err_lexer_invalid_escape_sequence;
        ("\n"                                       => Whitespace);
        ("\"\\x;\""                                 => String("\u{FFFD}")),
                                             (3, 3) => err_lexer_unicode_escape_missing_digits;
        ("\n"                                       => Whitespace);
        ("\"\\X;\""                                 => String("\u{FFFD}")),
                                             (3, 3) => err_lexer_unicode_escape_missing_digits;
        ("\n"                                       => Whitespace);
        ("\"\\xdesu\""                              => String("\u{00DE}su")),
                                             (5, 5) => err_lexer_unicode_escape_missing_semicolon;
        ("\n"                                       => Whitespace);
        ("\"\\x11111111111111111x\""                => String("\u{FFFD}x")),
                                           (20, 20) => err_lexer_unicode_escape_missing_semicolon,
                                            (1, 20) => err_lexer_invalid_unicode_range;
        ("\n"                                       => Whitespace);
        ("\"\\x3711\\a\""                           => String("\u{3711}\u{0007}")),
                                             (7, 7) => err_lexer_unicode_escape_missing_semicolon;
        ("\n"                                       => Whitespace);
        ("\"\\x0ded\""                              => String("\u{0DED}")),
                                             (7, 7) => err_lexer_unicode_escape_missing_semicolon;
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Numbers: integer

#[test]
fn numbers_integer_basic() {
    check! {
        ("0"                    => Number("0"));
        (" "                    => Whitespace);
        ("12345"                => Number("12345"));
        (" "                    => Whitespace);
        ("4294967296"           => Number("4294967296"));
        (" "                    => Whitespace);
        ("9223372036854775808"  => Number("9223372036854775808"));
        (" "                    => Whitespace);
        ("170141183460469231731687303715884105727" => Number("170141183460469231731687303715884105727"));
    }
}

#[test]
fn numbers_integer_signed() {
    check! {
        ("+9000"                => Number("+9000"));
        (" "                    => Whitespace);
        ("-1234"                => Number("-1234"));
        (" "                    => Whitespace);
        ("-0"                   => Number("-0"));
        (" "                    => Whitespace);
        ("+1"                   => Number("+1"));
        (" "                    => Whitespace);
        ("-9223372036854775808" => Number("-9223372036854775808"));
    }
}

#[test]
fn numbers_integer_radix() {
    check! {
        ("#b0"                  => Number("#b0"));
        (" "                    => Whitespace);
        ("#B1"                  => Number("#B1"));
        (" "                    => Whitespace);
        ("#b1111111101011011"   => Number("#b1111111101011011"));
        ("\n"                   => Whitespace);
        ("#o755"                => Number("#o755"));
        (" "                    => Whitespace);
        ("#O0"                  => Number("#O0"));
        (" "                    => Whitespace);
        ("#o01234567"           => Number("#o01234567"));
        ("\n"                   => Whitespace);
        ("#d1234138910"         => Number("#d1234138910"));
        (" "                    => Whitespace);
        ("#D0000123"            => Number("#D0000123"));
        (" "                    => Whitespace);
        ("#D3"                  => Number("#D3"));
        ("\n"                   => Whitespace);
        ("#x0123456789ABCDEF"   => Number("#x0123456789ABCDEF"));
        (" "                    => Whitespace);
        ("#X0123456789abcdef"   => Number("#X0123456789abcdef"));
        (" "                    => Whitespace);
        ("#xD"                  => Number("#xD"));
    }
}

#[test]
fn numbers_integer_exactness() {
    check! {
        ("#e9000"               => Number("#e9000"));
        (" "                    => Whitespace);
        ("#i386"                => Number("#i386"));
        (" "                    => Whitespace);
        ("#E3"                  => Number("#E3"));
        (" "                    => Whitespace);
        ("#I1"                  => Number("#I1"));
    }
}

#[test]
fn numbers_integer_both_radix_and_exactness() {
    check! {
        ("#b#e10101"            => Number("#b#e10101"));
        (" "                    => Whitespace);
        ("#E#B01010"            => Number("#E#B01010"));
        (" "                    => Whitespace);
        ("#i#o640"              => Number("#i#o640"));
        (" "                    => Whitespace);
        ("#x#EffF"              => Number("#x#EffF"));
        (" "                    => Whitespace);
        ("#d#I123"              => Number("#d#I123"));
    }
}

#[test]
fn numbers_integer_all_inclusive() {
    check! {
        ("#d-9000"              => Number("#d-9000"));
        (" "                    => Whitespace);
        ("#I#X+DEAD"            => Number("#I#X+DEAD"));
        (" "                    => Whitespace);
        ("#o#E+0"               => Number("#o#E+0"));
    }
}

#[test]
fn recover_numbers_integer_prefixed_garbage() {
    check! {
        ("+\u{1}\u{2}\u{3}"     => Identifier("+\u{1}\u{2}\u{3}")),
                         (1, 2) => err_lexer_invalid_identifier_character,
                         (2, 3) => err_lexer_invalid_identifier_character,
                         (3, 4) => err_lexer_invalid_identifier_character;
        ("\n"                   => Whitespace);
        ("#O-\u{0}"             => Identifier("-\u{0}")),
                         (0, 2) => err_lexer_prefixed_identifier,
                         (3, 4) => err_lexer_invalid_identifier_character;
        ("\n"                   => Whitespace);
        ("#i#X\u{F}"            => Identifier("\u{F}")),
                         (0, 4) => err_lexer_prefixed_identifier,
                         (4, 5) => err_lexer_invalid_identifier_character;
        ("\n"                   => Whitespace);
        ("#b#e-+-5"             => Identifier("-+-5")),
                         (0, 4) => err_lexer_prefixed_identifier;
    }
}

#[test]
fn recover_numbers_integer_duplicate_prefixes() {
    check! {
        ("#i#e5"                => Number("#i#e5")),
                         (2, 4) => err_lexer_multiple_exactness;
        ("\n"                   => Whitespace);
        ("#E#e5"                => Number("#E#e5")),
                         (2, 4) => err_lexer_multiple_exactness;
        ("\n"                   => Whitespace);
        ("#D#x00101"            => Number("#D#x00101")),
                         (2, 4) => err_lexer_multiple_number_radices;
        ("\n"                   => Whitespace);
        ("#X#o#bF00FA"          => Number("#X#o#bF00FA")),
                         (2, 4) => err_lexer_multiple_number_radices,
                         (4, 6) => err_lexer_multiple_number_radices;
        ("\n"                   => Whitespace);
        ("#E#o#e#x#I#d#X#i#B9"  => Number("#E#o#e#x#I#d#X#i#B9")),
                        (4,  6) => err_lexer_multiple_exactness,
                        (6,  8) => err_lexer_multiple_number_radices,
                        (8, 10) => err_lexer_multiple_exactness,
                       (10, 12) => err_lexer_multiple_number_radices,
                       (12, 14) => err_lexer_multiple_number_radices,
                       (14, 16) => err_lexer_multiple_exactness,
                       (16, 18) => err_lexer_multiple_number_radices,
                       (18, 19) => err_lexer_invalid_number_digit;
    }
}

#[test]
fn recover_numbers_integer_invalid_prefixes() {
    check! {
        ("#@#"                  => Identifier("")),
                         (0, 2) => err_lexer_invalid_number_prefix,
                         (2, 3) => err_lexer_invalid_number_prefix,
                         (0, 3) => err_lexer_prefixed_identifier;
        (" "                    => Whitespace);
        ("##"                   => Identifier("")),
                         (0, 1) => err_lexer_invalid_number_prefix,
                         (1, 2) => err_lexer_invalid_number_prefix,
                         (0, 2) => err_lexer_prefixed_identifier;
        (" "                    => Whitespace);
        ("#"                    => Identifier("")),
                         (0, 1) => err_lexer_invalid_number_prefix,
                         (0, 1) => err_lexer_prefixed_identifier;
        (" "                    => Whitespace);
        ("##123"                => Number("##123")),
                         (0, 1) => err_lexer_invalid_number_prefix,
                         (1, 2) => err_lexer_invalid_number_prefix;
        (" "                    => Whitespace);
        ("#123"                 => LabelMark("123")),
                         (4, 4) => err_lexer_missing_datum_label_terminator;
        (" "                    => Whitespace);
        ("#+123"                => Number("#+123")),
                         (0, 1) => err_lexer_invalid_number_prefix;
        (" "                    => Whitespace);
        ("#c123"                => Number("#c123")),
                         (0, 2) => err_lexer_invalid_number_prefix;
        (" "                    => Whitespace);
        ("#c-123"               => Number("#c-123")),
                         (0, 2) => err_lexer_invalid_number_prefix;
        (" "                    => Whitespace);
        ("#d#a0"                => Number("#d#a0")),
                         (2, 4) => err_lexer_invalid_number_prefix;
        (" "                    => Whitespace);
        ("#x##a0"               => Number("#x##a0")),
                         (2, 3) => err_lexer_invalid_number_prefix,
                         (3, 5) => err_lexer_invalid_number_prefix;
        (" "                    => Whitespace);
        ("#o#"                  => Identifier("")),
                         (2, 3) => err_lexer_invalid_number_prefix,
                         (0, 3) => err_lexer_prefixed_identifier;
        (" "                    => Whitespace);
        ("#x#"                  => Identifier("")),
                         (2, 3) => err_lexer_invalid_number_prefix,
                         (0, 3) => err_lexer_prefixed_identifier;
        (" "                    => Whitespace);
        ("#"                    => Identifier("")),
                         (0, 1) => err_lexer_invalid_number_prefix,
                         (0, 1) => err_lexer_prefixed_identifier;
    }
}

#[test]
fn recover_numbers_integer_invalid_digits() {
    check! {
        ("123BOMB"              => Number("123BOMB")),
                        (3,  4) => err_lexer_invalid_number_character,
                        (4,  5) => err_lexer_invalid_number_character,
                        (5,  6) => err_lexer_invalid_number_character,
                        (6,  7) => err_lexer_invalid_number_character;
        (" "                    => Whitespace);
        ("#b010051011b"         => Number("#b010051011b")),
                        (6,  7) => err_lexer_invalid_number_digit,
                       (11, 12) => err_lexer_invalid_number_character;
        (" "                    => Whitespace);
        ("#O0123456789abc"      => Number("#O0123456789abc")),
                       (10, 11) => err_lexer_invalid_number_digit,
                       (11, 12) => err_lexer_invalid_number_digit,
                       (12, 13) => err_lexer_invalid_number_character,
                       (13, 14) => err_lexer_invalid_number_character,
                       (14, 15) => err_lexer_invalid_number_character;
        (" "                    => Whitespace);
        ("#d123123c"            => Number("#d123123c")),
                        (8,  9) => err_lexer_invalid_number_character;
    }
}

#[test]
fn recover_numbers_integer_invalid_characters() {
    check! {
        ("0!OMG*I*CAN"          => Number("0!OMG*I*CAN")),
                        (1,  2) => err_lexer_invalid_number_character,
                        (2,  3) => err_lexer_invalid_number_character,
                        (3,  4) => err_lexer_invalid_number_character,
                        (4,  5) => err_lexer_invalid_number_character,
                        (5,  6) => err_lexer_invalid_number_character,
                        (6,  7) => err_lexer_invalid_number_character,
                        (7,  8) => err_lexer_invalid_number_character,
                        (8,  9) => err_lexer_invalid_number_character,
                        (9, 10) => err_lexer_invalid_number_character,
                       (10, 11) => err_lexer_invalid_number_character;
        (" "                    => Whitespace);
        ("#xDEADFOOD"           => Number("#xDEADFOOD")),
                        (7,  8) => err_lexer_invalid_number_character,
                        (8,  9) => err_lexer_invalid_number_character;
        (" "                    => Whitespace);
        ("1\u{2}3\u{4}"         => Number("1\u{2}3\u{4}")),
                        (1,  2) => err_lexer_invalid_number_character,
                        (3,  4) => err_lexer_invalid_number_character;
        (" "                    => Whitespace);
        ("#\u{0435}9\u{0434}\u{0443}\u{0440}\u{0430}!" => Number("#\u{0435}9\u{0434}\u{0443}\u{0440}\u{0430}!")),
                        (0,  3) => err_lexer_invalid_number_prefix,
                        (4,  6) => err_lexer_invalid_number_character,
                        (6,  8) => err_lexer_invalid_number_character,
                        (8, 10) => err_lexer_invalid_number_character,
                       (10, 12) => err_lexer_invalid_number_character,
                       (12, 13) => err_lexer_invalid_number_character;
        (" "                    => Whitespace);
        ("#x0C\u{0327}FE"       => Number("#x0C\u{0327}FE")),
                        (4,  6) => err_lexer_invalid_number_character;
        (" "                    => Whitespace);
        ("123#567"              => Number("123#567")),
                        (3,  4) => err_lexer_invalid_number_character;
    }
}

#[test]
fn recover_numbers_integer_multiple_signs() {
    check! {
        // All of these are peculiar identifiers, unfortunately.
        ("++2"                  => Identifier("++2"));
        (" "                    => Whitespace);
        ("#x--DEAD"             => Identifier("--DEAD")),
                         (0, 2) => err_lexer_prefixed_identifier;
        (" "                    => Whitespace);
        ("#i+-+-+"              => Identifier("+-+-+")),
                         (0, 2) => err_lexer_prefixed_identifier;
        (" "                    => Whitespace);
        // And this is treated as a complex number.
        ("2++"                  => Number("2++")),
                         (2, 3) => err_lexer_invalid_number_character,
                         (3, 3) => err_lexer_digits_missing,
                         (3, 3) => err_lexer_missing_i;
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Numbers: floating point

#[test]
fn numbers_float_decimal() {
    check! {
        ("123.456"      => Number("123.456"));
        (" "            => Whitespace);
        ("0.0000000001" => Number("0.0000000001"));
        (" "            => Whitespace);
        ("9."           => Number("9."));
        (" "            => Whitespace);
        ("+.9"          => Number("+.9"));
        (" "            => Whitespace);
        ("#i2.2"        => Number("#i2.2"));
        (" "            => Whitespace);
        ("#E-3."        => Number("#E-3."));
        (" "            => Whitespace);
        ("#I.5"         => Number("#I.5"));
    }
}

#[test]
fn numbers_float_exponent() {
    check! {
        ("123e20"       => Number("123e20"));
        (" "            => Whitespace);
        ("0E0"          => Number("0E0"));
        (" "            => Whitespace);
        ("-5e+999"      => Number("-5e+999"));
        (" "            => Whitespace);
        ("+92382e-1231" => Number("+92382e-1231"));
        (" "            => Whitespace);
        ("#E2E4"        => Number("#E2E4"));
    }
}

#[test]
fn numbers_float_decimal_exponent() {
    check! {
        ("3.14e20"      => Number("3.14e20"));
        (" "            => Whitespace);
        ("-.0E+0"       => Number("-.0E+0"));
        (" "            => Whitespace);
        ("#e+9.e-8"     => Number("#e+9.e-8"));
        (" "            => Whitespace);
        ("#d123.45e6"   => Number("#d123.45e6"));
    }
}

#[test]
fn numbers_float_exponent_precision_specs() {
    check! {
        ("3e10"         => Number("3e10"));
        (" "            => Whitespace);
        ("3E10"         => Number("3E10"));
        (" "            => Whitespace);
        ("3s10"         => Number("3s10"));
        (" "            => Whitespace);
        ("3S10"         => Number("3S10"));
        (" "            => Whitespace);
        ("3f10"         => Number("3f10"));
        (" "            => Whitespace);
        ("3F10"         => Number("3F10"));
        (" "            => Whitespace);
        ("3l10"         => Number("3l10"));
        (" "            => Whitespace);
        ("3L10"         => Number("3L10"));
        (" "            => Whitespace);
        ("3d10"         => Number("3d10"));
        (" "            => Whitespace);
        ("3D10"         => Number("3D10"));
        (" "            => Whitespace);
        ("3.f0"         => Number("3.f0"));
        (" "            => Whitespace);
        ("-.9s+5"       => Number("-.9s+5"));
    }
}

#[test]
fn numbers_float_ieee754_specials() {
    check! {
        ("+inf.0"       => Number("+inf.0"));
        (" "            => Whitespace);
        ("-INF.0"       => Number("-INF.0"));
        (" "            => Whitespace);
        ("+NaN.0"       => Number("+NaN.0"));
        (" "            => Whitespace);
        ("-nAn.0"       => Number("-nAn.0"));
        (" "            => Whitespace);
        ("#b+inf.0"     => Number("#b+inf.0"));
        (" "            => Whitespace);
        ("#o-inf.0"     => Number("#o-inf.0"));
        (" "            => Whitespace);
        ("#X+NAN.0"     => Number("#X+NAN.0"));
        (" "            => Whitespace);
        ("#e-Inf.0"     => Number("#e-Inf.0"));
        (" "            => Whitespace);
        ("#i#d+nan.0"   => Number("#i#d+nan.0"));
    }
}

#[test]
fn recover_numbers_float_nondecimal() {
    check! {
        ("#D3.14"           => Number("#D3.14"));
        (" "                => Whitespace);
        ("#o.234"           => Number("#o.234")),
                     (0, 2) => err_lexer_nondecimal_real;
        (" "                => Whitespace);
        ("#xFF.37"          => Number("#xFF.37")),
                     (0, 2) => err_lexer_nondecimal_real;
        (" "                => Whitespace);
        ("#B100110."        => Number("#B100110.")),
                     (0, 2) => err_lexer_nondecimal_real;
        (" "                => Whitespace);
        ("#d.0e+1"          => Number("#d.0e+1"));
        (" "                => Whitespace);
        ("#O5.ff"           => Number("#O5.ff")),
                     (4, 5) => err_lexer_invalid_number_character,
                     (5, 6) => err_lexer_invalid_number_character,
                     (0, 2) => err_lexer_nondecimal_real;
        (" "                => Whitespace);
        ("#X5.5ef"          => Number("#X5.5ef")),
                     (0, 2) => err_lexer_nondecimal_real;
        (" "                => Whitespace);
        ("#b1.d-1"          => Number("#b1.d-1")),
                     (4, 5) => err_lexer_invalid_number_character,
                     (7, 7) => err_lexer_missing_i,
                     (0, 2) => err_lexer_nondecimal_real;
        (" "                => Whitespace);
        ("#d42e+9"          => Number("#d42e+9"));
        (" "                => Whitespace);
        ("#b010010e-01101"  => Number("#b010010e-01101")),
                     (8, 9) => err_lexer_invalid_number_character,
                   (15, 15) => err_lexer_missing_i;
        (" "                => Whitespace);
        ("#b111s123"        => Number("#b111s123")),
                     (5, 6) => err_lexer_invalid_number_character,
                     (7, 8) => err_lexer_invalid_number_digit,
                     (8, 9) => err_lexer_invalid_number_digit;
        (" "                => Whitespace);
        ("#i#o373e0"        => Number("#i#o373e0")),
                     (7, 8) => err_lexer_invalid_number_character;
        (" "                => Whitespace);
        ("#xabcdef"         => Number("#xabcdef"));
        (" "                => Whitespace);
        ("#xabcde+f"        => Number("#xabcde+f")),
                     (9, 9) => err_lexer_missing_i;
    }
}

#[test]
fn recover_numbers_float_digits_missing() {
    check! {
        (".e0"              => Identifier(".e0"));
        (" "                => Whitespace);
        ("#i.e0"            => Identifier(".e0")),
                     (0, 2) => err_lexer_prefixed_identifier;
        (" "                => Whitespace);
        ("#d.e0"            => Identifier(".e0")),
                     (0, 2) => err_lexer_prefixed_identifier;
        (" "                => Whitespace);
        ("#.e0"             => Identifier(".e0")),
                     (0, 1) => err_lexer_invalid_number_prefix,
                     (0, 1) => err_lexer_prefixed_identifier;
        (" "                => Whitespace);
        ("#."               => Number("#.")),
                     (0, 1) => err_lexer_invalid_number_prefix,
                     (1, 2) => err_lexer_digits_missing;
        (" "                => Whitespace);
        ("#de"              => Identifier("e")),
                     (0, 2) => err_lexer_prefixed_identifier;
        (" "                => Whitespace);
        ("#de+20"           => Identifier("e+20")),
                     (0, 2) => err_lexer_prefixed_identifier;
    }
}

#[test]
fn recover_numbers_float_exponent_missing() {
    check! {
        ("5.0e"             => Number("5.0e")),
                     (4, 4) => err_lexer_digits_missing;
        (" "                => Whitespace);
        ("0e+"              => Number("0e+")),
                     (3, 3) => err_lexer_digits_missing;
        (" "                => Whitespace);
        ("123e-"            => Number("123e-")),
                     (5, 5) => err_lexer_digits_missing;
        (" "                => Whitespace);
        ("#b.1e"            => Number("#b.1e")),
                     (4, 5) => err_lexer_invalid_number_character,
                     (0, 2) => err_lexer_nondecimal_real;
        (" "                => Whitespace);
        ("#x-BAD-"          => Number("#x-BAD-")),
                     (7, 7) => err_lexer_digits_missing,
                     (7, 7) => err_lexer_missing_i;
        (" "                => Whitespace);
        ("#xBAD"            => Number("#xBAD"));
        (" "                => Whitespace);
        ("100e!!!"          => Number("100e!!!")),
                     (4, 5) => err_lexer_invalid_number_character,
                     (5, 6) => err_lexer_invalid_number_character,
                     (6, 7) => err_lexer_invalid_number_character,
                     (4, 7) => err_lexer_digits_missing;
        (" "                => Whitespace);
        ("+2E"              => Number("+2E")),
                     (3, 3) => err_lexer_digits_missing;
    }
}

#[test]
fn recover_numbers_float_exponent_invalid_characters() {
    check! {
        ("100e++++"         => Number("100e++++")),
                     (5, 6) => err_lexer_invalid_number_character,
                     (6, 7) => err_lexer_invalid_number_character,
                     (7, 8) => err_lexer_invalid_number_character,
                     (8, 8) => err_lexer_digits_missing;
        (" "                => Whitespace);
        ("12.3E\u{0}"       => Number("12.3E\u{0}")),
                     (5, 6) => err_lexer_invalid_number_character,
                     (5, 6) => err_lexer_digits_missing;
        (" "                => Whitespace);
        (".0e\u{2212}9"     => Number(".0e\u{2212}9")),
                     (3, 6) => err_lexer_invalid_number_character;
        (" "                => Whitespace);
        ("5343x+23"         => Number("5343x+23")),
                     (4, 5) => err_lexer_invalid_number_character,
                     (8, 8) => err_lexer_missing_i;
    }
}

#[test]
fn recover_numbers_float_exponent_multiple_exponents() {
    check! {
        ("1e2e3e4"          => Number("1e2e3e4")),
                     (3, 4) => err_lexer_invalid_number_character,
                     (5, 6) => err_lexer_invalid_number_character;
        (" "                => Whitespace);
        ("#o9e8s1"          => Number("#o9e8s1")),
                     (2, 3) => err_lexer_invalid_number_digit,
                     (3, 4) => err_lexer_invalid_number_character,
                     (4, 5) => err_lexer_invalid_number_digit,
                     (5, 6) => err_lexer_invalid_number_character;
        (" "                => Whitespace);
        ("#x1e-2e3e+4"      => Number("#x1e-2e3e+4")),
                     (4, 9) => err_lexer_extra_complex_part,
                   (11, 11) => err_lexer_missing_i;
        (" "                => Whitespace);
        ("1e+2E-4S+D"       => Number("1e+2E-4S+D")),
                     (4, 5) => err_lexer_invalid_number_character,
                     (5, 6) => err_lexer_invalid_number_character,
                     (7, 8) => err_lexer_invalid_number_character,
                     (8, 9) => err_lexer_invalid_number_character,
                    (9, 10) => err_lexer_invalid_number_character;
    }
}

#[test]
fn recover_numbers_float_misplaced_decimal_dot() {
    check! {
        ("127.0.0.1"        => Number("127.0.0.1")),
                     (5, 6) => err_lexer_invalid_number_character,
                     (7, 8) => err_lexer_invalid_number_character;
        (" "                => Whitespace);
        ("343e+5.43"        => Number("343e+5.43")),
                     (6, 7) => err_lexer_invalid_number_character;
        (" "                => Whitespace);
        ("543s.45"          => Number("543s.45")),
                     (4, 5) => err_lexer_invalid_number_character;
        (" "                => Whitespace);
        ("9D-.9"            => Number("9D-.9")),
                     (3, 4) => err_lexer_invalid_number_character;
        (" "                => Whitespace);
        ("9.33E123."        => Number("9.33E123.")),
                     (8, 9) => err_lexer_invalid_number_character;
    }
}

#[test]
fn recover_numbers_float_iee754_specials() {
    check! {
        ("+inf"             => Identifier("+inf"));
        (" "                => Whitespace);
        ("-infinity"        => Identifier("-infinity"));
        (" "                => Whitespace);
        ("#d-infinity"      => Identifier("-infinity")),
                    (0,  2) => err_lexer_prefixed_identifier;
        (" "                => Whitespace);
        ("+inf.01234"       => Number("+inf.01234")),
                    (6, 10) => err_lexer_infnan_suffix;
        (" "                => Whitespace);
        ("-NaN.1"           => Identifier("-NaN.1"));
        (" "                => Whitespace);
        ("+nan.0e+10"       => Number("+nan.0e+10")),
                    (6, 10) => err_lexer_infnan_suffix;
        (" "                => Whitespace);
        ("+nane+5"          => Identifier("+nane+5"));
        (" "                => Whitespace);
        ("+nan.e5"          => Identifier("+nan.e5"));
        (" "                => Whitespace);
        ("inf.0"            => Identifier("inf.0"));
        (" "                => Whitespace);
        ("NaN.0"            => Identifier("NaN.0"));
        (" "                => Whitespace);
        ("#xInf.0"          => Identifier("Inf.0")),
                     (0, 2) => err_lexer_prefixed_identifier;
        (" "                => Whitespace);
        ("#inAn.0"          => Identifier("nAn.0")),
                     (0, 2) => err_lexer_prefixed_identifier;
    }
}

#[test]
fn recover_numbers_float_multiple_signs() {
    check! {
        ("---inf.0"         => Identifier("---inf.0")); // peculiar
        (" "                => Whitespace);
        ("++2.345"          => Identifier("++2.345")); // peculiar
        (" "                => Whitespace);
        ("#d++5e10"         => Identifier("++5e10")), // peculiar
                     (0, 2) => err_lexer_prefixed_identifier;
        (" "                => Whitespace);
        ("1e++5"            => Number("1e++5")),
                     (3, 4) => err_lexer_invalid_number_character;
        (" "                => Whitespace);
        ("1.23S-+--+-++5"   => Number("1.23S-+--+-++5")),
                     (6, 7) => err_lexer_invalid_number_character,
                     (7, 8) => err_lexer_invalid_number_character,
                     (8, 9) => err_lexer_invalid_number_character,
                    (9, 10) => err_lexer_invalid_number_character,
                   (10, 11) => err_lexer_invalid_number_character,
                   (11, 12) => err_lexer_invalid_number_character,
                   (12, 13) => err_lexer_invalid_number_character;
        (" "                => Whitespace);
        ("1.2S---"          => Number("1.2S---")),
                     (5, 6) => err_lexer_invalid_number_character,
                     (6, 7) => err_lexer_invalid_number_character,
                     (7, 7) => err_lexer_digits_missing;
        (" "                => Whitespace);
        ("1.2---"           => Number("1.2---")),
                     (4, 5) => err_lexer_invalid_number_character,
                     (5, 6) => err_lexer_invalid_number_character,
                     (6, 6) => err_lexer_digits_missing,
                     (6, 6) => err_lexer_missing_i;
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Numbers: rational

#[test]
fn numbers_rational_simple() {
    check! {
        ("1/2"              => Number("1/2"));
        (" "                => Whitespace);
        ("123456789/12"     => Number("123456789/12"));
        (" "                => Whitespace);
        ("1/23456789"       => Number("1/23456789"));
        (" "                => Whitespace);
        ("0/0"              => Number("0/0"));
    }
}

#[test]
fn numbers_rational_prefixed() {
    check! {
        ("+5/7"             => Number("+5/7"));
        (" "                => Whitespace);
        ("-11/2"            => Number("-11/2"));
        (" "                => Whitespace);
        ("#D#i1/2"          => Number("#D#i1/2"));
        (" "                => Whitespace);
        ("#e+1/12"          => Number("#e+1/12"));
    }
}

#[test]
fn numbers_rational_nondecimal() {
    check! {
        ("#b11101011/101"   => Number("#b11101011/101"));
        (" "                => Whitespace);
        ("#o-755/777"       => Number("#o-755/777"));
        (" "                => Whitespace);
        ("#xDEAD/BEEF"      => Number("#xDEAD/BEEF"));
    }
}

#[test]
fn recover_numbers_rational_missing_numerator() {
    check! {
        ("+/9"              => Identifier("+/9"));
        (" "                => Whitespace);
        ("#d+/9"            => Identifier("+/9")),
                     (0, 2) => err_lexer_prefixed_identifier;
        (" "                => Whitespace);
        ("#e/0"             => Identifier("/0")),
                     (0, 2) => err_lexer_prefixed_identifier;
        (" "                => Whitespace);
        ("/123"             => Identifier("/123"));
    }
}

#[test]
fn recover_numbers_rational_missing_denominator() {
    check! {
        ("5/"               => Number("5/")),
                     (2, 2) => err_lexer_digits_missing;
        (" "                => Whitespace);
        ("#i-9/"            => Number("#i-9/")),
                     (5, 5) => err_lexer_digits_missing;
    }
}

#[test]
fn recover_numbers_rational_denominator_sign() {
    check! {
        ("+1/-2"            => Number("+1/-2")),
                     (3, 4) => err_lexer_invalid_number_character;
        (" "                => Whitespace);
        ("3/+4"             => Number("3/+4")),
                     (2, 3) => err_lexer_invalid_number_character;
    }
}

#[test]
fn recover_numbers_rational_exponent() {
    check! {
        ("1/2e10"           => Number("1/2e10")),
                     (2, 6) => err_lexer_noninteger_rational;
        (" "                => Whitespace);
        ("1/e10"            => Number("1/e10")),
                     (2, 2) => err_lexer_digits_missing,
                     (2, 5) => err_lexer_noninteger_rational;
        (" "                => Whitespace);
        ("1/10e+"           => Number("1/10e+")),
                     (6, 6) => err_lexer_digits_missing,
                     (2, 6) => err_lexer_noninteger_rational;
        (" "                => Whitespace);
        ("1/10e+1"          => Number("1/10e+1")),
                     (2, 7) => err_lexer_noninteger_rational;
        (" "                => Whitespace);
        ("#xA/ee5"          => Number("#xA/ee5"));
        (" "                => Whitespace);
        ("#xA/ee+5"         => Number("#xA/ee+5")),
                     (8, 8) => err_lexer_missing_i;
        (" "                => Whitespace);
        ("#xA/e-5"          => Number("#xA/e-5")),
                     (7, 7) => err_lexer_missing_i;
        (" "                => Whitespace);
        ("123e45/6"         => Number("123e45/6")),
                     (0, 6) => err_lexer_noninteger_rational;
        (" "                => Whitespace);
        ("#e+1e-2e3/4e5e+6" => Number("#e+1e-2e3/4e5e+6")),
                    (7,  8) => err_lexer_invalid_number_character,
                   (13, 14) => err_lexer_invalid_number_character,
                   (14, 15) => err_lexer_invalid_number_character,
                    (2,  9) => err_lexer_noninteger_rational,
                   (10, 16) => err_lexer_noninteger_rational;
    }
}

#[test]
fn recover_numbers_rational_fractional() {
    check! {
        ("123.456/789"      => Number("123.456/789")),
                     (0, 7) => err_lexer_noninteger_rational;
        (" "                => Whitespace);
        ("123/456.789"      => Number("123/456.789")),
                    (4, 11) => err_lexer_noninteger_rational;
        (" "                => Whitespace);
        ("+1/2.3e4"         => Number("+1/2.3e4")),
                     (3, 8) => err_lexer_noninteger_rational;
        (" "                => Whitespace);
        ("#o.1/2"           => Number("#o.1/2")),
                     (2, 4) => err_lexer_noninteger_rational,
                     (0, 2) => err_lexer_nondecimal_real;
        (" "                => Whitespace);
        ("./5"              => Identifier("./5"));
        (" "                => Whitespace);
        ("1./5"             => Number("1./5")),
                     (0, 2) => err_lexer_noninteger_rational;
        (" "                => Whitespace);
        ("1/.5"             => Number("1/.5")),
                     (2, 4) => err_lexer_noninteger_rational;
        (" "                => Whitespace);
        ("9/."              => Number("9/.")),
                     (2, 3) => err_lexer_digits_missing,
                     (2, 3) => err_lexer_noninteger_rational;
        (" "                => Whitespace);
        ("9./"              => Number("9./")),
                     (3, 3) => err_lexer_digits_missing,
                     (0, 2) => err_lexer_noninteger_rational;
        (" "                => Whitespace);
        ("1/2.3/4.5"        => Number("1/2.3/4.5")),
                     (5, 6) => err_lexer_invalid_number_character,
                     (7, 8) => err_lexer_invalid_number_character,
                     (2, 9) => err_lexer_noninteger_rational;
        (" "                => Whitespace);
        ("1.2/3.4/5"        => Number("1.2/3.4/5")),
                     (7, 8) => err_lexer_invalid_number_character,
                     (0, 3) => err_lexer_noninteger_rational,
                     (4, 9) => err_lexer_noninteger_rational;
    }
}

#[test]
fn recover_numbers_rational_ieee754_specials() {
    check! {
        ("+inf.0/9"         => Number("+inf.0/9")),
                     (0, 6) => err_lexer_infnan_rational;
        (" "                => Whitespace);
        ("#e-NaN.0/0"       => Number("#e-NaN.0/0")),
                     (2, 8) => err_lexer_infnan_rational;
        (" "                => Whitespace);
        ("5/+inf.0"         => Number("5/+inf.0")),
                     (2, 3) => err_lexer_invalid_number_character,
                     (2, 8) => err_lexer_infnan_rational;
        (" "                => Whitespace);
        ("0/+NaN.0"         => Number("0/+NaN.0")),
                     (2, 3) => err_lexer_invalid_number_character,
                     (2, 8) => err_lexer_infnan_rational;
        (" "                => Whitespace);
        ("+inf.0/-inf.0"    => Number("+inf.0/-inf.0")),
                    (7,  8) => err_lexer_invalid_number_character,
                    (0,  6) => err_lexer_infnan_rational,
                    (7, 13) => err_lexer_infnan_rational;
    }
}

#[test]
fn recover_number_rational_multiple_signs() {
    check! {
        ("++2/3"            => Identifier("++2/3")); // peculiar
        (" "                => Whitespace);
        ("-2/--3"           => Number("-2/--3")),
                     (3, 4) => err_lexer_invalid_number_character,
                     (4, 5) => err_lexer_invalid_number_character;
        (" "                => Whitespace);
        ("-2/3--"           => Number("-2/3--")),
                     (5, 6) => err_lexer_invalid_number_character,
                     (6, 6) => err_lexer_digits_missing,
                     (6, 6) => err_lexer_missing_i;
        (" "                => Whitespace);
        ("---nan.0/1"       => Identifier("---nan.0/1")); // peculiar
        (" "                => Whitespace);
        ("0/+++inf.0"       => Number("0/+++inf.0")),
                     (2, 3) => err_lexer_invalid_number_character,
                     (3, 4) => err_lexer_invalid_number_character,
                     (4, 5) => err_lexer_invalid_number_character,
                    (2, 10) => err_lexer_infnan_rational;
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Numbers: complex

#[test]
fn numbers_complex_integer() {
    check! {
        ("0+0i"             => Number("0+0i"));
        (" "                => Whitespace);
        ("-4-2i"            => Number("-4-2i"));
        (" "                => Whitespace);
        ("#XDEAD+BEEI"      => Number("#XDEAD+BEEI"));
        (" "                => Whitespace);
        ("#b11011+1i"       => Number("#b11011+1i"));
        (" "                => Whitespace);
        ("#o-775-400i"      => Number("#o-775-400i"));
        (" "                => Whitespace);
        ("#d9i"             => Number("#d9i"));
        (" "                => Whitespace);
        ("-0i"              => Number("-0i"));
        (" "                => Whitespace);
        ("#i+1231I"         => Number("#i+1231I"));
    }
}

#[test]
fn numbers_complex_peculiar() {
    check! {
        ("+i"               => Number("+i"));
        (" "                => Whitespace);
        ("-i"               => Number("-i"));
        (" "                => Whitespace);
        ("#x+i"             => Number("#x+i"));
        (" "                => Whitespace);
        ("#i-i"             => Number("#i-i"));
        (" "                => Whitespace);
        ("8+i"              => Number("8+i"));
        (" "                => Whitespace);
        ("#d-0-i"           => Number("#d-0-i"));
    }
}

#[test]
fn numbers_complex_float() {
    check! {
        ("1.2+3.4i"         => Number("1.2+3.4i"));
        (" "                => Whitespace);
        (".1-2.i"           => Number(".1-2.i"));
        (" "                => Whitespace);
        ("+1.+.2i"          => Number("+1.+.2i"));
        (" "                => Whitespace);
        ("-3+1.4i"          => Number("-3+1.4i"));
        (" "                => Whitespace);
        ("#i5.4+0i"         => Number("#i5.4+0i"));
        (" "                => Whitespace);
        ("-3.14I"           => Number("-3.14I"));
        (" "                => Whitespace);
        ("-3e+5-1e1i"       => Number("-3e+5-1e1i"));
        (" "                => Whitespace);
        ("+.8s92-432.f0I"   => Number("+.8s92-432.f0I"));
        (" "                => Whitespace);
        ("-9+5e2i"          => Number("-9+5e2i"));
        (" "                => Whitespace);
        ("3.14d5+2I"        => Number("3.14d5+2I"));
        (" "                => Whitespace);
        ("2.718-i"          => Number("2.718-i"));
        (" "                => Whitespace);
        ("-5e+100I"         => Number("-5e+100I"));
        (" "                => Whitespace);
        ("+.7e0I"           => Number("+.7e0I"));
        (" "                => Whitespace);
    }
}

#[test]
fn numbers_complex_infnan() {
    check! {
        ("+inf.0-nan.0i"    => Number("+inf.0-nan.0i"));
        (" "                => Whitespace);
        ("-NAN.0+INF.0I"    => Number("-NAN.0+INF.0I"));
        (" "                => Whitespace);
        ("123456-inf.0i"    => Number("123456-inf.0i"));
        (" "                => Whitespace);
        ("#x+NaN.0-ABCDI"   => Number("#x+NaN.0-ABCDI"));
        (" "                => Whitespace);
        ("#d#i-inf.0I"      => Number("#d#i-inf.0I"));
        (" "                => Whitespace);
        ("+NAN.0i"          => Number("+NAN.0i"));
        (" "                => Whitespace);
        ("#e+inf.0+i"       => Number("#e+inf.0+i"));
    }
}

#[test]
fn numbers_complex_rational() {
    check! {
        ("0/0+0/0i"         => Number("0/0+0/0i"));
        (" "                => Whitespace);
        ("-12/3-4/56i"      => Number("-12/3-4/56i"));
        (" "                => Whitespace);
        ("#e-1+2/3i"        => Number("#e-1+2/3i"));
        (" "                => Whitespace);
        ("#xDEAD-BE/EFi"    => Number("#xDEAD-BE/EFi"));
        (" "                => Whitespace);
        ("#o7/11-i"         => Number("#o7/11-i"));
        (" "                => Whitespace);
        ("2/3i"             => Number("2/3i"));
        (" "                => Whitespace);
        ("#i1/1i"           => Number("#i1/1i"));
    }
}

#[test]
fn numbers_complex_altogether() {
    check! {
        ("1.2+3/4i"         => Number("1.2+3/4i"));
        (" "                => Whitespace);
        ("+inf.0-4.2e1i"    => Number("+inf.0-4.2e1i"));
        (" "                => Whitespace);
        ("5/8-nan.0i"       => Number("5/8-nan.0i"));
    }
}

#[test]
fn numbers_complex_polar_integer() {
    check! {
        ("0@0"              => Number("0@0"));
        (" "                => Whitespace);
        ("+123@-456"        => Number("+123@-456"));
        (" "                => Whitespace);
        ("1@-0"             => Number("1@-0"));
        (" "                => Whitespace);
        ("+0@2"             => Number("+0@2"));
        (" "                => Whitespace);
        ("#xDEAD@BEEF"      => Number("#xDEAD@BEEF"));
        (" "                => Whitespace);
        ("#b1101@110101"    => Number("#b1101@110101"));
        (" "                => Whitespace);
        ("#I#O400@-755"     => Number("#I#O400@-755"));
    }
}

#[test]
fn numbers_complex_polar_float() {
    check! {
        ("1.2@3.4"          => Number("1.2@3.4"));
        (" "                => Whitespace);
        ("1.@.2"            => Number("1.@.2"));
        (" "                => Whitespace);
        (".3@4."            => Number(".3@4."));
        (" "                => Whitespace);
        ("+5.67e8@-9.f10"   => Number("+5.67e8@-9.f10"));
        (" "                => Whitespace);
        ("-0@-0e0"          => Number("-0@-0e0"));
        (" "                => Whitespace);
        ("1.e1@3"           => Number("1.e1@3"));
    }
}

#[test]
fn numbers_complex_polar_infnan() {
    check! {
        ("+inf.0@-nan.0"    => Number("+inf.0@-nan.0"));
        (" "                => Whitespace);
        ("#xDEAD@-inf.0"    => Number("#xDEAD@-inf.0"));
        (" "                => Whitespace);
        ("#E+NAN.0@0"       => Number("#E+NAN.0@0"));
        (" "                => Whitespace);
        ("-nAn.0@+iNF.0"    => Number("-nAn.0@+iNF.0"));
    }
}

#[test]
fn numbers_complex_polar_rational() {
    check! {
        ("1/2@3/4"          => Number("1/2@3/4"));
        (" "                => Whitespace);
        ("-12/3@+4/56"      => Number("-12/3@+4/56"));
        (" "                => Whitespace);
        ("0@-7/8"           => Number("0@-7/8"));
        (" "                => Whitespace);
        ("#b+1101/1@-1"     => Number("#b+1101/1@-1"));
    }
}

#[test]
fn numbers_complex_polar_altogether() {
    check! {
        ("1/2@-3.4e+56"     => Number("1/2@-3.4e+56"));
        (" "                => Whitespace);
        ("#x+inf.0@4/1"     => Number("#x+inf.0@4/1"));
        (" "                => Whitespace);
        ("3.4@-NAN.0"       => Number("3.4@-NAN.0"));
        (" "                => Whitespace);
        ("#i-5.e23@+7/8"    => Number("#i-5.e23@+7/8"));
    }
}

#[test]
fn recover_numbers_complex_missing_digits() {
    check! {
        ("123+"             => Number("123+")),
                     (4, 4) => err_lexer_digits_missing,
                     (4, 4) => err_lexer_missing_i;
        (" "                => Whitespace);
        ("#d#i-45+e10i"     => Number("#d#i-45+e10i")),
                     (8, 8) => err_lexer_digits_missing;
        (" "                => Whitespace);
        ("0+e+0i"           => Number("0+e+0i")),
                     (2, 2) => err_lexer_digits_missing;
        (" "                => Whitespace);
        ("-e45i"            => Identifier("-e45i")); // peculiar
        (" "                => Whitespace);
        ("-23/I"            => Number("-23/I")),
                     (4, 4) => err_lexer_digits_missing;
        (" "                => Whitespace);
        ("+/23i"            => Identifier("+/23i")); // peculiar
    }
}

#[test]
fn recover_numbers_complex_no_digits() {
    check! {
        ("123+hi"           => Number("123+hi")),
                     (4, 5) => err_lexer_invalid_number_character,
                     (4, 5) => err_lexer_digits_missing;
        (" "                => Whitespace);
        ("456-omg"          => Number("456-omg")),
                     (4, 5) => err_lexer_invalid_number_character,
                     (5, 6) => err_lexer_invalid_number_character,
                     (6, 7) => err_lexer_invalid_number_character,
                     (4, 7) => err_lexer_digits_missing,
                     (7, 7) => err_lexer_missing_i;
        (" "                => Whitespace);
        ("456-omfg"         => Number("456-omfg")),
                     (4, 5) => err_lexer_invalid_number_character,
                     (5, 6) => err_lexer_invalid_number_character,
                     (4, 6) => err_lexer_digits_missing,
                     (7, 8) => err_lexer_invalid_number_character,
                     (7, 8) => err_lexer_digits_missing,
                     (8, 8) => err_lexer_missing_i;
        (" "                => Whitespace);
        ("789+ei"           => Number("789+ei")),
                     (4, 4) => err_lexer_digits_missing,
                     (5, 5) => err_lexer_digits_missing;
        (" "                => Whitespace);
        ("#xABC-1/i"        => Number("#xABC-1/i")),
                     (8, 8) => err_lexer_digits_missing;
        (" "                => Whitespace);
        ("#xDEF+/2i"        => Number("#xDEF+/2i")),
                     (6, 6) => err_lexer_digits_missing;
        (" "                => Whitespace);
        ("#o755-/I"         => Number("#o755-/I")),
                     (6, 6) => err_lexer_digits_missing,
                     (7, 7) => err_lexer_digits_missing;
    }
}

#[test]
fn recover_numbers_complex_missing_i() {
    check! {
        ("1+2"              => Number("1+2")),
                     (3, 3) => err_lexer_missing_i;
        (" "                => Whitespace);
        ("#xB00-CAFE"       => Number("#xB00-CAFE")),
                   (10, 10) => err_lexer_missing_i;
        (" "                => Whitespace);
        ("3.14-3/"          => Number("3.14-3/")),
                     (7, 7) => err_lexer_digits_missing,
                     (7, 7) => err_lexer_missing_i;
        (" "                => Whitespace);
        ("0+2/3"            => Number("0+2/3")),
                     (5, 5) => err_lexer_missing_i;
        (" "                => Whitespace);
        ("0+17e-2"          => Number("0+17e-2")),
                     (7, 7) => err_lexer_missing_i;
        (" "                => Whitespace);
        ("2+e+2"            => Number("2+e+2")),
                     (2, 2) => err_lexer_digits_missing,
                     (5, 5) => err_lexer_missing_i;
        (" "                => Whitespace);
        ("+4.2-3.4j"        => Number("+4.2-3.4j")),
                     (8, 9) => err_lexer_invalid_number_character,
                     (9, 9) => err_lexer_missing_i;
    }
}

#[test]
fn recover_numbers_complex_misplaced_i() {
    check! {
        ("2i+10"            => Number("2i+10")),
                     (1, 2) => err_lexer_misplaced_i,
                     (5, 5) => err_lexer_missing_i;
        (" "                => Whitespace);
        ("-4i+3.e10"        => Number("-4i+3.e10")),
                     (2, 3) => err_lexer_misplaced_i,
                     (9, 9) => err_lexer_missing_i;
        (" "                => Whitespace);
        ("+3.14e10i+5"      => Number("+3.14e10i+5")),
                     (8, 9) => err_lexer_misplaced_i,
                   (11, 11) => err_lexer_missing_i;
        (" "                => Whitespace);
        ("#o2/3i-0"         => Number("#o2/3i-0")),
                     (5, 6) => err_lexer_misplaced_i,
                     (8, 8) => err_lexer_missing_i;
        (" "                => Whitespace);
        ("123hi+4"          => Number("123hi+4")),
                     (3, 4) => err_lexer_invalid_number_character,
                     (4, 5) => err_lexer_misplaced_i,
                     (7, 7) => err_lexer_missing_i;
        (" "                => Whitespace);
        ("9irk+8"           => Number("9irk+8")),
                     (1, 2) => err_lexer_invalid_number_character,
                     (2, 3) => err_lexer_invalid_number_character,
                     (3, 4) => err_lexer_invalid_number_character,
                     (6, 6) => err_lexer_missing_i;
        (" "                => Whitespace);
        ("5ii-1"            => Number("5ii-1")),
                     (1, 2) => err_lexer_invalid_number_character,
                     (2, 3) => err_lexer_misplaced_i,
                     (5, 5) => err_lexer_missing_i;
    }
}

#[test]
fn recover_numbers_complex_missing_denominator() {
    check! {
        ("+1/+3"            => Number("+1/+3")),
                     (3, 4) => err_lexer_invalid_number_character;
        (" "                => Whitespace);
        ("+1/+3i"           => Number("+1/+3i")),
                     (3, 4) => err_lexer_invalid_number_character;
        (" "                => Whitespace);
        ("#x-0/-nan.0"      => Number("#x-0/-nan.0")),
                     (5, 6) => err_lexer_invalid_number_character,
                    (5, 11) => err_lexer_infnan_rational;
    }
}

#[test]
fn recover_numbers_complex_multiple_exponents() {
    check! {
        ("+1e+2f+3+4i"      => Number("+1e+2f+3+4i")),
                     (5, 6) => err_lexer_invalid_number_character,
                     (6, 7) => err_lexer_invalid_number_character;
        (" "                => Whitespace);
        ("-5-6s-7-8d-9i"    => Number("-5-6s-7-8d-9i")),
                     (2, 7) => err_lexer_extra_complex_part;
        (" "                => Whitespace);
        ("+1e-2+e-3+i"      => Number("+1e-2+e-3+i")),
                     (6, 6) => err_lexer_digits_missing,
                     (5, 9) => err_lexer_extra_complex_part;
        (" "                => Whitespace);
        ("+1e-2e-3+i"       => Number("+1e-2e-3+i")),
                     (5, 6) => err_lexer_invalid_number_character,
                     (6, 7) => err_lexer_invalid_number_character;
    }
}

#[test]
fn recover_numbers_complex_multiple_parts() {
    check! {
        ("1+2+3"            => Number("1+2+3")),
                     (1, 3) => err_lexer_extra_complex_part,
                     (5, 5) => err_lexer_missing_i;
        (" "                => Whitespace);
        ("1i+2i+3i"         => Number("1i+2i+3i")),
                     (1, 2) => err_lexer_misplaced_i,
                     (4, 5) => err_lexer_misplaced_i,
                     (2, 5) => err_lexer_extra_complex_part;
        (" "                => Whitespace);
        ("1+2i+3i"          => Number("1+2i+3i")),
                     (3, 4) => err_lexer_misplaced_i,
                     (1, 4) => err_lexer_extra_complex_part;
        (" "                => Whitespace);
        ("1i+2i+3"          => Number("1i+2i+3")),
                     (1, 2) => err_lexer_misplaced_i,
                     (4, 5) => err_lexer_misplaced_i,
                     (2, 5) => err_lexer_extra_complex_part,
                     (7, 7) => err_lexer_missing_i;
        (" "                => Whitespace);
        ("1+2+3j+4i"        => Number("1+2+3j+4i")),
                     (1, 3) => err_lexer_extra_complex_part,
                     (5, 6) => err_lexer_invalid_number_character,
                     (3, 6) => err_lexer_extra_complex_part;
    }
}

#[test]
fn recover_numbers_complex_duplicate_signs() {
    check! {
        ("-2++3i"           => Number("-2++3i")),
                     (3, 4) => err_lexer_invalid_number_character;
        (" "                => Whitespace);
        ("--2+3i"           => Identifier("--2+3i")); // peculiar
        (" "                => Whitespace);
        ("7/8-+3.14i"       => Number("7/8-+3.14i")),
                     (4, 5) => err_lexer_invalid_number_character;
        (" "                => Whitespace);
        ("11.e5+-i"         => Number("11.e5+-i")),
                     (6, 7) => err_lexer_invalid_number_character;
        (" "                => Whitespace);
        ("123e++45"         => Number("123e++45")),
                     (5, 6) => err_lexer_invalid_number_character;
        (" "                => Whitespace);
        ("123d--45i"        => Number("123d--45i")),
                     (5, 6) => err_lexer_invalid_number_character;
        (" "                => Whitespace);
        ("456S+-+7+8i"      => Number("456S+-+7+8i")),
                     (5, 6) => err_lexer_invalid_number_character,
                     (6, 7) => err_lexer_invalid_number_character;
        (" "                => Whitespace);
        ("0-5e++7i"         => Number("0-5e++7i")),
                     (5, 6) => err_lexer_invalid_number_character;
        (" "                => Whitespace);
        ("2i+++4"           => Number("2i+++4")),
                     (1, 2) => err_lexer_misplaced_i,
                     (3, 4) => err_lexer_invalid_number_character,
                     (4, 5) => err_lexer_invalid_number_character,
                     (6, 6) => err_lexer_missing_i;
        (" "                => Whitespace);
        ("+4----inf.0i"     => Number("+4----inf.0i")),
                     (3, 4) => err_lexer_invalid_number_character,
                     (4, 5) => err_lexer_invalid_number_character,
                     (5, 6) => err_lexer_invalid_number_character;
    }
}

#[test]
fn recover_numbers_complex_polar_missing_digits() {
    check! {
        ("42@"              => Number("42@")),
                     (3, 3) => err_lexer_digits_missing;
        (" "                => Whitespace);
        ("42@+"             => Number("42@+")),
                     (4, 4) => err_lexer_digits_missing;
        (" "                => Whitespace);
        ("42@omg"           => Number("42@omg")),
                     (3, 4) => err_lexer_invalid_number_character,
                     (4, 5) => err_lexer_invalid_number_character,
                     (5, 6) => err_lexer_invalid_number_character,
                     (3, 6) => err_lexer_digits_missing;
        (" "                => Whitespace);
        ("@42"              => Identifier("@42")); // peculiar
        (" "                => Whitespace);
        ("42@/"             => Number("42@/")),
                     (3, 3) => err_lexer_digits_missing,
                     (4, 4) => err_lexer_digits_missing;
        (" "                => Whitespace);
        ("42@1/"            => Number("42@1/")),
                     (5, 5) => err_lexer_digits_missing;
        (" "                => Whitespace);
        ("42@/2"            => Number("42@/2")),
                     (3, 3) => err_lexer_digits_missing;
        (" "                => Whitespace);
        ("42@+/"            => Number("42@+/")),
                     (4, 4) => err_lexer_digits_missing,
                     (5, 5) => err_lexer_digits_missing;
        (" "                => Whitespace);
        ("42@-1/"           => Number("42@-1/")),
                     (6, 6) => err_lexer_digits_missing;
        (" "                => Whitespace);
        ("42@+/2"           => Number("42@+/2")),
                     (4, 4) => err_lexer_digits_missing;
        (" "                => Whitespace);
        ("42@e10"           => Number("42@e10")),
                     (3, 3) => err_lexer_digits_missing;
        (" "                => Whitespace);
        ("42@10E"           => Number("42@10E")),
                     (6, 6) => err_lexer_digits_missing;
        (" "                => Whitespace);
        ("42@-e-"           => Number("42@-e-")),
                     (4, 4) => err_lexer_digits_missing,
                     (6, 6) => err_lexer_digits_missing;
    }
}

#[test]
fn recover_numbers_complex_polar_no_digits() {
    check! {
        ("123@omg"          => Number("123@omg")),
                     (4, 5) => err_lexer_invalid_number_character,
                     (5, 6) => err_lexer_invalid_number_character,
                     (6, 7) => err_lexer_invalid_number_character,
                     (4, 7) => err_lexer_digits_missing;
        (" "                => Whitespace);
        ("456@+omg"         => Number("456@+omg")),
                     (5, 6) => err_lexer_invalid_number_character,
                     (6, 7) => err_lexer_invalid_number_character,
                     (7, 8) => err_lexer_invalid_number_character,
                     (5, 8) => err_lexer_digits_missing;
        (" "                => Whitespace);
        ("456@-omfg"        => Number("456@-omfg")),
                     (5, 6) => err_lexer_invalid_number_character,
                     (6, 7) => err_lexer_invalid_number_character,
                     (5, 7) => err_lexer_digits_missing,
                     (8, 9) => err_lexer_invalid_number_character,
                     (8, 9) => err_lexer_digits_missing;
        (" "                => Whitespace);
        ("12e+@-e"          => Number("12e+@-e")),
                     (4, 4) => err_lexer_digits_missing,
                     (6, 6) => err_lexer_digits_missing,
                     (7, 7) => err_lexer_digits_missing;
        (" "                => Whitespace);
        ("34e@e-"           => Number("34e@e-")),
                     (3, 3) => err_lexer_digits_missing,
                     (4, 4) => err_lexer_digits_missing,
                     (6, 6) => err_lexer_digits_missing;
        (" "                => Whitespace);
        ("56e/@ei"          => Number("56e/@ei")),
                     (3, 3) => err_lexer_digits_missing,
                     (4, 4) => err_lexer_digits_missing,
                     (0, 3) => err_lexer_noninteger_rational,
                     (5, 5) => err_lexer_digits_missing,
                     (6, 7) => err_lexer_invalid_number_character,
                     (6, 7) => err_lexer_digits_missing;
        (" "                => Whitespace);
        ("78/e@..."         => Number("78/e@...")),
                     (3, 3) => err_lexer_digits_missing,
                     (4, 4) => err_lexer_digits_missing,
                     (3, 4) => err_lexer_noninteger_rational,
                     (6, 7) => err_lexer_invalid_number_character,
                     (7, 8) => err_lexer_invalid_number_character,
                     (5, 8) => err_lexer_digits_missing;
        (" "                => Whitespace);
        ("90e.@+"           => Number("90e.@+")),
                     (3, 4) => err_lexer_invalid_number_character,
                     (3, 4) => err_lexer_digits_missing,
                     (6, 6) => err_lexer_digits_missing;
    }
}

#[test]
fn recover_numbers_complex_polar_extra_i() {
    check! {
        ("123i@456"         => Number("123i@456")),
                     (3, 4) => err_lexer_invalid_number_character;
        (" "                => Whitespace);
        ("123@456i"         => Number("123@456i")),
                     (7, 8) => err_lexer_invalid_number_character;
        (" "                => Whitespace);
        ("+1i@-2i"          => Number("+1i@-2i")),
                     (2, 3) => err_lexer_invalid_number_character,
                     (6, 7) => err_lexer_invalid_number_character;
        (" "                => Whitespace);
        ("+i@-i"            => Identifier("+i@-i")); // peculiar
    }
}

#[test]
fn recover_numbers_complex_polar_missing_denominator() {
    check! {
        ("+1/+2@+3"         => Number("+1/+2@+3")),
                     (3, 4) => err_lexer_invalid_number_character;
        (" "                => Whitespace);
        ("+1@+2/+3"         => Number("+1@+2/+3")),
                     (6, 7) => err_lexer_invalid_number_character;
        (" "                => Whitespace);
        ("+/1@+2"           => Identifier("+/1@+2")); // peculiar
        (" "                => Whitespace);
        ("+1/@+2"           => Number("+1/@+2")),
                     (3, 3) => err_lexer_digits_missing;
        (" "                => Whitespace);
        ("+1@/+2"           => Number("+1@/+2")),
                     (3, 3) => err_lexer_digits_missing,
                     (4, 5) => err_lexer_invalid_number_character;
        (" "                => Whitespace);
        ("+1@+/2"           => Number("+1@+/2")),
                     (4, 4) => err_lexer_digits_missing;
        (" "                => Whitespace);
        ("+1@+2/"           => Number("+1@+2/")),
                     (6, 6) => err_lexer_digits_missing;
    }
}

#[test]
fn recover_numbers_complex_polar_duplicate_signs() {
    check! {
        ("2++3@4"           => Number("2++3@4")),
                     (2, 3) => err_lexer_invalid_number_character,
                     (1, 4) => err_lexer_extra_complex_part;
        (" "                => Whitespace);
        ("23@++4"           => Number("23@++4")),
                     (4, 5) => err_lexer_invalid_number_character;
        (" "                => Whitespace);
        ("++23@4"           => Identifier("++23@4")); // peculiar
        (" "                => Whitespace);
        ("23@4++"           => Number("23@4++")),
                     (2, 4) => err_lexer_extra_complex_part,
                     (5, 6) => err_lexer_invalid_number_character,
                     (6, 6) => err_lexer_digits_missing,
                     (6, 6) => err_lexer_missing_i;
        (" "                => Whitespace);
        ("1e--2@34"         => Number("1e--2@34")),
                     (3, 4) => err_lexer_invalid_number_character;
        (" "                => Whitespace);
        ("1@2F++3"          => Number("1@2F++3")),
                     (5, 6) => err_lexer_invalid_number_character;
        (" "                => Whitespace);
        ("+-+nan.0@4"       => Identifier("+-+nan.0@4")); // peculiar
        (" "                => Whitespace);
        ("1@-+-+INF.0"      => Number("1@-+-+INF.0")),
                     (3, 4) => err_lexer_invalid_number_character,
                     (4, 5) => err_lexer_invalid_number_character,
                     (5, 6) => err_lexer_invalid_number_character;
    }
}

#[test]
fn recover_numbers_complex_polar_multiple_exponents() {
    check! {
        ("+1e-2s+d3@-4f+5L+6" => Number("+1e-2s+d3@-4f+5L+6")),
                     (5, 6) => err_lexer_invalid_number_character,
                     (6, 7) => err_lexer_invalid_number_character,
                     (7, 8) => err_lexer_invalid_number_character,
                   (15, 16) => err_lexer_invalid_number_character,
                   (16, 17) => err_lexer_invalid_number_character;
        (" "                => Whitespace);
        ("+1e-2s+4d@-4f+L5+6" => Number("+1e-2s+4d@-4f+L5+6")),
                     (5, 6) => err_lexer_invalid_number_character,
                     (6, 7) => err_lexer_invalid_number_character,
                     (8, 9) => err_lexer_invalid_number_character,
                   (14, 15) => err_lexer_invalid_number_character,
                    (9, 16) => err_lexer_extra_complex_part,
                   (18, 18) => err_lexer_missing_i;
        (" "                => Whitespace);
        ("1E2sD3@4F5l6"     => Number("1E2sD3@4F5l6")),
                     (3, 4) => err_lexer_invalid_number_character,
                     (4, 5) => err_lexer_invalid_number_character,
                   (10, 11) => err_lexer_invalid_number_character;
        (" "                => Whitespace);
        ("+1e-2+s4d@-4f+L5+6" => Number("+1e-2+s4d@-4f+L5+6")),
                     (6, 6) => err_lexer_digits_missing,
                     (8, 9) => err_lexer_invalid_number_character,
                     (5, 9) => err_lexer_extra_complex_part,
                   (14, 15) => err_lexer_invalid_number_character,
                    (9, 16) => err_lexer_extra_complex_part,
                   (18, 18) => err_lexer_missing_i;
    }
}

#[test]
fn recover_numbers_complex_polar_multiple_parts() {
    check! {
        ("1@2@3"            => Number("1@2@3")),
                     (1, 3) => err_lexer_extra_complex_part;
        (" "                => Whitespace);
        ("1i@2i@3i"         => Number("1i@2i@3i")),
                     (1, 2) => err_lexer_invalid_number_character,
                     (4, 5) => err_lexer_invalid_number_character,
                     (2, 5) => err_lexer_extra_complex_part,
                     (7, 8) => err_lexer_invalid_number_character;
        (" "                => Whitespace);
        ("1@2+3@4"          => Number("1@2+3@4")),
                     (1, 3) => err_lexer_extra_complex_part,
                     (3, 5) => err_lexer_extra_complex_part;
        (" "                => Whitespace);
        ("1@2+3i@+4i"       => Number("1@2+3i@+4i")),
                     (1, 3) => err_lexer_extra_complex_part,
                     (5, 6) => err_lexer_invalid_number_character,
                     (3, 6) => err_lexer_extra_complex_part,
                    (9, 10) => err_lexer_invalid_number_character;
    }
}

#[test]
fn recover_numbers_complex_mixed_rectangular_and_polar() {
    check! {
        ("1+2i@34+5i"       => Number("1+2i@34+5i")),
                     (3, 4) => err_lexer_invalid_number_character,
                     (1, 4) => err_lexer_extra_complex_part,
                     (4, 7) => err_lexer_extra_complex_part;
        (" "                => Whitespace);
        ("1@2+3@4"          => Number("1@2+3@4")),
                     (1, 3) => err_lexer_extra_complex_part,
                     (3, 5) => err_lexer_extra_complex_part;
        (" "                => Whitespace);
        ("1i+2@3i+4"        => Number("1i+2@3i+4")),
                     (1, 2) => err_lexer_misplaced_i,
                     (2, 4) => err_lexer_extra_complex_part,
                     (6, 7) => err_lexer_invalid_number_character,
                     (4, 7) => err_lexer_extra_complex_part,
                     (9, 9) => err_lexer_missing_i;
    }
}

#[test]
fn recover_numbers_complex_garbage() {
    check! {
        ("12/3+4_haha!_i"   => Number("12/3+4_haha!_i")),
                    (6,  7) => err_lexer_invalid_number_character,
                    (7,  8) => err_lexer_invalid_number_character,
                    (8,  9) => err_lexer_invalid_number_character,
                    (9, 10) => err_lexer_invalid_number_character,
                   (10, 11) => err_lexer_invalid_number_character,
                   (11, 12) => err_lexer_invalid_number_character,
                   (12, 13) => err_lexer_invalid_number_character;
        (" "                => Whitespace);
        ("12/3+4ihaha!"     => Number("12/3+4ihaha!")),
                    (6,  7) => err_lexer_invalid_number_character,
                    (7,  8) => err_lexer_invalid_number_character,
                    (8,  9) => err_lexer_invalid_number_character,
                    (9, 10) => err_lexer_invalid_number_character,
                   (10, 11) => err_lexer_invalid_number_character,
                   (11, 12) => err_lexer_invalid_number_character,
                   (12, 12) => err_lexer_missing_i;
        (" "                => Whitespace);
        ("12/34ihaha!"      => Number("12/34ihaha!")),
                    (5,  6) => err_lexer_invalid_number_character,
                    (6,  7) => err_lexer_invalid_number_character,
                    (7,  8) => err_lexer_invalid_number_character,
                    (8,  9) => err_lexer_invalid_number_character,
                    (9, 10) => err_lexer_invalid_number_character,
                   (10, 11) => err_lexer_invalid_number_character;
        (" "                => Whitespace);
        ("12/3+4i+haha!"    => Number("12/3+4i+haha!")),
                    (6,  7) => err_lexer_misplaced_i,
                    (4,  7) => err_lexer_extra_complex_part,
                    (8,  9) => err_lexer_invalid_number_character,
                    (9, 10) => err_lexer_invalid_number_character,
                   (10, 11) => err_lexer_invalid_number_character,
                   (11, 12) => err_lexer_invalid_number_character,
                   (12, 13) => err_lexer_invalid_number_character,
                    (8, 13) => err_lexer_digits_missing,
                   (13, 13) => err_lexer_missing_i;
        (" "                => Whitespace);
        ("12+inf.0ooops"    => Number("12+inf.0ooops")),
                    (8, 13) => err_lexer_infnan_suffix,
                   (13, 13) => err_lexer_missing_i;
        (" "                => Whitespace);
        ("34+NAN.0ihihi"    => Number("34+NAN.0ihihi")),
                    (8, 12) => err_lexer_infnan_suffix;
        (" "                => Whitespace);
        ("+nan.0IHIHI+0"    => Number("+nan.0IHIHI+0")),
                    (6, 10) => err_lexer_infnan_suffix,
                   (10, 11) => err_lexer_misplaced_i,
                   (13, 13) => err_lexer_missing_i;
        (" "                => Whitespace);
        ("56+Inf.0i-nAn.01234+inf.0" => Number("56+Inf.0i-nAn.01234+inf.0")),
                     (8, 9) => err_lexer_misplaced_i,
                     (2, 9) => err_lexer_extra_complex_part,
                   (15, 19) => err_lexer_infnan_suffix,
                    (9, 19) => err_lexer_extra_complex_part,
                   (25, 25) => err_lexer_missing_i;
        (" "                => Whitespace);
        ("0-inf.0e+12i"     => Number("0-inf.0e+12i")),
                    (7, 11) => err_lexer_infnan_suffix;
        (" "                => Whitespace);
        ("0-inf.0E+12+i"    => Number("0-inf.0E+12+i")),
                    (7, 11) => err_lexer_infnan_suffix,
                    (1, 11) => err_lexer_extra_complex_part;
        (" "                => Whitespace);
        ("0-inf.E+12+i"     => Number("0-inf.E+12+i")),
                     (2, 3) => err_lexer_invalid_number_character,
                     (3, 4) => err_lexer_invalid_number_character,
                     (2, 4) => err_lexer_digits_missing,
                     (5, 6) => err_lexer_invalid_number_character,
                     (6, 7) => err_lexer_invalid_number_character,
                     (7, 8) => err_lexer_invalid_number_character,
                    (1, 10) => err_lexer_extra_complex_part;
    }
}

#[test]
fn recover_numbers_complex_invalid_exponents() {
    check! {
        ("1.+2e3i"          => Number("1.+2e3i"));
        (" "                => Whitespace);
        ("1.23e+4i"         => Number("1.23e+4i"));
        (" "                => Whitespace);
        ("1.23e4+5i"        => Number("1.23e4+5i"));
        (" "                => Whitespace);
        ("1.23e+4+5i"       => Number("1.23e+4+5i"));
        (" "                => Whitespace);
        ("1.23e+4.+5i"      => Number("1.23e+4.+5i")),
                     (7, 8) => err_lexer_invalid_number_character;
        (" "                => Whitespace);
        ("1.23e+4/.+5i"     => Number("1.23e+4/.+5i")),
                     (8, 9) => err_lexer_digits_missing,
                     (0, 7) => err_lexer_noninteger_rational,
                     (8, 9) => err_lexer_noninteger_rational;
        (" "                => Whitespace);
        ("1ei"              => Number("1ei")),
                     (2, 2) => err_lexer_digits_missing;
        (" "                => Whitespace);
        ("1e+i"             => Number("1e+i")),
                     (3, 3) => err_lexer_digits_missing;
        (" "                => Whitespace);
        ("1e++i"            => Number("1e++i")),
                     (3, 4) => err_lexer_invalid_number_character,
                     (4, 4) => err_lexer_digits_missing;
        (" "                => Whitespace);
        ("#x1e+i"           => Number("#x1e+i"));
    }
}

#[test]
fn recover_numbers_complex_separator_madness() {
    check! {
        ("+@i"              => Identifier("+@i"));
        (" "                => Whitespace);
        ("+@i1"             => Identifier("+@i1"));
        (" "                => Whitespace);
        ("+i@"              => Identifier("+i@"));
        (" "                => Whitespace);
        ("+i@1"             => Identifier("+i@1"));
        (" "                => Whitespace);
        ("1+@+i"            => Number("1+@+i")),
                     (2, 2) => err_lexer_digits_missing,
                     (1, 2) => err_lexer_extra_complex_part,
                     (4, 5) => err_lexer_invalid_number_character,
                     (4, 5) => err_lexer_digits_missing;
        (" "                => Whitespace);
        ("1+@+i2"           => Number("1+@+i2")),
                     (2, 2) => err_lexer_digits_missing,
                     (1, 2) => err_lexer_extra_complex_part,
                     (4, 5) => err_lexer_invalid_number_character;
        (" "                => Whitespace);
        ("1+@i"             => Number("1+@i")),
                     (2, 2) => err_lexer_digits_missing,
                     (1, 2) => err_lexer_extra_complex_part,
                     (3, 4) => err_lexer_invalid_number_character,
                     (3, 4) => err_lexer_digits_missing;
        (" "                => Whitespace);
        ("1+@i+"            => Number("1+@i+")),
                     (2, 2) => err_lexer_digits_missing,
                     (1, 2) => err_lexer_extra_complex_part,
                     (3, 4) => err_lexer_invalid_number_character,
                     (3, 4) => err_lexer_digits_missing,
                     (2, 4) => err_lexer_extra_complex_part,
                     (5, 5) => err_lexer_digits_missing,
                     (5, 5) => err_lexer_missing_i;
        (" "                => Whitespace);
        ("1+@i+2"           => Number("1+@i+2")),
                     (2, 2) => err_lexer_digits_missing,
                     (1, 2) => err_lexer_extra_complex_part,
                     (3, 4) => err_lexer_invalid_number_character,
                     (3, 4) => err_lexer_digits_missing,
                     (2, 4) => err_lexer_extra_complex_part,
                     (6, 6) => err_lexer_missing_i;
        (" "                => Whitespace);
        ("1+@i1"            => Number("1+@i1")),
                     (2, 2) => err_lexer_digits_missing,
                     (1, 2) => err_lexer_extra_complex_part,
                     (3, 4) => err_lexer_invalid_number_character;
        (" "                => Whitespace);
        ("1+i@"             => Number("1+i@")),
                     (2, 3) => err_lexer_misplaced_i,
                     (1, 3) => err_lexer_extra_complex_part,
                     (4, 4) => err_lexer_digits_missing;
        (" "                => Whitespace);
        ("1+i@1"            => Number("1+i@1")),
                     (2, 3) => err_lexer_misplaced_i,
                     (1, 3) => err_lexer_extra_complex_part;
        (" "                => Whitespace);
        ("1@+i"             => Number("1@+i")),
                     (3, 4) => err_lexer_invalid_number_character,
                     (3, 4) => err_lexer_digits_missing;
        (" "                => Whitespace);
        ("1@+i2"            => Number("1@+i2")),
                     (3, 4) => err_lexer_invalid_number_character;
        (" "                => Whitespace);
        ("1@i+"             => Number("1@i+")),
                     (2, 3) => err_lexer_invalid_number_character,
                     (2, 3) => err_lexer_digits_missing,
                     (1, 3) => err_lexer_extra_complex_part,
                     (4, 4) => err_lexer_digits_missing,
                     (4, 4) => err_lexer_missing_i;
        (" "                => Whitespace);
        ("1@i+2"            => Number("1@i+2")),
                     (2, 3) => err_lexer_invalid_number_character,
                     (2, 3) => err_lexer_digits_missing,
                     (1, 3) => err_lexer_extra_complex_part,
                     (5, 5) => err_lexer_missing_i;
        (" "                => Whitespace);
        ("1i+@"             => Number("1i+@")),
                     (1, 2) => err_lexer_misplaced_i,
                     (3, 3) => err_lexer_digits_missing,
                     (2, 3) => err_lexer_extra_complex_part,
                     (4, 4) => err_lexer_digits_missing;
        (" "                => Whitespace);
        ("1i+@2"            => Number("1i+@2")),
                     (1, 2) => err_lexer_misplaced_i,
                     (3, 3) => err_lexer_digits_missing,
                     (2, 3) => err_lexer_extra_complex_part;
        (" "                => Whitespace);
        ("1i@+"             => Number("1i@+")),
                     (1, 2) => err_lexer_invalid_number_character,
                     (4, 4) => err_lexer_digits_missing;
        (" "                => Whitespace);
        ("1i@+2"            => Number("1i@+2")),
                     (1, 2) => err_lexer_invalid_number_character;
        (" "                => Whitespace);
        ("@+i"              => Identifier("@+i"));
        (" "                => Whitespace);
        ("@+i1"             => Identifier("@+i1"));
        (" "                => Whitespace);
        ("@i+"              => Identifier("@i+"));
        (" "                => Whitespace);
        ("@i+1"             => Identifier("@i+1"));
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Peculiar identifiers

#[test]
fn identifiers_peculiar_explicit_sign() {
    check! {
        ("+"                => Identifier("+"));
        (" "                => Whitespace);
        ("-"                => Identifier("-"));
        ("("                => Open(Parenthesis));
        (" "                => Whitespace);
        ("+"                => Identifier("+"));
    }
}

#[test]
fn identifiers_peculiar_explicit_sign_dot() {
    check! {
        ("+.x"              => Identifier("+.x"));
        (" "                => Whitespace);
        ("-.-"              => Identifier("-.-"));
        (" "                => Whitespace);
        ("+.@"              => Identifier("+.@"));
        (" "                => Whitespace);
        ("+.._..+"          => Identifier("+.._..+"));
        (" "                => Whitespace);
        ("+."               => Identifier("+."));
        (" "                => Whitespace);
        ("+.i"              => Identifier("+.i"));
    }
}

#[test]
fn identifiers_peculiar_explicit_sign_nondot() {
    check! {
        ("-some-"           => Identifier("-some-"));
        (" "                => Whitespace);
        ("+++"              => Identifier("+++"));
        (" "                => Whitespace);
        ("+@--o"            => Identifier("+@--o"));
        (" "                => Whitespace);
        ("+_+"              => Identifier("+_+"));
        (" "                => Whitespace);
        ("+I1"              => Identifier("+I1"));
        (" "                => Whitespace);
        ("-not"             => Identifier("-not"));
        (" "                => Whitespace);
        ("-NANI"            => Identifier("-NANI"));
    }
}

#[test]
fn identifiers_peculiar_dot() {
    check! {
        ("."                => Dot);
        (" "                => Whitespace);
        (".."               => Identifier(".."));
        (" "                => Whitespace);
        ("..."              => Identifier("..."));
        (" "                => Whitespace);
        (".Net"             => Identifier(".Net"));
        (" "                => Whitespace);
        (".-."              => Identifier(".-."));
        (" "                => Whitespace);
        (".!."              => Identifier(".!."));
        (" "                => Whitespace);
        (".@com"            => Identifier(".@com"));
        (" "                => Whitespace);
        (".i"               => Identifier(".i"));
    }
}

#[test]
fn recover_identifiers_peculiar_prefixed() {
    check! {
        ("#identifier"      => Identifier("dentifier")),
                     (0, 2) => err_lexer_prefixed_identifier;
        (" "                => Whitespace);
        ("#.+"              => Identifier(".+")),
                     (0, 1) => err_lexer_invalid_number_prefix,
                     (0, 1) => err_lexer_prefixed_identifier;
        (" "                => Whitespace);
        ("#+."              => Identifier("+.")),
                     (0, 1) => err_lexer_invalid_number_prefix,
                     (0, 1) => err_lexer_prefixed_identifier;
        (" "                => Whitespace);
        ("#/i"              => Identifier("i")),
                     (0, 2) => err_lexer_invalid_number_prefix,
                     (0, 2) => err_lexer_prefixed_identifier;
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Escaped identifiers

#[test]
fn identifiers_escaped_basic() {
    check! {
        ("|test|"       => Identifier("test"));
        (" "            => Whitespace);
        ("||"           => Identifier(""));
        (" "            => Whitespace);
        ("|TesT|"       => Identifier("TesT"));
        (" "            => Whitespace);
        ("|12312|"      => Identifier("12312"));
        (" "            => Whitespace);
        ("| |"          => Identifier(" "));
        ("|\t|"         => Identifier("\t"));
        ("|.|"          => Identifier("."));
        ("|\"|"         => Identifier("\""));
        (" "            => Whitespace);
        ("|\u{0}|"      => Identifier("\u{0000}"));
        (" "            => Whitespace);
        ("|\u{044E}\u{043D}\u{0438}\u{043A}\u{043E}\u{0434}|"
                        => Identifier("\u{044E}\u{043D}\u{0438}\u{043A}\u{043E}\u{0434}"));
        (" "            => Whitespace);
        ("|\u{1112}\u{1161}\u{11AB}\u{1100}\u{116E}\u{11A8}\u{110B}\u{1165}|"
                        => Identifier("\u{1112}\u{1161}\u{11AB}\u{1100}\u{116E}\u{11A8}\u{110B}\u{1165}"));
    }
}

#[test]
fn identifiers_escaped_escape_sequences() {
    check! {
        ("|\\a|"        => Identifier("\u{0007}"));
        ("|\\b|"        => Identifier("\u{0008}"));
        ("|\\t|"        => Identifier("\u{0009}"));
        ("|\\n|"        => Identifier("\u{000A}"));
        ("|\\r|"        => Identifier("\u{000D}"));
        ("|\\\"|"       => Identifier("\u{0022}"));
        ("|\\\\|"       => Identifier("\u{005C}"));
        ("|\\||"        => Identifier("\u{007C}"));
        ("|\\r\\n|"     => Identifier("\u{000D}\u{000A}"));
    }
}

#[test]
fn identifiers_escaped_unicode_escapes() {
    check! {
        ("|\\x0000;|"       => Identifier("\u{0000}"));
        (" "                => Whitespace);
        ("|\\X1234;|"       => Identifier("\u{1234}"));
        (" "                => Whitespace);
        ("|\\xBeeb;|"       => Identifier("\u{BEEB}"));
        (" "                => Whitespace);
        ("|\\Xf0F0C;|"      => Identifier("\u{0F0F0C}"));
        (" "                => Whitespace);
        ("|\\x00000001;|"   => Identifier("\u{0001}"));
        (" "                => Whitespace);
        ("|\\x10FFFF;|"     => Identifier("\u{10FFFF}"));
    }
}

#[test]
fn identifiers_escaped_newlines() {
    check! {
        ("|one\nline|"          => Identifier("one\nline"));
        ("\n"                   => Whitespace);
        ("|other\r\nline|"      => Identifier("other\r\nline"));
        ("\n"                   => Whitespace);
        ("|third\n\nline|"      => Identifier("third\n\nline"));
        ("\n"                   => Whitespace);
        ("|bare\rCR|"           => Identifier("bare\rCR"));
        ("\n"                   => Whitespace);
        ("|\n|"                 => Identifier("\n"));
        ("|\r|"                 => Identifier("\r"));
        ("|\r\n|"               => Identifier("\r\n"));
    }
}

#[test]
fn identifiers_escaped_line_escape() {
    check! {
        ("|text with \\  \t  \r\n \t one line|"     => Identifier("text with   \t  \r\n \t one line")),
                                           (11, 13) => err_lexer_invalid_escape_sequence;
        ("\n"                                       => Whitespace);
        ("|another\\\nline|"                        => Identifier("another\nline")),
                                            (8, 10) => err_lexer_invalid_escape_sequence;
        ("\n"                                       => Whitespace);
        ("|\\\n|"                                   => Identifier("\n")),
                                             (1, 3) => err_lexer_invalid_escape_sequence;
        ("\n"                                       => Whitespace);
        ("|\\ \n |"                                 => Identifier(" \n ")),
                                             (1, 3) => err_lexer_invalid_escape_sequence;
        ("\n"                                       => Whitespace);
        ("|<\\\r\n\r\t\r\n>|"                       => Identifier("<\r\n\r\t\r\n>")),
                                             (2, 4) => err_lexer_invalid_escape_sequence;
    }
}

#[test]
fn recover_identifiers_escaped_eof_1() {
    check! {
        ("|endless" => Unrecognized),
            (0, 8) => fatal_lexer_unterminated_identifier;
    }
}

#[test]
fn recover_identifiers_escaped_eof_2() {
    check! {
        ("|" => Unrecognized),
            (0, 1) => fatal_lexer_unterminated_identifier;
    }
}

#[test]
fn recover_identifiers_escaped_eof_3() {
    check! {
        ("|\\|" => Unrecognized),
            (0, 3) => fatal_lexer_unterminated_identifier;
    }
}

#[test]
fn recover_identifiers_escaped_eof_4() {
    check! {
        ("|\\ " => Unrecognized),
            (1, 3) => err_lexer_invalid_escape_sequence,
            (0, 3) => fatal_lexer_unterminated_identifier;
    }
}

#[test]
fn recover_identifiers_escaped_eof_5() {
    check! {
        ("|\\x1" => Unrecognized),
            (4, 4) => err_lexer_unicode_escape_missing_semicolon,
            (0, 4) => fatal_lexer_unterminated_identifier;
    }
}

#[test]
fn recover_identifiers_escaped_escape_sequences() {
    check! {
        ("|\\m|"            => Identifier("m")),
                     (1, 3) => err_lexer_invalid_escape_sequence;
        (" "                => Whitespace);
        ("|\\1\\2\\3|"      => Identifier("123")),
                     (1, 3) => err_lexer_invalid_escape_sequence,
                     (3, 5) => err_lexer_invalid_escape_sequence,
                     (5, 7) => err_lexer_invalid_escape_sequence;
        (" "                => Whitespace);
        ("|\\\u{0}|"        => Identifier("\u{0000}")),
                     (1, 3) => err_lexer_invalid_escape_sequence;
        (" "                => Whitespace);
        ("|\\\r|"           => Identifier("\r")),
                     (1, 3) => err_lexer_invalid_escape_sequence;
    }
}

#[test]
fn recover_identifiers_escaped_unicode_escapes() {
    check! {
        ("|\\xD7FF;\\xD800;\\xDFFF;\\xC000;|"       => Identifier("\u{D7FF}\u{FFFD}\u{FFFD}\u{C000}")),
                                            (8, 15) => err_lexer_invalid_unicode_range,
                                           (15, 22) => err_lexer_invalid_unicode_range;
        ("\n"                                       => Whitespace);
        ("|\\XfaBBCbCBDb9BCdeeeAa2123987005;|"      => Identifier("\u{FFFD}")),
                                            (1, 33) => err_lexer_invalid_unicode_range;
        ("\n"                                       => Whitespace);
        ("|\\x110000;\\x0F00000;|"                  => Identifier("\u{FFFD}\u{FFFD}")),
                                            (1, 10) => err_lexer_invalid_unicode_range,
                                           (10, 20) => err_lexer_invalid_unicode_range;
        ("|\\x|"                                    => Identifier("x")),
                                             (1, 3) => err_lexer_invalid_escape_sequence;
        ("\n"                                       => Whitespace);
        ("|\\X|"                                    => Identifier("X")),
                                             (1, 3) => err_lexer_invalid_escape_sequence;
        ("\n"                                       => Whitespace);
        ("|\\x!|"                                   => Identifier("x!")),
                                             (1, 3) => err_lexer_invalid_escape_sequence;
        ("\n"                                       => Whitespace);
        ("|\\x;|"                                   => Identifier("\u{FFFD}")),
                                             (3, 3) => err_lexer_unicode_escape_missing_digits;
        ("\n"                                       => Whitespace);
        ("|\\X;|"                                   => Identifier("\u{FFFD}")),
                                             (3, 3) => err_lexer_unicode_escape_missing_digits;
        ("\n"                                       => Whitespace);
        ("|\\xdesu|"                                => Identifier("\u{00DE}su")),
                                             (5, 5) => err_lexer_unicode_escape_missing_semicolon;
        ("\n"                                       => Whitespace);
        ("|\\x11111111111111111x|"                  => Identifier("\u{FFFD}x")),
                                           (20, 20) => err_lexer_unicode_escape_missing_semicolon,
                                            (1, 20) => err_lexer_invalid_unicode_range;
        ("\n"                                       => Whitespace);
        ("|\\x3711\\a|"                             => Identifier("\u{3711}\u{0007}")),
                                             (7, 7) => err_lexer_unicode_escape_missing_semicolon;
        ("\n"                                       => Whitespace);
        ("|\\x0ded|"                                => Identifier("\u{0DED}")),
                                             (7, 7) => err_lexer_unicode_escape_missing_semicolon;
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// ASCII identifiers

#[test]
fn identifiers_ascii_basic() {
    check! {
        ("test"             => Identifier("test"));
        (" "                => Whitespace);
        ("Testing"          => Identifier("Testing"));
        (" "                => Whitespace);
        ("_123"             => Identifier("_123"));
        (" "                => Whitespace);
        ("var1"             => Identifier("var1"));
        (" "                => Whitespace);
        ("set!"             => Identifier("set!"));
        (" "                => Whitespace);
        ("equals"           => Identifier("equals"));
        (" "                => Whitespace);
        ("car"              => Identifier("car"));
        (" "                => Whitespace);
        ("cdr"              => Identifier("cdr"));
        (" "                => Whitespace);
        ("?"                => Identifier("?"));
        (" "                => Whitespace);
        ("_"                => Identifier("_"));
        (" "                => Whitespace);
        ("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789" => Identifier("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"));
    }
}

#[test]
fn identifiers_ascii_special_initial() {
    check! {
        ("!!!FUN!!!"        => Identifier("!!!FUN!!!"));
        (" "                => Whitespace);
        ("$100"             => Identifier("$100"));
        (" "                => Whitespace);
        ("%csh"             => Identifier("%csh"));
        (" "                => Whitespace);
        ("&rest"            => Identifier("&rest"));
        (" "                => Whitespace);
        ("*magic*"          => Identifier("*magic*"));
        (" "                => Whitespace);
        ("/usr/bin/scheme"  => Identifier("/usr/bin/scheme"));
        (" "                => Whitespace);
        (":option"          => Identifier(":option"));
        (" "                => Whitespace);
        ("<html>"           => Identifier("<html>"));
        (" "                => Whitespace);
        ("=C3=A2"           => Identifier("=C3=A2"));
        (" "                => Whitespace);
        (">8---"            => Identifier(">8---"));
        (" "                => Whitespace);
        ("?parolas"         => Identifier("?parolas"));
        (" "                => Whitespace);
        ("^_^"              => Identifier("^_^"));
        (" "                => Whitespace);
        ("___"              => Identifier("___"));
        (" "                => Whitespace);
        ("~nyoro~~on"       => Identifier("~nyoro~~on"));
        (" "                => Whitespace);
        ("!$%&*/:<=>?^_~"   => Identifier("!$%&*/:<=>?^_~"));
    }
}

#[test]
fn identifiers_ascii_special_subsequent() {
    check! {
        ("Alice+Bob"            => Identifier("Alice+Bob"));
        (" "                    => Whitespace);
        ("long-identifier"      => Identifier("long-identifier"));
        (" "                    => Whitespace);
        ("username@example.com" => Identifier("username@example.com"));
    }
}

#[test]
fn identifiers_ascii_delimiters() {
    check! {
        ("("                => Open(Parenthesis));
        ("test"             => Identifier("test"));
        (")"                => Close(Parenthesis));
        ("+"                => Identifier("+"));
        ("|omg|"            => Identifier("omg"));
        ("test"             => Identifier("test"));
        ("|omg|"            => Identifier("omg"));
        ("; comment\n"      => Comment);
        ("var1"             => Identifier("var1"));
        (","                => Comma);
        ("var2"             => Identifier("var2"));
        ("\"\""             => String(""));
        ("var3"             => Identifier("var3"));
    }
}

#[test]
fn recover_identifiers_ascii_backslash() {
    check! {
        ("C:\\WINDOWS"      => Identifier("C:\\WINDOWS")),
                     (2, 3) => err_lexer_invalid_identifier_character;
        (" "                => Whitespace);
        ("\\x34"            => Identifier("\\x34")),
                     (0, 1) => err_lexer_invalid_identifier_character;
        (";\\x35;\\n"       => Comment);
    }
}

#[test]
fn recover_identifiers_ascii_unprintable() {
    check! {
        ("!\u{00}\u{01}"    => Identifier("!\u{00}\u{01}")),
                     (1, 2) => err_lexer_invalid_identifier_character,
                     (2, 3) => err_lexer_invalid_identifier_character;
        (" "                => Whitespace);
        ("x\u{16}\u{1F}"    => Identifier("x\u{16}\u{1F}")),
                     (1, 2) => err_lexer_invalid_identifier_character,
                     (2, 3) => err_lexer_invalid_identifier_character;
        (" "                => Whitespace);
        ("$\u{7F}"          => Identifier("$\u{7F}")),
                     (1, 2) => err_lexer_invalid_identifier_character;
        (" "                => Whitespace);
        ("\u{10}\u{07}"     => Identifier("\u{10}\u{07}")),
                     (0, 1) => err_lexer_invalid_identifier_character,
                     (1, 2) => err_lexer_invalid_identifier_character;
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Unicode identifiers

#[test]
#[cfg(feature = "unicode")]
fn identifiers_unicode_basic() {
    check! {
        // Lu
        ("\u{041F}\u{0420}\u{041E}\u{0412}\u{0415}\u{0420}\u{041A}\u{0410}" => Identifier("\u{041F}\u{0420}\u{041E}\u{0412}\u{0415}\u{0420}\u{041A}\u{0410}"));
        (" " => Whitespace);
        ("\u{1D447}\u{1D438}\u{1D446}\u{1D447}" => Identifier("\u{0054}\u{0045}\u{0053}\u{0054}"));
        (" " => Whitespace);
        // Ll
        ("\u{03B5}\u{03BE}\u{03AD}\u{03C4}\u{03B1}\u{03C3}\u{03B7}" => Identifier("\u{03B5}\u{03BE}\u{03AD}\u{03C4}\u{03B1}\u{03C3}\u{03B7}"));
        (" " => Whitespace);
        ("\u{1E936}\u{1E93E}\u{1E930}" => Identifier("\u{1E936}\u{1E93E}\u{1E930}"));
        (" " => Whitespace);
        // Lt
        ("\u{01F2}\u{0061}" => Identifier("\u{0044}\u{007A}\u{0061}"));
        (" " => Whitespace);
        ("\u{1FAA}" => Identifier("\u{1FAA}"));
        (" " => Whitespace);
        // Lm
        ("\u{1D2E}\u{1D3C}\u{1D3C}\u{1D2E}\u{1D4E}\u{1D4B}" => Identifier("\u{0042}\u{004F}\u{004F}\u{0042}\u{1D4E}\u{025B}"));
        (" " => Whitespace);
        ("\u{A9CF}\u{16F93}" => Identifier("\u{A9CF}\u{16F93}"));
        (" " => Whitespace);
        // Lo
        ("\u{0627}\u{062E}\u{062A}\u{0628}\u{0627}\u{0631}" => Identifier("\u{0627}\u{062E}\u{062A}\u{0628}\u{0627}\u{0631}"));
        (" " => Whitespace);
        ("\u{6E2C}\u{8A66}" => Identifier("\u{6E2C}\u{8A66}"));
        (" " => Whitespace);
        ("\u{16B29}\u{16B6C}\u{1458B}\u{13365}" => Identifier("\u{16B29}\u{16B6C}\u{1458B}\u{13365}"));
        (" " => Whitespace);
        // Mn (subsequent-only)
        ("\u{0074}\u{0065}\u{0300}\u{0073}\u{0074}" => Identifier("\u{0074}\u{00E8}\u{0073}\u{0074}"));
        (" " => Whitespace);
        ("\u{0078}\u{05BD}" => Identifier("\u{0078}\u{05BD}"));
        (" " => Whitespace);
        ("\u{58DE}\u{1E949}" => Identifier("\u{58DE}\u{1E949}"));
        (" " => Whitespace);
        // Mc (subsequent-only)
        ("\u{0433}\u{16F54}" => Identifier("\u{0433}\u{16F54}"));
        (" " => Whitespace);
        ("\u{043E}\u{0C82}" => Identifier("\u{043E}\u{0C82}"));
        (" " => Whitespace);
        // Me (subsequent-only)
        ("\u{0044}\u{20E3}\u{0065}\u{20E3}\u{0073}\u{20E3}\u{0075}\u{20E3}" => Identifier("\u{0044}\u{20E3}\u{0065}\u{20E3}\u{0073}\u{20E3}\u{0075}\u{20E3}"));
        (" " => Whitespace);
        // Nd (subsequent-only)
        ("\u{0401}\u{07C0}\u{07C7}\u{09EE}\u{0BEB}\u{1811}\u{1D7F9}\u{1E954}" => Identifier("\u{0401}\u{07C0}\u{07C7}\u{09EE}\u{0BEB}\u{1811}\u{0033}\u{1E954}"));
        (" " => Whitespace);
        // Nl
        ("\u{2167}\u{216E}\u{2180}\u{1015B}\u{1242C}\u{3028}" => Identifier("\u{0056}\u{0049}\u{0049}\u{0049}\u{0044}\u{2180}\u{1015B}\u{1242C}\u{3028}"));
        (" " => Whitespace);
        // No
        ("\u{0078}\u{00BC}" => Identifier("\u{0078}\u{0031}\u{2044}\u{0034}"));
        (" " => Whitespace);
        ("\u{0BF0}\u{246C}\u{108FD}\u{10CFA}" => Identifier("\u{0BF0}\u{0031}\u{0033}\u{108FD}\u{10CFA}"));
        (" " => Whitespace);
        // Pd
        ("\u{2014}\u{2012}\u{3030}\u{FF0D}" => Identifier("\u{2014}\u{2012}\u{3030}\u{002D}"));
        (" " => Whitespace);
        // Pc
        ("\u{203F}\u{002E}\u{203F}" => Identifier("\u{203F}\u{002E}\u{203F}"));
        (" " => Whitespace);
        ("\u{FE34}\u{FE4F}\u{FE34}" => Identifier("\u{005F}\u{005F}\u{005F}"));
        (" " => Whitespace);
        // Po
        ("\u{00B6}\u{00BF}\u{066A}\u{07F7}\u{0DF4}" => Identifier("\u{00B6}\u{00BF}\u{066A}\u{07F7}\u{0DF4}"));
        (" " => Whitespace);
        ("\u{1809}\u{2021}\u{2049}\u{2E2D}\u{A875}" => Identifier("\u{1809}\u{2021}\u{0021}\u{003F}\u{2E2D}\u{A875}"));
        (" " => Whitespace);
        ("\u{FE57}\u{FF20}\u{12470}\u{1DA87}" => Identifier("\u{0021}\u{0040}\u{12470}\u{1DA87}"));
        (" " => Whitespace);
        // Sc
        ("\u{00A2}\u{20A5}\u{20A8}\u{20B8}" => Identifier("\u{00A2}\u{20A5}\u{0052}\u{0073}\u{20B8}"));
        (" " => Whitespace);
        // Sm
        ("\u{00D7}\u{21A3}\u{2203}\u{220F}\u{222D}" => Identifier("\u{00D7}\u{21A3}\u{2203}\u{220F}\u{222B}\u{222B}\u{222B}"));
        (" " => Whitespace);
        ("\u{229E}\u{25FF}\u{27F1}\u{2AF7}\u{1D6C1}" => Identifier("\u{229E}\u{25FF}\u{27F1}\u{2AF7}\u{2207}"));
        (" " => Whitespace);
        // Sk
        ("\u{005E}\u{02E5}\u{FBB2}\u{1F612}\u{1F3FB}" => Identifier("\u{005E}\u{02E5}\u{FBB2}\u{1F612}\u{1F3FB}"));
        (" " => Whitespace);
        // So
        ("\u{00A9}\u{06DE}\u{0BF5}\u{0F16}" => Identifier("\u{00A9}\u{06DE}\u{0BF5}\u{0F16}"));
        (" " => Whitespace);
        ("\u{21B3}\u{2318}\u{23CF}\u{2414}\u{2541}" => Identifier("\u{21B3}\u{2318}\u{23CF}\u{2414}\u{2541}"));
        (" " => Whitespace);
        ("\u{259C}\u{267C}\u{2720}\u{2FA9}\u{336A}\u{FFFD}\u{1D21D}\u{1F0F2}" => Identifier("\u{259C}\u{267C}\u{2720}\u{961C}\u{0031}\u{0038}\u{70B9}\u{FFFD}\u{1D21D}\u{1F0F2}"));
        (" " => Whitespace);
        // Co
        ("\u{E000}\u{F000}\u{F8FF}" => Identifier("\u{E000}\u{F000}\u{F8FF}"));
        (" " => Whitespace);
        ("\u{F0000}\u{F5000}\u{FFFFD}" => Identifier("\u{F0000}\u{F5000}\u{FFFFD}"));
        (" " => Whitespace);
        ("\u{100000}\u{101234}\u{10FFFD}" => Identifier("\u{100000}\u{101234}\u{10FFFD}"));
        (" " => Whitespace);
        // ZWNJ, ZWJ (subsequent-only)
        ("\u{05E2}\u{05B2}\u{05D5}\u{200C}\u{05B9}\u{05E0}\u{05B9}\u{05EA}" => Identifier("\u{05E2}\u{05B2}\u{05D5}\u{05B9}\u{05E0}\u{05B9}\u{05EA}"));
        (" " => Whitespace);
        ("\u{0915}\u{094D}\u{200D}\u{0937}" => Identifier("\u{0915}\u{094D}\u{0937}"));
    }
}

#[test]
fn identifiers_unicode_escaped() {
    check! {
        // Normal Unicode as in plain identifiers
        ("|\u{0442}\u{043E}\u{0445}\u{043E}|" => Identifier("\u{0442}\u{043E}\u{0445}\u{043E}"));
        (" " => Whitespace);
        ("|\u{05DE}\u{05B4}\u{05D1}\u{05B0}\u{05D7}\u{05B8}\u{05DF}|" => Identifier("\u{05DE}\u{05B4}\u{05D1}\u{05B0}\u{05D7}\u{05B8}\u{05DF}"));
        (" " => Whitespace);
        // Anything is allowed and preserved as is
        ("|\u{24A3}\u{24A0}\u{24A7}\u{24A7}\u{24AA}|" => Identifier("\u{24A3}\u{24A0}\u{24A7}\u{24A7}\u{24AA}"));
        (" " => Whitespace);
        ("|\u{0000}\u{10FFFF}\u{200D}\u{0099}\u{E0001}\u{E0071}\u{E007F}|" => Identifier("\u{0000}\u{10FFFF}\u{200D}\u{0099}\u{E0001}\u{E0071}\u{E007F}"));
    }
}

#[test]
#[cfg(feature = "unicode")]
fn recover_identifiers_unicode_restricted_initial() {
    check! {
        // Mn
        ("\u{1772}\u{2D7F}" => Identifier("\u{1772}\u{2D7F}")),
            (0, 3) => err_lexer_invalid_identifier_character;
        (" " => Whitespace);
        ("\u{1DA58}\u{1DA61}" => Identifier("\u{1DA58}\u{1DA61}")),
            (0, 4) => err_lexer_invalid_identifier_character;
        (" " => Whitespace);
        ("\u{E01EE}" => Identifier("\u{E01EE}")),
            (0, 4) => err_lexer_invalid_identifier_character;
        (" " => Whitespace);
        // Mc
        ("\u{0BC8}\u{0BCB}" => Identifier("\u{0BC8}\u{0BCB}")),
            (0, 3) => err_lexer_invalid_identifier_character;
        (" " => Whitespace);
        // Me
        ("\u{20DD}\u{0039}" => Identifier("\u{20DD}\u{0039}")),
            (0, 3) => err_lexer_invalid_identifier_character;
        (" " => Whitespace);
        // Nd
        ("\u{FF11}\u{FF12}\u{FF13}" => Identifier("\u{0031}\u{0032}\u{0033}")),
            (0, 3) => err_lexer_invalid_identifier_character,
            (0, 9) => warn_lexer_identifier_looks_like_number;
        (" " => Whitespace);
        // ZWNJ, ZWJ
        ("\u{200C}\u{0445}\u{0430}\u{200C}\u{002D}\u{0445}\u{0430}" => Identifier("\u{0445}\u{0430}\u{002D}\u{0445}\u{0430}")),
            (0, 3) => err_lexer_invalid_identifier_character;
        (" " => Whitespace);
        ("\u{200D}\u{0C1A}\u{0C46}\u{0C21}\u{200D}\u{0C41}\u{200D}" => Identifier("\u{0C1A}\u{0C46}\u{0C21}\u{0C41}")),
            (0, 3) => err_lexer_invalid_identifier_character;
    }
}

#[test]
#[cfg(feature = "unicode")]
fn recover_identifiers_unicode_unmapped() {
    check! {
        // Cc
        ("\u{0000}\u{006E}\u{0069}\u{006C}" => Identifier("\u{0000}\u{006E}\u{0069}\u{006C}")),
            (0, 1) => err_lexer_invalid_identifier_character;
        (" " => Whitespace);
        ("\u{10DB}\u{10EC}\u{0091}\u{10D5}\u{10D0}\u{10DC}\u{10D4}\u{0083}" => Identifier("\u{10DB}\u{10EC}\u{0091}\u{10D5}\u{10D0}\u{10DC}\u{10D4}\u{0083}")),
            (6, 8) => err_lexer_invalid_identifier_character,
            (20, 22) => err_lexer_invalid_identifier_character;
        (" " => Whitespace);
        // Cf (except for ZWNJ, ZWJ)
        ("\u{0440}\u{0430}\u{0437}\u{00AD}\u{043B}\u{043E}\u{043C}" => Identifier("\u{0440}\u{0430}\u{0437}\u{00AD}\u{043B}\u{043E}\u{043C}")),
            (6, 8) => err_lexer_invalid_identifier_character;
        (" " => Whitespace);
        ("\u{0DC4}\u{180E}\u{0DBB}\u{0DD2}\u{0DAD}" => Identifier("\u{0DC4}\u{180E}\u{0DBB}\u{0DD2}\u{0DAD}")),
            (3, 6) => err_lexer_invalid_identifier_character;
        (" " => Whitespace);
        ("\u{0079}\u{0065}\u{015F}\u{200B}\u{0069}\u{006C}" => Identifier("\u{0079}\u{0065}\u{015F}\u{200B}\u{0069}\u{006C}")),
            (4, 7) => err_lexer_invalid_identifier_character;
        (" " => Whitespace);
        ("\u{200E}\u{0642}\u{0631}\u{0645}\u{0632}\u{206F}" => Identifier("\u{200E}\u{0642}\u{0631}\u{0645}\u{0632}\u{206F}")),
            (0, 3) => err_lexer_invalid_identifier_character,
            (11, 14) => err_lexer_invalid_identifier_character;
        (" " => Whitespace);
        // Ps, Pe
        ("\u{27E6}\u{03B1}\u{276F}" => Identifier("\u{27E6}\u{03B1}\u{276F}")),
            (0, 3) => err_lexer_invalid_identifier_character,
            (5, 8) => err_lexer_invalid_identifier_character;
        (" " => Whitespace);
        ("\u{201E}\u{178F}\u{2E23}" => Identifier("\u{201E}\u{178F}\u{2E23}")),
            (0, 3) => err_lexer_invalid_identifier_character,
            (6, 9) => err_lexer_invalid_identifier_character;
        (" " => Whitespace);
        // Pi, Pf
        ("\u{00AB}\u{50BE}\u{659C}\u{2E05}" => Identifier("\u{00AB}\u{50BE}\u{659C}\u{2E05}")),
            (0, 2) => err_lexer_invalid_identifier_character,
            (8, 11) => err_lexer_invalid_identifier_character;
        (" " => Whitespace);
        // Zl
        ("\u{26B5}\u{2028}\u{2F33}" => Identifier("\u{26B5}\u{2028}\u{5E7A}")),
            (3, 6) => err_lexer_invalid_identifier_character;
        (" " => Whitespace);
        // Zp
        ("\u{0430}\u{0431}\u{0437}\u{0430}\u{0446}\u{002E}\u{2029}" => Identifier("\u{0430}\u{0431}\u{0437}\u{0430}\u{0446}\u{002E}\u{2029}")),
            (11, 14) => err_lexer_invalid_identifier_character;
        (" " => Whitespace);
        // Zs
        ("\u{0073}\u{0070}\u{0061}\u{2000}\u{0063}\u{0065}" => Identifier("\u{0073}\u{0070}\u{0061}\u{0020}\u{0063}\u{0065}")),
            (3, 6) => err_lexer_invalid_identifier_character;
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Directives

#[test]
fn directives_basic() {
    check! {
        ("#!fold-case"              => Directive("fold-case"));
        (" "                        => Whitespace);
        ("#!no-fold-case"           => Directive("no-fold-case"));
        ("\t"                       => Whitespace);
        ("#!FOLD-CASE"              => Directive("fold-case"));
        ("||"                       => Identifier(""));
        ("#!NO-FOLD-CASE"           => Directive("no-fold-case"));
        (";\n"                      => Comment);
        ("#!FoLd-CaSe"              => Directive("fold-case"));
        ("("                        => Open(Parenthesis));
        ("#!no-FOLD-caSE"           => Directive("no-fold-case"));
        (")"                        => Close(Parenthesis));
        ("#!fOLD-cASe"              => Directive("fold-case"));
    }
}

#[test]
fn recover_directives_invalid() {
    check! {
        ("#!fold"                   => Directive("fold")),
                             (0, 6) => err_lexer_unknown_directive;
        (" "                        => Whitespace);
        ("#!f01d-c453"              => Directive("f01d-c453")),
                            (0, 11) => err_lexer_unknown_directive;
        (" "                        => Whitespace);
        ("#!NO_MORE_BANANA"         => Directive("no_more_banana")),
                            (0, 16) => err_lexer_unknown_directive;
        ("\"test\""                 => String("test"));
        ("#!no-fold-case-please"    => Directive("no-fold-case-please")),
                            (0, 21) => err_lexer_unknown_directive;
        (" "                        => Whitespace);
        ("#!@#$%^&*"                => Directive("@#$%^&*")),
                             (0, 9) => err_lexer_unknown_directive;
        (" "                        => Whitespace);
        ("#!#!#!!1!1"               => Directive("#!#!!1!1")),
                            (0, 10) => err_lexer_unknown_directive;
        (" "                        => Whitespace);
        ("#!/bin/sh"                => Directive("/bin/sh")),
                             (0, 9) => err_lexer_unknown_directive;
        (" "                        => Whitespace);
        ("#!\x01\x02\x03"           => Directive("\u{01}\u{02}\u{03}")),
                             (0, 5) => err_lexer_unknown_directive;
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Case control: ASCII

#[test]
fn directives_case_control_ascii() {
    check! {
        ("Test"                     => Identifier("Test"));
        (" "                        => Whitespace);
        ("|Test|"                   => Identifier("Test"));
        (" "                        => Whitespace);
        ("\"tESt\""                 => String("tESt"));
        (" "                        => Whitespace);
        ("#xDEAD"                   => Number("#xDEAD"));
        (" "                        => Whitespace);
        ("+Inf.0"                   => Number("+Inf.0"));
        (" "                        => Whitespace);
        ("#\\R"                     => Character('R'));
        (" "                        => Whitespace);
        ("#\\x"                     => Character('x'));
        (" "                        => Whitespace);
        ("#\\newline"               => Character('\u{000A}'));
        (" "                        => Whitespace);
        ("#\\NEWLINE"               => Character('\u{FFFD}')),
                             (0, 9) => err_lexer_unknown_character_name;
        (" "                        => Whitespace);
        ("#| Comment |#"            => Comment);
        (" "                        => Whitespace);

        ("#!FOLD-CASE"              => Directive("fold-case"));
        ("\n"                       => Whitespace);

        ("Test"                     => Identifier("test"));
        (" "                        => Whitespace);
        ("|Test|"                   => Identifier("Test"));
        (" "                        => Whitespace);
        ("\"tESt\""                 => String("tESt"));
        (" "                        => Whitespace);
        ("#xDEAD"                   => Number("#xDEAD"));
        (" "                        => Whitespace);
        ("+Inf.0"                   => Number("+Inf.0"));
        (" "                        => Whitespace);
        ("#\\R"                     => Character('R'));
        (" "                        => Whitespace);
        ("#\\x"                     => Character('x'));
        (" "                        => Whitespace);
        ("#\\newline"               => Character('\u{000A}'));
        (" "                        => Whitespace);
        ("#\\NEWLINE"               => Character('\u{000A}'));
        (" "                        => Whitespace);
        ("#| Comment |#"            => Comment);
        (" "                        => Whitespace);

        ("#!No-Fold-Case"           => Directive("no-fold-case"));
        ("\n"                       => Whitespace);

        ("Test"                     => Identifier("Test"));
        (" "                        => Whitespace);
        ("|Test|"                   => Identifier("Test"));
        (" "                        => Whitespace);
        ("\"tESt\""                 => String("tESt"));
        (" "                        => Whitespace);
        ("#xDEAD"                   => Number("#xDEAD"));
        (" "                        => Whitespace);
        ("+Inf.0"                   => Number("+Inf.0"));
        (" "                        => Whitespace);
        ("#\\R"                     => Character('R'));
        (" "                        => Whitespace);
        ("#\\x"                     => Character('x'));
        (" "                        => Whitespace);
        ("#\\newline"               => Character('\u{000A}'));
        (" "                        => Whitespace);
        ("#\\NEWLINE"               => Character('\u{FFFD}')),
                             (0, 9) => err_lexer_unknown_character_name;
        (" "                        => Whitespace);
        ("#| Comment |#"            => Comment);
        (" "                        => Whitespace);

        ("#!NO-fold-case"           => Directive("no-fold-case"));
        ("\n"                       => Whitespace);
        ("#!FOLD-CASE"              => Directive("fold-case"));
        ("\n"                       => Whitespace);
        ("#!no-FOLD-case"           => Directive("no-fold-case"));
        ("\n"                       => Whitespace);
        ("#!no-fold-CASE"           => Directive("no-fold-case"));
        ("\n"                       => Whitespace);
        ("#!fold-case"              => Directive("fold-case"));
        ("\n"                       => Whitespace);
        ("; #!no-fold-case\n"       => Comment);

        ("Test"                     => Identifier("test"));
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Case control: Unicode

#[test]
#[cfg(feature = "unicode")]
fn directives_case_control_unicode() {
    check! {
        ("\u{0411}\u{043B}\u{041E}\u{043D}\u{0414}\u{0438}"                     => Identifier("\u{0411}\u{043B}\u{041E}\u{043D}\u{0414}\u{0438}"));
        (" "                                                                    => Whitespace);
        ("|\u{0054}\u{0065}\u{FB06}|"                                           => Identifier("\u{0054}\u{0065}\u{FB06}"));
        (" "                                                                    => Whitespace);
        ("\"\u{0111}\u{006F}\u{0302}\u{0300}\u{0053}\u{0055}\u{031B}\u{0301}\"" => String("\u{0111}\u{006F}\u{0302}\u{0300}\u{0053}\u{0055}\u{031B}\u{0301}"));
        (" "                                                                    => Whitespace);
        ("#x\u{1D56F}\u{1D570}\u{1D56C}\u{1D56F}"                               => Identifier("DEAD")),
                                                                         (0, 2) => err_lexer_prefixed_identifier;
        (" "                                                                    => Whitespace);
        ("#\\\u{2126}"                                                          => Character('\u{2126}'));
        (" "                                                                    => Whitespace);
        ("#\\\u{1D697}\u{1D68E}\u{1D6A0}\u{1D695}\u{1D692}\u{1D697}\u{1D68E}"   => Character('\u{000A}'));
        (" "                                                                    => Whitespace);
        ("#\\\u{1D4DD}\u{1D4D4}\u{1D4E6}\u{1D4DB}\u{1D4D8}\u{1D4DD}\u{1D4D4}"   => Character('\u{FFFD}')),
                                                                        (0, 30) => err_lexer_unknown_character_name;
        (" "                                                                    => Whitespace);

        ("#!fold-case"                                                          => Directive("fold-case"));
        ("\n"                                                                   => Whitespace);

        ("\u{0411}\u{043B}\u{041E}\u{043D}\u{0414}\u{0438}"                     => Identifier("\u{0431}\u{043B}\u{043E}\u{043D}\u{0434}\u{0438}"));
        (" "                                                                    => Whitespace);
        ("|\u{0054}\u{0065}\u{FB06}|"                                           => Identifier("\u{0054}\u{0065}\u{FB06}"));
        (" "                                                                    => Whitespace);
        ("\"\u{0111}\u{006F}\u{0302}\u{0300}\u{0053}\u{0055}\u{031B}\u{0301}\"" => String("\u{0111}\u{006F}\u{0302}\u{0300}\u{0053}\u{0055}\u{031B}\u{0301}"));
        (" "                                                                    => Whitespace);
        ("#x\u{1D56F}\u{1D570}\u{1D56C}\u{1D56F}"                               => Identifier("dead")),
                                                                         (0, 2) => err_lexer_prefixed_identifier;
        (" "                                                                    => Whitespace);
        ("#\\\u{2126}"                                                          => Character('\u{2126}'));
        (" "                                                                    => Whitespace);
        ("#\\\u{1D697}\u{1D68E}\u{1D6A0}\u{1D695}\u{1D692}\u{1D697}\u{1D68E}"   => Character('\u{000A}'));
        (" "                                                                    => Whitespace);
        ("#\\\u{1D4DD}\u{1D4D4}\u{1D4E6}\u{1D4DB}\u{1D4D8}\u{1D4DD}\u{1D4D4}"   => Character('\u{000A}'));
        (" "                                                                    => Whitespace);

        ("#!no-fold-case"                                                       => Directive("no-fold-case"));
        ("\n"                                                                   => Whitespace);

        ("\u{0411}\u{043B}\u{041E}\u{043D}\u{0414}\u{0438}"                     => Identifier("\u{0411}\u{043B}\u{041E}\u{043D}\u{0414}\u{0438}"));
        (" "                                                                    => Whitespace);
        ("|\u{0054}\u{0065}\u{FB06}|"                                           => Identifier("\u{0054}\u{0065}\u{FB06}"));
        (" "                                                                    => Whitespace);
        ("\"\u{0111}\u{006F}\u{0302}\u{0300}\u{0053}\u{0055}\u{031B}\u{0301}\"" => String("\u{0111}\u{006F}\u{0302}\u{0300}\u{0053}\u{0055}\u{031B}\u{0301}"));
        (" "                                                                    => Whitespace);
        ("#x\u{1D56F}\u{1D570}\u{1D56C}\u{1D56F}"                               => Identifier("DEAD")),
                                                                         (0, 2) => err_lexer_prefixed_identifier;
        (" "                                                                    => Whitespace);
        ("#\\\u{2126}"                                                          => Character('\u{2126}'));
        (" "                                                                    => Whitespace);
        ("#\\\u{1D697}\u{1D68E}\u{1D6A0}\u{1D695}\u{1D692}\u{1D697}\u{1D68E}"   => Character('\u{000A}'));
        (" "                                                                    => Whitespace);
        ("#\\\u{1D4DD}\u{1D4D4}\u{1D4E6}\u{1D4DB}\u{1D4D8}\u{1D4DD}\u{1D4D4}"   => Character('\u{FFFD}')),
                                                                        (0, 30) => err_lexer_unknown_character_name;
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Unicode normalization

#[test]
#[cfg(feature = "unicode")]
fn normalization_characters_explicit() {
    // Explicit characters are not normalized in any way.
    check! {
        // All these characters normalize into something different:
        ("#\\\u{2000}"                  => Character('\u{2000}'));
        (" "                            => Whitespace);
        ("#\\\u{095D}"                  => Character('\u{095D}'));
        (" "                            => Whitespace);
        ("#\\\u{1E9B}"                  => Character('\u{1E9B}'));
        (" "                            => Whitespace);
        ("#\\\u{2126}"                  => Character('\u{2126}'));
        (" "                            => Whitespace);
        ("#\\\u{1EBF}"                  => Character('\u{1EBF}'));
        (" "                            => Whitespace);

        // Unicode escapes are not normalized as well:
        ("#\\x2000"                     => Character('\u{2000}'));
        (" "                            => Whitespace);
        ("#\\x095D"                     => Character('\u{095D}'));
        (" "                            => Whitespace);
        ("#\\x1E9B"                     => Character('\u{1E9B}'));
        (" "                            => Whitespace);
        ("#\\x2126"                     => Character('\u{2126}'));
        (" "                            => Whitespace);
        ("#\\x1EBF"                     => Character('\u{1EBF}'));
        (" "                            => Whitespace);

        // Canonical equivalence is *not* observed:
        ("#\\\u{0065}\u{0301}\u{0302}"  => Character('\u{FFFD}')),
                                 (0, 7) => err_lexer_unknown_character_name;
        (" "                            => Whitespace);
        ("#\\\u{0438}\u{0306}"          => Character('\u{FFFD}')),
                                 (0, 6) => err_lexer_unknown_character_name;
    }
}

#[test]
#[cfg(feature = "unicode")]
fn normalization_characters_named() {
    // Character names are normalized as case-sensitive identifiers.
    check! {
        // They are NFKC-normalized:
        ("#\\\u{02B3}\u{1D49}turn"      => Character('\u{000D}'));
        (" "                            => Whitespace);

        // ZWNJ and ZWJ are ignored:
        ("#\\nu\u{200C}ll"              => Character('\u{0000}'));
        (" "                            => Whitespace);

        // Names are still case-sensitive by default:
        ("#\\\u{24C9}\u{24B6}\u{24B7}"  => Character('\u{FFFD}')),
                                (0, 11) => err_lexer_unknown_character_name;
        (" "                            => Whitespace);
        ("#!fold-case"                  => Directive("fold-case"));
        (" "                            => Whitespace);
        ("#\\\u{24C9}\u{24B6}\u{24B7}"  => Character('\u{0009}'));
    }
}

#[test]
#[cfg(feature = "unicode")]
fn normalization_directives() {
    // Directive names are normalized as case-insensitive identifiers.
    check! {
        // They are NFKC-normalized and case-folded:
        ("#!\u{1D571}\u{1D594}\u{1D591}\u{1D589}\u{200C}\u{002D}\u{1D56E}\u{1D586}\u{1D598}\u{1D58A}" => Directive("fold-case"));
        (" " => Whitespace);

        // This normalizes into a different string if NFC, NFD, or NFKD are used:
        ("#!\u{01C4}\u{03D4}\u{1E9B}\u{FBA5}\u{FEFA}" => Directive("\u{0064}\u{017E}\u{03CB}\u{1E61}\u{06C0}\u{0644}\u{0625}")),
            (0, 15) => err_lexer_unknown_directive;
        (" " => Whitespace);

        // This string has combining marks in non-canonical order:
        ("#!A\u{1DCE}\u{0327}\u{0334}\u{1DF5}\u{0333}" => Directive("a\u{0334}\u{0327}\u{1DCE}\u{0333}\u{1DF5}")),
            (0, 15) => err_lexer_unknown_directive;
    }
}

#[test]
#[cfg(feature = "unicode")]
fn normalization_identifiers_plain() {
    // Plain identifiers are NFKC-normalized.
    check! {
        // This normalizes into a different string if NFC, NFD, or NFKD are used:
        ("\u{01C4}\u{03D4}\u{1E9B}\u{FBA5}\u{FEFA}" => Identifier("\u{0044}\u{017D}\u{03AB}\u{1E61}\u{06C0}\u{0644}\u{0625}"));
        (" " => Whitespace);

        // This string has combining marks in non-canonical order:
        ("A\u{1DCE}\u{0327}\u{0334}\u{1DF5}\u{0333}" => Identifier("A\u{0334}\u{0327}\u{1DCE}\u{0333}\u{1DF5}"));
    }
}

#[test]
#[cfg(feature = "unicode")]
fn normalization_identifiers_escaped() {
    // Escaped identifiers are never normalized (just like strings).
    check! {
        // Regular strings that normalize into different strings:
        ("|\u{2000}|"                       => Identifier("\u{2000}"));
        (" "                                => Whitespace);
        ("|\u{095D}\u{095E}\u{095F}|"       => Identifier("\u{095D}\u{095E}\u{095F}"));
        (" "                                => Whitespace);
        ("|\u{1E9B}|"                       => Identifier("\u{1E9B}"));
        (" "                                => Whitespace);
        ("|\u{2126}|"                       => Identifier("\u{2126}"));
        (" "                                => Whitespace);
        ("|\u{1EBF}|"                       => Identifier("\u{1EBF}"));
        (" "                                => Whitespace);
        ("|\u{0064}\u{0301}\u{0302}|"       => Identifier("\u{0064}\u{0301}\u{0302}"));
        (" "                                => Whitespace);
        ("|\u{0064}\u{0302}\u{0301}|"       => Identifier("\u{0064}\u{0302}\u{0301}"));
        (" "                                => Whitespace);

        // Unicode escapes are also not normalized:
        ("|\\x2000;|"                       => Identifier("\u{2000}"));
        (" "                                => Whitespace);
        ("|\\x095D;\\x095E;\\x095F;|"       => Identifier("\u{095D}\u{095E}\u{095F}"));
        (" "                                => Whitespace);
        ("|\\x1E9B;|"                       => Identifier("\u{1E9B}"));
        (" "                                => Whitespace);
        ("|\\x2126;|"                       => Identifier("\u{2126}"));
        (" "                                => Whitespace);
        ("|\\x1EBF;|"                       => Identifier("\u{1EBF}"));
        (" "                                => Whitespace);
        ("|\\x0064;\\x0301;\\x0302;|"       => Identifier("\u{0064}\u{0301}\u{0302}"));
        (" "                                => Whitespace);
        ("|\\x0064;\\x0302;\\x0301;|"       => Identifier("\u{0064}\u{0302}\u{0301}"));
    }
}

#[test]
#[cfg(feature = "unicode")]
fn normalization_labels() {
    // Labels are not normalized in any way.
    check! {
        ("#\u{FF11}\u{FF12}\u{FF13}="   => Identifier("23=")),
                                 (0, 4) => err_lexer_invalid_number_prefix,
                                 (0, 4) => err_lexer_prefixed_identifier,
                                 (4, 7) => err_lexer_invalid_identifier_character,
                                (4, 11) => warn_lexer_identifier_looks_like_number;
        (" "                            => Whitespace);
        ("#1\u{FF12}\u{FF13}="          => LabelMark("1\u{FF12}\u{FF13}")),
                                 (2, 5) => err_lexer_invalid_number_character,
                                 (5, 8) => err_lexer_invalid_number_character;
    }
}

#[test]
#[cfg(feature = "unicode")]
fn normalization_numbers() {
    // Numbers are not normalized in any way.
    check! {
        ("123\u{FF14}5"                 => Number("123\u{FF14}5")),
                                 (3, 6) => err_lexer_invalid_number_character;
        (" "                            => Whitespace);
        ("\u{FF11}\u{FF12}\u{FF13}"     => Identifier("123")),
                                 (0, 3) => err_lexer_invalid_identifier_character,
                                 (0, 9) => warn_lexer_identifier_looks_like_number;
        (" "                            => Whitespace);
        ("3\u{FF0E}14"                  => Number("3\u{FF0E}14")),
                                 (1, 4) => err_lexer_invalid_number_character;
        (" "                            => Whitespace);
        ("1.0\u{1D41E}+20"              => Number("1.0\u{1D41E}+20")),
                                 (3, 7) => err_lexer_invalid_number_character,
                               (10, 10) => err_lexer_missing_i;
        (" "                            => Whitespace);
        ("1.0\u{212E}+20"               => Number("1.0\u{212E}+20")),
                                 (3, 6) => err_lexer_invalid_number_character,
                                 (9, 9) => err_lexer_missing_i;
        (" "                            => Whitespace);
        ("1\u{207A}2i"                  => Number("1\u{207A}2i")),
                                 (1, 4) => err_lexer_invalid_number_character;
        (" "                            => Whitespace);
        ("5\u{FE6B}6"                   => Number("5\u{FE6B}6")),
                                 (1, 4) => err_lexer_invalid_number_character;
        (" "                            => Whitespace);
        ("5\u{FE6B}-6"                  => Number("5\u{FE6B}-6")),
                                 (1, 4) => err_lexer_invalid_number_character,
                                 (6, 6) => err_lexer_missing_i;
        (" "                            => Whitespace);
        ("-79\u{2170}"                  => Number("-79\u{2170}")),
                                 (3, 6) => err_lexer_invalid_number_character;
        (" "                            => Whitespace);
        ("+1-2\u{2170}"                 => Number("+1-2\u{2170}")),
                                 (4, 7) => err_lexer_invalid_number_character,
                                 (7, 7) => err_lexer_missing_i;
        (" "                            => Whitespace);
        ("3+4\u{1D4BE}"                 => Number("3+4\u{1D4BE}")),
                                 (3, 7) => err_lexer_invalid_number_character,
                                 (7, 7) => err_lexer_missing_i;
        (" "                            => Whitespace);
        ("5-67\u{2111}"                 => Number("5-67\u{2111}")),
                                 (4, 7) => err_lexer_invalid_number_character,
                                 (7, 7) => err_lexer_missing_i;
        (" "                            => Whitespace);
        ("+\u{1D7CF}\u{1D7D0}\u{1D7D1}" => Identifier("+123")),
                                (0, 13) => warn_lexer_identifier_looks_like_number;
        (" "                            => Whitespace);
        ("+\u{1D697}a\u{1D67D}.0"       => Identifier("+naN.0")),
                                (0, 12) => warn_lexer_identifier_looks_like_number;
        (" "                            => Whitespace);
        ("-\u{00BD}"                    => Identifier("-1\u{2044}2")),
                                 (0, 3) => warn_lexer_identifier_looks_like_number;
        (" "                            => Whitespace);
        ("\u{00BD}"                     => Identifier("1\u{2044}2")),
                                 (0, 2) => err_lexer_invalid_identifier_character,
                                 (0, 2) => warn_lexer_identifier_looks_like_number;
        (" "                            => Whitespace);
        ("\u{FF0E}123"                  => Identifier(".123")),
                                 (0, 6) => warn_lexer_identifier_looks_like_number;
        (" "                            => Whitespace);
        ("\u{FF0D}\u{2139}"             => Identifier("-i")),
                                 (0, 6) => warn_lexer_identifier_looks_like_number;
    }
}

#[test]
#[cfg(feature = "unicode")]
fn normalization_strings() {
    // Strings are not normalized in any way.
    check! {
        // Regular strings that normalize into different strings:
        ("\"\u{2000}\""                     => String("\u{2000}"));
        (" "                                => Whitespace);
        ("\"\u{095D}\u{095E}\u{095F}\""     => String("\u{095D}\u{095E}\u{095F}"));
        (" "                                => Whitespace);
        ("\"\u{1E9B}\""                     => String("\u{1E9B}"));
        (" "                                => Whitespace);
        ("\"\u{2126}\""                     => String("\u{2126}"));
        (" "                                => Whitespace);
        ("\"\u{1EBF}\""                     => String("\u{1EBF}"));
        (" "                                => Whitespace);
        ("\"\u{0064}\u{0301}\u{0302}\""     => String("\u{0064}\u{0301}\u{0302}"));
        (" "                                => Whitespace);
        ("\"\u{0064}\u{0302}\u{0301}\""     => String("\u{0064}\u{0302}\u{0301}"));
        (" "                                => Whitespace);

        // Unicode escapes are also not normalized:
        ("\"\\x2000;\""                     => String("\u{2000}"));
        (" "                                => Whitespace);
        ("\"\\x095D;\\x095E;\\x095F;\""     => String("\u{095D}\u{095E}\u{095F}"));
        (" "                                => Whitespace);
        ("\"\\x1E9B;\""                     => String("\u{1E9B}"));
        (" "                                => Whitespace);
        ("\"\\x2126;\""                     => String("\u{2126}"));
        (" "                                => Whitespace);
        ("\"\\x1EBF;\""                     => String("\u{1EBF}"));
        (" "                                => Whitespace);
        ("\"\\x0064;\\x0301;\\x0302;\""     => String("\u{0064}\u{0301}\u{0302}"));
        (" "                                => Whitespace);
        ("\"\\x0064;\\x0302;\\x0301;\""     => String("\u{0064}\u{0302}\u{0301}"));
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Disabled Unicode

#[test]
#[cfg(not(feature = "unicode"))]
fn no_unicode_identifiers_non_ascii_invalid_in_plain() {
    check! {
        ("\u{01C5}\u{0207}\u{005F}\u{0442}\u{0435}\u{0441}\u{0442}"     => Identifier("\u{01C5}\u{0207}\u{005F}\u{0442}\u{0435}\u{0441}\u{0442}")),
                                                                 (0, 2) => err_lexer_invalid_identifier_character,
                                                                 (2, 4) => err_lexer_invalid_identifier_character,
                                                                 (5, 7) => err_lexer_invalid_identifier_character,
                                                                 (7, 9) => err_lexer_invalid_identifier_character,
                                                                (9, 11) => err_lexer_invalid_identifier_character,
                                                               (11, 13) => err_lexer_invalid_identifier_character;
        (" "                                                            => Whitespace);
        ("\u{0915}\u{094D}\u{200D}\u{0937}"                             => Identifier("\u{0915}\u{094D}\u{200D}\u{0937}")),
                                                                 (0, 3) => err_lexer_invalid_identifier_character,
                                                                 (3, 6) => err_lexer_invalid_identifier_character,
                                                                 (6, 9) => err_lexer_invalid_identifier_character,
                                                                (9, 12) => err_lexer_invalid_identifier_character;
        (" "                                                            => Whitespace);
        ("\u{1D59B}\u{1D586}\u{1D591}\u{1D58E}\u{1D589}"                => Identifier("\u{1D59B}\u{1D586}\u{1D591}\u{1D58E}\u{1D589}")),
                                                                 (0, 4) => err_lexer_invalid_identifier_character,
                                                                 (4, 8) => err_lexer_invalid_identifier_character,
                                                                (8, 12) => err_lexer_invalid_identifier_character,
                                                               (12, 16) => err_lexer_invalid_identifier_character,
                                                               (16, 20) => err_lexer_invalid_identifier_character;
    }
}

#[test]
#[cfg(not(feature = "unicode"))]
fn no_unicode_identifiers_non_ascii_okay_in_escaped() {
    check! {
        ("|\u{01C5}\u{0207}\u{005F}\u{0442}\u{0435}\u{0441}\u{0442}|"   => Identifier("\u{01C5}\u{0207}\u{005F}\u{0442}\u{0435}\u{0441}\u{0442}"));
        (" "                                                            => Whitespace);
        ("|\u{0915}\u{094D}\u{200D}\u{0937}|"                           => Identifier("\u{0915}\u{094D}\u{200D}\u{0937}"));
        (" "                                                            => Whitespace);
        ("|\u{1D59B}\u{1D586}\\x1D591;\u{1D58E}\u{1D589}|"              => Identifier("\u{1D59B}\u{1D586}\u{1D591}\u{1D58E}\u{1D589}"));
    }
}

#[test]
#[cfg(not(feature = "unicode"))]
fn no_unicode_characters_non_ascii_okay() {
    check! {
        ("#\\\u{03BB}"  => Character('\u{03BB}'));
        (" "            => Whitespace);
        ("#\\x03BB"     => Character('\u{03BB}'));
    }
}

#[test]
#[cfg(not(feature = "unicode"))]
fn no_unicode_characters_no_normalization() {
    check! {
        ("#\\\u{1106}\u{1169}\u{11A8}"  => Character('\u{FFFD}')),
                                (0, 11) => err_lexer_unknown_character_name;
        (" "                            => Whitespace);
        ("#\\new\u{2000C}line"          => Character('\u{FFFD}')),
                                (0, 13) => err_lexer_unknown_character_name;
    }
}

#[test]
#[cfg(not(feature = "unicode"))]
fn no_unicode_strings_non_ascii_okay() {
    check! {
        ("\"\u{FF8A}\u{FF91}\u{FF7D}\u{FF80}\u{FF70}\"" => String("\u{FF8A}\u{FF91}\u{FF7D}\u{FF80}\u{FF70}"));
        (" "                                            => Whitespace);
        ("\"\\xFF8A;\\xFF91;\\xFF7D;\\xFF80;\\xFF70;\"" => String("\u{FF8A}\u{FF91}\u{FF7D}\u{FF80}\u{FF70}"));
    }
}

#[test]
#[cfg(not(feature = "unicode"))]
fn no_unicode_strings_no_normalization() {
    check! {
        ("\"\u{01C4}\u{03D4}\u{1E9B}\u{FBA5}\u{FEFA}\"" => String("\u{01C4}\u{03D4}\u{1E9B}\u{FBA5}\u{FEFA}"));
    }
}

#[test]
#[cfg(not(feature = "unicode"))]
fn no_unicode_directives_no_normalization() {
    check! {
        ("#!\u{1D571}\u{1D594}\u{1D591}\u{1D589}\u{200C}\u{002D}\u{1D56E}\u{1D586}\u{1D598}\u{1D58A}" => Directive("\u{1D571}\u{1D594}\u{1D591}\u{1D589}\u{200C}\u{002D}\u{1D56E}\u{1D586}\u{1D598}\u{1D58A}")),
            (0, 38) => err_lexer_unknown_directive;
    }
}

#[test]
#[cfg(not(feature = "unicode"))]
fn no_unicode_labels_no_normalization() {
    check! {
        ("#1\u{FF12}\u{FF13}="  => LabelMark("1\u{FF12}\u{FF13}")),
            (2, 5) => err_lexer_invalid_number_character,
            (5, 8) => err_lexer_invalid_number_character;
    }
}

#[test]
#[cfg(not(feature = "unicode"))]
fn no_unicode_numbers_no_normalization() {
    check! {
        ("123\u{FF14}5"                 => Number("123\u{FF14}5")),
                                 (3, 6) => err_lexer_invalid_number_character;
        (" "                            => Whitespace);
        ("\u{FF11}\u{FF12}\u{FF13}"     => Identifier("\u{FF11}\u{FF12}\u{FF13}")),
                                 (0, 3) => err_lexer_invalid_identifier_character,
                                 (3, 6) => err_lexer_invalid_identifier_character,
                                 (6, 9) => err_lexer_invalid_identifier_character;
        (" "                            => Whitespace);
        ("3\u{FF0E}14"                  => Number("3\u{FF0E}14")),
                                 (1, 4) => err_lexer_invalid_number_character;
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Test helpers

use std::cell::RefCell;
use std::rc::Rc;

use utils::diff::sequence::{self, Diff};
use utils::stubs::{SinkReporter};
use utils::pretty::diff::sequence as pretty_sequence;

struct ScannerTestDiagnostic {
    pub from: usize,
    pub to: usize,
    pub kind: DiagnosticKind,
}

struct ScannerTestSlice<'a> {
    pub slice: &'a str,
    pub token: Token,
    pub diagnostics: &'a [ScannerTestDiagnostic],
}

struct ScannerTestResults {
    pub tokens: Vec<ScannedToken>,
    pub diagnostics: Vec<Diagnostic>,
}

/// Check whether the scanner produces expected results and reports expected diagnostics
/// when given a sequence of test string slices. Panic if this is not true.
fn check(pool: &InternPool, slices: &[ScannerTestSlice]) {
    let test_string = concatenate_test_slices(slices);
    let expected = compute_expected_results(slices);
    let actual = compute_scanning_results(&test_string, pool);

    let token_mismatches =
        verify("Tokens", &expected.tokens, &actual.tokens,
            |token| print_token(token, &test_string, pool));

    let diagnostic_mismatches =
        verify("Diagnostics", &expected.diagnostics, &actual.diagnostics,
            |diagnostic| print_diagnostic(diagnostic, &test_string));

    if token_mismatches.is_err() || diagnostic_mismatches.is_err() {
        println!("");

        if let Err(string) = token_mismatches {
            println!("{}", string);
        }
        if let Err(string) = diagnostic_mismatches {
            println!("{}", string);
        }

        panic!("test failed");
    }
}

/// Construct the string to be scanned from all the slices.
fn concatenate_test_slices(test_slices: &[ScannerTestSlice]) -> String {
    let mut test_string = String::new();

    for ref test_slice in test_slices {
        test_string.push_str(test_slice.slice);
    }

    return test_string;
}

/// Build expected tokens and diagnostics from slices by filling in the offsets.
fn compute_expected_results(test_slices: &[ScannerTestSlice]) -> ScannerTestResults {
    let mut tokens = Vec::new();
    let mut diagnostics = Vec::new();
    let mut byte_offset = 0;

    for ref test_slice in test_slices {
        let token_length = test_slice.slice.len();

        tokens.push(ScannedToken {
            tok: test_slice.token.clone(),
            span: Span::new(byte_offset, byte_offset + token_length),
        });

        for ref diagnostic in test_slice.diagnostics {
            assert!(test_slice.slice.is_char_boundary(diagnostic.from));
            assert!(test_slice.slice.is_char_boundary(diagnostic.to));

            diagnostics.push(Diagnostic {
                kind: diagnostic.kind.clone(),
                loc: Some(Span::new(byte_offset + diagnostic.from,
                                    byte_offset + diagnostic.to)),
            });
        }

        byte_offset += token_length;
    }

    tokens.push(ScannedToken {
        tok: Token::Eof,
        span: Span::new(byte_offset, byte_offset),
    });

    return ScannerTestResults {
        tokens: tokens,
        diagnostics: diagnostics,
    };
}

/// Scan over the string and remember all produced tokens and diagnostics.
fn compute_scanning_results(string: &str, pool: &InternPool) -> ScannerTestResults {
    let diagnostics = Rc::new(RefCell::new(Vec::new()));
    let mut tokens = Vec::new();
    {
        let reporter = SinkReporter::new(diagnostics.clone());
        let handler = Handler::with_reporter(Box::new(reporter));

        let mut scanner = StringScanner::new(string, &handler, pool);

        loop {
            let token = scanner.next_token();
            let done = token.tok == Token::Eof;
            tokens.push(token);
            if done { break; }
        }

        assert!(scanner.at_eof());
    }
    return ScannerTestResults {
        tokens: tokens,
        diagnostics: Rc::try_unwrap(diagnostics).unwrap().into_inner(),
    };
}

/// Print out and verifies produced results. Returns Ok if everything is fine,
/// otherwise an Err with a string to be shown the user is returned.
fn verify<T, F>(title: &str, expected: &[T], actual: &[T], to_string: F) -> Result<(), String>
    where T: Eq, F: Fn(&T) -> String
{
    let diff = sequence::unfold_replacements(sequence::diff(expected, actual));

    if diff.iter().all(|item|{ match *item { Diff::Equal(_, _) => true, _ => false } }) {
        return Ok(());
    }

    let mut report = String::new();

    report.push_str(title);
    report.push_str(":\n");

    for line in pretty_sequence::format_unified_with(&diff, to_string).lines() {
        report.push_str(&line[0..1]);
        report.push_str(" ");
        report.push_str(&line[1..]);
        report.push_str("\n");
    }

    return Err(report);
}

/// Pretty-print a token in diffs.
fn print_token(token: &ScannedToken, buf: &str, pool: &InternPool) -> String {
    format!("{token} @ [{from}, {to}] = {slice:?}",
        token = pretty_print_token(token, pool),
        from  = token.span.from,
        to    = token.span.to,
        slice = &buf[token.span.from..token.span.to],
    )
}

/// Pretty-print a diagnostic in diffs.
fn print_diagnostic(diagnostic: &Diagnostic, buf: &str) -> String {
    if let Some(ref loc) = diagnostic.loc {
        format!("{diagnostic} @ [{from}, {to}] = {slice:?}",
            diagnostic = pretty_print_diagnostic(diagnostic),
            from       = loc.from,
            to         = loc.to,
            slice      = &buf[loc.from..loc.to],
        )
    } else {
        format!("{diagnostic} @ nowhere",
            diagnostic = pretty_print_diagnostic(diagnostic),
        )
    }
}

/// Pretty-print a token.
fn pretty_print_token(token: &ScannedToken, pool: &InternPool) -> String {
    match token.tok {
        Token::String(value) => {
            format!("String({:?})", pool.get(value))
        }
        Token::Number(value) => {
            format!("Number({:?})", pool.get(value))
        }
        Token::Identifier(value) => {
            format!("Identifier({:?})", pool.get(value))
        }
        Token::Directive(value) => {
            format!("Directive({:?})", pool.get(value))
        }
        _ => format!("{:?}", token.tok)
    }
}

/// Pretty-print a diagnostic.
fn pretty_print_diagnostic(diagnostic: &Diagnostic) -> String {
    match diagnostic.kind {
        _ => format!("{:?}", diagnostic.kind)
    }
}
