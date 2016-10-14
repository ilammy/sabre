// Copyright (c) 2016, ilammy
//
// Licensed under MIT license (see LICENSE file in the root directory).
// This file may be copied, distributed, and modified only in accordance
// with the terms specified by this license.

//! Lexer test suite.
//!
//! This verifies that the lexer recognizes all expected tokens and errors.

extern crate reader;

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
    { $pool:expr, Character($value:expr) }          => { Token::Character($value) };
    { $pool:expr, String($value:expr) }             => { Token::String($pool.intern($value)) };
    { $pool:expr, Identifier($value:expr) }         => { Token::Identifier($pool.intern($value)) };
    { $pool:expr, Number($value:expr) }             => { Token::Number($pool.intern($value)) };
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Smoke test of test harness

#[test]
fn empty_string() {
    check! { }
}

#[test]
fn whitespace() {
    check! {
        ("   \t\t\r\n  \t \t\n" => Whitespace);
    }
}

#[test]
fn garbage() {
    check! {
        ("\x01\x02\x03\x04" => Unrecognized),
            (0, 4) => err_lexer_unrecognized;
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
fn quotations() {
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
fn parentheses() {
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
fn brackets_and_braces() {
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
fn recover_open_vector() {
    check! {
        ("#ahaha-oh-wow"    => Unrecognized),
                    (0, 13) => err_lexer_unrecognized;
        ("("                => Open(Parenthesis));
        ("#:"               => Unrecognized),
                    (0, 2)  => err_lexer_unrecognized;
        ("["                => Open(Bracket));
        ("#"                => Unrecognized),
                    (0, 1)  => err_lexer_unrecognized;
        (","                => Comma);
        ("{"                => Open(Brace));
        ("#"                => Unrecognized),
                    (0, 1)  => err_lexer_unrecognized;
        (" "                => Whitespace);
        ("#"                => Unrecognized),
                    (0, 1)  => err_lexer_unrecognized;
        (",@"               => CommaSplicing);
        (" "                => Whitespace);
        ("#"                => Unrecognized),
                    (0, 1)  => err_lexer_unrecognized;
        ("`"                => Backquote);
        ("["                => Open(Bracket));
        (" "                => Whitespace);
        ("#####"            => Number("#####")),
                    (0, 1)  => err_lexer_invalid_number_prefix,
                    (1, 2)  => err_lexer_invalid_number_prefix,
                    (2, 3)  => err_lexer_invalid_number_prefix,
                    (3, 4)  => err_lexer_invalid_number_prefix,
                    (4, 5)  => err_lexer_invalid_number_prefix,
                    (5, 5)  => err_lexer_digits_missing;
        ("("                => Open(Parenthesis));
        (" "                => Whitespace);
        ("#."               => Number("#.")),
                    (0, 1)  => err_lexer_invalid_number_prefix,
                    (1, 2)  => err_lexer_digits_missing;
        ("{"                => Open(Brace));
    }
}

#[test]
fn recover_open_bytevector() {
    check! {
        ("#u("              => OpenBytevector(Parenthesis)),
                    (0, 3)  => err_lexer_invalid_bytevector;
        (" "                => Whitespace);
        ("#U["              => OpenBytevector(Bracket)),
                    (0, 3)  => err_lexer_invalid_bytevector;
        (" "                => Whitespace);
        ("#ufo"             => Unrecognized),
                    (0, 4)  => err_lexer_unrecognized;
        ("("                => Open(Parenthesis));
        (" "                => Whitespace);
        ("#u16{"            => OpenBytevector(Brace)),
                    (0, 5)  => err_lexer_invalid_bytevector;
        (" "                => Whitespace);
        ("#U571"            => Unrecognized),
                    (0, 5)  => err_lexer_unrecognized;
        ("["                => Open(Bracket));
        (" "                => Whitespace);
        ("#u90_ex.a@mp!le1" => Unrecognized),
                    (0, 16) => err_lexer_unrecognized;
        ("("                => Open(Parenthesis));
        (" "                => Whitespace);
        ("#8"               => Number("#8")),
                    (0, 1)  => err_lexer_invalid_number_prefix;
        ("["                => Open(Bracket));
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Characters

#[test]
fn characters_immediate() {
    check! {
        ("#\\t"         => Character('t'));
        ("#\\e"         => Character('e'));
        ("#\\s"         => Character('s'));
        ("#\\t"         => Character('t'));
        (" "            => Whitespace);
        ("#\\X"         => Character('X'));
        (" "            => Whitespace);
        ("#\\\u{1234}"  => Character('\u{1234}'));
        (" "            => Whitespace);
        ("#\\("         => Character('('));
        ("#\\)"         => Character(')'));
        ("#\\."         => Character('.'));
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
        ("#\\\\"        => Character('\\'));
        ("#\\#"         => Character('#'));
        (" "            => Whitespace);
        ("#\\\""        => Character('\"'));
        (" "            => Whitespace);
        ("#\\("         => Character('('));
        ("("            => Open(Parenthesis));
        (" "            => Whitespace);
        ("#\\;"         => Character(';'));
        ("#\\;"         => Character(';'));
        (" "            => Whitespace);
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
fn recover_character_names() {
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
        ("#\\\u{0}\u{1}\u{2}"               => Character('\u{FFFD}')),
                                     (0, 5) => err_lexer_unknown_character_name;
        ("#\\\u{4}\u{5}\u{6}"               => Character('\u{FFFD}')),
                                     (0, 5) => err_lexer_unknown_character_name;
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
        ("+\u{1}\u{2}\u{3}"     => Unrecognized),
                         (0, 4) => err_lexer_unrecognized;
        ("\n"                   => Whitespace);
        ("#O-\u{0}"             => Number("#O-\u{0}")),
                         (3, 4) => err_lexer_invalid_number_character;
        ("\n"                   => Whitespace);
        ("#i#X\u{F}"            => Number("#i#X\u{F}")),
                         (4, 5) => err_lexer_invalid_number_character;
        ("\n"                   => Whitespace);
        ("#b#e-+-5"             => Number("#b#e-+-5")),
                         (5, 6) => err_lexer_invalid_number_character,
                         (6, 7) => err_lexer_invalid_number_character,
                         (7, 8) => err_lexer_invalid_number_digit;
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
        ("#@#"                  => Number("#@#")),
                         (0, 2) => err_lexer_invalid_number_prefix,
                         (2, 3) => err_lexer_invalid_number_prefix,
                         (3, 3) => err_lexer_digits_missing;
        (" "                    => Whitespace);
        ("##"                   => Number("##")),
                         (0, 1) => err_lexer_invalid_number_prefix,
                         (1, 2) => err_lexer_invalid_number_prefix,
                         (2, 2) => err_lexer_digits_missing;
        (" "                    => Whitespace);
        ("#"                    => Unrecognized),
                         (0, 1) => err_lexer_unrecognized;
        (" "                    => Whitespace);
        ("##123"                => Number("##123")),
                         (0, 1) => err_lexer_invalid_number_prefix,
                         (1, 2) => err_lexer_invalid_number_prefix;
        (" "                    => Whitespace);
        ("#123"                 => Number("#123")),
                         (0, 1) => err_lexer_invalid_number_prefix;
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
        ("#o#"                  => Number("#o#")),
                         (2, 3) => err_lexer_invalid_number_prefix,
                         (3, 3) => err_lexer_digits_missing;
        (" "                    => Whitespace);
        ("#x#"                  => Number("#x#")),
                         (2, 3) => err_lexer_invalid_number_prefix,
                         (3, 3) => err_lexer_digits_missing;
        (" "                    => Whitespace);
        ("#"                    => Unrecognized),
                         (0, 1) => err_lexer_unrecognized;
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
        ("0!OMG-I-CAN"          => Number("0!OMG-I-CAN")),
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
                     (5, 6) => err_lexer_invalid_number_character,
                     (0, 2) => err_lexer_nondecimal_real;
        (" "                => Whitespace);
        ("#d42e+9"          => Number("#d42e+9"));
        (" "                => Whitespace);
        ("#b010010e-01101"  => Number("#b010010e-01101")),
                    (8,  9) => err_lexer_invalid_number_character,
                    (9, 10) => err_lexer_invalid_number_character;
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
                     (7, 8) => err_lexer_invalid_number_character;
    }
}

#[test]
fn recover_numbers_float_digits_missing() {
    check! {
        (".e0"              => Unrecognized),
                     (0, 3) => err_lexer_unrecognized;
        (" "                => Whitespace);
        ("#i.e0"            => Number("#i.e0")),
                     (2, 3) => err_lexer_digits_missing;
        (" "                => Whitespace);
        ("#d.e0"            => Number("#d.e0")),
                     (2, 3) => err_lexer_digits_missing;
        (" "                => Whitespace);
        ("#.e0"             => Number("#.e0")),
                     (0, 1) => err_lexer_invalid_number_prefix,
                     (1, 2) => err_lexer_digits_missing;
        (" "                => Whitespace);
        ("#."               => Number("#.")),
                     (0, 1) => err_lexer_invalid_number_prefix,
                     (1, 2) => err_lexer_digits_missing;
        (" "                => Whitespace);
        ("#de"              => Number("#de")),
                     (2, 2) => err_lexer_digits_missing,
                     (3, 3) => err_lexer_digits_missing;
        (" "                => Whitespace);
        ("#de+20"           => Number("#de+20")),
                     (2, 2) => err_lexer_digits_missing;
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
                     (6, 7) => err_lexer_invalid_number_character;
        (" "                => Whitespace);
        ("#xBAD"            => Number("#xBAD"));
        (" "                => Whitespace);
        ("100e!!!"          => Number("100e!!!")),
                     (4, 5) => err_lexer_invalid_number_character,
                     (5, 6) => err_lexer_invalid_number_character,
                     (6, 7) => err_lexer_invalid_number_character;
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
                     (7, 8) => err_lexer_invalid_number_character;
        (" "                => Whitespace);
        ("12.3E\u{0}"       => Number("12.3E\u{0}")),
                     (5, 6) => err_lexer_invalid_number_character;
        (" "                => Whitespace);
        (".0e\u{2212}9"     => Number(".0e\u{2212}9")),
                     (3, 6) => err_lexer_invalid_number_character;
        (" "                => Whitespace);
        ("5343x+23"         => Number("5343x+23")),
                     (4, 5) => err_lexer_invalid_number_character,
                     (5, 6) => err_lexer_invalid_number_character;
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
                    (4,  5) => err_lexer_invalid_number_character,
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
        ("+inf"             => Unrecognized),
                     (0, 4) => err_lexer_unrecognized;
        (" "                => Whitespace);
        ("-infinity"        => Unrecognized),
                     (0, 9) => err_lexer_unrecognized;
        (" "                => Whitespace);
        ("#d-infinity"      => Unrecognized),
                    (0, 11) => err_lexer_unrecognized;
        (" "                => Whitespace);
        ("+inf.01234"       => Number("+inf.01234")),
                    (6, 10) => err_lexer_infnan_suffix;
        (" "                => Whitespace);
        ("-NaN.1"           => Unrecognized),
                     (0, 6) => err_lexer_unrecognized;
        (" "                => Whitespace);
        ("+nan.0e+10"       => Number("+nan.0e+10")),
                    (6, 10) => err_lexer_infnan_suffix;
        (" "                => Whitespace);
        ("+nane+5"          => Unrecognized),
                     (0, 7) => err_lexer_unrecognized;
        (" "                => Whitespace);
        ("+nan.e5"          => Unrecognized),
                     (0, 7) => err_lexer_unrecognized;
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
// Test helpers

use std::cell::RefCell;
use std::fmt::Write;
use std::rc::Rc;

use utils::diff::sequence::{self, Diff};
use utils::stubs::{SinkReporter};

mod utils;

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
    let mut report = String::new();

    writeln!(&mut report, "{}", title).unwrap();

    let diff = sequence::diff(expected, actual);

    let mut okay = true;

    for entry in diff {
        match entry {
            Diff::Equal(ref actual, _) => {
                writeln!(&mut report, "  {}", to_string(actual)).unwrap();
            }
            Diff::Replace(ref actual, ref expected) => {
                okay = false;
                writeln!(&mut report, "- {}", to_string(actual)).unwrap();
                writeln!(&mut report, "+ {}", to_string(expected)).unwrap();
            }
            Diff::Left(ref actual) => {
                okay = false;
                writeln!(&mut report, "- {}", to_string(actual)).unwrap();
            }
            Diff::Right(ref expected) => {
                okay = false;
                writeln!(&mut report, "+ {}", to_string(expected)).unwrap();
            }
        }
    }

    return if okay { Ok(()) } else { Err(report) };
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
        _ => format!("{:?}", token.tok)
    }
}

/// Pretty-print a diagnostic.
fn pretty_print_diagnostic(diagnostic: &Diagnostic) -> String {
    match diagnostic.kind {
        _ => format!("{:?}", diagnostic.kind)
    }
}
