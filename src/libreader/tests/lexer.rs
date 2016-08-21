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
use reader::lexer::{ScannedToken, StringScanner, Scanner};
use reader::tokens::{Token, ParenType};

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Test macro helpers

macro_rules! check {
    { $( ($str:expr => $($token:tt)+) $(, ($from:expr, $to:expr) => $kind:ident)* ; )* } => {{
        let slices = &[
            $(ScannerTestSlice {
                slice: $str,
                token: token!($($token)+),
                diagnostics: &[
                    $(ScannerTestDiagnostic {
                        kind: DiagnosticKind::$kind,
                        from: $from, to: $to,
                    },)*
                ],
            },)*
        ];
        check(slices);
    }}
}

macro_rules! token {
    { $tok:ident }                      => { Token::$tok };
    { Open($ptype:ident) }              => { Token::Open(ParenType::$ptype) };
    { OpenVector($ptype:ident) }        => { Token::OpenVector(ParenType::$ptype) };
    { OpenBytevector($ptype:ident) }    => { Token::OpenBytevector(ParenType::$ptype) };
    { Close($ptype:ident) }             => { Token::Close(ParenType::$ptype) };
    { Character($value:expr) }          => { Token::Character($value) };
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
        ("#"                => Unrecognized),
                    (0, 1)  => err_lexer_unrecognized;
        ("#"                => Unrecognized),
                    (0, 1)  => err_lexer_unrecognized;
        ("#"                => Unrecognized),
                    (0, 1)  => err_lexer_unrecognized;
        ("#"                => Unrecognized),
                    (0, 1)  => err_lexer_unrecognized;
        ("#("               => OpenVector(Parenthesis));
        (" "                => Whitespace);
        ("#."               => Unrecognized),
                    (0, 2)  => err_lexer_unrecognized;
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
        ("#8"               => Unrecognized),
                    (0, 2)  => err_lexer_unrecognized;
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
fn check(slices: &[ScannerTestSlice]) {
    let test_string = concatenate_test_slices(slices);
    let expected = compute_expected_results(slices);
    let actual = compute_scanning_results(&test_string);

    let token_mismatches =
        verify("Tokens", &expected.tokens, &actual.tokens,
            |token| print_token(token, &test_string));

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
fn compute_scanning_results(string: &str) -> ScannerTestResults {
    let diagnostics = Rc::new(RefCell::new(Vec::new()));
    let mut tokens = Vec::new();
    {
        let reporter = SinkReporter::new(diagnostics.clone());
        let handler = Handler::with_reporter(Box::new(reporter));

        let mut scanner = StringScanner::new(string, &handler);

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

    let diff = sequence::diff(actual, expected);

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
fn print_token(token: &ScannedToken, buf: &str) -> String {
    format!("{token} @ [{from}, {to}] = {slice:?}",
        token = pretty_print_token(token),
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
fn pretty_print_token(token: &ScannedToken) -> String {
    match token.tok {
        _ => format!("{:?}", token.tok)
    }
}

/// Pretty-print a diagnostic.
fn pretty_print_diagnostic(diagnostic: &Diagnostic) -> String {
    match diagnostic.kind {
        _ => format!("{:?}", diagnostic.kind)
    }
}
