// Copyright (c) 2016, ilammy
//
// Licensed under MIT license (see LICENSE file in the root directory).
// This file may be copied, distributed, and modified only in accordance
// with the terms specified by this license.

//! Scheme lexical analyzer.
//!
//! This module contains definition of the _lexical analyzer_ which breaks a stream of characters
//! into tokens.

use std::char;

use diagnostics::{Span, Handler, DiagnosticKind};
use intern_pool::{InternPool};
use tokens::{Token, ParenType};

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Helper data structures
//

/// A scanned token with extents information.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ScannedToken {
    /// The token itself.
    pub tok: Token,

    /// Span of the token.
    pub span: Span,
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// The scanner
//

/// Scanner interface.
///
/// A scanner splits a string of characters into tokens. It does not process the characters it
/// sees; it only recognizes the tokens and provides their general category and extents in the
/// string. Being that stupid, the scanner is single-pass and does not provide any backtracking
/// for error recovery. If a token seems to be malformed the scanner will do its best to guess
/// the intended meaning, report any encountered errors, and will carry on scanning if possible.
/// However, a scanner will never reconsider its decisions about what it saw and where it was.
pub trait Scanner {
    /// Checks whether the end of token stream has been reached.
    /// If it is reached, the scanner will produce only `Token::Eof`.
    fn at_eof(&self) -> bool;

    /// Extracts and returns the next token from the stream. Returns `Token::Eof`
    /// if the end of stream has been reached and no more tokens are available.
    fn next_token(&mut self) -> ScannedToken;
}

/// A scanner for strings.
pub struct StringScanner<'a> {
    /// The string being scanned.
    buf: &'a str,

    /// The pool where scanned strings are stored.
    pool: &'a InternPool,

    // Scanning state
    //
    //     buf
    //     ----+---+---+---+-----+---+---+---+---+-----+---+---+---+
    //     ... | f | o | o | ... | b |   ä   | r | ... | e | n | d |
    //     ----+---+---+---+-----+---+---+---+---+-----+---+---+---+
    //          ^   ^                 ^       ^                     ^
    //          |   pos               |       pos                   pos, prev_pos
    //          |                     |
    //          prev_pos              prev_pos
    //
    //     cur == Some('f')           cur == Some('ä')              cur == None

    /// Most recently read character (located at `prev_pos` in the stream).
    cur: Option<char>,

    /// Byte offset of the next character to be read (after `cur`).
    pos: usize,

    /// Byte offset of the last character that was read (`cur`).
    prev_pos: usize,

    //
    // Diagnostic reporting
    //

    /// Designated responsible for diagnostic processing.
    diagnostic: &'a Handler,
}

impl<'a> Scanner for StringScanner<'a> {
    fn at_eof(&self) -> bool {
        self.cur.is_none()
    }

    fn next_token(&mut self) -> ScannedToken {
        self.next()
    }
}

impl<'a> StringScanner<'a> {
    /// Make a new scanner for the given string which will report scanning errors to the
    /// given handle and will store scanned strings into the given intern pool.
    pub fn new(s: &'a str, handler: &'a Handler, pool: &'a InternPool) -> StringScanner<'a> {
        let mut scanner = StringScanner {
            buf: s,
            pool: pool,
            cur: None, pos: 0, prev_pos: 0,
            diagnostic: handler,
        };
        scanner.read();
        return scanner;
    }

    /// Read in the next character and update `cur`, `pos`, `prev_pos`.
    fn read(&mut self) {
        self.prev_pos = self.pos;
        self.cur = self.peek();
        match self.cur {
            Some(c) => { self.pos += c.len_utf8(); }
            None    => { }
        }
    }

    /// Peek the next character without updating anything.
    fn peek(&self) -> Option<char> {
        self.buf[self.pos..].chars().nth(0)
    }

    /// Check whether current character (`cur`) is `c`.
    fn cur_is(&self, c: char) -> bool {
        self.cur == Some(c)
    }

    /// Check whether next character (`peek()`) is `c`.
    fn peek_is(&self, c: char) -> bool {
        self.peek() == Some(c)
    }

    /// Extract the next token from the stream.
    fn next(&mut self) -> ScannedToken {
        if self.at_eof() {
            return ScannedToken {
                tok: Token::Eof,
                span: Span::new(self.pos, self.pos)
            };
        }

        let start = self.prev_pos;
        let tok = self.scan_token();
        let end = self.prev_pos;

        assert!(tok != Token::Eof);

        return ScannedToken {
            tok: tok,
            span: Span::new(start, end)
        };
    }

    /// Scan over the next token.
    fn scan_token(&mut self) -> Token {
        assert!(!self.at_eof());

        match self.cur.unwrap() {
            ' ' | '\t' | '\n' | '\r' => {
                self.scan_whitespace()
            }
            '(' => { self.read(); Token::Open(ParenType::Parenthesis) }
            '[' => { self.read(); Token::Open(ParenType::Bracket) }
            '{' => { self.read(); Token::Open(ParenType::Brace) }
            '}' => { self.read(); Token::Close(ParenType::Brace) }
            ']' => { self.read(); Token::Close(ParenType::Bracket) }
            ')' => { self.read(); Token::Close(ParenType::Parenthesis) }
            '`' => { self.read(); Token::Backquote }
           '\'' => { self.read(); Token::Quote }
            '.' => { self.read(); Token::Dot }
            ';' => {
                self.scan_line_comment()
            }
            ',' => {
                self.read();
                if self.cur_is('@') {
                    self.read();
                    Token::CommaSplicing
                } else {
                    Token::Comma
                }
            }
            '#' => {
                self.scan_hash_token()
            }
            '"' => {
                self.scan_string_literal()
            }
            _ => {
                let start = self.prev_pos;
                self.scan_unrecognized(start)
            }
        }
    }

    /// Scan over a sequence of whitespace.
    fn scan_whitespace(&mut self) -> Token {
        while !self.at_eof() {
            match self.cur.unwrap() {
                ' ' | '\t' | '\n' | '\r' => { self.read(); }
                _                        => { break; }
            }
        }
        return Token::Whitespace;
    }

    /// Scan over an unrecognized sequence of characters.
    fn scan_unrecognized(&mut self, start: usize) -> Token {
        while !self.at_eof() {
            if is_delimiter(self.cur.unwrap()) {
                break;
            }
            self.read();
        }
        let end = self.prev_pos;

        self.diagnostic.report(DiagnosticKind::err_lexer_unrecognized, Span::new(start, end));

        return Token::Unrecognized;
    }

    /// Scan a line comment starting with `;`.
    fn scan_line_comment(&mut self) -> Token {
        assert!(self.cur_is(';'));

        loop {
            match self.cur {
                Some('\n') => {
                    self.read();
                    break;
                }
                Some('\r') => {
                    self.read();
                    if self.cur_is('\n') {
                        self.read();
                    }
                    break;
                }
                Some(_) => {
                    self.read();
                }
                None => {
                    break;
                }
            }
        }

        return Token::Comment;
    }

    /// Scan a possibly nested block comment `#| ... |#`.
    fn scan_block_comment(&mut self, start: usize) -> Token {
        assert!(self.cur_is('|'));
        self.read();

        let mut nesting_level = 1;

        while nesting_level > 0 {
            match self.cur {
                Some('|') => {
                    self.read();
                    if self.cur_is('#') {
                        self.read();
                        nesting_level -= 1;
                    }
                }
                Some('#') => {
                    self.read();
                    if self.cur_is('|') {
                        self.read();
                        nesting_level += 1;
                    }
                }
                Some(_) => {
                    self.read();
                }
                None => {
                    self.diagnostic.report(DiagnosticKind::fatal_lexer_unterminated_comment,
                        Span::new(start, self.prev_pos));

                    return Token::Unrecognized;
                }
            }
        }

        return Token::Comment;
    }

    /// Scan over a token starting with a hash `#`.
    fn scan_hash_token(&mut self) -> Token {
        let start = self.prev_pos;
        assert!(self.cur_is('#'));
        self.read();

        match self.cur {
            Some('(') => { self.read(); Token::OpenVector(ParenType::Parenthesis) }
            Some('[') => { self.read(); Token::OpenVector(ParenType::Bracket) }
            Some('{') => { self.read(); Token::OpenVector(ParenType::Brace) }
            Some(';') => { self.read(); Token::CommentPrefix }
            Some('|') => {
                self.scan_block_comment(start)
            }
            Some('\\') => {
                self.scan_character_literal(start)
            }
            Some('u') | Some('U') => {
                self.scan_bytevector_open(start)
            }
            _ => { self.scan_unrecognized(start) }
        }
    }

    /// Scan over a bytevector opener `#u8(`.
    fn scan_bytevector_open(&mut self, start: usize) -> Token {
        assert!(self.cur_is('u') || self.cur_is('U'));
        self.read();

        // Try the happy path first.
        if self.cur_is('8') {
            match self.peek() {
                Some('(') => {
                    self.read();
                    self.read();
                    return Token::OpenBytevector(ParenType::Parenthesis);
                }
                Some('[') => {
                    self.read();
                    self.read();
                    return Token::OpenBytevector(ParenType::Bracket);
                }
                Some('{') => {
                    self.read();
                    self.read();
                    return Token::OpenBytevector(ParenType::Brace);
                }
                // Otherwise skip to recovery slow path.
                _ => { }
            }
        }

        // We've seen `#u` or maybe `#u8` at this point. All we need now is a parenthesis.
        // This would allow us to conclude that this is a bytevector opener with a typo.
        // Allow from zero to two decimal digits between `u` and the parenthesis, on the
        // assumption that the user has typed slightly less or more than needed. Anything
        // else is a clear violation of syntax so we bail out to the nearest delimiter.
        for _ in 0..3 {
            match self.cur {
                Some('0') | Some('1') | Some('2') | Some('3') | Some('4') |
                Some('5') | Some('6') | Some('7') | Some('8') | Some('9') => {
                    self.read();
                }
                Some('(') => {
                    self.read();
                    self.diagnostic.report(DiagnosticKind::err_lexer_invalid_bytevector,
                        Span::new(start, self.prev_pos));
                    return Token::OpenBytevector(ParenType::Parenthesis);
                }
                Some('[') => {
                    self.read();
                    self.diagnostic.report(DiagnosticKind::err_lexer_invalid_bytevector,
                        Span::new(start, self.prev_pos));
                    return Token::OpenBytevector(ParenType::Bracket);
                }
                Some('{') => {
                    self.read();
                    self.diagnostic.report(DiagnosticKind::err_lexer_invalid_bytevector,
                        Span::new(start, self.prev_pos));
                    return Token::OpenBytevector(ParenType::Brace);
                }
                _ => { break; }
            }
        }

        return self.scan_unrecognized(start);
    }

    /// Scan a character literal (`#\\!`, `#\\x000F`, `#\\return`).
    fn scan_character_literal(&mut self, start: usize) -> Token {
        assert!(self.cur_is('\\'));
        self.read();

        // To be honest, syntax of character literals in Scheme is awfully ambiguous for machines
        // and humans alike. However, this is life and legacy design so we have to deal with it.

        match self.cur {
            // Whitespace *is* for delimiters. I don't care if this is not R7RS-compliant, but
            // I will not allow "#\ " as fancy way to write a space character. There are named
            // characters like #\space or #\newline for these cases. So if we run in any of these
            // abominations (or if we run out of characters at all) then report it and get out.
            Some(' ') | Some('\t') | Some('\r') | Some('\n') | None => {
                self.diagnostic.report(DiagnosticKind::err_lexer_character_missing,
                    Span::new(self.prev_pos, self.prev_pos));

                return Token::Character(REPLACEMENT_CHARACTER);
            }

            // Handle the edge case when the literal is for a delimiter character,
            // and continue with more complex cases.
            Some(c) => {
                if is_delimiter(c) {
                    self.read();
                    return Token::Character(c);
                }
            }
        }

        let name_start = self.prev_pos;
        let first_character = self.cur.unwrap();

        // Okay. We see `#\\x`. This may be...
        if first_character == 'x' || first_character == 'X' {
            self.read();

            // ...a literal `x` or `X` if the following character is a delimiter.
            if self.at_eof() || is_delimiter(self.cur.unwrap()) {
                return Token::Character(first_character);
            }

            // Otherwise it is either a hexcoded character literal, or some named literal starting
            // with `x` (there are no valid ones). We have no idea which one it is until we try.
            // Scan this as a hexcoded literal until we are done or encounter a non-hex digit.

            let mut value: u32 = 0;

            loop {
                // If we run into a delimiter or out of characters then we are done. Check the
                // resulting code point value for correctness and return it.
                if self.at_eof() || is_delimiter(self.cur.unwrap()) {
                    if let Some(c) = char::from_u32(value) {
                        return Token::Character(c);
                    } else {
                        self.diagnostic.report(DiagnosticKind::err_lexer_invalid_unicode_range,
                            Span::new(start, self.prev_pos));

                        return Token::Character(REPLACEMENT_CHARACTER);
                    }
                }

                // If we see something that should not be in a hexcoded literal then fall back
                // to scanning an invalid named literal. Leave the already scanned part as is.
                let c = self.cur.unwrap();
                if !is_digit(16, c) {
                    break;
                }

                // Otherwise it is a hex digit that should be added it to the accumulated value.
                // The user may write literals of any length so handle cases like `#\x000000000001`
                // and `#\xFFFFDEADCAFEBABE` without overflows.
                if value <= 0x00FFFFFF {
                    value = (value << 4) | hex_value(c) as u32;
                }

                self.read();
            }
        }

        // Ambiguity resolution rule in R7RS relies on the notion of alphabetic characters and
        // identifier syntax to distingush between a named literal and a single-character literal
        // immediately followed by something else. However, this approach requires full Unicode
        // support for proper implementation, so we use a more simple, practical solution here:
        // scan everything up to a delimiter and if there is more than one character there then
        // we have a named literal. Identifier syntax in Scheme is *very* liberal, so the cases
        // where the difference should matter are lexically invalid anyway.

        while !self.at_eof() {
            if is_delimiter(self.cur.unwrap()) {
                break;
            }
            self.read();
        }

        let name_end = self.prev_pos;

        // This is safe as read() keeps the indices inside the string and at code unit boundaries.
        let name = unsafe { self.buf.slice_unchecked(name_start, name_end) };

        // A fancy O(1) way to check if the string has only one known character in it.
        if name.len() == first_character.len_utf8() {
            return Token::Character(first_character);
        }

        // Finally, handle named character literals. Note that they are always case-sensitive.
        match name {
            "alarm"     => { return Token::Character('\u{0007}'); }
            "escape"    => { return Token::Character('\u{0008}'); }
            "delete"    => { return Token::Character('\u{007F}'); }
            "newline"   => { return Token::Character('\u{000A}'); }
            "null"      => { return Token::Character('\u{0000}'); }
            "return"    => { return Token::Character('\u{000D}'); }
            "space"     => { return Token::Character('\u{0020}'); }
            "tab"       => { return Token::Character('\u{0009}'); }
            _ => {
                self.diagnostic.report(DiagnosticKind::err_lexer_unknown_character_name,
                    Span::new(start, name_end));

                return Token::Character(REPLACEMENT_CHARACTER);
            }
        }
    }

    /// Scan a string literal. This method also expands any escape sequences and normalizes line
    /// endings in the scanned string.
    fn scan_string_literal(&mut self) -> Token {
        let start = self.prev_pos;
        assert!(self.cur_is('"'));
        self.read();

        let mut value = String::new();

        loop {
            match self.cur {
                // If we see a terminating quote then the string literal is over.
                Some('"') => {
                    self.read();
                    break;
                }

                // Backslashes start escape sequences which may or may not have a value.
                Some('\\') => {
                    if let Some(c) = self.scan_string_escape_sequence() {
                        value.push(c);
                    }
                }

                // Handle non-LF line endings (CR-LF and just CR), inserting LF into the string.
                Some('\r') => {
                    self.read();
                    if self.cur_is('\n') {
                        self.read();
                    }
                    value.push('\n');
                }

                // Scan over and add all other characters to the string value.
                Some(c) => {
                    self.read();
                    value.push(c);
                }

                // If we suddenly run out of characters in the stream then we're toasted.
                None => {
                    self.diagnostic.report(DiagnosticKind::fatal_lexer_unterminated_string,
                        Span::new(start, self.pos));

                    return Token::Unrecognized;
                }
            }
        }

        return Token::String(self.pool.intern_string(value));
    }

    /// Scan a single escape sequence inside a string. Returns None if the sequence produces no
    /// character (e.g., it is a line escape seqeunce) or an unexpected EOF occurs. Otherwise
    /// returns Some value of the escape sequence (which may be REPLACEMENT_CHARACTER if the
    /// escape sequence is invalid and the lexer recovers from an error).
    fn scan_string_escape_sequence(&mut self) -> Option<char> {
        let escape_start = self.prev_pos;
        assert!(self.cur_is('\\'));
        self.read();

        match self.cur {
            // Handle traditional escape sequences.
            Some('a')  => { self.read(); return Some('\u{0007}'); }
            Some('b')  => { self.read(); return Some('\u{0008}'); }
            Some('t')  => { self.read(); return Some('\u{0009}'); }
            Some('n')  => { self.read(); return Some('\u{000A}'); }
            Some('r')  => { self.read(); return Some('\u{000D}'); }
            Some('"')  => { self.read(); return Some('\u{0022}'); }
            Some('\\') => { self.read(); return Some('\u{005C}'); }
            Some('|')  => { self.read(); return Some('\u{007C}'); }

            // A backslash followed by whitespace starts a line escape which is ignored.
            Some(' ') | Some('\t') | Some('\r') | Some('\n') => {
                self.scan_string_line_escape_sequence(escape_start);
                return None;
            }

            // A backslash followed by `x` starts a hexcoded Unicode character escape
            // (or is an invalid escape sequence, we handle both below).
            Some('x') | Some('X') => {
                let c = self.scan_string_unicode_escape_sequence(escape_start);
                return Some(c);
            }

            // Any other character is not expected after a backslash, it is an error. Report
            // the error and return the character, assuming that the backslash itself is a typo.
            Some(c) => {
                self.diagnostic.report(DiagnosticKind::err_lexer_invalid_escape_sequence,
                    Span::new(escape_start, self.pos));
                self.read();
                return Some(c);
            }

            // If we encounter an EOF then just return None. The caller will report this condition.
            None => {
                return None;
            }
        }
    }

    /// Scan a line escape sequence in string.
    fn scan_string_line_escape_sequence(&mut self, escape_start: usize) {
        // First we scan the optional sequence of intraline whitespace before the line ending.
        loop {
            match self.cur {
                // Break out of the loop if we see our line ending.
                Some('\n') => {
                    self.read();
                    break;
                }
                Some('\r') => {
                    self.read();
                    if self.cur_is('\n') {
                        self.read();
                    }
                    break;
                }

                // Scan over intraline whitespace (non-line-endings).
                Some(' ') | Some('\t') => {
                    self.read();
                }

                // We do not expect any other character here or the EOF condition. If this happens
                // then report bad syntax and get out.
                Some(_) | None => {
                    self.diagnostic.report(DiagnosticKind::err_lexer_invalid_line_escape,
                        Span::new(escape_start, self.prev_pos));

                    return;
                }
            }
        }

        // Then we scan the second optional sequence of intraline whitespace after the line ending.
        loop {
            match self.cur {
                // Scan over intraline whitespace.
                Some(' ') | Some('\t') => {
                    self.read();
                }

                // Any other character (including line endings) means that we're done with the
                // line escape. This is also true for an (unexpected) EOF, the caller will
                // report it properly to the user later.
                Some(_) | None => {
                    break;
                }
            }
        }
    }

    /// Scan a Unicode escape sequence in string and return its value.
    fn scan_string_unicode_escape_sequence(&mut self, escape_start: usize) -> char {
        assert!(self.cur_is('x') || self.cur_is('X'));
        let first_char = self.cur.unwrap();
        self.read();

        // Handle the happy path first. A proper Unicode escape matches /\\[xX][0-9A-Fa-f]+;/.
        if self.cur.is_some() && is_digit(16, self.cur.unwrap()) {
            let mut value: u32 = 0;

            loop {
                match self.cur {
                    // Semicolon terminates Unicode escape sequence in strings.
                    Some(';') => {
                        self.read();
                        break;
                    }

                    // Hexadecimal digits constitute the code point value. The user may write
                    // literals like "\x00000000000000000001;" or "\xFFFFFFFFFFFFFFFF", so we
                    // should be careful about overflows.
                    Some(c) if is_digit(16, c) => {
                        if value <= 0x00FFFFFF {
                            value = (value << 4) | hex_value(c) as u32;
                        }
                        self.read();
                    }

                    // Any other character is treated as an unexpected terminator of the escape
                    // sequence, assuming that the user has forgotten to type the semicolon. This
                    // also includes the unexpected EOF which will be reported by the caller.
                    Some(_) | None => {
                        self.diagnostic.report(DiagnosticKind::err_lexer_unicode_escape_missing_semicolon,
                            Span::new(self.prev_pos, self.prev_pos));
                        break;
                    }
                }
            }

            // Check the resulting code point for correctness and return the value.
            if let Some(c) = char::from_u32(value) {
                return c;
            } else {
                self.diagnostic.report(DiagnosticKind::err_lexer_invalid_unicode_range,
                    Span::new(escape_start, self.prev_pos));

                return REPLACEMENT_CHARACTER;
            }
        }

        // Now we have the "\x" sequence which is not followed by a hexadecimal digit. Treat "\x;"
        // as a special case, assuming it to be a Unicode escape sequence with no digits (the user
        // may have forgotten to type the only digit there). If anything else follows then "\x"
        // is an invalid escape sequence.
        if self.cur_is(';') {
            self.diagnostic.report(DiagnosticKind::err_lexer_unicode_escape_missing_digits,
                Span::new(self.prev_pos, self.prev_pos));
            self.read();

            return REPLACEMENT_CHARACTER;
        } else {
            self.diagnostic.report(DiagnosticKind::err_lexer_invalid_escape_sequence,
                Span::new(escape_start, self.prev_pos));

            return first_char;
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Utility definitions
//

/// Check if a character is a delimiter for tokens.
fn is_delimiter(c: char) -> bool {
    match c {
        /// R7RS defines only the following delimiters:
        ' ' | '\t' | '\n' | '\r' | '|' | '"' | ';' | '(' | ')' | '[' | ']' | '{' | '}' |
        // But we add to this list all quotes as they are expanded into forms,
        // so effectively they can be treatead as opening parentheses, and
        // the hash sign as it is a starter for directives and fixed-spelling
        // tokens, and it also cannot be used inside anything else.
        '\'' | ',' | '`' | '#' => true,
        _ => false,
    }
}

/// A replacement character used when we need to return a character, but don't have one.
const REPLACEMENT_CHARACTER: char = '\u{FFFD}';

/// Check whether `c` is a valid digit of base `base`.
fn is_digit(base: u8, c: char) -> bool {
    match base {
         2 => {  ('0' <= c) && (c <= '1') }
         8 => {  ('0' <= c) && (c <= '7') }
        10 => {  ('0' <= c) && (c <= '9') }
        16 => { (('0' <= c) && (c <= '9')) ||
                (('a' <= c) && (c <= 'f')) ||
                (('A' <= c) && (c <= 'F')) }
         _ => { panic!("invalid numeric base {}", base); }
    }
}

/// Convert an ASCII hex digit character to its numeric value.
fn hex_value(c: char) -> u8 {
    assert!(is_digit(16, c));

    const H: &'static [u8; 128] = &[
        0, 0, 0, 0, 0, 0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0, 0, 0, 0, 0, 0, 0,0,0,0,0,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9,0,0,0,0,0,0,
        0,10,11,12,13,14,15,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,10,11,12,13,14,15,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    ];

    return H[c as usize];
}
