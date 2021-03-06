// Copyright (c) 2016, Sabre developers
//
// Licensed under the Apache License, Version 2.0 (see LICENSE.Apache in the
// root directory) or MIT license (see LICENSE.MIT in the root directory),
// at your option. This file may be copied, distributed, and modified only
// in accordance with the terms specified by the chosen license.

//! Scheme lexical analyzer.
//!
//! This module contains definition of the _lexical analyzer_ which breaks a stream of characters
//! into tokens.

use liblocus::diagnostics::{DiagnosticKind, Handler, Span};

use crate::intern_pool::InternPool;
use crate::tokens::{ParenType, Token};

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

    /// Should we fold case of scanned identifiers and character names?
    folding_case: bool,

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
            pool,
            cur: None, pos: 0, prev_pos: 0,
            folding_case: false,
            diagnostic: handler,
        };
        scanner.read();
        scanner
    }

    /// Read in the next character and update `cur`, `pos`, `prev_pos`.
    fn read(&mut self) {
        self.prev_pos = self.pos;
        self.cur = self.peek();
        if let Some(c) = self.cur {
            self.pos += c.len_utf8();
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

        ScannedToken {
            tok,
            span: Span::new(start, end)
        }
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
            '|' => {
                self.scan_escaped_identifier()
            }
            '0'..='9' => {
                self.scan_number_literal()
            }
            '-' | '+' => {
                self.scan_number_literal()
            }
            '.' => {
                if self.peek().map_or(true, is_delimiter) {
                    self.read();
                    Token::Dot
                } else {
                    self.scan_number_literal()
                }
            }

            // Syntax of Scheme identifiers is *sooo* permissive that it makes
            // pretty much sense to have them as a catch-all clause.
            _ => {
                self.scan_identifier()
            }
        }
    }

    /// Scan over a sequence of whitespace.
    fn scan_whitespace(&mut self) -> Token {
        while let Some(c) = self.cur {
            if !is_whitespace(c) {
                break;
            }
            self.read();
        }
        Token::Whitespace
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

        Token::Comment
    }

    /// Scan a possibly nested block comment `#| ... |#`.
    fn scan_block_comment(&mut self) -> Token {
        let start = self.prev_pos;
        assert!(self.cur_is('#'));
        self.read();
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

        Token::Comment
    }

    /// Scan over a token starting with a hash `#`.
    fn scan_hash_token(&mut self) -> Token {
        assert!(self.cur_is('#'));

        match self.peek() {
            Some('(') => { self.read(); self.read(); Token::OpenVector(ParenType::Parenthesis) }
            Some('[') => { self.read(); self.read(); Token::OpenVector(ParenType::Bracket) }
            Some('{') => { self.read(); self.read(); Token::OpenVector(ParenType::Brace) }
            Some(';') => { self.read(); self.read(); Token::CommentPrefix }
            Some('|') => {
                self.scan_block_comment()
            }
            Some('\\') => {
                self.scan_character_literal()
            }
            Some('!') => {
                self.scan_directive()
            }
            Some('u') | Some('U') => {
                self.scan_bytevector_open()
            }
            Some('t') | Some('T') | Some('f') | Some('F') => {
                self.scan_boolean_literal()
            }
            Some('b') | Some('B') | Some('o') | Some('O') | Some('d') | Some('D') |
            Some('x') | Some('X') | Some('i') | Some('I') | Some('e') | Some('E') => {
                self.scan_number_literal()
            }
            Some(c) if is_digit(10, c) => {
                self.scan_datum_label()
            }
            _ => {
                // Okay, we've exhausted lexically valid continuations, now we start guessing.
                //
                // Except for the tokens handled above, only numbers allow various hash prefixes.
                // So we try interpreting the string as a number literal with some invalid prefix
                // if possible. Otherwise we are likely to succeed by treating it as a peculiar
                // identifier with an (invalid) number prefix. Number scanning code handles this.

                self.scan_number_literal()
            }
        }
    }

    /// Scan over a boolean literal `#f`, `#true`.
    fn scan_boolean_literal(&mut self) -> Token {
        assert!(self.cur_is('#'));

        if self.try_scan_exactly("#t") {
            return Token::Boolean(true);
        }

        if self.try_scan_exactly("#f") {
            return Token::Boolean(false);
        }

        if self.try_scan_exactly("#true") {
            return Token::Boolean(true);
        }

        if self.try_scan_exactly("#false") {
            return Token::Boolean(false);
        }

        // Recover as `scan_hash_token()` would do.
        self.scan_number_literal()
    }

    /// Scan over a bytevector opener `#u8(`.
    fn scan_bytevector_open(&mut self) -> Token {
        assert!(self.cur_is('#'));

        if self.try_scan("#u8(") {
            return Token::OpenBytevector(ParenType::Parenthesis);
        }

        if self.try_scan("#u8[") {
            return Token::OpenBytevector(ParenType::Bracket);
        }

        if self.try_scan("#u8{") {
            return Token::OpenBytevector(ParenType::Brace);
        }

        // Recover as `scan_hash_token()` would do.
        self.scan_number_literal()
    }

    /// Scan a character literal (`#\\!`, `#\\x000F`, `#\\return`).
    fn scan_character_literal(&mut self) -> Token {
        let start = self.prev_pos;
        assert!(self.cur_is('#'));
        self.read();
        assert!(self.cur_is('\\'));
        self.read();

        // To be honest, syntax of character literals in Scheme is awfully ambiguous for machines
        // and humans alike. However, this is life and legacy design so we have to deal with it.

        // Whitespace *is* for delimiters. I don't care if this is not R7RS-compliant, but I will
        // not allow "#\ " as fancy way to write a space character. There are named characters
        // like #\space or #\newline for these cases. So if we run in any of these abominations
        // (or if we run out of characters at all) then report it and get out.
        if self.cur.map_or(true, is_whitespace) {
            self.diagnostic.report(DiagnosticKind::err_lexer_character_missing,
                Span::new(self.prev_pos, self.prev_pos));

            return Token::Character(REPLACEMENT_CHARACTER);
        }

        let name_start = self.prev_pos;
        let first_character = self.cur.unwrap();

        // Okay. We see `#\\x`. This may be...
        if first_character == 'x' || first_character == 'X' {
            self.read();

            // ...a literal `x` or `X` if the following character is a delimiter.
            if self.cur.map_or(true, is_delimiter) {
                return Token::Character(first_character);
            }

            // Otherwise it is either a hexcoded character literal, or some named literal starting
            // with `x` (there are no valid ones). We have no idea which one it is until we try.
            // Scan this as a hexcoded literal until we are done or encounter a non-hex digit.

            let mut value = 0;

            loop {
                // If we run into a delimiter or out of characters then we are done. Check the
                // resulting code point value for correctness and return it.
                if self.cur.map_or(true, is_delimiter) {
                    if let Some(c) = std::char::from_u32(value) {
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
                value = saturating_add_unicode_nibble(value, c);

                self.read();
            }
        } else {
            // Handle the edge case when the literal is for a delimiter character.
            if is_delimiter(first_character) {
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

        loop {
            match self.cur {
                Some(c) if is_delimiter(c) => { break; }
                None                       => { break; }
                Some(_) => { self.read(); }
            }
        }

        let name_end = self.prev_pos;

        let name = &self.buf[name_start..name_end];

        // A fancy O(1) way to check if the string has only one known character in it.
        if name.len() == first_character.len_utf8() {
            return Token::Character(first_character);
        }

        // Normalize character names before matching (this is important mostly for the case when
        // #!fold-case is in effect).
        let name = self.normalize_identifer(name);

        // Finally, handle named character literals.
        match &name[..] {
            "alarm"     => { Token::Character('\u{0007}') }
            "escape"    => { Token::Character('\u{0008}') }
            "delete"    => { Token::Character('\u{007F}') }
            "newline"   => { Token::Character('\u{000A}') }
            "null"      => { Token::Character('\u{0000}') }
            "return"    => { Token::Character('\u{000D}') }
            "space"     => { Token::Character('\u{0020}') }
            "tab"       => { Token::Character('\u{0009}') }
            _ => {
                self.diagnostic.report(DiagnosticKind::err_lexer_unknown_character_name,
                    Span::new(start, name_end));

                Token::Character(REPLACEMENT_CHARACTER)
            }
        }
    }

    /// Scan a datum label `#123=` or `#456#` (both forms).
    fn scan_datum_label(&mut self) -> Token {
        assert!(self.cur_is('#'));
        self.read();

        let start = self.prev_pos;
        loop {
            match self.cur {
                // Scan over valid digits.
                Some(c) if is_digit(10, c) => { self.read(); }

                // Stop at label terminators.
                Some('#') | Some('=')      => { break; }

                // Also stop if we see an explicit delimiter.
                Some(c) if is_delimiter(c) => { break; }
                None                       => { break; }

                // Scan over and report everything else.
                Some(_) => {
                    self.diagnostic.report(DiagnosticKind::err_lexer_invalid_number_character,
                        Span::new(self.prev_pos, self.pos));
                    self.read();
                }
            }
        }
        let end = self.prev_pos;

        // This handles both valid marks and the recovery path. An extra datum label
        // mark is less error-prone than a reference in case we did a wrong guess here.
        let is_reference = self.cur_is('#');

        // Scan over the terminator or report its absence.
        if self.cur_is('#') || self.cur_is('=') {
            self.read();
        } else {
            assert!(self.cur.map_or(true, is_delimiter));
            self.diagnostic.report(DiagnosticKind::err_lexer_missing_datum_label_terminator,
                Span::new(self.prev_pos, self.prev_pos));
        }

        let value = &self.buf[start..end];
        let atom = self.pool.intern(value);

        if is_reference { Token::LabelRef(atom) } else { Token::LabelMark(atom) }
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

        Token::String(self.pool.intern_string(value))
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
            Some('a')  => { self.read(); Some('\u{0007}') }
            Some('b')  => { self.read(); Some('\u{0008}') }
            Some('t')  => { self.read(); Some('\u{0009}') }
            Some('n')  => { self.read(); Some('\u{000A}') }
            Some('r')  => { self.read(); Some('\u{000D}') }
            Some('"')  => { self.read(); Some('\u{0022}') }
            Some('\\') => { self.read(); Some('\u{005C}') }
            Some('|')  => { self.read(); Some('\u{007C}') }

            // A backslash followed by whitespace starts a line escape which is ignored.
            Some(c) if is_whitespace(c) => {
                self.scan_string_line_escape_sequence(escape_start);
                None
            }

            // A backslash followed by `x` starts a hexcoded Unicode character escape
            // (or is an invalid escape sequence, we handle both below).
            Some('x') | Some('X') => {
                let c = self.scan_string_unicode_escape_sequence(escape_start);
                Some(c)
            }

            // Any other character is not expected after a backslash, it is an error. Report
            // the error and return the character, assuming that the backslash itself is a typo.
            Some(c) => {
                self.diagnostic.report(DiagnosticKind::err_lexer_invalid_escape_sequence,
                    Span::new(escape_start, self.pos));
                self.read();
                Some(c)
            }

            // If we encounter an EOF then just return None. The caller will report this condition.
            None => { None }
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
        if self.cur.map_or(false, |c| is_digit(16, c)) {
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
                        value = saturating_add_unicode_nibble(value, c);
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
            if let Some(c) = std::char::from_u32(value) {
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

            REPLACEMENT_CHARACTER
        } else {
            self.diagnostic.report(DiagnosticKind::err_lexer_invalid_escape_sequence,
                Span::new(escape_start, self.prev_pos));

            first_char
        }
    }

    /// Scan over a directive.
    fn scan_directive(&mut self) -> Token {
        assert!(self.cur_is('#') && self.peek_is('!'));

        let directive_start = self.prev_pos;
        self.read();
        self.read();

        let name_start = self.prev_pos;
        while !self.cur.map_or(true, is_delimiter) {
            self.read();
        }
        let name_end = self.prev_pos;

        // Directives are matched as case-insensitive identifiers.
        let value = normalize_case_insensitive_identifier(&self.buf[name_start..name_end]);

        match &value[..] {
            // Handle case-folding directives.
            "fold-case"     => { self.folding_case = true; }
            "no-fold-case"  => { self.folding_case = false; }

            // Report anything else as unknown.
            _ => {
                self.diagnostic.report(DiagnosticKind::err_lexer_unknown_directive,
                    Span::new(directive_start, name_end));
            }
        }

        Token::Directive(self.pool.intern_string(value))
    }

    /// Scan over an identifier (non-escaped).
    fn scan_identifier(&mut self) -> Token {
        let start = self.prev_pos;
        if !self.cur.map_or(true, is_delimiter) {
            // The first characters of identifiers are a bit more restrictive.
            if !is_identifier_initial(self.cur.unwrap()) {
                self.diagnostic.report(DiagnosticKind::err_lexer_invalid_identifier_character,
                    Span::new(self.prev_pos, self.pos));
            }

            self.read();

            loop {
                match self.cur {
                    // Scan over valid identifier constituents.
                    Some(c) if is_identifier_subsequent(c) => { self.read(); }

                    // Stop at the first delimiter.
                    Some(c) if is_delimiter(c) => { break; }
                    None                       => { break; }

                    // Report and scan over anything else.
                    Some(_) => {
                        self.diagnostic.report(DiagnosticKind::err_lexer_invalid_identifier_character,
                            Span::new(self.prev_pos, self.pos));
                        self.read();
                    }
                }
            }
        }
        let end = self.prev_pos;

        let value = &self.buf[start..end];

        // Normalize identifier spelling and handle `#!fold-case` mode.
        let value = self.normalize_identifer(value);

        // Warn the user if the identifier can be parsed as a number after normalization, maybe
        // they intended to write a number, but, for example, their editor used fullwidth digits.
        if looks_like_number_prefix(&value, 10) {
            self.diagnostic.report(DiagnosticKind::warn_lexer_identifier_looks_like_number,
                Span::new(start, end));
        }

        Token::Identifier(self.pool.intern_string(value))
    }

    /// Scan an escaped identifier. This method also expands any escape sequences in the scanned
    /// identifier.
    fn scan_escaped_identifier(&mut self) -> Token {
        let start = self.prev_pos;
        assert!(self.cur_is('|'));
        self.read();

        let mut value = String::new();

        loop {
            match self.cur {
                // If we see a terminating vertical bar then the identifier is over.
                Some('|') => {
                    self.read();
                    break;
                }

                // Backslashes start escape sequences which may or may not have a value.
                Some('\\') => {
                    if let Some(c) = self.scan_identifier_escape_sequence() {
                        value.push(c);
                    }
                }

                // All other characters are scanned verbatim in identifiers. We do not perform any
                // line ending conversions here.
                Some(c) => {
                    self.read();
                    value.push(c);
                }

                // If we suddenly run out of characters in the stream then we're toasted.
                None => {
                    self.diagnostic.report(DiagnosticKind::fatal_lexer_unterminated_identifier,
                        Span::new(start, self.pos));

                    return Token::Unrecognized;
                }
            }
        }

        // Note that escaped identifiers are always verbatim. They are case-sensitive and they are
        // not normalized in any way.
        Token::Identifier(self.pool.intern_string(value))
    }

    /// Scan a single escape sequence inside an identifier. Returns None if an unexpected
    /// EOF occurs. Otherwise returns Some value of the escape sequence (which may be
    /// REPLACEMENT_CHARACTER if the escape sequence is invalid and the lexer recovers
    /// from an error).
    fn scan_identifier_escape_sequence(&mut self) -> Option<char> {
        let escape_start = self.prev_pos;
        assert!(self.cur_is('\\'));
        self.read();

        match self.cur {
            // Handle traditional escape sequences.
            Some('a')  => { self.read(); Some('\u{0007}') }
            Some('b')  => { self.read(); Some('\u{0008}') }
            Some('t')  => { self.read(); Some('\u{0009}') }
            Some('n')  => { self.read(); Some('\u{000A}') }
            Some('r')  => { self.read(); Some('\u{000D}') }
            Some('"')  => { self.read(); Some('\u{0022}') }
            Some('\\') => { self.read(); Some('\u{005C}') }
            Some('|')  => { self.read(); Some('\u{007C}') }

            // A backslash followed by `x` starts a hexcoded Unicode character escape
            // (or is an invalid escape sequence, we handle both below).
            Some('x') | Some('X') => {
                let c = self.scan_string_unicode_escape_sequence(escape_start);
                Some(c)
            }

            // Any other character is not expected after a backslash, it is an error. Report
            // the error and return the character, assuming that the backslash itself is a typo.
            Some(c) => {
                self.diagnostic.report(DiagnosticKind::err_lexer_invalid_escape_sequence,
                    Span::new(escape_start, self.pos));
                self.read();
                Some(c)
            }

            // If we encounter an EOF then just return None. The caller will report this condition.
            None => { None }
        }
    }

    /// Scan a number literal.
    fn scan_number_literal(&mut self) -> Token {
        let mut has_prefix = false;
        let mut radix_value = None;
        let mut exact_value = None;
        let mut radix_location = None;
        let mut exact_location = None;
        let mut effective_radix = 10;
        let mut not_integer = false;

        let start = self.prev_pos;

        // Start with checking for the prefix. It is quite important: if the prefix is present
        // then we are quite sure that the user has meant a number, not some fancy identifier.
        if self.cur_is('#') {
            has_prefix = true;

            self.scan_number_prefix(&mut radix_value, &mut radix_location,
                                    &mut exact_value, &mut exact_location);

            effective_radix = radix_value.unwrap_or(10);
        }

        // If the token does not have a prefix then it can still look like a starter of a number
        // while actually being a *pecualiar* identifier. Let's check for these abominations
        // right here and now, and forget about them for the rest of the _number-scanning_ code.
        // If the token has a prefix but surely is a pecualiar identifier then report the prefix.
        // Accept all decimal digits regardless of specified radix to allow for user mistakes.
        let check_radix = if effective_radix <= 10 { 10 } else { effective_radix };
        if self.peculiar_identifier_ahead(check_radix) {
            if has_prefix {
                self.diagnostic.report(DiagnosticKind::err_lexer_prefixed_identifier,
                    Span::new(start, self.prev_pos));
            }

            return self.scan_identifier();
        }

        // Now scan the actual number value.
        self.scan_number_part(effective_radix, ComplexScanningMode::Initial,
            &mut not_integer);

        // Scan possible (complex) parts of a number.
        loop {
            let part_start = self.prev_pos;
            match self.cur {
                // If we stopped at an @ sign then this is a complex number in polar form and
                // we've just scanned over the modulus. Skip the @ and scan the argument.
                Some('@') => {
                    self.read();

                    self.scan_number_part(effective_radix, ComplexScanningMode::Argument,
                        &mut not_integer);
                }

                // If we stopped at a sign then this is a complex number in rectangular form and
                // we've just scanned over of the real part of it. Leave the sign (for possible
                // infnans) and scan the imaginary part.
                Some('+') | Some('-') => {
                    self.scan_number_part(effective_radix, ComplexScanningMode::Subsequent,
                        &mut not_integer);
                }

                // Anything else should have been handled by `scan_number_part()`.
                _ =>  { assert!(self.cur.map_or(true, is_delimiter)); }
            }
            let part_end = self.prev_pos;

            // Break out of the loop as soon as we run into a delimiter.
            if self.cur.map_or(true, is_delimiter) {
                break;
            } else {
                // Drop a warning about every non-final part. Valid numbers contain at most two.
                self.diagnostic.report(DiagnosticKind::err_lexer_extra_complex_part,
                    Span::new(part_start, part_end));
            }
        }

        let end = self.prev_pos;

        // Late check for radix of a number. Scheme does not allow to use exponent and float forms
        // with non-decimal numbers.
        if not_integer && (effective_radix != 10) {
            self.diagnostic.report(DiagnosticKind::err_lexer_nondecimal_real,
                radix_location.expect("non-decimal radix is always explicit"));
        }

        let value = &self.buf[start..end];

        Token::Number(self.pool.intern(value))
    }

    /// Scan all number literal prefixes. Fills in values and locations of radix and exactness
    /// prefixes (if encountered).
    fn scan_number_prefix(&mut self,
        radix_value: &mut Option<u8>, radix_location: &mut Option<Span>,
        exact_value: &mut Option<bool>, exact_location: &mut Option<Span>)
    {
        assert!(self.cur_is('#'));

        // Check if we can scan the next prefix (i.e., we have some # ahead), and then scan it.
        // If it's not it then we are done with the prefix part of a number literal.
        while self.cur_is('#') {
            let start = self.prev_pos;
            self.read();

            match self.cur {
                // Handle radix specifiers. Do not allow multiple of them.
                Some('b') | Some('B') | Some('o') | Some('O') | Some('x') | Some('X') |
                Some('d') | Some('D') => {
                    let c = self.cur.unwrap();
                    self.read();
                    let end = self.prev_pos;

                    let location = Span::new(start, end);
                    let value = match c {
                        'b' | 'B' => 2,
                        'o' | 'O' => 8,
                        'x' | 'X' => 16,
                        'd' | 'D' => 10,
                        _ => unreachable!(),
                    };

                    if radix_value.is_none() {
                        *radix_value = Some(value);
                        *radix_location = Some(location);
                    } else {
                        self.diagnostic.report(DiagnosticKind::err_lexer_multiple_number_radices,
                            location);
                    }
                }

                // Handle exactness specifiers. Do not allow multiple of them.
                Some('i') | Some('I') | Some('e') | Some('E') => {
                    let c = self.cur.unwrap();
                    self.read();
                    let end = self.prev_pos;

                    let location = Span::new(start, end);
                    let value = match c {
                        'i' | 'I' => false,
                        'e' | 'E' => true,
                        _ => unreachable!(),
                    };

                    if exact_value.is_none() {
                        *exact_value = Some(value);
                        *exact_location = Some(location);
                    } else {
                        self.diagnostic.report(DiagnosticKind::err_lexer_multiple_exactness,
                            location);
                    }
                }

                // We do not expect anything else after a hash, so this is definitely an invalid
                // number prefix. Try to recover.
                _ => {
                    // If we encounter consecutive hashes then suppose that the user has mistyped
                    // a prefix and forgotten to type the character between the hashes. Report the
                    // lone hash and continue scanning treating this hash a start of a new prefix.
                    if self.cur_is('#') {
                        self.diagnostic.report(DiagnosticKind::err_lexer_invalid_number_prefix,
                            Span::new(start, self.prev_pos));
                        continue;
                    }

                    // A delimiter means that this does not even seem to be a number, but meh.
                    let delimiter = self.cur.map_or(true, is_delimiter);

                    // A digit, a sign, or a dot means that the user has missed the actual prefix
                    // character and started typing the number right away. Okay.
                    let digit = self.cur.map_or(false, |c| is_digit(10, c));
                    let sign = self.cur_is('+') || self.cur_is('-');
                    let dot = self.cur_is('.');

                    // Stop scanning the prefix if we encounter any of these immediately after
                    // the hash character.
                    if delimiter || digit || sign || dot {
                        self.diagnostic.report(DiagnosticKind::err_lexer_invalid_number_prefix,
                            Span::new(start, self.prev_pos));
                        break;
                    }

                    // Otherwise eat the character after the hash, report this pair as invalid,
                    // and continue scanning (maybe there are more prefixes ahead).
                    self.read();

                    self.diagnostic.report(DiagnosticKind::err_lexer_invalid_number_prefix,
                        Span::new(start, self.prev_pos));
                }
            }
        }
    }

    /// Check whether we were fooled and there is a peculiar identifier ahead instead of a number.
    fn peculiar_identifier_ahead(&self, radix: u8) -> bool {
        !looks_like_number_prefix(&self.buf[self.prev_pos..], radix)
    }

    /// Scan a real part of a number (an integer, a fractional, or a rational number).
    /// Sets `not_integer` to true if the number is fractional or has an exponent anywhere.
    fn scan_number_part(&mut self, radix: u8, complex_mode: ComplexScanningMode,
        not_integer: &mut bool)
    {
        let mut infnan_numerator = false;
        let mut infnan_denominator = false;
        let mut noninteger_numerator = false;
        let mut noninteger_denominator = false;
        let mut terminal_i = false;

        let numerator_start = self.prev_pos;
        self.scan_number_value(radix, RationalScanningMode::Numerator, complex_mode,
            &mut infnan_numerator, &mut noninteger_numerator, &mut terminal_i);

        let numerator_end = self.prev_pos;

        if self.cur_is('/') {
            self.read();

            let denominator_start = self.prev_pos;
            self.scan_number_value(radix, RationalScanningMode::Denominator, complex_mode,
                &mut infnan_denominator, &mut noninteger_denominator, &mut terminal_i);
            let denominator_end = self.prev_pos;

            // Fractions must be exact, they cannot contain floating-point parts.
            if infnan_numerator {
                self.diagnostic.report(DiagnosticKind::err_lexer_infnan_rational,
                    Span::new(numerator_start, numerator_end));
            }
            if infnan_denominator {
                self.diagnostic.report(DiagnosticKind::err_lexer_infnan_rational,
                    Span::new(denominator_start, denominator_end));
            }
            if noninteger_numerator {
                self.diagnostic.report(DiagnosticKind::err_lexer_noninteger_rational,
                    Span::new(numerator_start, numerator_end));
            }
            if noninteger_denominator {
                self.diagnostic.report(DiagnosticKind::err_lexer_noninteger_rational,
                    Span::new(denominator_start, denominator_end));
            }
        }

        if noninteger_numerator || noninteger_denominator {
            *not_integer = true;
        }

        // There should be only one subsequent part (followed by a delimiter), and it should be
        // imaginary (i.e., the digits should be followed by an 'i').
        if complex_mode == ComplexScanningMode::Subsequent &&
            self.cur.map_or(true, is_delimiter) &&
            !terminal_i
        {
            self.diagnostic.report(DiagnosticKind::err_lexer_missing_i,
                Span::new(self.prev_pos, self.prev_pos));
        }
    }

    /// Scan a single numeric value consisting of an actual value part and an optional exponent.
    /// Sets `is_infnan` to true if the scanned value was /[+-]inf.0/ or /[+-]nan.0/.
    /// Sets `not_integer` to true if the number is fractional and/or has an exponent.
    /// Sets `terminal_i` to true if the scanned (imaginary) value is terminated by an 'i'.
    fn scan_number_value(&mut self, radix: u8,
        rational_mode: RationalScanningMode, complex_mode: ComplexScanningMode,
        is_infnan: &mut bool, not_integer: &mut bool, terminal_i: &mut bool)
    {
        // Skip over an optional sign.
        if self.cur_is('+') || self.cur_is('-') {
            // Allow signs right after a rational slash, but report them.
            if rational_mode == RationalScanningMode::Denominator {
                self.diagnostic.report(DiagnosticKind::err_lexer_invalid_number_character,
                    Span::new(self.prev_pos, self.pos));
            }

            // Scan over any extra signs that may follow the first one if the user was sleeping
            // and forgot to release the key. Do not treat them as a starter of imaginary part
            // of a complex number.
            while self.peek_is('-') || self.peek_is('+') {
                self.read();
                self.diagnostic.report(DiagnosticKind::err_lexer_invalid_number_character,
                    Span::new(self.prev_pos, self.pos));
            }

            // Handle IEEE 754 special values which must always have an explicit sign.
            // (Just "inf.0" is a valid identifier.)
            if self.try_scan_number_infnan() {
                *is_infnan = true;

                self.scan_infnan_suffix(radix, rational_mode);

                // Scan over a terminating 'i', reporting it if it's in non-final position.
                if self.cur_is('i') || self.cur_is('I') {
                    if self.peek_is('+') || self.peek_is('-') {
                        self.diagnostic.report(DiagnosticKind::err_lexer_misplaced_i,
                            Span::new(self.prev_pos, self.pos));
                    } else {
                        // Guaranteed by scan_infnan_suffix().
                        assert!(self.peek().map_or(true, is_delimiter));
                    }

                    *terminal_i = true;
                    self.read();
                }

                return;
            }

            // Read the sign at last.
            assert!(self.cur_is('+') || self.cur_is('-'));
            self.read();

            // Handle the special case of a sign immediately followed by an 'i' and a delimiter.
            // This form is valid in rectangular form of complex numbers. However, treat 'i' as
            // a regular invalid character in polar form.
            if (complex_mode != ComplexScanningMode::Argument) &&
                (self.cur_is('i') || self.cur_is('I'))
            {
                if self.peek().map_or(true, is_delimiter) {
                    *terminal_i = true;
                    self.read();
                    return;
                }

                if self.peek_is('+') || self.peek_is('-') || self.peek_is('@') {
                    self.diagnostic.report(DiagnosticKind::err_lexer_misplaced_i,
                        Span::new(self.prev_pos, self.pos));

                    *terminal_i = true;
                    self.read();
                    return;
                }
            }
        }

        self.scan_number_digits(radix, FractionScanningMode::Value,
            rational_mode, complex_mode, not_integer, terminal_i);

        if self.cur.map_or(false, is_exponent_marker) {
            *not_integer = true;

            self.read();

            if self.cur_is('+') || self.cur_is('-') {
                self.read();

                // Also scan over any extra signs that may follow the first one if the user
                // was sleeping and forgot to release the key. Do not treat them as starters
                // of a complex number part.
                while self.cur_is('-') || self.cur_is('+') {
                    self.diagnostic.report(DiagnosticKind::err_lexer_invalid_number_character,
                        Span::new(self.prev_pos, self.pos));
                    self.read();
                }
            }

            self.scan_number_digits(radix, FractionScanningMode::Exponent,
                rational_mode, complex_mode, not_integer, terminal_i);
        }
    }

    /// Scan significant digits of a number (including a decimal dot). The scanning mode determines
    /// what special characters are treated as terminators (otherwise they are scanned over just
    /// like any other invalid character). Sets `not_integer` to true if a decimal dot has been
    /// scanned over. Sets `terminal_i` to true if the digits terminate with an 'i' followed by
    /// a delimiter, concluding the imaginary part of a complex number in rectangular form.
    fn scan_number_digits(&mut self, radix: u8,
        fraction_mode: FractionScanningMode,
        rational_mode: RationalScanningMode,
        complex_mode: ComplexScanningMode,
        not_integer: &mut bool,
        terminal_i: &mut bool)
    {
        let mut seen_dot = false;
        let mut seen_i = false;

        let start = self.prev_pos;

        loop {
            match self.cur {
                // Handle exponent markers. Some of them overlap with hexadecimal digits,
                // so we allow exponents only when digits from 0 to 9 are involved.
                Some(c) if (radix == 10) && is_exponent_marker(c) => {
                    // If we are scanning the value part then it ends right here.
                    if fraction_mode == FractionScanningMode::Value {
                        break;
                    }

                    // Otherwise scan over the exponent marker along with the optional sign
                    // after it. Do not treat this sign as a delimiter of rectangular form
                    // of complex numbers. Just report this stuff as invalid characters.

                    self.diagnostic.report(DiagnosticKind::err_lexer_invalid_number_character,
                        Span::new(self.prev_pos, self.pos));
                    self.read();

                    if self.cur_is('+') || self.cur_is('-') {
                        self.diagnostic.report(DiagnosticKind::err_lexer_invalid_number_character,
                            Span::new(self.prev_pos, self.pos));
                        self.read();
                    }
                }

                // Handle decimal dot. Allow only one per number, treat extra ones as errors.
                // Exponents are always integral, so no dots are allowed there.
                Some('.') if !seen_dot && (fraction_mode != FractionScanningMode::Exponent) => {
                    seen_dot = true;
                    self.read();
                }

                // Handle rational slash. Only when we are scanning the numerator.
                Some('/') if (rational_mode == RationalScanningMode::Numerator) => {
                    break;
                }

                // Handle rectangular form of complex numbers.
                Some('+') | Some('-') => {
                    break;
                }

                // Handle polar form of complex numbers.
                Some('@') => {
                    break;
                }

                // Scan over all valid digits.
                Some(c) if is_digit(radix, c) => {
                    self.read();
                }

                // Stop scanning as soon as we see an 'i' followed by a delimiter. Do not treat
                // this 'i' as special when scanning argument of a polar form (it is just another
                // invalid character then).
                Some('i') | Some('I')
                    if (complex_mode != ComplexScanningMode::Argument) &&
                        self.peek().map_or(true, is_delimiter) =>
                {
                    seen_i = true;
                    break;
                }

                // Also stop when we see an 'i' followed by a sign. This probably means the user
                // wanted to write the imaginary part of a rectangular form followed by the real
                // part.
                Some('i') | Some('I')
                    if (complex_mode != ComplexScanningMode::Argument) &&
                        (self.peek_is('+') || self.peek_is('-')) =>
                {
                    self.diagnostic.report(DiagnosticKind::err_lexer_misplaced_i,
                        Span::new(self.prev_pos, self.pos));

                    seen_i = true;
                    break;
                }

                // Stop scanning as soon as we encounter a delimiter.
                Some(c) if is_delimiter(c) => {
                    break;
                }
                None => {
                    break;
                }

                // Scan over an report any other characters. If these are some digits then they
                // are digits of an unexpected radix (e.g., 9 used in binary literals).
                Some(c) => {
                    if is_digit(10, c) {
                        self.diagnostic.report(DiagnosticKind::err_lexer_invalid_number_digit,
                            Span::new(self.prev_pos, self.pos));
                    } else {
                        self.diagnostic.report(DiagnosticKind::err_lexer_invalid_number_character,
                            Span::new(self.prev_pos, self.pos));
                    }
                    self.read();
                }
            }
        }

        let end = self.prev_pos;

        // Check that there is at least one digit in the slice we have scanned over.
        // Treat all decimal digits as valid to allow for user mistakes.
        let check_radix = if radix <= 10 { 10 } else { radix };
        if !self.buf[start..end].chars().any(|c| is_digit(check_radix, c)) {
            self.diagnostic.report(DiagnosticKind::err_lexer_digits_missing,
                Span::new(start, end));
        }

        // Read over the 'i' we've seen before. This is done here so that the test above does not
        // include the 'i' into the digits part. The special cases '+i' and '-i' are handled by
        // the caller (`scan_number_value()`) and should be never encountered here. However, we
        // can still report missing digits when (start == end): e.g., "123/i".
        if seen_i {
            assert!(self.cur_is('i') || self.cur_is('I'));
            self.read();
        }

        if seen_dot {
            *not_integer = true;
        }

        if seen_i {
            *terminal_i = true;
        }
    }

    /// Try scanning a numeric `+inf.0` or `-nan.0` value. Returns true if such value has been
    /// successfully scanned over. Otherwise returns false and does not modify the scanning state.
    fn try_scan_number_infnan(&mut self) -> bool {
        assert!(self.cur_is('+') || self.cur_is('-'));

        if self.try_scan("+inf.0") || self.try_scan("-inf.0") {
            return true;
        }

        if self.try_scan("+nan.0") || self.try_scan("-nan.0") {
            return true;
        }

        false
    }

    /// If the current string remainder starts with a given prefix (case-insensitive, ASCII-only),
    /// then scan over it and return true. Otherwise do not modify scanning state and return false.
    fn try_scan(&mut self, prefix: &str) -> bool {
        let has_prefix = has_ascii_prefix_ci(&self.buf[self.prev_pos..], prefix);
        if has_prefix {
            for _ in 0..prefix.len() { self.read(); }
        }
        has_prefix
    }

    /// If the current string remainder starts with a given prefix (case-insensitive, ASCII-only),
    /// and a delimiter is present after the prefix, then scan over it and return true. Otherwise
    /// do not modify scanning state and return false.
    fn try_scan_exactly(&mut self, prefix: &str) -> bool {
        let has_prefix = has_ascii_prefix_ci_exact(&self.buf[self.prev_pos..], prefix);
        if has_prefix {
            for _ in 0..prefix.len() { self.read(); }
        }
        has_prefix
    }

    /// Scan over an (invalid) suffix of an `+inf.0` or `-nan.0` value.
    fn scan_infnan_suffix(&mut self, radix: u8, rational_mode: RationalScanningMode) {
        let suffix_start = self.prev_pos;
        loop {
            match self.cur {
                // Stop scanning suffix on slash only if we haven't seen one yet.
                Some('/') if rational_mode == RationalScanningMode::Numerator => { break; }

                // Scan over exponent markers *and* their optional signs. Do not treat
                // signs as starters of the imaginary part of a complex number.
                Some(c) if (radix == 10) && is_exponent_marker(c) => {
                    self.read();
                    if self.cur_is('-') || self.cur_is('+') {
                        self.read();
                    }
                }

                // Stop scanning the suffix if a terminating 'i' is ahead.
                Some('i') | Some('I') => {
                    let delimiter_ahead = self.peek().map_or(true, is_delimiter);
                    let sign_ahead = self.peek_is('+') || self.peek_is('-');

                    // The 'i' is terminating if it is followed by a delimiter.
                    // Or if it ends an (invalid) non-final imaginary part.
                    if delimiter_ahead || sign_ahead {
                        break;
                    }

                    // Otherwise treat this 'i' just like any other part of the suffix.
                    self.read();
                }

                // Stop scanning the suffix if we see a starter of a complex part.
                Some('+') | Some('-') => { break; }
                Some('@')             => { break; }

                // Also obviously stop scanning if we're faced with a delimiter.
                Some(c) if is_delimiter(c) => { break; }
                None                       => { break; }

                // Scan over anything else.
                Some(_) => { self.read(); }
            }
        }
        let suffix_end = self.prev_pos;

        if suffix_start != suffix_end {
            self.diagnostic.report(DiagnosticKind::err_lexer_infnan_suffix,
                Span::new(suffix_start, suffix_end));
        }
    }

    /// Normalize an identifier name.
    ///
    /// We normalize textual identifier names to NFKC (compatibility composition) in order to
    /// fold any visual ambiguities resulting from very permissive identifier syntax. We also
    /// remove any Default_Ignorable_Code_Points.
    ///
    /// When `#!fold-case` is in effect we apply NFKC_Casefold transformation which simultaneously
    /// folds case, normalizes text to NFKC, and removes Default_Ignorable_Code_Points.
    ///
    /// As noted in the discussion of SRFI 52, NFKC is not an ideal transformation, but it is
    /// a standardized best practice, so we go with it.
    fn normalize_identifer(&self, s: &str) -> String {
        if self.folding_case {
            normalize_case_insensitive_identifier(s)
        } else {
            normalize_case_sensitive_identifier(s)
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Utility definitions
//

/// Specifies how to handle special characters of fractional numbers (dots and exponents)
/// in `scan_number_digits()`.
#[derive(Clone, Copy, Eq, PartialEq)]
enum FractionScanningMode {
    /// We are scanning the value part of a number (before the exponent). At most one decimal dot
    /// is expected here. Exponent markers terminate the number.
    Value,

    /// We are scanning the exponent part of a number. Decimal dots and exponent markers are
    /// invalid here.
    Exponent,
}

/// Specifies how to handle special characters of rational numbers (slashes)
/// in `scan_number_digits()`.
#[derive(Clone, Copy, Eq, PartialEq)]
enum RationalScanningMode {
    /// We are scanning a numerator of a number. Rational slash terminates the numerator.
    Numerator,

    /// We are scanning a denominator of a rational number. A slash is treated as invalid.
    Denominator,
}

/// Specifies how to handle special characters of complex numbers (signs, @, i)
/// in `scan_number_digits()`.
#[derive(Clone, Copy, Eq, PartialEq)]
enum ComplexScanningMode {
    /// We are scanning the initial part of a number.
    Initial,

    /// We are scanning the argument of a complex number in polar form.
    Argument,

    /// We are scanning the subsequent part of a complex number in rectangular form.
    /// There should be only one subsequent part and it should end with an 'i'.
    Subsequent,
}

/// Check if a character is a whitespace.
fn is_whitespace(c: char) -> bool {
    match c {
        ' ' | '\t' | '\n' | '\r' => true,
        _ => false,
    }
}

/// Check if a character is a delimiter for tokens.
fn is_delimiter(c: char) -> bool {
    match c {
        // R7RS defines only the following delimiters:
        ' ' | '\t' | '\n' | '\r' | '|' | '"' | ';' | '(' | ')' | '[' | ']' | '{' | '}' |
        // But we add to this list all quotes as they are expanded into forms,
        // so effectively they can be treatead as opening parentheses.
        '\'' | ',' | '`' => true,
        _ => false,
    }
}

/// Check if a character is an exponent marker in number literals.
fn is_exponent_marker(c: char) -> bool {
    match c {
        // R7RS defines only the following marker:
        'e' | 'E' |
        // But it also allows to include these ones:
        's' | 'S' | 'd' | 'D' | 'f' | 'F' | 'l' | 'L' => true,
        _ => false,
    }
}

/// Check if a character is an initial of an identifer.
#[cfg(not(feature = "unicode"))]
fn is_identifier_initial(c: char) -> bool {
    match c {
        // <letter>
        'A'..='Z' | 'a'..='z' => true,
        // <special-initial>
        '!' | '$' | '%' | '&' | '*' | '/' | ':' |
        '<' | '=' | '>' | '?' | '^' | '_' | '~' => true,
        // <pecualiar> (= <special-subsequent>, they can be in the first position)
        '+' | '-' | '.' | '@' => true,
        // anything else is not allowed to start identifiers
        _ => false,
    }
}

/// Check if a character is a subsequent of an identifer.
#[cfg(not(feature = "unicode"))]
fn is_identifier_subsequent(c: char) -> bool {
    match c {
        // <initial>
        c if is_identifier_initial(c) => true,
        // <digit>
        '0'..='9' => true,
        // anything else is not allowed to be used in identifiers
        _ => false,
    }
}

/// Check if a character is an initial of an identifer.
#[cfg(feature = "unicode")]
fn is_identifier_initial(c: char) -> bool {
    libunicode::scheme_identifiers::is_initial(c)
}

/// Check if a character is a subsequent of an identifer.
#[cfg(feature = "unicode")]
fn is_identifier_subsequent(c: char) -> bool {
    libunicode::scheme_identifiers::is_subsequent(c)
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

    const H: &[u8; 128] = &[
        0, 0, 0, 0, 0, 0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0, 0, 0, 0, 0, 0, 0,0,0,0,0,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9,0,0,0,0,0,0,
        0,10,11,12,13,14,15,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,10,11,12,13,14,15,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    ];

    H[c as usize]
}

/// Appends hexadecimal nibble to a Unicode code point without integer overflow.
fn saturating_add_unicode_nibble(value: u32, c: char) -> u32 {
    if value <= 0x00_FF_FF_FF {
        (value << 4) | u32::from(hex_value(c))
    } else {
        value
    }
}

/// Normalize a string as a case-sensitive identifier.
#[cfg(not(feature = "unicode"))]
fn normalize_case_sensitive_identifier(s: &str) -> String {
    s.to_owned()
}

/// Normalize a string as a case-insensitive identifier.
#[cfg(not(feature = "unicode"))]
fn normalize_case_insensitive_identifier(s: &str) -> String {
    s.to_ascii_lowercase()
}

/// Normalize a string as a case-sensitive identifier.
#[cfg(feature = "unicode")]
fn normalize_case_sensitive_identifier(s: &str) -> String {
    const ZERO_WIDTH_NON_JOINER: char = '\u{200C}';
    const ZERO_WIDTH_JOINER: char = '\u{200D}';

    let normalized = libunicode::normalization::nfkc(s);

    // Scheme identifier syntax permits only ZWNJ and ZWJ to occur in valid identifiers,
    // so we remove only these (and do not need a table of Default_Ignorable_Code_Points).
    normalized.chars()
              .filter(|&c| !(c == ZERO_WIDTH_NON_JOINER || c == ZERO_WIDTH_JOINER))
              .collect()
}

/// Normalize a string as a case-insensitive identifier.
#[cfg(feature = "unicode")]
fn normalize_case_insensitive_identifier(s: &str) -> String {
    libunicode::case_algorithms::to_nfkc_casefold(s)
}

/// Check whether a string should be parsed as a number or it can be glanced off as an identifier
/// (possibly peculiar one). Returns true if it is a number.
fn looks_like_number_prefix(s: &str, radix: u8) -> bool {
    // Now, the whole notion of peculiar identifiers is abominable. That's what you get when
    // you make identifier syntax as permissive as it is in Scheme. Well, whatever. This is
    // just another hysterical raisin to deal with.
    //
    // Here is the formal grammar of peculiar identifiers from R7RS:
    //
    //     <peculiar-identifier> ::= <explicit-sign>                                    (1)
    //                            |  <explicit-sign>  <sign-subsequent> <subsequent>*   (2)
    //                            |  <explicit-sign> . <dot-subsequent> <subsequent>*   (3)
    //                            |                  . <dot-subsequent> <subsequent>*   (4)
    //
    //     <explicit-sign> ::= + | -
    //
    //     <sign-subsequent> ::= <initial> | <explicit-sign> | @
    //
    //     <dot-subsequent> ::= <sign-subsequent> | .
    //
    // where <subsequent> can be thought of as effectively anything except for <delimiter>,
    // and <initial> is <subsequent> minus decimal digits and [+-.@]
    //
    // And there is an exception: +i, -i, +inf.0, -inf.0, +nan.0, -nan.0 (case-insensitive)
    // are parsed as numbers, not peculiar identifiers. Note that they must match exactly.
    //
    // One wacky point about these exceptions, which is not immediately obvious from the
    // formal grammar and R7RS commentary, is whether a string like "+inf.0+40000monkeys"
    // is a valid peculiar identifer, or an invalid number literal, or something else.
    //
    // This could be a peculiar identifier as it matches its grammar and does not match
    // the grammar of numbers. However, there is another rule that says that an identifier
    // cannot have a valid number as its prefix, and in our case we have this "+inf.0".
    // So we should treat this string as ungrammatical and we can report anything we want.
    // But that rule also invalidates all identifiers starting with "+i", just this one
    // special letter. Which is obviously dumb.
    //
    // Thus we take the following stance on this issue: if we see /[+-](inf|nan).0/i
    // as a prefix then this is *not* a peculiar identifier. Otherwise it is, except
    // for the exact case /[+-]i[:delimiter:]/i which is a number.

    let first  = s.chars().nth(0);
    let second = s.chars().nth(1);
    let third  = s.chars().nth(2);

    // Cases (1), (2), (3)
    if first == Some('+') || first == Some('-') {
        // Case (1)
        if second.map_or(true, is_delimiter) {
            return false;
        }

        // Case (3)
        if second == Some('.') {
            return third.map_or(false, |c| !is_delimiter(c) && is_digit(radix, c));
        }

        // Case (2) with exceptions
        if second.map_or(true, |c| !is_digit(radix, c)) {
            let after_sign = &s[1..];
            if has_ascii_prefix_ci(after_sign, "inf.0") {
                return true;
            }
            if has_ascii_prefix_ci(after_sign, "nan.0") {
                return true;
            }
            if has_ascii_prefix_ci_exact(after_sign, "i") {
                return true;
            }
            return false;
        }

        // Anything starting with a sign followed by a digit must be a number.
        return true;
    }

    // Case (4)
    if first == Some('.') {
        return second.map_or(false, |c| is_delimiter(c) || is_digit(radix, c));
    }

    // Anything starting with a digit must be a number.
    first.map_or(false, |c| is_digit(radix, c))
}

/// Return true if `s` has a given ASCII prefix ignoring case.
fn has_ascii_prefix_ci(s: &str, prefix: &str) -> bool {
    assert!(prefix.chars().all(|c| c.is_ascii() && c == c.to_ascii_lowercase()));

    s.chars()
     .map(|c| c.to_ascii_lowercase())
     .take(prefix.len())
     .eq(prefix.chars())
}

/// Return true if `s` has a given ASCII prefix ignoring case, and a delimiter after it.
fn has_ascii_prefix_ci_exact(s: &str, prefix: &str) -> bool {
    if !has_ascii_prefix_ci(s, prefix) {
        return false;
    }
    s.chars()
     .nth(prefix.len())
     .map_or(true, is_delimiter)
}
