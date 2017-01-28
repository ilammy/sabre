// Copyright (c) 2017, Sabre developers
//
// Licensed under the Apache License, Version 2.0 (see LICENSE.Apache in the
// root directory) or MIT license (see LICENSE.MIT in the root directory),
// at your option. This file may be copied, distributed, and modified only
// in accordance with the terms specified by the chosen license.

//! Diagnostic kinds.
//!
//! Here we keep our long list of all possible diagnostics that Sabre can produce.

/// Kinds of repoted diagnostics.
#[allow(non_camel_case_types)]
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum DiagnosticKind {
    /// Lexer has encountered a block comment without matching termination token.
    fatal_lexer_unterminated_comment,

    /// Lexer has found a hex-coded Unicode code point literal outside of Unicode range.
    err_lexer_invalid_unicode_range,

    /// Lexer has expected an inline character after `#\` but there wasn't any.
    err_lexer_character_missing,

    /// Lexer has scanned a named character with unknown name.
    err_lexer_unknown_character_name,

    /// Lexer has encountered a string without a closing quote.
    fatal_lexer_unterminated_string,

    /// Lexer has encountered an invalid escape sequence.
    err_lexer_invalid_escape_sequence,

    /// Lexer has encountered an invalid line escape, not followed by only whitespace.
    err_lexer_invalid_line_escape,

    /// Lexer has expected hex digits in a hexcoded character escape but there weren't any.
    err_lexer_unicode_escape_missing_digits,

    /// Lexer has expected a semicolon to terminate a hexcoded character escape
    /// but there wasn't any.
    err_lexer_unicode_escape_missing_semicolon,

    /// Lexer has encountered multiple numeric radix specifiers in a number literal.
    err_lexer_multiple_number_radices,

    /// Lexer has encountered multiple exactness specifiers in a number literal.
    err_lexer_multiple_exactness,

    /// Lexer has encountered an invalid number prefix.
    err_lexer_invalid_number_prefix,

    /// Lexer has encountered a digit of invalid radix.
    err_lexer_invalid_number_digit,

    /// Lexer has encountered a character which is not allowed in numbers.
    err_lexer_invalid_number_character,

    /// Lexer has encountered a real number with non-decimal radix prefix.
    err_lexer_nondecimal_real,

    /// Lexer has expected some number digits but there weren't any.
    err_lexer_digits_missing,

    /// Lexer has expected an 'i' (for imaginary part) here but there weren't any.
    err_lexer_missing_i,

    /// Lexer did not expect an 'i' here but there is one.
    err_lexer_misplaced_i,

    /// Lexer has scanned over an unxpected part of a complex number.
    err_lexer_extra_complex_part,

    /// Lexer has scanned over unexpected suffix of an inf/nan literal.
    err_lexer_infnan_suffix,

    /// Lexer has scanned over a non-integer part of a rational number.
    err_lexer_noninteger_rational,

    /// Lexer has scanned over an infnan numerator or denominator of a rational number.
    err_lexer_infnan_rational,

    /// Lexer has encountered a number prefix before what appears to be an identifier.
    err_lexer_prefixed_identifier,

    /// Lexer has expected `=` or `#` to terminate a datum label here.
    err_lexer_missing_datum_label_terminator,

    /// Lexer has scanned over a character that is not allowed in identifers.
    err_lexer_invalid_identifier_character,

    /// Lexer has scanned over a plain identifier which looks like a number after normalization.
    warn_lexer_identifier_looks_like_number,

    /// Lexer has scanned over an unrecognized directive.
    err_lexer_unknown_directive,

    /// Lexer has encountered an escaped identifier without a closing vertical bar.
    fatal_lexer_unterminated_identifier,

    /// Parser has encountered a mismatched closing delimiter (e.g., `(cons 1 2]`).
    err_parser_mismatched_delimiter,

    /// Parser has encountered an opening delimiter without a matching closing one.
    fatal_parser_unterminated_delimiter,

    /// Parser has encountered an extra closing delimiter without a matching opening one.
    err_parser_extra_delimiter,

    /// Parser has encountered a non-number element in a bytevector.
    err_parser_invalid_bytevector_element,

    /// Parser has encountered a dot outside of a list.
    err_parser_misplaced_dot,

    /// Parser has expected a datum here for an abbreviation, a label, or a datum comment.
    err_parser_missing_datum,
}
