// Copyright (c) 2016, Sabre developers
//
// Licensed under the Apache License, Version 2.0 (see LICENSE.Apache in the
// root directory) or MIT license (see LICENSE.MIT in the root directory),
// at your option. This file may be copied, distributed, and modified only
// in accordance with the terms specified by the chosen license.

//! Pretty-printing lexer tokens.

use std::fmt;

use reader::intern_pool::InternPool;
use reader::lexer::ScannedToken;
use reader::tokens::Token;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// ScannedToken

/// Displayable wrapper over `ScannedToken`.
#[doc(hidden)]
pub struct ScannedTokenDisplay<'a> {
    token: &'a ScannedToken,
    buffer: &'a str,
    pool: &'a InternPool,
}

/// Display a scanned token with context.
pub fn pretty_scanned_token<'a>(token: &'a ScannedToken, buffer: &'a str, pool: &'a InternPool)
    -> ScannedTokenDisplay<'a>
{
    ScannedTokenDisplay { token: token, buffer: buffer, pool: pool }
}

impl<'a> fmt::Display for ScannedTokenDisplay<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f,
            "{token} @ [{from}, {to}] = {slice:?}",
            token = pretty_token(&self.token.tok, self.pool),
            from  = self.token.span.from,
            to    = self.token.span.to,
            slice = &self.buffer[self.token.span.from..self.token.span.to],
        )
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Token

/// Displayable wrapper over `Token`.
#[doc(hidden)]
pub struct TokenDisplay<'a> {
    token: &'a Token,
    pool: &'a InternPool,
}

/// Display a scanned token with context.
pub fn pretty_token<'a>(token: &'a Token, pool: &'a InternPool) -> TokenDisplay<'a> {
    TokenDisplay { token: token, pool: pool }
}

impl<'a> fmt::Display for TokenDisplay<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self.token {
            Token::String(value)        => write!(f, "String({:?})",        self.pool.get(value)),
            Token::Number(value)        => write!(f, "Number({:?})",        self.pool.get(value)),
            Token::Identifier(value)    => write!(f, "Identifier({:?})",    self.pool.get(value)),
            Token::Directive(value)     => write!(f, "Directive({:?})",     self.pool.get(value)),
            _                           => write!(f, "{:?}", self.token),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use reader::diagnostics::Span;
    use reader::intern_pool::InternPool;
    use reader::lexer::ScannedToken;
    use reader::tokens::{Token, ParenType};

    #[test]
    fn token_string() {
        let buffer = "\"test\"";
        let pool = InternPool::new();
        let token = ScannedToken {
            tok: Token::String(pool.intern("test")),
            span: Span::new(0, 6),
        };

        assert_eq!("String(\"test\") @ [0, 6] = \"\\\"test\\\"\"",
            format!("{}", pretty_scanned_token(&token, &buffer, &pool)));
    }

    #[test]
    fn token_number() {
        let buffer = "#x23+4i";
        let pool = InternPool::new();
        let token = ScannedToken {
            tok: Token::Number(pool.intern("#x23+4i")),
            span: Span::new(0, 7),
        };

        assert_eq!("Number(\"#x23+4i\") @ [0, 7] = \"#x23+4i\"",
            format!("{}", pretty_scanned_token(&token, &buffer, &pool)));
    }

    #[test]
    fn token_identifier() {
        let buffer = "demodemo";
        let pool = InternPool::new();
        let token = ScannedToken {
            tok: Token::Identifier(pool.intern("demodemo")),
            span: Span::new(0, 8),
        };

        assert_eq!("Identifier(\"demodemo\") @ [0, 8] = \"demodemo\"",
            format!("{}", pretty_scanned_token(&token, &buffer, &pool)));
    }

    #[test]
    fn token_directive() {
        let buffer = "#!fold-case";
        let pool = InternPool::new();
        let token = ScannedToken {
            tok: Token::Directive(pool.intern("fold-case")),
            span: Span::new(0, 11),
        };

        assert_eq!("Directive(\"fold-case\") @ [0, 11] = \"#!fold-case\"",
            format!("{}", pretty_scanned_token(&token, &buffer, &pool)));
    }

    #[test]
    fn token_simple() {
        let buffer = "#(";
        let pool = InternPool::new();
        let token = ScannedToken {
            tok: Token::OpenVector(ParenType::Parenthesis),
            span: Span::new(0, 2),
        };

        assert_eq!("OpenVector(Parenthesis) @ [0, 2] = \"#(\"",
            format!("{}", pretty_scanned_token(&token, &buffer, &pool)));
    }
}
