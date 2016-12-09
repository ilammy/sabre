// Copyright (c) 2016, Sabre developers
//
// Licensed under the Apache License, Version 2.0 (see LICENSE.Apache in the
// root directory) or MIT license (see LICENSE.MIT in the root directory),
// at your option. This file may be copied, distributed, and modified only
// in accordance with the terms specified by the chosen license.

//! Pretty-printing diagnostics.

use std::fmt;

use reader::diagnostics::Diagnostic;

/// Displayable wrapper over `Diagnostic`.
#[doc(hidden)]
pub struct DiagnosticDisplay<'a> {
    diagnostic: &'a Diagnostic,
    buffer: &'a str,
}

/// Display a diagnostic with context.
pub fn pretty_diagnostic<'a>(diagnostic: &'a Diagnostic, buffer: &'a str)
    -> DiagnosticDisplay<'a>
{
    DiagnosticDisplay { diagnostic: diagnostic, buffer: buffer }
}

impl<'a> fmt::Display for DiagnosticDisplay<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(ref loc) = self.diagnostic.loc {
            write!(f,
                "{kind:?} @ [{from}, {to}] = {slice:?}",
                kind    = self.diagnostic.kind,
                from    = loc.from,
                to      = loc.to,
                slice   = &self.buffer[loc.from..loc.to],
            )
        } else {
            write!(f,
                "{kind:?} @ nowhere",
                kind    = self.diagnostic.kind,
            )
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use reader::diagnostics::{Diagnostic, DiagnosticKind, Span};

    #[test]
    fn diagnostic_with_location() {
        let buffer = "#O123456789";
        let diagnostic = Diagnostic {
            kind: DiagnosticKind::err_lexer_invalid_number_digit,
            loc: Some(Span::new(9, 10)),
        };

        assert_eq!("err_lexer_invalid_number_digit @ [9, 10] = \"8\"",
            format!("{}", pretty_diagnostic(&diagnostic, &buffer)));
    }

    #[test]
    fn diagnostic_without_location() {
        let buffer = "it's a trap";
        let diagnostic = Diagnostic {
            kind: DiagnosticKind::fatal_lexer_unterminated_comment,
            loc: None,
        };

        assert_eq!("fatal_lexer_unterminated_comment @ nowhere",
            format!("{}", pretty_diagnostic(&diagnostic, &buffer)));
    }
}
