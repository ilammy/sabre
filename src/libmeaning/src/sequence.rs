// Copyright (c) 2019, Sabre developers
//
// Licensed under the Apache License, Version 2.0 (see LICENSE.Apache in the
// root directory) or MIT license (see LICENSE.MIT in the root directory),
// at your option. This file may be copied, distributed, and modified only
// in accordance with the terms specified by the chosen license.

//! Utilities for sequence splicing.

use libexpand::expression::{Expression, ExpressionKind};

// I personally consider the splicing role of the (begin term ...) form to be a wart on Scheme
// syntax and semantics. It could have been better if it was named "splice". Or did not exist
// at all.
//
// The splicing "begin" form is not very consistent with the sequencing role of the (begin expr1
// exprs ...) form, the 'implicit sequence' of top-level expressions, and the 'internal
// definitions' introduced by lambda forms and syntax derived from them (e.g., letrec). It feels
// that this form was introduced and maintained purely because of historical reasons.
//
// The main motivation for the splicing "begin" are macros. It is a desirable feature for macros
// to be able to introduce definitions that are visible outside of the immediately expanded
// expression: e.g., define-values define multiple variables which are accessible to expressions
// that come after the "define-values" form.
//
// However, this can be achieved by allowing macros to expand into multiple terms which are then
// spliced into the expansion site. There is no need for a new special form "splice" or for
// overloaded "begin" to achieve that.
//
// With this,
// - "begin" form has only one meaning
// - macros still have rewriting as expansion-time semantics
// - splicing magic is still there if you need it
// - macros do not introduce an implicit scope when they are expanded
// - something like (let () ...) can be used to explicitly delimit the lexical scope for internal
//   definitions if needed
//
// But instead we have a splicing "begin" and have to support it. It sucks to be backwards-
// compatible and to eat histerical raisins, I guess.
//
// Either way, Abathur would certainly like this function name, so that alone justifies it.

/// Iterate over expressions, flattening the splicing sequences.
pub fn splice_in_sequences(expressions: &[Expression]) -> SequenceSplicingIterator {
    SequenceSplicingIterator {
        current: Some(expressions.iter()),
        postponed: Vec::new(),
    }
}

type Expressions<'a> = std::slice::Iter<'a, Expression>;

/// Splicing iterator over expressions.
pub struct SequenceSplicingIterator<'a> {
    current: Option<Expressions<'a>>,
    postponed: Vec<Expressions<'a>>,
}

impl<'a> Iterator for SequenceSplicingIterator<'a> {
    type Item = &'a Expression;

    fn next(&mut self) -> Option<&'a Expression> {
        loop {
            // If there is an iterator we're currently walking through then take it.
            // Otherwise we're done and there is nothing to do anymore. Note that
            // we have to *not* borrow self.current as we may need to change it later.
            if let Some(mut iter) = self.current.take() {
                // If the current iterator still has some values in it then take them.
                // Otherwise get back to postponed iterators and try again with them.
                if let Some(expression) = iter.next() {
                    // If the next expression is a sequence then instead of returning it
                    // splice its elements in by iterating over them now and getting back
                    // to the previous iterator later.
                    if let ExpressionKind::Sequence(ref expressions) = expression.kind {
                        self.postponed.push(iter);
                        self.current = Some(expressions.iter());
                        continue;
                    }
                    self.current = Some(iter);
                    return Some(expression);
                }
                self.current = self.postponed.pop();
                continue;
            }
            return None;
        }
    }
}
