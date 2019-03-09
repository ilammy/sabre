// Copyright (c) 2019, Sabre developers
//
// Licensed under the Apache License, Version 2.0 (see LICENSE.Apache in the
// root directory) or MIT license (see LICENSE.MIT in the root directory),
// at your option. This file may be copied, distributed, and modified only
// in accordance with the terms specified by the chosen license.

//! Meaning definitions.

use std::fmt;

use liblocus::diagnostics::Span;

/// Meaning of a Scheme expression.
pub struct Meaning {
    /// The meaning itself.
    pub kind: MeaningKind,
    /// Span of the original expression.
    pub span: Span,
}

/// Core scheme expressions.
pub enum MeaningKind {
    /// Value: undefined.
    Undefined,
    /// Value: constant.
    Constant(usize),
    /// Value: local variable.
    ArgumentReference(usize, usize),
    /// Value: global variable.
    GlobalReference(usize),
    /// Value: imported binding.
    ImportedReference(usize),
    /// Value: closure with fixed argument list.
    ClosureFixed(usize, Box<Meaning>),
    /// Effect: local assignment.
    ArgumentSet(usize, usize, Box<Meaning>),
    /// Effect: global assignment.
    GlobalSet(usize, Box<Meaning>),
    /// Control: condition.
    Alternative(Box<Meaning>, Box<Meaning>, Box<Meaning>),
    /// Control: evaluation sequencing.
    Sequence(Vec<Meaning>),
    /// Control: procedure call.
    ProcedureCall(Box<Meaning>, Vec<Meaning>),
}

impl fmt::Debug for Meaning {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.kind {
            MeaningKind::Undefined => write!(f, "(Undefined)"),
            MeaningKind::Constant(index) => write!(f, "(Constant {})", index),
            MeaningKind::ArgumentReference(depth, index) => {
                write!(f, "(ArgumentReference {} {})", depth, index)
            }
            MeaningKind::GlobalReference(index) => write!(f, "(GlobalReference {})", index),
            MeaningKind::ImportedReference(index) => write!(f, "(ImportedReference {})", index),
            MeaningKind::ClosureFixed(arg_count, ref body) => {
                write!(f, "(ClosureFixed {} {:?})", arg_count, body)
            }
            MeaningKind::ArgumentSet(depth, index, ref value) => {
                write!(f, "(ArgumentSet {} {} {:?})", depth, index, value)
            }
            MeaningKind::GlobalSet(index, ref value) => {
                write!(f, "(GlobalSet {} {:?})", index, value)
            }
            MeaningKind::Alternative(ref condition, ref consequent, ref alternate) => write!(
                f,
                "(Alternative {:?} {:?} {:?})",
                condition, consequent, alternate
            ),
            MeaningKind::Sequence(ref computations) => {
                write!(f, "(Sequence")?;
                for c in computations {
                    write!(f, " {:?}", c)?;
                }
                write!(f, ")")?;
                Ok(())
            }
            MeaningKind::ProcedureCall(ref procedure, ref args) => {
                write!(f, "(ProcedureCall {:?}", procedure)?;
                for a in args {
                    write!(f, " {:?}", a)?;
                }
                write!(f, ")")?;
                Ok(())
            }
        }
    }
}
