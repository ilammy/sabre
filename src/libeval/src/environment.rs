// Copyright (c) 2017, Sabre developers
//
// Licensed under the Apache License, Version 2.0 (see LICENSE.Apache in the
// root directory) or MIT license (see LICENSE.MIT in the root directory),
// at your option. This file may be copied, distributed, and modified only
// in accordance with the terms specified by the chosen license.

//! Lexical environments.

use std::rc::Rc;

use libreader::intern_pool::Atom;

use crate::expression::Variable;

/// Lexical environment.
///
/// Environments describe meanings of identifiers in Scheme programs. They roughly correspond to
/// visibility scopes which are nested one into another.
pub struct Environment {
    kind: EnvironmentKind,
    variables: Vec<Variable>,
    parent: Option<Rc<Environment>>,
}

enum EnvironmentKind {
    Local,
    Global,
    Imported,
}

/// Type of a variable.
pub enum VariableKind {
    /// Locally-bound variable, defined by a procedure.
    ///
    /// Local variables are identified by their (zero-based) index in the activation record of
    /// their defining procedure and by the nesting depth of the procedure (zero is current one).
    Local {
        depth: usize,
        index: usize,
    },
    /// Globally-defined mutable variable.
    ///
    /// Global variables are identified by their index in the global variable table.
    Global {
        index: usize,
    },
    /// Imported immutable variable.
    ///
    /// Imported variables are identified by their index in the import table.
    Imported {
        index: usize,
    },
    /// An unresolved variable.
    ///
    /// No environment contains a definition of the requested variable. It is an error to use
    /// such variables.
    Unresolved,
}

impl Environment {
    /// Create a new local environment with specified variables.
    pub fn new_local(variables: &[Variable], parent: &Rc<Environment>) -> Rc<Environment> {
        Rc::new(Environment {
            kind: EnvironmentKind::Local,
            variables: variables.to_vec(),
            parent: Some(parent.clone()),
        })
    }

    /// Create a new global environment with specified variables.
    ///
    /// # Panics
    ///
    /// The environment stack should contain exactly one global environment, so the parent must be
    /// an imported environment, or else this function panics.
    pub fn new_global(variables: &[Variable], parent: &Rc<Environment>) -> Rc<Environment> {
        assert!(match parent.kind { EnvironmentKind::Imported => true, _ => false });
        Rc::new(Environment {
            kind: EnvironmentKind::Global,
            variables: variables.to_vec(),
            parent: Some(parent.clone()),
        })
    }

    /// Create a new imported environment with specified variables.
    ///
    /// Import environment is the base environment of a Scheme module, it does not have a parent.
    pub fn new_imported(variables: &[Variable]) -> Rc<Environment> {
        Rc::new(Environment {
            kind: EnvironmentKind::Imported,
            variables: variables.to_vec(),
            parent: None,
        })
    }

    /// Resolve a variable in this environment.
    pub fn resolve_variable(&self, name: Atom) -> VariableKind {
        // First, try to resolve the name locally.
        for (index, local) in self.variables.iter().enumerate() {
            if name == local.name {
                return match self.kind {
                    EnvironmentKind::Local => VariableKind::Local { index, depth: 0 },
                    EnvironmentKind::Global => VariableKind::Global { index },
                    EnvironmentKind::Imported => VariableKind::Imported { index },
                };
            }
        }

        // If that fails then look into parent environment (if it's available).
        if let Some(ref parent) = self.parent {
            let mut variable = parent.resolve_variable(name);
            // Bump the nesting depth for local variables.
            if let VariableKind::Local { ref mut depth, .. } = variable {
                *depth += 1;
            }
            return variable;
        }

        // The variable cannot be resolved if it is absent in all environments.
        VariableKind::Unresolved
    }
}
