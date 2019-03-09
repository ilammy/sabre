// Copyright (c) 2017, Sabre developers
//
// Licensed under the Apache License, Version 2.0 (see LICENSE.Apache in the
// root directory) or MIT license (see LICENSE.MIT in the root directory),
// at your option. This file may be copied, distributed, and modified only
// in accordance with the terms specified by the chosen license.

//! Lexical environments.

use std::collections::HashMap;
use std::rc::Rc;

use liblocus::diagnostics::Span;
use libreader::intern_pool::Atom;

use crate::expand::Expander;
use crate::expression::Variable;

/// Lexical environment.
///
/// Environments describe meanings of identifiers in Scheme programs. They roughly correspond to
/// visibility scopes which are nested one into another.
pub struct Environment {
    kind: EnvironmentKind,
    variables: HashMap<Atom, EnvironmentVariable>,
    parent: Option<Rc<Environment>>,
}

enum EnvironmentKind {
    Local,
    Global,
    Imported,
}

struct EnvironmentVariable {
    /// Kind of a variable stored in the environment.
    kind: VariableKind,
    /// Definition site of the variable.
    span: Span,
}

enum VariableKind {
    /// Run-time variable requiring storage.
    Runtime {
        /// Index of the variable storage location.
        ///
        /// For local variables it's the stack frame, for global variables it's the global table,
        /// for imported variables it's the import table.
        index: usize,
    },
    /// Syntactic variable bound to a macro expander.
    Syntactic {
        /// The macro expander.
        expander: Box<Expander>,
    }
}

/// Reference to a variable from environment.
pub struct VariableReference<'a> {
    /// Kind of the variable referenced.
    pub kind: ReferenceKind<'a>,
    /// Location of the variable definition.
    pub span: Span,
}

/// Kind of a referenced variable.
pub enum ReferenceKind<'a> {
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
    /// Syntactic variable.
    ///
    /// Syntactic variables are bound to macro expanders.
    Syntactic {
        expander: &'a Expander,
    }
}

impl Environment {
    /// Create a new local environment with specified variables.
    pub fn new_local(variables: &[Variable], parent: &Rc<Environment>) -> Rc<Environment> {
        Rc::new(Environment {
            kind: EnvironmentKind::Local,
            variables: enumerate_runtime_variables(variables),
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
            variables: enumerate_runtime_variables(variables),
            parent: Some(parent.clone()),
        })
    }

    /// Create a new imported environment with specified variables.
    ///
    /// Import environment is the base environment of a Scheme module, it does not have a parent.
    pub fn new_imported(
        runtime_variables: &[Variable],
        syntactic_variables: Vec<(Variable, Box<Expander>)>,
    ) -> Rc<Environment> {
        Rc::new(Environment {
            kind: EnvironmentKind::Imported,
            variables: combine_variables(runtime_variables, syntactic_variables),
            parent: None,
        })
    }

    /// Resolve a variable in this environment.
    ///
    /// Returns Some variable from the environment or its parents, or None if the variable
    /// could not be found in any environment.
    pub fn resolve_variable(&self, name: Atom) -> Option<VariableReference> {
        // First, try to resolve the name locally.
        if let Some(variable) = self.variables.get(&name) {
            let kind = match variable.kind {
                VariableKind::Runtime { index } => {
                    match self.kind {
                        EnvironmentKind::Local => ReferenceKind::Local { index, depth: 0 },
                        EnvironmentKind::Global => ReferenceKind::Global { index },
                        EnvironmentKind::Imported => ReferenceKind::Imported { index },
                    }
                }
                VariableKind::Syntactic { ref expander } => {
                    ReferenceKind::Syntactic { expander: expander.as_ref() }
                }
            };
            return Some(VariableReference { kind, span: variable.span });
        }

        // If that fails then look into parent environment (if it's available).
        if let Some(ref parent) = self.parent {
            if let Some(mut variable) = parent.resolve_variable(name) {
                // Bump the nesting depth for local variables.
                if let ReferenceKind::Local { ref mut depth, .. } = variable.kind {
                    *depth += 1;
                }
                return Some(variable);
            }
        }

        // The variable cannot be resolved if it is absent in all environments.
        None
    }
}

fn enumerate_runtime_variables(variables: &[Variable]) -> HashMap<Atom, EnvironmentVariable> {
    variables.iter()
        .enumerate()
        .map(|(index, variable)| {
            (variable.name, EnvironmentVariable {
                kind: VariableKind::Runtime { index },
                span: variable.span,
            })
        })
        .collect()
}

fn combine_variables(
    runtime_variables: &[Variable],
    syntactic_variables: Vec<(Variable, Box<Expander>)>
) -> HashMap<Atom, EnvironmentVariable> {
    let mut variables = HashMap::new();

    for (index, variable) in runtime_variables.iter().enumerate() {
        let entry = EnvironmentVariable {
            kind: VariableKind::Runtime { index },
            span: variable.span,
        };
        let previous = variables.insert(variable.name, entry);

        if previous.is_some() {
            panic!("duplicate variable name: {:?}", variable.name);
        }
    }

    for (variable, expander) in syntactic_variables {
        let entry = EnvironmentVariable {
            kind: VariableKind::Syntactic { expander },
            span: variable.span,
        };
        let previous = variables.insert(variable.name, entry);

        if previous.is_some() {
            panic!("duplicate variable name: {:?}", variable.name);
        }
    }

    variables
}
