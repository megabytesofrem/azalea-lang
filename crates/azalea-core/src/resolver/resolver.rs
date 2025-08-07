use std::{collections::HashMap, vec};

use crate::ast::ast_types::Ty;
use crate::{resolver::Return, resolver::error::SemanticError};

#[derive(Debug, Clone)]
pub struct Scope {
    pub variables: HashMap<String, Ty>,
    pub functions: HashMap<String, Ty>,
}

impl Scope {
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
            functions: HashMap::new(),
        }
    }

    pub fn define_variable(&mut self, name: String, ty: Ty) -> Return<()> {
        if self.variables.contains_key(&name) {
            return Err(SemanticError::RedefinedVariable(name));
        }
        self.variables.insert(name, ty);
        Ok(())
    }

    pub fn lookup_variable(&self, name: &str) -> Option<&Ty> {
        self.variables.get(name)
    }

    /// Define a function in this scope
    pub fn define_function(&mut self, name: String, ty: Ty) -> Return<()> {
        if self.functions.contains_key(&name) {
            return Err(SemanticError::RedefinedVariable(name));
        }
        self.functions.insert(name, ty);
        Ok(())
    }

    /// Look up a function in this scope
    pub fn lookup_function(&self, name: &str) -> Option<&Ty> {
        self.functions.get(name)
    }
}

#[derive(Debug, Clone)]
pub struct Resolver {
    scopes: Vec<Scope>,
}

impl Resolver {
    pub fn new() -> Self {
        let global_scope = Scope::new();
        Self {
            scopes: vec![global_scope],
        }
    }

    /// Push a new scope onto the scope stack (for entering blocks, functions, etc.)
    pub fn push_scope(&mut self) {
        self.scopes.push(Scope::new());
    }

    /// Pop the current scope from the scope stack (for exiting blocks, functions, etc.)
    pub fn pop_scope(&mut self) -> Return<()> {
        if self.scopes.len() <= 1 {
            return Err(SemanticError::EmptyScope);
        }
        self.scopes.pop();
        Ok(())
    }

    /// Define a variable in the current scope
    pub fn define_variable(&mut self, name: String, ty: Ty) -> Return<()> {
        if let Some(current_scope) = self.scopes.last_mut() {
            current_scope.define_variable(name, ty)
        } else {
            Err(SemanticError::EmptyScope)
        }
    }

    /// Look up a variable in the scope chain (from innermost to outermost)
    pub fn lookup_variable(&self, name: &str) -> Option<&Ty> {
        // Search from innermost scope to outermost
        for scope in self.scopes.iter().rev() {
            if let Some(ty) = scope.lookup_variable(name) {
                return Some(ty);
            }
        }
        None
    }

    /// Check if a variable is defined in any scope
    pub fn is_variable_defined(&self, name: &str) -> bool {
        self.lookup_variable(name).is_some()
    }

    /// Get the current scope depth (0 = global scope)
    pub fn scope_depth(&self) -> usize {
        self.scopes.len() - 1
    }

    /// Define a function in the current scope
    pub fn define_function(&mut self, name: String, ty: Ty) -> Return<()> {
        if let Some(current_scope) = self.scopes.last_mut() {
            current_scope.define_function(name, ty)
        } else {
            Err(SemanticError::EmptyScope)
        }
    }

    /// Look up a function in the scope chain (from innermost to outermost)
    pub fn lookup_function(&self, name: &str) -> Option<&Ty> {
        // Search from innermost scope to outermost
        for scope in self.scopes.iter().rev() {
            if let Some(ty) = scope.lookup_function(name) {
                return Some(ty);
            }
        }
        None
    }

    /// Check if a function is defined in any scope
    pub fn is_function_defined(&self, name: &str) -> bool {
        self.lookup_function(name).is_some()
    }

    /// Define a variable in the current scope, allowing redefinition (for let statements)
    pub fn define_or_redefine_variable(&mut self, name: String, ty: Ty) {
        if let Some(current_scope) = self.scopes.last_mut() {
            current_scope.variables.insert(name, ty);
        }
    }

    /// Clear all scopes and reset to a single global scope
    pub fn clear(&mut self) {
        self.scopes.clear();
        self.scopes.push(Scope::new());
    }
}
