use std::{collections::HashMap, vec};

use azalea_parse::ast::ast_types::Ty;

#[derive(Debug, Clone)]
pub struct Scope {
    pub variables: HashMap<String, Ty>,
}

#[derive(Debug, Clone)]
pub struct Resolver {
    scopes: Vec<Scope>,
}

impl Resolver {
    pub fn new() -> Self {
        let init = Scope {
            variables: HashMap::new(),
        };

        Self { scopes: vec![init] }
    }
}
