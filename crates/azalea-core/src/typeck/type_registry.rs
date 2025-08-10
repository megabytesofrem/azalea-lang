use std::collections::HashMap;

use crate::{
    ast::ast_types::{Enum, Record},
    resolver::error::SemanticError,
};

/// TypeRegistry manages the registration and lookup of record and enum types
/// during the typechecking phase.
#[derive(Debug, Clone)]
pub struct TypeRegistry {
    pub records: HashMap<String, Record>,
    pub enums: HashMap<String, Enum>,
}

impl TypeRegistry {
    pub fn new() -> Self {
        TypeRegistry {
            records: HashMap::new(),
            enums: HashMap::new(),
        }
    }

    pub fn register_record(&mut self, record: Record) -> Result<(), SemanticError> {
        if self.records.contains_key(&record.name) {
            return Err(SemanticError::RedefinedVariable(record.name.clone()));
        }

        self.records.insert(record.name.clone(), record.clone());

        Ok(())
    }

    pub fn register_enum(&mut self, en: Enum) -> Result<(), SemanticError> {
        if self.enums.contains_key(&en.name) {
            return Err(SemanticError::RedefinedVariable(en.name.clone()));
        }

        self.enums.insert(en.name.clone(), en);
        Ok(())
    }

    pub fn is_record_defined(&self, name: &str) -> bool {
        self.records.contains_key(name)
    }

    pub fn lookup_record(&self, name: &str) -> Option<&Record> {
        self.records.get(name)
    }

    pub fn lookup_enum(&self, name: &str) -> Option<&Enum> {
        self.enums.get(name)
    }
}
