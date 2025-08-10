use std::collections::HashMap;

use crate::ast::ast_types::{Enum, Function, Record};
use crate::resolver::error::SemanticError;

/// TypeRegistry manages the registration and lookup of record and enum types
/// during the typechecking phase.
#[derive(Debug, Clone)]
pub struct TypeRegistry {
    pub records: HashMap<String, Record>,
    pub enums: HashMap<String, Enum>,

    // Bindings to externally defined JavaScript functions
    pub js_extern_functions: HashMap<String, Function>,
}

impl TypeRegistry {
    pub fn new() -> Self {
        TypeRegistry {
            records: HashMap::new(),
            enums: HashMap::new(),
            js_extern_functions: HashMap::new(),
        }
    }

    pub fn define_record(&mut self, record: Record) {
        self.records.insert(record.name.clone(), record.clone());
    }

    pub fn define_enum(&mut self, en: Enum) {
        self.enums.insert(en.name.clone(), en);
    }

    pub fn define_extern(&mut self, func: Function) {
        self.js_extern_functions.insert(func.name.clone(), func);
    }

    pub fn is_record_defined(&self, name: &str) -> bool {
        self.records.contains_key(name)
    }

    pub fn is_enum_defined(&self, name: &str) -> bool {
        self.enums.contains_key(name)
    }

    pub fn is_extern_function(&self, name: &str) -> bool {
        self.js_extern_functions.contains_key(name)
    }

    pub fn lookup_extern_function(&self, name: &str) -> Option<&Function> {
        self.js_extern_functions.get(name)
    }

    pub fn lookup_record(&self, name: &str) -> Option<&Record> {
        self.records.get(name)
    }

    pub fn lookup_enum(&self, name: &str) -> Option<&Enum> {
        self.enums.get(name)
    }
}
