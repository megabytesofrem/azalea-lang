use crate::resolver::semantic_error::SemanticError;

pub mod resolver;
pub mod semantic_error;

pub mod resolver_tests;

pub type Return<'a, T> = Result<T, SemanticError>;
