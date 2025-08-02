use crate::semantic_error::SemanticError;

pub mod resolver;
pub mod semantic_error;

#[cfg(test)]
mod resolver_tests;

pub type Return<'a, T> = Result<T, SemanticError>;
