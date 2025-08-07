use crate::resolver::error::SemanticError;

pub mod error;
pub mod resolver;

#[allow(unused_imports)]
pub mod tests;

pub type Return<'a, T> = Result<T, SemanticError>;
