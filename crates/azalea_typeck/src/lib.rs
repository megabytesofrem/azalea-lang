use azalea_resolver::semantic_error::SemanticError;

pub mod type_ops;
pub mod typecheck;
mod typecheck_tests;

pub type Return<'a, T> = Result<T, SemanticError>;
