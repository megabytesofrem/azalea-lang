use azalea_parse::{ast::ast_types::Ty, lexer::SourceLoc};
use thiserror::Error;

#[derive(Error, Debug, Clone)]
pub enum SemanticError {
    #[error("Unification error: {0}")]
    UnificationError(String),

    #[error("Undefined variable: {0}")]
    UndefinedVariable(String),

    #[error("Redefined variable: {0}")]
    RedefinedVariable(String),

    #[error("Cannot pop from empty scope stack")]
    EmptyScope,

    #[error("{location}: Occurs check (infinite type expansion)")]
    OccursCheck { location: SourceLoc },

    #[error("{location}: Type mismatch: expected {expected:?}, found {found:?}")]
    TypeMismatch {
        expected: Ty,
        found: Ty,
        location: SourceLoc,
    },

    #[error("{location}: Expected a lambda expression")]
    ExpectedLambda { location: SourceLoc },
}
