use crate::{ast::ast_types::Ty, lexer::SourceLoc};

#[derive(Debug, Clone)]
pub enum SemanticError {
    UnificationError(String),

    UndefinedVariable(String),
    RedefinedVariable(String),
    UndefinedFunction(String),
    EmptyScope,

    OccursCheck {
        location: SourceLoc,
    },
    TypeMismatch {
        expected: Ty,
        found: Ty,
        location: SourceLoc,
    },

    ExpectedLambda {
        location: SourceLoc,
    },
}
