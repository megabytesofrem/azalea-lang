use thiserror::Error;

use crate::lexer::{SourceLoc, TokenKind};

#[derive(Error, Debug, Clone)]
pub enum SyntaxError {
    #[error("{location}: Expected {expected_any:?}, but got {token:?}")]
    UnexpectedToken {
        token: TokenKind,
        expected_any: Vec<TokenKind>,
        location: SourceLoc,
    },

    #[error("Unexpected end of file")]
    UnexpectedEOF,

    #[error("{0}: Expected expression")]
    ExpectedExpr(SourceLoc),

    #[error("{0}: Expected statement")]
    ExpectedStmt(SourceLoc),

    #[error("{0}: Expected type")]
    ExpectedType(SourceLoc),
}
