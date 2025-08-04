use crate::{
    error::error_report::ErrorReport,
    lexer::{SourceLoc, TokenKind},
};

#[derive(Debug, Clone)]
pub enum SyntaxError {
    UnexpectedToken {
        token: TokenKind,
        expected_any: Vec<TokenKind>,
        location: SourceLoc,
    },

    ExpectedExpr(SourceLoc),
    ExpectedStmt(SourceLoc),
    ExpectedType(SourceLoc),

    MissingClosingParen(SourceLoc),
    MissingClosingSquareBracket(SourceLoc),
    MissingClosingBrace(SourceLoc),
    InvalidTokenAfterDot(SourceLoc),
    InvalidArrayIndex(SourceLoc),
    InvalidFunctionCall(SourceLoc),
    InvalidRecordFormat(SourceLoc),

    UnexpectedEOF,
}

impl SyntaxError {
    pub fn report(&self) -> ErrorReport {
        match self {
            SyntaxError::UnexpectedToken {
                token,
                expected_any,
                location,
            } => ErrorReport::new(
                format!(
                    "Unexpected token: {:?}, expected one of: {:?}",
                    token, expected_any
                ),
                "Syntax error".to_string(),
                location.clone(),
            ),

            SyntaxError::ExpectedExpr(location) => ErrorReport::new(
                "Expected an expression".to_string(),
                "Syntax error".to_string(),
                location.clone(),
            ),

            SyntaxError::ExpectedStmt(location) => ErrorReport::new(
                "Expected a statement".to_string(),
                "Syntax error".to_string(),
                location.clone(),
            ),

            SyntaxError::ExpectedType(location) => ErrorReport::new(
                "Expected a type".to_string(),
                "Syntax error".to_string(),
                location.clone(),
            ),

            SyntaxError::MissingClosingParen(location) => ErrorReport::new(
                "Missing closing parenthesis `)`".to_string(),
                "Syntax error".to_string(),
                location.clone(),
            ),

            SyntaxError::MissingClosingSquareBracket(location) => ErrorReport::new(
                "Missing closing square bracket `]`".to_string(),
                "Syntax error".to_string(),
                location.clone(),
            ),

            SyntaxError::MissingClosingBrace(location) => ErrorReport::new(
                "Missing closing brace `}`".to_string(),
                "Syntax error".to_string(),
                location.clone(),
            ),

            SyntaxError::InvalidTokenAfterDot(location) => ErrorReport::new(
                "Invalid token after `.`".to_string(),
                "Syntax error".to_string(),
                location.clone(),
            ),

            SyntaxError::InvalidArrayIndex(location) => ErrorReport::new(
                "Invalid array index".to_string(),
                "Syntax error".to_string(),
                location.clone(),
            ),

            SyntaxError::InvalidFunctionCall(location) => ErrorReport::new(
                "Invalid function call".to_string(),
                "Syntax error".to_string(),
                location.clone(),
            ),

            SyntaxError::InvalidRecordFormat(location) => ErrorReport::new(
                "Invalid record format".to_string(),
                "Syntax error".to_string(),
                location.clone(),
            ),

            SyntaxError::UnexpectedEOF => ErrorReport::new(
                "Unexpected end of file (EOF)".to_string(),
                "Syntax error".to_string(),
                SourceLoc::default(),
            ),
        }
    }
}
