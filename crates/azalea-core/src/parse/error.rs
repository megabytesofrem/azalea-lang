use crate::{
    ast::Expr,
    error::error_report::ErrorReport,
    lexer::{LexicalError, SourceLoc, TokenKind},
};

#[derive(Debug, Clone)]
pub enum ParserError {
    UnexpectedToken {
        token: TokenKind,
        location: SourceLoc,
    },

    /// Expected an expression
    ExpectedExpr(SourceLoc),

    /// Expected a statement
    ExpectedStmt(SourceLoc),

    /// Expected a type (e.g., in a variable declaration)
    ExpectedType(SourceLoc),

    MissingClosingParen(SourceLoc),
    MissingClosingSquareBracket(SourceLoc),
    MissingClosingBrace(SourceLoc),
    InvalidTokenAfterDot(SourceLoc),
    InvalidArrayIndex(SourceLoc),
    InvalidFunctionCall(SourceLoc),
    InvalidRecordFormat(SourceLoc),

    InvalidAssignment {
        target: Expr,
        location: SourceLoc,
    },

    /// A lexical error that occurred during tokenization
    LexicalError(LexicalError),

    UnexpectedEOF,
    InvalidPattern,
}

impl ParserError {
    pub fn report(&self) -> ErrorReport {
        match self {
            ParserError::UnexpectedToken { token, location } => ErrorReport::new(
                format!("Unexpected token: {:?}", token),
                "Syntax error".to_string(),
                location.clone(),
            ),

            ParserError::ExpectedExpr(location) => ErrorReport::new(
                "Expected an expression".to_string(),
                "Syntax error".to_string(),
                location.clone(),
            ),

            ParserError::ExpectedStmt(location) => ErrorReport::new(
                "Expected a statement".to_string(),
                "Syntax error".to_string(),
                location.clone(),
            ),

            ParserError::ExpectedType(location) => ErrorReport::new(
                "Expected a type".to_string(),
                "Syntax error".to_string(),
                location.clone(),
            )
            .with_hint("Did you forget to specify a type?".to_string()),

            ParserError::MissingClosingParen(location) => ErrorReport::new(
                "Missing closing parenthesis `)`".to_string(),
                "Syntax error".to_string(),
                location.clone(),
            ),

            ParserError::MissingClosingSquareBracket(location) => ErrorReport::new(
                "Missing closing square bracket `]`".to_string(),
                "Syntax error".to_string(),
                location.clone(),
            ),

            ParserError::MissingClosingBrace(location) => ErrorReport::new(
                "Missing closing brace `}`".to_string(),
                "Syntax error".to_string(),
                location.clone(),
            ),

            ParserError::InvalidTokenAfterDot(location) => ErrorReport::new(
                "Invalid token after `.`".to_string(),
                "Syntax error".to_string(),
                location.clone(),
            ),

            ParserError::InvalidArrayIndex(location) => ErrorReport::new(
                "Invalid array index".to_string(),
                "Syntax error".to_string(),
                location.clone(),
            ),

            ParserError::InvalidFunctionCall(location) => ErrorReport::new(
                "Invalid function call".to_string(),
                "Syntax error".to_string(),
                location.clone(),
            ),

            ParserError::InvalidRecordFormat(location) => ErrorReport::new(
                "Invalid record format".to_string(),
                "Syntax error".to_string(),
                location.clone(),
            ),

            ParserError::InvalidAssignment { target, location } => {
                let message = format!("Invalid assignment target: {:?}", target);
                ErrorReport::new(message, "Syntax error".to_string(), location.clone())
                    .with_hint("Assignment targets must be variables or properties".to_string())
            }

            ParserError::LexicalError(lex_error) => {
                let (message, source, location, hint) = match lex_error {
                    LexicalError::UnterminatedString { location, source } => (
                        "Unterminated string literal".to_string(),
                        source,
                        location.clone(),
                        Some("Add a closing quote `\"` to terminate the string".to_string()),
                    ),
                    LexicalError::InvalidHexDigit {
                        location,
                        found,
                        source,
                    } => (
                        format!("Invalid hexadecimal digit: '{}'", found),
                        source,
                        location.clone(),
                        Some("Hexadecimal digits must be 0-9, a-f, or A-F".to_string()),
                    ),
                    LexicalError::InvalidIntegerFormat {
                        location,
                        literal,
                        source,
                    } => (
                        format!("Invalid integer format: '{}'", literal),
                        source,
                        location.clone(),
                        Some("Check the integer format and radix/base".to_string()),
                    ),
                    LexicalError::InvalidFloatFormat {
                        location,
                        literal,
                        source,
                    } => (
                        format!("Invalid floating point format: '{}'", literal),
                        source,
                        location.clone(),
                        Some("Floating point numbers should be formatted like '3.14'".to_string()),
                    ),
                    LexicalError::BadEscapeSequence {
                        location,
                        sequence,
                        source,
                    } => (
                        format!("Invalid escape sequence: '{}'", sequence),
                        source,
                        location.clone(),
                        Some("Valid escape sequences are '\\n, \\t, \\r, \\\\, \\\"'".to_string()),
                    ),
                    LexicalError::Unknown => (
                        "Unknown lexical error".to_string(),
                        &"Lexical error".to_string(),
                        SourceLoc::default(),
                        None,
                    ),
                };

                let mut report = ErrorReport::new(message, source.to_string(), location);

                if let Some(hint_text) = hint {
                    report = report.with_hint(hint_text);
                }

                report
            }

            ParserError::InvalidPattern => ErrorReport::new(
                "Invalid pattern".to_string(),
                "Syntax error".to_string(),
                SourceLoc::default(),
            ),

            ParserError::UnexpectedEOF => ErrorReport::new(
                "Unexpected end of file (EOF)".to_string(),
                "Syntax error".to_string(),
                SourceLoc::default(),
            ),
        }
    }
}
