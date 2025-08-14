use crate::{ast::ast_types::Ty, error::error_report::ErrorReport, lexer::SourceLoc};

#[derive(Debug, Clone)]
pub enum SemanticError {
    UnificationError {
        message: String,
        location: SourceLoc,
    },

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

    UndefinedFieldOrVariant {
        field: String,
        structure: String,
        location: SourceLoc,
    },

    ArityMismatch {
        expected: usize,
        found: usize,
        location: SourceLoc,
    },

    InvalidAssignment {
        location: SourceLoc,
    },
    InvalidTarget {
        location: SourceLoc,
    },
    NoMatch,
}

impl SemanticError {
    pub fn report(&self) -> ErrorReport {
        match self {
            SemanticError::UnificationError { message, location } => ErrorReport::new(
                format!("Unification error: {}", message),
                "Type error".to_string(),
                location.clone(),
            ),

            SemanticError::UndefinedVariable(name) => ErrorReport::new(
                format!("Undefined variable: {}", name),
                "Semantic error".to_string(),
                SourceLoc::default(),
            ),
            SemanticError::RedefinedVariable(name) => ErrorReport::new(
                format!("Redefined variable: {}", name),
                "Semantic error".to_string(),
                SourceLoc::default(),
            ),
            SemanticError::UndefinedFunction(name) => ErrorReport::new(
                format!("Undefined function: {}", name),
                "Semantic error".to_string(),
                SourceLoc::default(),
            ),
            SemanticError::EmptyScope => ErrorReport::new(
                "Attempted to pop an empty scope".to_string(),
                "Semantic error".to_string(),
                SourceLoc::default(),
            ),

            SemanticError::OccursCheck { location } => ErrorReport::new(
                "Occurs check failed".to_string(),
                "Type error".to_string(),
                location.clone(),
            )
            .with_hint("Check for infinite types or circular type references".to_string()),

            SemanticError::TypeMismatch {
                expected,
                found,
                location,
            } => ErrorReport::new(
                format!("Type mismatch: expected {:?}, found {:?}", expected, found),
                "Type error".to_string(),
                location.clone(),
            ),

            SemanticError::ExpectedLambda { location } => ErrorReport::new(
                "Expected a lambda expression".to_string(),
                "Semantic error".to_string(),
                location.clone(),
            )
            .with_hint("Expected a lambda in the format of: \\(params) -> expr".to_string()),

            SemanticError::UndefinedFieldOrVariant {
                field,
                structure,
                location,
            } => ErrorReport::new(
                format!("Undefined field or variant: {} in {}", field, structure),
                "Semantic error".to_string(),
                location.clone(),
            ),

            SemanticError::ArityMismatch {
                expected,
                found,
                location,
            } => ErrorReport::new(
                format!("Arity mismatch: expected {}, found {}", expected, found),
                "Semantic error".to_string(),
                location.clone(),
            ),

            SemanticError::InvalidAssignment { location } => ErrorReport::new(
                "Invalid assignment target".to_string(),
                "Semantic error".to_string(),
                location.clone(),
            )
            .with_hint("Assignment target must be a variable or member access".to_string()),

            SemanticError::InvalidTarget { location } => ErrorReport::new(
                "Invalid target".to_string(),
                "Semantic error".to_string(),
                location.clone(),
            ),

            SemanticError::NoMatch => ErrorReport::new(
                "No matching branch found".to_string(),
                "Semantic error".to_string(),
                SourceLoc::default(),
            ),
        }
    }
}
