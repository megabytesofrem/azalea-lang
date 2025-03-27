use super::ast::Expr;

#[derive(Debug, Clone)]
pub enum Ty {
    Int,
    Float,
    String,
    Bool,
    Unit,

    /// Represents a type that we don't know yet, will be resolved later on
    UnknownForNow,
    User(String),
    Array(Box<Ty>),

    /// Allows us to pass functions around as values
    Fn(Box<Function>),

    /// Record and enum types
    Record(Box<Record>),
    Enum(Box<Enum>),
}

/// A pair of a name and a type
pub type TypedPair = (String, Ty);

/// Records are semantically equivalent to JavaScript objects
#[derive(Debug, Clone)]
pub struct Record {
    pub name: String,
    pub fields: Vec<(String, Ty)>,
}

#[derive(Debug, Clone)]
pub struct Enum {
    pub name: String,
    pub variants: Vec<(String, Option<Ty>)>,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub args: Vec<(String, Ty)>,
    pub return_ty: Ty,

    pub body: Vec<Expr>,
}
