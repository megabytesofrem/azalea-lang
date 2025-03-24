use super::ast::Expr;

#[derive(Debug, Clone)]
pub enum Ty {
    Int,
    Float,
    String,
    Bool,
    Unit,
    User(String),
    Function(Box<Function>),
    Record(Box<Record>),
}

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
