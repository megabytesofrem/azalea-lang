use super::ast_types::{Enum, Function, Record, RecordExpr, Ty};
use crate::{lexer::Op, span::Span};

/// A block of statements delimited by `do` and `end`
pub type Block = Vec<Span<Stmt>>;

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Int(i32),
    Float(f32),
    String(String),
    Bool(bool),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Member {
    pub target: Box<Span<Expr>>,
    pub name: String,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    /// An expression, used in place of a statement
    Expr(Span<Expr>),

    /// A `let` statement, *not* a binding
    Let {
        name: String,
        ty: Ty,
        value: Box<Span<Expr>>,
    },

    /// A record declaration
    RecordDecl(Record),

    EnumDecl(Enum),

    /// A function declaration
    FnDecl(Function),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Literal(Literal),
    Ident(String),
    MemberAccess(Member),

    BinOp(Box<Span<Expr>>, Op, Box<Span<Expr>>),
    UnOp(Op, Box<Span<Expr>>),

    Record(RecordExpr),

    Array {
        elements: Vec<Span<Expr>>,
    },

    ArrayIndex {
        target: Box<Span<Expr>>,
        index: Box<Span<Expr>>,
    },

    /// A function call: f(x, y)
    FnCall {
        target: Box<Span<Expr>>,
        args: Vec<Span<Expr>>,
    },

    /// A lambda expression: \(x) -> x + 1
    Lam {
        args: Vec<(String, Ty)>,
        return_ty: Ty,
        body: Box<Span<Expr>>,
    },

    If {
        cond: Box<Span<Expr>>,
        then: Box<Span<Expr>>,
        else_: Option<Box<Span<Expr>>>,
    },
}
