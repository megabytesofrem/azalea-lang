use std::fmt;

use super::ast_types::{Enum, Function, Record, RecordExpr, Ty};
use crate::{lexer::Op, parse::span::Span};

/// A block of statements delimited by `do` and `end`
pub type Block = Vec<Span<Stmt>>;

/// The root of the AST where the parser should start parsing from.
pub type ParseRoot = Vec<Span<ToplevelStmt>>;

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
pub enum ToplevelStmt {
    Stmt(Span<Stmt>),

    /// A record declaration
    RecordDecl(Record),

    /// An enum declaration
    EnumDecl(Enum),

    /// An `extern` declaration for a JavaScript function
    /// This is used to bind JavaScript functions to Azalea functions.
    ExternDecl(Function),

    FnDecl(Function),
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

    Mut {
        name: String,
        ty: Ty,
        value: Box<Span<Expr>>,
    },

    /// An assignment statement: name = expr
    Assign {
        target: Box<Span<Expr>>,
        value: Box<Span<Expr>>,
    },

    For {
        target: Box<Span<Expr>>,
        iterable: Box<Span<Expr>>,
        body: Vec<Span<Stmt>>,
    },

    While {
        cond: Box<Span<Expr>>,
        body: Vec<Span<Stmt>>,
    },
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
        then: Vec<Span<Stmt>>,
        else_: Option<Vec<Span<Stmt>>>,
    },
}
