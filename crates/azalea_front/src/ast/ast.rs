use super::ast_types::Ty;
use crate::{lexer::Op, span::Span};

pub type Block = Vec<Span<Stmt>>;

#[derive(Debug, Clone)]
pub enum Literal {
    Int(i32),
    Float(f32),
    String(String),
    Bool(bool),
}

#[derive(Debug, Clone)]
pub struct Member {
    pub target: Box<Span<Expr>>,
    pub ty: Ty,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    /// An expression, used in place of a statement
    Expr(Span<Expr>),

    /// A record declaration
    RecordDecl {
        name: String,
        fields: Vec<(String, Ty)>,
    },

    EnumDecl {
        name: String,
        variants: Vec<(String, Option<Ty>)>,
    },

    /// A function declaration
    FnDecl {
        name: String,
        args: Vec<(String, Ty)>,
        return_ty: Ty,
        body: Box<Block>,
    },

    /// A `let` statement, not to be confused with a binding
    Let {
        name: String,
        ty: Ty,
        value: Box<Span<Expr>>,
    },
}

#[derive(Debug, Clone)]
pub enum Expr {
    Literal(Literal),
    Ident(String),
    Member(Member),

    Binop(Box<Span<Expr>>, Op, Box<Span<Expr>>),
    Unop(Op, Box<Span<Expr>>),

    /// A function call
    FnCall {
        target: Box<Span<Expr>>,
        args: Vec<Span<Expr>>,
    },

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
