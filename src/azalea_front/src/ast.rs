use crate::ast_types::Ty;

use super::{lexer::Op, span::Span};

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
pub enum Expr {
    Literal(Literal),
    Ident(String),
    Member(Member),

    Binop(Box<Span<Expr>>, Op, Box<Span<Expr>>),
    Unop(Op, Box<Span<Expr>>),

    Let {
        name: String,
        ty: Ty,
        value: Box<Span<Expr>>,
    },

    Call {
        target: Box<Span<Expr>>,
        args: Vec<Span<Expr>>,
    },
}
