use std::fmt;

use super::ast_types::{Enum, Function, Record, RecordExpr, Ty};
use crate::{lexer::Op, parse::span::Span};

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

    For {
        name: String,
        iterable: Box<Span<Expr>>,
        body: Vec<Span<Stmt>>,
    },

    While {
        cond: Box<Span<Expr>>,
        body: Vec<Span<Stmt>>,
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
        then: Vec<Span<Stmt>>,
        else_: Option<Vec<Span<Stmt>>>,
    },
}

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Op::Neg => write!(f, "-"),
            Op::Add => write!(f, "+"),
            Op::Sub => write!(f, "-"),
            Op::Mul => write!(f, "*"),
            Op::Div => write!(f, "/"),
            Op::DoubleEq => write!(f, "=="),
            Op::Not => write!(f, "!"),
            Op::NotEq => write!(f, "!="),
            Op::Less => write!(f, "<"),
            Op::Greater => write!(f, ">"),
            Op::LessEq => write!(f, "<="),
            Op::GreaterEq => write!(f, ">="),
        }
    }
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Literal::Int(i) => write!(f, "{}", i),
            Literal::Float(fl) => write!(f, "{}", fl),
            Literal::String(s) => write!(f, "{}", s),
            Literal::Bool(b) => write!(f, "{}", b),
        }
    }
}

impl fmt::Display for RecordExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let fields: Vec<String> = self
            .fields
            .iter()
            .map(|(name, expr)| format!("{}: {}", name, expr))
            .collect();
        write!(f, "{} {{ {} }}", self.name, fields.join(", "))
    }
}

impl fmt::Display for Member {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}.{}", self.target.target, self.name)
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Literal(lit) => write!(f, "{}", lit),
            Expr::Ident(ident) => write!(f, "{}", ident),
            Expr::MemberAccess(member) => write!(f, "{}", member),
            Expr::BinOp(left, op, right) => {
                write!(f, "({} {} {})", left.target, op, right.target)
            }
            Expr::UnOp(op, expr) => write!(f, "{}({})", op, expr.target),
            Expr::Record(record) => write!(f, "{}", record),
            Expr::Array { elements } => {
                let elems: Vec<String> = elements.iter().map(|e| e.target.to_string()).collect();
                write!(f, "[{}]", elems.join(", "))
            }
            Expr::ArrayIndex { target, index } => write!(f, "{}[{}]", target.target, index.target),
            Expr::FnCall { target, args } => {
                let args_str: Vec<String> = args.iter().map(|a| a.target.to_string()).collect();
                write!(f, "{}({})", target.target, args_str.join(", "))
            }

            Expr::Lam {
                args,
                return_ty,
                body,
            } => {
                let args_str: Vec<String> = args.iter().map(|(n, _)| n.clone()).collect();
                write!(
                    f,
                    "\\({}) -> {} : {}",
                    args_str.join(", "),
                    body.target,
                    return_ty
                )
            }

            Expr::If { cond, then, else_ } => {
                let else_str = if let Some(else_body) = else_ {
                    format!(
                        " else {}",
                        else_body
                            .iter()
                            .map(|s| "<stmt>".to_string()) // Placeholder for actual statement formatting
                            .collect::<Vec<_>>()
                            .join("; ")
                    )
                } else {
                    String::new()
                };
                write!(
                    f,
                    "if {} then do\n{}\nend {}",
                    cond.target,
                    then.iter()
                        .map(|s| "<stmt>".to_string()) // Placeholder for actual statement formatting
                        .collect::<Vec<_>>()
                        .join("; "),
                    else_str
                )
            }
        }
    }
}
