//! Operator conversions

use crate::lexer::{Op, TokenKind};

impl TokenKind {
    pub fn to_operator(&self) -> Op {
        match self {
            TokenKind::Plus => Op::Add,
            TokenKind::Minus => Op::Sub,
            TokenKind::Star => Op::Mul,
            TokenKind::Slash => Op::Div,
            TokenKind::DoubleEq => Op::DoubleEq,
            TokenKind::NotEq => Op::NotEq,
            TokenKind::Less => Op::Less,
            TokenKind::LessEq => Op::LessEq,
            TokenKind::Greater => Op::Greater,
            TokenKind::GreaterEq => Op::GreaterEq,
            TokenKind::Dollar => Op::Dollar,
            TokenKind::Underscore => Op::Wildcard,
            _ => panic!("TokenKind::to_operator called on non-operator token"),
        }
    }

    pub(crate) fn is_binary_operator(&self) -> bool {
        matches!(
            self,
            TokenKind::Plus
                | TokenKind::Minus
                | TokenKind::Star
                | TokenKind::Slash
                | TokenKind::DoubleEq
                | TokenKind::NotEq
                | TokenKind::Less
                | TokenKind::LessEq
                | TokenKind::Greater
                | TokenKind::GreaterEq
                | TokenKind::Dollar
        )
    }

    pub(crate) fn is_unary_operator(&self) -> bool {
        matches!(self, TokenKind::Minus)
    }
}

impl Op {
    pub fn is_comparison(&self) -> bool {
        matches!(
            self,
            Op::DoubleEq | Op::NotEq | Op::Less | Op::Greater | Op::LessEq | Op::GreaterEq
        )
    }
}
