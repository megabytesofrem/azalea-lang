//! Tiny, miscellaneous parsers that don't fit into any other category.

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
        )
    }

    pub(crate) fn is_unary_operator(&self) -> bool {
        matches!(self, TokenKind::Minus)
    }
}
