//! Miscellaneous parsers that don't fit into any other category.

use crate::ast::ast_types::{Ty, TypedPair};
use crate::lexer::{Op, TokenKind};

use super::syntax_error::SyntaxError;
use super::{Parser, base as parser};

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

impl<'a> Parser<'a> {
    pub(crate) fn parse_type(&mut self) -> parser::Return<Ty> {
        let token = self.next().ok_or(SyntaxError::UnexpectedEOF)?;

        match token.kind {
            TokenKind::Name => match token.literal {
                "Int" => Ok(Ty::Int),
                "Float" => Ok(Ty::Float),
                "String" => Ok(Ty::String),
                "Bool" => Ok(Ty::Bool),
                "Unit" => Ok(Ty::Unit),
                "Array" => {
                    self.expect(TokenKind::LSquare)?;
                    let ty = self.parse_type()?;
                    self.expect(TokenKind::RSquare)?;
                    Ok(Ty::Array(Box::new(ty)))
                }

                // User defined types
                _ => Ok(Ty::User(token.literal.to_string())),
            },

            _ => Err(SyntaxError::ExpectedType(token.location)),
        }
    }

    pub(crate) fn parse_typed_pair(&mut self) -> parser::Return<TypedPair> {
        let name = self.expect(TokenKind::Name)?.literal.to_string();
        self.expect(TokenKind::Colon)?;
        let ty = self.parse_type()?;

        Ok((name, ty))
    }

    pub(crate) fn parse_annotated_params(&mut self) -> parser::Return<Vec<TypedPair>> {
        let mut params = Vec::new();

        loop {
            match self.peek() {
                None => break,
                Some(token) if token.kind == TokenKind::RParen => break,

                _ => {
                    let param = self.parse_typed_pair()?;
                    params.push(param);

                    // Expect a comma if there are more parameters
                    if self.peek().map(|t| t.kind) != Some(TokenKind::RParen) {
                        break;
                    }

                    self.expect(TokenKind::Comma)?;
                }
            }
        }

        Ok(params)
    }
}
