//! Tiny, miscellaneous parsers that don't fit into any other category.

use crate::ast::ast_types::{Function, Ty, TypedPair};
use crate::lexer::{Op, TokenKind};

use super::error::ParserError;
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
    pub(crate) fn parse_typename(&mut self) -> parser::Return<Ty> {
        let token = self.next().ok_or(ParserError::UnexpectedEOF)?;

        match token.kind {
            TokenKind::KwType(ref type_name) => match type_name.as_str() {
                "Int" => Ok(Ty::Int),
                "Float" => Ok(Ty::Float),
                "String" => Ok(Ty::String),
                "Bool" => Ok(Ty::Bool),
                "Unit" => Ok(Ty::Unit),
                _ => Err(ParserError::ExpectedType(token.location)),
            },
            TokenKind::Name => match token.literal {
                "int" => Ok(Ty::Int),
                "float" => Ok(Ty::Float),
                "string" => Ok(Ty::String),
                "bool" => Ok(Ty::Bool),
                "unit" => Ok(Ty::Unit),
                "array" => self.parse_array_type(),
                "fn" => self.parse_fn_type(),

                // User defined types
                // TODO: Handle generic instantiation
                _ => Ok(Ty::TypeCons(token.literal.to_string(), Vec::new())),
            },

            _ => Err(ParserError::ExpectedType(token.location)),
        }
    }

    fn parse_array_type(&mut self) -> parser::Return<Ty> {
        // [int]
        self.check(TokenKind::LSquare)?;
        let ty = self.parse_typename()?;
        self.expect(TokenKind::RSquare)?;

        Ok(Ty::Array(Box::new(ty)))
    }

    fn parse_fn_type(&mut self) -> parser::Return<Ty> {
        // fn (int, int) -> int
        self.check(TokenKind::KwFn)?;

        self.expect(TokenKind::LParen)?;
        let types = self.parse_typename_list()?;
        self.expect(TokenKind::RParen)?;
        self.expect(TokenKind::Arrow)?;
        let return_ty = self.parse_typename()?;

        // TODO: Should we generate unique type names for fn types?
        let args = types.into_iter().map(|ty| ("".to_string(), ty)).collect();
        let func = Function::new_with_empty("lambda".to_string(), args, return_ty);

        Ok(Ty::Fn(Box::new(func)))
    }

    pub(crate) fn parse_typed_pair(&mut self) -> parser::Return<TypedPair> {
        let name = self.expect(TokenKind::Name)?.literal.to_string();
        self.expect(TokenKind::Colon)?;
        let ty = self.parse_typename()?;

        Ok((name, ty))
    }

    pub(crate) fn parse_typename_list(&mut self) -> parser::Return<Vec<Ty>> {
        let mut types = Vec::new();

        loop {
            match self.peek() {
                None => break,
                Some(token) if token.kind == TokenKind::RParen => break,

                _ => {
                    let ty = self.parse_typename()?;
                    types.push(ty);

                    // Expect a comma if there are more types
                    if self.peek().map(|t| t.kind) != Some(TokenKind::RParen) {
                        self.expect(TokenKind::Comma)?;
                    }
                }
            }
        }

        Ok(types)
    }

    pub(crate) fn parse_typed_pair_list(&mut self) -> parser::Return<Vec<TypedPair>> {
        let mut params = Vec::new();

        loop {
            match self.peek() {
                None => break,
                Some(token) if token.kind == TokenKind::RParen => break,

                _ => {
                    let param = self.parse_typed_pair()?;
                    params.push(param);

                    // Check if there are more parameters
                    if self.peek().map(|t| t.kind) == Some(TokenKind::Comma) {
                        self.next(); // consume the comma
                    } else {
                        // No comma means we're done with parameters
                        break;
                    }
                }
            }
        }

        Ok(params)
    }
}
