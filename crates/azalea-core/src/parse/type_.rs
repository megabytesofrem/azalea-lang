//! Parsers for type names and related constructs

use super::Parser;
use crate::ast::ast_types::{Function, Ty, TypedPair};
use crate::lexer::{Token, TokenKind};
use crate::parse::base as parser;
use crate::parse::error::ParserError;

impl<'a> Parser<'a> {
    pub(crate) fn parse_typename(&mut self) -> parser::Return<Ty> {
        let token = self.next().ok_or(ParserError::UnexpectedEOF)?;

        let reserved_types = ["Int", "Float", "String", "Bool", "Unit", "Array", "Fn"];

        match token.kind {
            // Reserved keywords for built-in, primitive types
            TokenKind::KwType(ref type_name) => match type_name.as_str() {
                "Int" => Ok(Ty::Int),
                "Float" => Ok(Ty::Float),
                "String" => Ok(Ty::String),
                "Bool" => Ok(Ty::Bool),
                "Unit" => Ok(Ty::Unit),
                "Fn" => self.parse_fn_type(),
                _ => self.parse_other_type(token),
            },

            // User defined types or built-in types
            TokenKind::Name => self.parse_other_type(token),

            t => panic!("Unexpected token for type name: {:?}", t),
        }
    }

    fn parse_other_type(&mut self, token: Token) -> parser::Return<Ty> {
        // Handle other types like Array or custom types
        if token.kind == TokenKind::Name {
            let base_name = token.literal.to_string();
            if self.peek().map(|t| t.kind) == Some(TokenKind::LSquare) {
                // Array type
                self.next(); // consume the LSquare
                self.parse_array_type(base_name)
            } else {
                // User-defined type
                Ok(Ty::TypeCons(base_name, vec![]))
            }
        } else {
            Err(ParserError::ExpectedType(token.location))
        }
    }

    fn parse_array_type(&mut self, base_name: String) -> parser::Return<Ty> {
        // Array[int]
        let mut type_args = Vec::new();

        loop {
            type_args.push(self.parse_typename()?);
            if self.peek().map(|t| t.kind) == Some(TokenKind::Comma) {
                self.next(); // consume the comma
            } else {
                break;
            }
        }

        self.expect(TokenKind::RSquare)?;
        Ok(Ty::TypeCons(base_name, type_args))
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
