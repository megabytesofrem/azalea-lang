//! Parsers for type names and related constructs

use std::collections::HashSet;

use super::Parser;
use crate::ast::ast_types::{Function, Ty, TypedPair};
use crate::lexer::{Token, TokenKind};
use crate::parse::error::ParserError;
use crate::parse::{base as parser, type_};

impl<'a> Parser<'a> {
    pub(crate) fn parse_typename(&mut self) -> parser::Return<Ty> {
        self.parse_primary_type(&HashSet::new())
    }

    pub fn parse_primary_type(&mut self, type_params: &HashSet<String>) -> parser::Return<Ty> {
        let token = self.next().ok_or(ParserError::UnexpectedEOF)?;

        match token.kind {
            TokenKind::Name => self.parse_name_or_type_with_ctx(token, type_params),
            TokenKind::KwFn => self.parse_fn_type(type_params),
            t => panic!("Unexpected token for type name: {:?}", t),
        }
    }

    fn parse_name_or_type_with_ctx(
        &mut self,
        token: Token,
        type_params: &HashSet<String>,
    ) -> parser::Return<Ty> {
        // Handle other types like Array or custom types
        if token.kind == TokenKind::Name {
            let base_name = token.literal.to_string();

            // Check if the base name is a built-in type
            match base_name.as_str() {
                "Int" => Ok(Ty::Int),
                "Float" => Ok(Ty::Float),
                "String" => Ok(Ty::String),
                "Bool" => Ok(Ty::Bool),
                "Unit" => Ok(Ty::Unit),
                "Any" => Ok(Ty::Any),

                // User defined types or arrays
                _ => {
                    if type_params.contains(&base_name) {
                        // Type variable
                        return Ok(Ty::Var(base_name));
                    }

                    if self.peek().map(|t| t.kind) == Some(TokenKind::LSquare) {
                        // Array type
                        self.next(); // consume the LSquare
                        self.parse_array_type_with_ctx(base_name, type_params)
                    } else {
                        // User-defined type
                        Ok(Ty::TypeCons(base_name, vec![]))
                    }
                }
            }
        } else {
            Err(ParserError::ExpectedType(token.location))
        }
    }

    fn parse_array_type_with_ctx(
        &mut self,
        base_name: String,
        type_params: &HashSet<String>,
    ) -> parser::Return<Ty> {
        // Array[int]
        let mut type_args = Vec::new();

        loop {
            type_args.push(self.parse_primary_type(type_params)?);
            if self.peek().map(|t| t.kind) == Some(TokenKind::Comma) {
                self.next(); // consume the comma
            } else {
                break;
            }
        }

        self.expect(TokenKind::RSquare)?;
        Ok(Ty::TypeCons(base_name, type_args))
    }

    fn parse_fn_type(&mut self, type_params: &HashSet<String>) -> parser::Return<Ty> {
        // fn (int, int) -> int
        self.expect(TokenKind::LParen)?;
        let types = self.parse_typename_list(type_params)?;
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

    pub(crate) fn parse_typename_list(
        &mut self,
        typed_params: &HashSet<String>,
    ) -> parser::Return<Vec<Ty>> {
        let mut types = Vec::new();

        loop {
            match self.peek() {
                None => break,
                Some(token) if token.kind == TokenKind::RParen => break,

                _ => {
                    let ty = self.parse_primary_type(typed_params)?;
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
