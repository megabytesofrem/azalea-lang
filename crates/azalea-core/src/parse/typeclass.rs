//! Parsers for typeclass declarations and instances

use super::Parser;
use crate::ast::Stmt;
use crate::ast::ast_types::{Function, Ty};
use crate::lexer::{SourceLoc, TokenKind};
use crate::parse::base as parser;
use crate::parse::span::{Span, spanned};

impl<'a> Parser<'a> {
    pub(crate) fn parse_signature(&mut self) -> parser::Return<Span<Function>> {
        // fn name(args: ty, ...): ty

        let location = self.peek().map(|t| t.location).unwrap_or_default();
        let name = self.expect(TokenKind::Name)?.literal;

        self.expect(TokenKind::LParen)?;
        let args = self.parse_typed_pair_list()?;
        self.expect(TokenKind::RParen)?;

        let return_ty = if self.peek().map(|t| t.kind) == Some(TokenKind::Colon) {
            self.next();
            self.parse_typename()?
        } else {
            // If no type is specified, default to `Unit`
            Ty::Unit
        };

        let func = Function::new_with_empty(name.to_string(), args, return_ty);
        Ok(spanned(func, location))
    }

    pub(crate) fn parse_type_params(&mut self) -> parser::Return<Vec<String>> {
        let mut type_params = Vec::new();
        self.expect(TokenKind::LSquare)?;

        loop {
            match self.peek() {
                None => break,
                Some(token) if token.kind == TokenKind::RSquare => break,
                _ => {
                    let name = self.expect(TokenKind::Name)?.literal.to_string();
                    type_params.push(name);

                    // Expect a comma if there are more type parameters
                    if self.peek().map(|t| t.kind) == Some(TokenKind::Comma) {
                        self.next(); // consume the comma
                    }
                }
            }
        }

        self.expect(TokenKind::RSquare)?;

        Ok(type_params)
    }

    pub(crate) fn parse_typeclass_decl(&mut self) -> parser::Return<Span<Stmt>> {
        // class Eq[A] where
        //   fn equals(self: A, other: A): Bool
        // end

        let location = self.peek().map(|t| t.location).unwrap_or_default();
        self.expect(TokenKind::KwClass)?;
        let name = self.expect(TokenKind::Name)?.literal;

        let type_params = if self.peek().map(|t| t.kind) == Some(TokenKind::LSquare) {
            self.parse_type_params()?
        } else {
            Vec::new()
        };

        self.expect(TokenKind::KwWhere)?;

        let mut methods = Vec::new();
        while self.peek().map(|t| t.kind) != Some(TokenKind::KwEnd) {
            let method = self.parse_signature()?;
            methods.push(method);
        }
        self.expect(TokenKind::KwEnd)?;

        todo!()
    }

    pub(crate) fn parse_typeclass_instance(&mut self) -> parser::Return<Span<Stmt>> {
        // impl Eq: Int where
        //   fn equals(self: Int, other: Int): Bool
        // end

        let location = self.peek().map(|t| t.location).unwrap_or_default();
        self.expect(TokenKind::KwImpl)?;

        let class_name = self.expect(TokenKind::Name)?.literal;
        self.expect(TokenKind::Colon)?;
        let type_name = self.expect(TokenKind::Name)?.literal;

        self.expect(TokenKind::KwWhere)?;

        let mut methods = Vec::new();
        while self.peek().map(|t| t.kind) != Some(TokenKind::KwEnd) {
            let method = self.parse_signature()?;
            methods.push(method);
        }

        self.expect(TokenKind::KwEnd)?;

        todo!()
    }
}
