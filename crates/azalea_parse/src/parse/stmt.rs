use super::Parser;
use super::base as parser;
use super::syntax_error::SyntaxError;
use crate::ast::Block;
use crate::ast::Expr;
use crate::ast::Stmt;
use crate::ast::ast_types::Enum;
use crate::ast::ast_types::Function;
use crate::ast::ast_types::Record;
use crate::ast::ast_types::Ty;
use crate::lexer::TokenKind;
use crate::span::Span;
use crate::span::spanned;

impl<'a> Parser<'a> {
    pub(crate) fn parse_stmt(&mut self) -> parser::Return<Span<Stmt>> {
        let expected_tokens = vec![
            TokenKind::KwLet,
            TokenKind::KwRecord,
            TokenKind::KwEnum,
            TokenKind::KwFn,
            TokenKind::Name,
        ];

        let location = self.peek().map(|t| t.location).unwrap_or_default();

        match self.peek().map(|t| t.kind) {
            Some(TokenKind::KwLet) => self.parse_let(),
            Some(TokenKind::KwRecord) => self.parse_record_decl(),
            Some(TokenKind::KwEnum) => self.parse_enum_decl(),
            Some(TokenKind::KwFn) => self.parse_fn_decl(),
            Some(TokenKind::Name) => {
                let name = self.expect(TokenKind::Name)?.literal;
                if self.peek().map(|t| t.kind) == Some(TokenKind::Eq) {
                    self.parse_let()
                } else {
                    Ok(spanned(Stmt::Expr(self.parse_expr()?), location))
                }
            }
            _ => Err(SyntaxError::UnexpectedToken {
                token: self.peek().map(|t| t.kind).unwrap(),
                expected_any: expected_tokens,
                location,
            }),
        }
    }

    pub(crate) fn parse_block(&mut self) -> parser::Return<Block> {
        let location = self.peek().map(|t| t.location).unwrap_or_default();
        self.expect(TokenKind::KwDo)?;

        let mut stmts_in_block = Vec::new();

        while self.peek().map(|t| t.kind) != Some(TokenKind::KwEnd) {
            let stmt = self.parse_stmt()?;
            stmts_in_block.push(stmt);
        }

        self.expect(TokenKind::KwEnd)?;

        Ok(stmts_in_block)
    }

    fn parse_let(&mut self) -> parser::Return<Span<Stmt>> {
        // let name: ty = expr or let name = expr
        let location = self.peek().map(|t| t.location).unwrap_or_default();

        self.expect(TokenKind::KwLet)?;
        let name = self.expect(TokenKind::Name)?.literal;

        let ty = if self.peek().map(|t| t.kind) == Some(TokenKind::Colon) {
            println!("Parsing type annotation in let statement");
            self.next();
            let ty = self.parse_typename()?;
            self.expect(TokenKind::Eq)?;
            // println!("Finished parsing type annotation, about to parse expression");

            ty
        } else {
            println!("No type annotation found, using UnknownForNow");
            self.next();
            Ty::Unresolved
        };

        let value = self.parse_expr()?;

        Ok(spanned(
            Stmt::Let {
                name: name.to_string(),
                ty,
                value: Box::new(value),
            },
            location,
        ))
    }

    fn parse_record_decl(&mut self) -> parser::Return<Span<Stmt>> {
        // record name = { field: ty, ... }
        let location = self.peek().map(|t| t.location).unwrap_or_default();

        self.expect(TokenKind::KwRecord)?;
        let name = self.expect(TokenKind::Name)?.literal;

        self.expect(TokenKind::Eq)?;

        self.expect(TokenKind::LBrace)?;
        let mut record_fields = Vec::new();

        while self.peek().map(|t| t.kind) != Some(TokenKind::RBrace) {
            let typed_field = self.parse_typed_pair()?;
            record_fields.push(typed_field);

            if self.peek().map(|t| t.kind) != Some(TokenKind::Comma) {
                break;
            }
            self.next();
        }

        self.expect(TokenKind::RBrace)?;

        Ok(spanned(
            Stmt::RecordDecl(Record {
                name: name.to_string(),
                fields: record_fields,
            }),
            location,
        ))
    }

    fn parse_enum_decl(&mut self) -> parser::Return<Span<Stmt>> {
        // enum name = { variant1, variant2, ... }
        let location = self.peek().map(|t| t.location).unwrap_or_default();

        self.expect(TokenKind::KwEnum)?;
        let name = self.expect(TokenKind::Name)?.literal;

        self.expect(TokenKind::Eq)?;

        self.expect(TokenKind::LBrace)?;
        let mut enum_variants = Vec::new();

        while self.peek().map(|t| t.kind) != Some(TokenKind::RBrace) {
            let variant = self.expect(TokenKind::Name)?.literal;
            enum_variants.push(variant.to_string());

            if self.peek().map(|t| t.kind) != Some(TokenKind::Comma) {
                break;
            }
            self.next();
        }

        self.expect(TokenKind::RBrace)?;

        Ok(spanned(
            Stmt::EnumDecl(Enum {
                name: name.to_string(),
                variants: enum_variants,
            }),
            location,
        ))
    }

    fn parse_fn_decl(&mut self) -> parser::Return<Span<Stmt>> {
        // fn name(args: ty, ...) -> ty =
        let location = self.peek().map(|t| t.location).unwrap_or_default();

        self.expect(TokenKind::KwFn)?;
        let name = self.expect(TokenKind::Name)?.literal;

        self.expect(TokenKind::LParen)?;
        let args = self.parse_typed_pair_list()?;
        self.expect(TokenKind::RParen)?;

        let return_ty = if self.peek().map(|t| t.kind) == Some(TokenKind::Colon) {
            self.next();
            self.parse_typename()?
        } else {
            // If no type is specified, default to `Unit`. `Unit` is a type that represents the absence
            // of returning a value from a function.
            Ty::Unit
        };

        self.expect(TokenKind::Eq)?;
        let mut body: Vec<Span<Stmt>> = Vec::new();

        // Check if the next token is `do`, and if so parse the block. Otherwise
        // we have a single expression and need to wrap that in a `Stmt::Expr` node.
        if self.peek().map(|t| t.kind) == Some(TokenKind::KwDo) {
            let block = self.parse_block()?;
            body.extend(block);
        } else {
            // Single expressions implicitly return their value to the function
            let expr = self.parse_expr()?;

            body.push(spanned(Stmt::Expr(expr), location.clone()));
        }

        let func = Function::new_with_stmts(name.to_string(), args, return_ty, body);

        Ok(spanned(Stmt::FnDecl(func), location))
    }
}
