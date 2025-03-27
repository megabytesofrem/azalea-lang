//! Expression parsing functions for Azalea

use super::syntax_error::SyntaxError;
use super::{Assoc, Parser};
use crate::ast::ast_types::Ty;
use crate::lexer::{Token, TokenKind};
use crate::parse::base as parser;
use crate::span::{Span, spanned};

use crate::ast::{Expr, Literal, Member};

impl<'a> Parser<'a> {
    // parse_expr_prec and parse_expr are helper functions for parsing expressions with precedence, while
    // parse_main_expr is for parsing the actual expressions themselves
    fn parse_expr_prec(&mut self, min_prec: u8) -> parser::Return<Span<Expr>> {
        let location = self.peek().map(|t| t.location).unwrap_or_default();
        let mut lhs = self.parse_main_expr()?;

        while let Some(op) = self.peek().clone() {
            let op = op.kind.clone().to_operator();
            let (prec, assoc) = self.get_precedence(op);

            // If the operator has a lower precedence than min_prec, break
            if prec < min_prec {
                break;
            }

            self.next();
            let rhs = self.parse_expr_prec(if assoc == Assoc::Left { prec + 1 } else { prec })?;

            lhs = spanned(
                Expr::Binop(Box::new(lhs), op, Box::new(rhs)),
                location.clone(),
            );
        }

        Ok(lhs)
    }

    fn parse_expr(&mut self) -> parser::Return<Span<Expr>> {
        self.parse_expr_prec(0)
    }

    fn parse_main_expr(&mut self) -> parser::Return<Span<Expr>> {
        let location = self.peek().map(|t| t.location).unwrap_or_default();
        let token = self
            .peek()
            .ok_or(SyntaxError::ExpectedExpr(location.clone()))?;

        match &token.kind {
            TokenKind::Minus => {
                let expr = self.parse_expr()?;
                Ok(spanned(
                    Expr::Unop(token.kind.to_operator(), Box::new(expr)),
                    location,
                ))
            }
            TokenKind::LSquare => self.parse_array(),
            TokenKind::LParen => {
                // Subexpressions
                let expr = self.parse_expr()?;
                self.expect(TokenKind::RParen)?;

                Ok(expr)
            }

            // Primary expressions
            TokenKind::IntLit
            | TokenKind::HexIntLit
            | TokenKind::FloatLit
            | TokenKind::StringLit => {
                let expr = self.parse_literal(&token)?;
                Ok(spanned(Expr::Literal(expr), location))
            }

            TokenKind::Name => {
                let ident = spanned(Expr::Ident(token.literal.to_string()), location.clone());
                self.next();
                self.parse_postfix(ident)
            }

            _ => Err(SyntaxError::ExpectedExpr(location)),
        }
    }

    // Postfix operator parsing, lifted from another older project of mine
    fn parse_postfix(&mut self, base_expr: Span<Expr>) -> parser::Return<Span<Expr>> {
        if let Some(next_token) = self.peek() {
            match next_token.kind {
                TokenKind::Dot => {
                    self.next();
                    let field = self.expect(TokenKind::Name).map(|t| {
                        Expr::MemberAccess(Member {
                            target: Box::new(base_expr.clone()),
                            name: t.literal.to_string(),
                        })
                    })?;

                    let location = base_expr.loc.clone();
                    let expr = spanned(field, location);
                    self.parse_postfix(expr)
                }

                TokenKind::LParen => {
                    todo!()
                }

                _ => Ok(base_expr),
            }
        } else {
            Ok(base_expr)
        }
    }

    fn parse_lambda(&mut self) -> parser::Return<Span<Expr>> {
        let location = self.peek().map(|t| t.location).unwrap_or_default();

        // \(x) -> body or \(x,y) -> body
        self.expect(TokenKind::Lambda)?;
        self.expect(TokenKind::LParen)?;

        let mut args: Vec<(String, Ty)> = Vec::new();
        while self.peek().map(|t| t.kind) != Some(TokenKind::RParen) {
            let name = self.expect(TokenKind::Name)?.literal.to_string();

            // Types marked as `UnknownForNow` will be inferred later on
            args.push((name, Ty::UnknownForNow));

            if self.peek().map(|t| t.kind) != Some(TokenKind::Comma) {
                break;
            }
            self.next();
        }

        self.expect(TokenKind::RParen)?;
        self.expect(TokenKind::Arrow)?;

        let body = self.parse_expr()?;

        Ok(spanned(
            Expr::Lam {
                args,
                return_ty: Ty::UnknownForNow,
                body: Box::new(body),
            },
            location,
        ))
    }

    fn parse_literal(&mut self, token: &Token) -> parser::Return<Literal> {
        let location = self.next().map(|t| t.location).unwrap_or_default();

        match token.kind {
            TokenKind::IntLit | TokenKind::HexIntLit => Ok(Literal::Int(token.to_int_literal())),
            TokenKind::FloatLit => Ok(Literal::Float(token.literal.parse().unwrap())),
            TokenKind::StringLit => Ok(Literal::String(token.literal.to_string())),
            _ => Err(SyntaxError::ExpectedExpr(location)),
        }
    }

    fn parse_array(&mut self) -> parser::Return<Span<Expr>> {
        let location = self.peek().map(|t| t.location).unwrap_or_default();

        self.expect(TokenKind::LSquare)?;
        let mut elements = Vec::new();

        while self.peek().map(|t| t.kind) != Some(TokenKind::RSquare) {
            let expr = self.parse_expr()?;
            elements.push(expr);

            if self.peek().map(|t| t.kind) != Some(TokenKind::Comma) {
                break;
            }
            self.next();
        }

        self.expect(TokenKind::RSquare)?;

        Ok(spanned(Expr::Array(elements), location))
    }
}
