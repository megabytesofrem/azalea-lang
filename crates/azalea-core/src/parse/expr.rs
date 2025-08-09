//! Expression parsing functions for Azalea

use super::error::ParserError;
use super::{Assoc, Parser};
use crate::ast::ast_types::{RecordExpr, Ty};
use crate::lexer::{Token, TokenKind};
use crate::parse::base as parser;
use crate::parse::span::{Span, spanned};

use crate::ast::{Expr, Literal, Member, Stmt};
use crate::lexer::Op;

impl<'a> Parser<'a> {
    // parse_expr_prec and parse_expr are helper functions for parsing expressions with precedence, while
    // parse_main_expr is for parsing the actual expressions themselves
    pub(crate) fn parse_expr_prec(&mut self, min_prec: u8) -> parser::Return<Span<Expr>> {
        let location = self.peek().map(|t| t.location).unwrap_or_default();
        let mut lhs = self.parse_main_expr()?;

        while let Some(op_token) = self.peek().clone() {
            // Check if the current token is actually a binary operator
            if !op_token.kind.is_binary_operator() {
                break;
            }

            let op = op_token.kind.clone().to_operator();
            let (prec, assoc) = self.get_precedence(op);

            // If the operator has a lower precedence than min_prec, break
            if prec < min_prec {
                break;
            }

            self.next();
            let rhs = self.parse_expr_prec(if assoc == Assoc::Left { prec + 1 } else { prec })?;

            lhs = spanned(
                Expr::BinOp(Box::new(lhs), op, Box::new(rhs)),
                location.clone(),
            );
        }

        Ok(lhs)
    }

    pub(crate) fn parse_expr(&mut self) -> parser::Return<Span<Expr>> {
        self.parse_expr_prec(0)
    }

    fn parse_main_expr(&mut self) -> parser::Return<Span<Expr>> {
        let location = self.peek().map(|t| t.location).unwrap_or_default();

        // Check for unary operators first
        if let Some(token) = self.peek() {
            if token.kind == TokenKind::Bang {
                self.next(); // consume the operator
                let expr = self.parse_main_expr()?;
                return Ok(spanned(Expr::UnOp(Op::Not, Box::new(expr)), location));
            }
        }

        let token = self
            .peek()
            .ok_or(ParserError::ExpectedExpr(location.clone()))?;

        // println!(
        //     "parse_main_expr: encountered token {:?} at {:?}",
        //     token.kind, token.location
        // );

        match &token.kind {
            TokenKind::Minus => {
                let expr = self.parse_expr()?;
                Ok(spanned(
                    Expr::UnOp(token.kind.to_operator(), Box::new(expr)),
                    location,
                ))
            }
            TokenKind::Dot => self.parse_record_expr(),
            TokenKind::LSquare => self.parse_array(),
            TokenKind::LParen => {
                // Subexpressions
                let expr = self.parse_expr()?;
                self.expect(TokenKind::RParen)
                    .map_err(|_| ParserError::MissingClosingParen(location))?;

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

            TokenKind::KwIf => {
                println!("Parsing if expression");
                self.parse_if()
            }

            TokenKind::Lambda => {
                println!("Parsing lambda expression");
                self.parse_lambda()
            }

            _ => Err(ParserError::ExpectedExpr(location)),
        }
    }

    // Postfix operator parsing, lifted from another older project of mine
    fn parse_postfix(&mut self, base_expr: Span<Expr>) -> parser::Return<Span<Expr>> {
        if let Some(next_token) = self.peek() {
            match next_token.kind {
                TokenKind::Dot => {
                    // Check if the next token is name (member access) or a left square bracket (array index)
                    // Member access: base_expr.field
                    // Array index: base_expr.[index]

                    // Consume the dot token
                    self.next();

                    // Peek at what comes after the dot
                    if let Some(next_token) = self.peek() {
                        match next_token.kind {
                            TokenKind::Name => {
                                // Member access: base_expr.field
                                let field_name = next_token.literal.to_string();
                                self.next(); // consume the field name

                                let field = Expr::MemberAccess(Member {
                                    target: Box::new(base_expr.clone()),
                                    name: field_name,
                                });

                                let location = base_expr.loc.clone();
                                let expr = spanned(field, location);
                                return self.parse_postfix(expr);
                            }

                            TokenKind::LSquare => {
                                // Array index: base_expr.[index]
                                self.next(); // consume the '['
                                let index_expr = self.parse_expr()?;
                                self.expect(TokenKind::RSquare).map_err(|_| {
                                    ParserError::MissingClosingSquareBracket(base_expr.loc.clone())
                                })?;

                                let array_index = Expr::ArrayIndex {
                                    target: Box::new(base_expr.clone()),
                                    index: Box::new(index_expr),
                                };

                                let location = base_expr.loc.clone();
                                let expr = spanned(array_index, location);
                                return self.parse_postfix(expr);
                            }

                            _ => {
                                // Invalid syntax after dot
                                let location = base_expr.loc.clone();
                                return Err(ParserError::InvalidTokenAfterDot(location));
                            }
                        }
                    } else {
                        // EOF after dot
                        let location = base_expr.loc.clone();
                        return Err(ParserError::InvalidTokenAfterDot(location));
                    }
                }

                TokenKind::LParen => {
                    // Function call: Parse the function call, and recurse to parse postfix operators
                    let fncall = self.parse_fncall(base_expr)?;
                    self.parse_postfix(fncall)
                }

                TokenKind::LBrace => {
                    // Record construction: Name { field: value, ... }
                    // Convert the base expression (which should be an identifier) to a record expression
                    if let Expr::Ident(name) = &base_expr.target {
                        println!("Parsing named record construction for: {}", name);

                        // Parse the record fields
                        let location = base_expr.loc.clone();
                        self.expect(TokenKind::LBrace)?;

                        let mut fields = Vec::new();
                        while self.peek().map(|t| t.kind) != Some(TokenKind::RBrace) {
                            // Parse field: expression pair
                            let field_name = match self.peek() {
                                Some(Token {
                                    kind: TokenKind::Name,
                                    literal,
                                    ..
                                }) => {
                                    let name = literal.to_string();
                                    self.next();
                                    name
                                }
                                Some(Token { location, .. }) => {
                                    return Err(ParserError::InvalidRecordFormat(location));
                                }
                                None => return Err(ParserError::UnexpectedEOF),
                            };

                            self.expect(TokenKind::Colon)?;
                            let field_expr = self.parse_expr()?;
                            fields.push((field_name, field_expr.target));

                            if self.peek().map(|t| t.kind) != Some(TokenKind::Comma) {
                                break;
                            }
                            self.next();
                        }

                        self.expect(TokenKind::RBrace)?;

                        let record_expr = spanned(
                            Expr::Record(RecordExpr {
                                name: name.clone(),
                                fields,
                            }),
                            location,
                        );

                        self.parse_postfix(record_expr)
                    } else {
                        // Base expression is not an identifier, can't do record construction
                        Ok(base_expr)
                    }
                }

                TokenKind::LSquare => {
                    // Array index: Parse the array index, and recurse to parse postfix operators
                    // e.g., array.[0][1] or arr.[0].field

                    // TODO: We may have to come back to this when we add generics using `[T]` syntax,
                    // since we need to check if the expression is a function call _or_ an array index
                    self.next(); // consume the '['
                    let index_expr = self.parse_expr()?;
                    self.expect(TokenKind::RSquare)?;

                    let array_index = Expr::ArrayIndex {
                        target: Box::new(base_expr.clone()),
                        index: Box::new(index_expr),
                    };

                    let location = base_expr.loc.clone();
                    let expr = spanned(array_index, location);
                    self.parse_postfix(expr)
                }

                _ => Ok(base_expr),
            }
        } else {
            Ok(base_expr)
        }
    }

    fn parse_literal(&mut self, token: &Token) -> parser::Return<Literal> {
        let location = self.next().map(|t| t.location).unwrap_or_default();

        match token.kind {
            TokenKind::IntLit | TokenKind::HexIntLit => Ok(Literal::Int(token.to_int_literal())),
            TokenKind::FloatLit => Ok(Literal::Float(token.literal.parse().unwrap())),
            TokenKind::StringLit => Ok(Literal::String(token.literal.to_string())),
            _ => Err(ParserError::ExpectedExpr(location)),
        }
    }

    fn parse_lambda(&mut self) -> parser::Return<Span<Expr>> {
        // \(x) -> body or \(x,y) -> body
        let location = self.peek().map(|t| t.location).unwrap_or_default();
        self.expect(TokenKind::Lambda)?;
        self.expect(TokenKind::LParen)?;

        let mut args: Vec<(String, Ty)> = Vec::new();
        while self.peek().map(|t| t.kind) != Some(TokenKind::RParen) {
            let name = self.expect(TokenKind::Name)?.literal.to_string();

            // Types marked as `UnknownForNow` will be inferred later on
            args.push((name, Ty::Unresolved));

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
                return_ty: Ty::Unresolved,
                body: Box::new(body),
            },
            location,
        ))
    }

    pub(crate) fn parse_if(&mut self) -> parser::Return<Span<Expr>> {
        // FIXME: This only handles single expressions in the then and else blocks.
        // We should handle blocks of statements in the future.

        // if condition then [block] else [block] end
        let location = self.peek().map(|t| t.location).unwrap_or_default();

        self.expect(TokenKind::KwIf)?;

        let cond = self.parse_expr()?;
        self.expect(TokenKind::KwThen)?;

        // For a simple if-then-end, we expect a single expression
        let then_expr = self.parse_expr()?;
        let then_loc = location.clone();
        let then_block = vec![spanned(Stmt::Expr(then_expr), then_loc)];

        let else_block = if self.peek().map(|t| t.kind) == Some(TokenKind::KwElse) {
            self.next(); // consume the else token

            // For a simple else block, we expect a single expression
            let else_expr = self.parse_expr()?;
            let else_loc = self.peek().map(|t| t.location).unwrap_or_default();
            Some(vec![spanned(Stmt::Expr(else_expr), else_loc)])
        } else {
            None
        };

        self.expect(TokenKind::KwEnd)?;

        Ok(spanned(
            Expr::If {
                cond: Box::new(cond),
                then: then_block,

                // Optional else block
                else_: else_block,
            },
            location,
        ))
    }

    fn parse_fncall(&mut self, target: Span<Expr>) -> parser::Return<Span<Expr>> {
        // f(x, y) - target is already parsed
        let location = target.loc.clone();

        self.expect(TokenKind::LParen)?;

        let mut args = Vec::new();
        while self.peek().map(|t| t.kind) != Some(TokenKind::RParen) {
            let arg = self.parse_expr()?;
            args.push(arg);

            if self.peek().map(|t| t.kind) != Some(TokenKind::Comma) {
                break;
            }
            self.next();
        }

        self.expect(TokenKind::RParen)?;

        Ok(spanned(
            Expr::FnCall {
                target: Box::new(target),
                args,
            },
            location,
        ))
    }

    /// Parse an array literal
    fn parse_array(&mut self) -> parser::Return<Span<Expr>> {
        // [1, 2, 3]
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

        Ok(spanned(Expr::Array { elements }, location))
    }

    fn parse_array_index(&mut self) -> parser::Return<Span<Expr>> {
        // array[0]
        let location = self.peek().map(|t| t.location).unwrap_or_default();

        let array = self.parse_main_expr()?;
        self.expect(TokenKind::LSquare)?;

        let index = self.parse_expr()?;

        self.expect(TokenKind::RSquare)?;

        Ok(spanned(
            Expr::ArrayIndex {
                target: Box::new(array),
                index: Box::new(index),
            },
            location,
        ))
    }

    /// Parse a record expression
    fn parse_record_expr(&mut self) -> parser::Return<Span<Expr>> {
        // .{ field1: 1, field2: 2 } or Named { field1: 1, field2: 2 }
        let location = self.peek().map(|t| t.location).unwrap_or_default();

        let name = match self.peek().map(|t| t.kind) {
            Some(TokenKind::Name) => {
                println!("Parsing named record expression");

                // Consume the next token e.g State { counter: 0 }
                let name_token = self.next().ok_or(ParserError::UnexpectedEOF)?;
                name_token.literal.to_string()
            }
            Some(TokenKind::Dot) => {
                println!("Parsing anonymous record expression");

                // Consume the dot token
                self.next();
                "".to_string() // No name specified, just a record expression
            }

            // Error: expected a dot (anonymous record) or a name (named record)
            _ => return Err(ParserError::ExpectedExpr(location)),
        };

        // Unconditionally expect a left brace
        self.expect(TokenKind::LBrace)?;

        let mut fields = Vec::new();
        while self.peek().map(|t| t.kind) != Some(TokenKind::RBrace) {
            // Parse field: expression pair for record value expressions
            let field_name = match self.peek() {
                Some(Token {
                    kind: TokenKind::Name,
                    literal,
                    ..
                }) => {
                    let name = literal.to_string();
                    self.next();
                    name
                }
                Some(Token { location, .. }) => return Err(ParserError::ExpectedExpr(location)),
                None => return Err(ParserError::UnexpectedEOF),
            };

            self.expect(TokenKind::Colon)?;
            let field_expr = self.parse_expr()?;
            fields.push((field_name, field_expr.target));

            if self.peek().map(|t| t.kind) != Some(TokenKind::Comma) {
                break;
            }
            self.next();
        }

        self.expect(TokenKind::RBrace)?;

        Ok(spanned(Expr::Record(RecordExpr { name, fields }), location))
    }
}
