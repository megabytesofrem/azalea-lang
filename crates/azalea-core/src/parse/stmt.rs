//! Statement and block parsers

use std::collections::HashSet;

use super::Parser;
use super::base as parser;
use super::error::ParserError;
use crate::ast::Block;
use crate::ast::ToplevelStmt;
use crate::ast::ast_types::Enum;
use crate::ast::ast_types::EnumVariant;
use crate::ast::ast_types::EnumVariantPayload;
use crate::ast::ast_types::Function;
use crate::ast::ast_types::Record;
use crate::ast::ast_types::Ty;
use crate::ast::{Expr, Stmt};
use crate::lexer::TokenKind;
use crate::parse::span::Span;
use crate::parse::span::spanned;

impl<'a> Parser<'a> {
    pub(crate) fn parse_stmt(&mut self) -> parser::Return<Span<Stmt>> {
        // Skip any comments
        while self.peek().map(|t| t.kind) == Some(TokenKind::Comment) {
            self.next(); // consume the comment token
        }

        let location = self.peek().map(|t| t.location).unwrap_or_default();

        match self.peek().map(|t| t.kind) {
            Some(TokenKind::KwLet) => self.parse_let(),
            Some(TokenKind::KwMut) => self.parse_mut(),
            Some(TokenKind::KwIf) => self.parse_if().map(|expr| {
                // Wrap the expression in a statement
                spanned(Stmt::Expr(expr), location)
            }),
            Some(TokenKind::KwFor) => self.parse_for(),
            Some(TokenKind::KwClass) => self.parse_typeclass_decl(),
            Some(TokenKind::KwImpl) => self.parse_typeclass_instance(),
            Some(TokenKind::KwWhile) => self.parse_while(),
            Some(TokenKind::Name) => {
                // Look ahead to see if this is an assignment (name = expr)
                // We'll save the current state and try parsing as assignment first
                // let name_token = self.next().unwrap(); // consume the name

                let mut lhs = spanned(
                    Expr::Ident(self.next().unwrap().literal.to_string()),
                    location.clone(),
                );

                lhs = self.parse_postfix(lhs)?;

                if self.peek().map(|t| t.kind) == Some(TokenKind::Eq) {
                    // This is an assignment: target = expr
                    self.next();
                    let value = self.parse_expr()?;

                    match &lhs.target {
                        Expr::Ident(_) | Expr::MemberAccess(_) => Ok(spanned(
                            Stmt::Assign {
                                target: Box::new(lhs),
                                value: Box::new(value),
                            },
                            location,
                        )),

                        _ => Err(ParserError::InvalidAssignment {
                            target: lhs.target.clone(),
                            location: lhs.loc.clone(),
                        }),
                    }
                } else {
                    // This is an expression statement starting with a name
                    // Continue parsing remaining binary operators
                    lhs = self.parse_remaining_expr(lhs)?;
                    Ok(spanned(Stmt::Expr(lhs), location))
                }
            }
            _ => Err(ParserError::UnexpectedToken {
                token: self.peek().map(|t| t.kind).unwrap(),
                location,
            }),
        }
    }

    pub(crate) fn parse_block(&mut self) -> parser::Return<Block> {
        self.expect(TokenKind::KwDo)?;

        let mut stmts_in_block = Vec::new();

        while self.peek().map(|t| t.kind) != Some(TokenKind::KwEnd) {
            let stmt = self.parse_stmt()?;
            stmts_in_block.push(stmt);
        }

        self.expect(TokenKind::KwEnd)?;

        Ok(stmts_in_block)
    }

    pub(crate) fn parse_starting_block(&mut self) -> parser::Return<Block> {
        self.expect_one_of(vec![TokenKind::KwDo, TokenKind::KwThen, TokenKind::KwElse])?;

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
            self.expect(TokenKind::Eq)?;
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

    fn parse_mut(&mut self) -> Result<Span<Stmt>, ParserError> {
        // mut name: ty = expr or mut name = expr
        let location = self.peek().map(|t| t.location).unwrap_or_default();

        self.expect(TokenKind::KwMut)?;
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
            self.expect(TokenKind::Eq)?;
            Ty::Unresolved
        };

        let value = self.parse_expr()?;

        Ok(spanned(
            Stmt::Mut {
                name: name.to_string(),
                ty,
                value: Box::new(value),
            },
            location,
        ))
    }

    fn parse_for(&mut self) -> parser::Return<Span<Stmt>> {
        // for name in expr do
        //   ...
        // end
        let location = self.peek().map(|t| t.location).unwrap_or_default();

        self.expect(TokenKind::KwFor)?;
        let target = self.parse_expr()?;

        self.expect(TokenKind::KwIn)?;
        println!("Parsing for loop, next token: {:?}", self.peek());

        let iterable = self.parse_expr()?;

        let body = self.parse_block()?;

        Ok(spanned(
            Stmt::For {
                target: Box::new(target),
                iterable: Box::new(iterable),
                body,
            },
            location,
        ))
    }

    fn parse_while(&mut self) -> parser::Return<Span<Stmt>> {
        // while expr do
        //   ...
        // end
        let location = self.peek().map(|t| t.location).unwrap_or_default();

        self.expect(TokenKind::KwWhile)?;

        let cond = self.parse_expr()?;

        let body = self.parse_block()?;

        Ok(spanned(
            Stmt::While {
                cond: Box::new(cond),
                body,
            },
            location,
        ))
    }

    fn parse_enum_variant(&mut self, type_params: &HashSet<String>) -> parser::Return<EnumVariant> {
        // Parse a single enum variant
        // Supported formats:
        // - simple_identifier: a simple identifier
        // - Just(A): a variant with parameters
        // - Just(A, B): a variant with multiple parameters
        // - { field: ty, ... }: a variant with fields

        let name = self.expect(TokenKind::Name)?.literal;

        if self.peek().map(|t| t.kind) == Some(TokenKind::LParen) {
            self.next();
            let mut tys = Vec::new();

            while self.peek().map(|t| t.kind) != Some(TokenKind::RParen) {
                let ty = self.parse_primary_type(&type_params)?;
                tys.push(ty);

                if self.peek().map(|t| t.kind) == Some(TokenKind::Comma) {
                    self.next(); // consume the comma
                } else {
                    break;
                }
            }

            self.expect(TokenKind::RParen)?;

            // Return the variant with its parameters
            Ok(EnumVariant {
                name: name.to_string(),
                payload: EnumVariantPayload::Tuple(tys),
            })
        } else if self.peek().map(|t| t.kind) == Some(TokenKind::LBrace) {
            // This is a record-like variant
            self.next(); // consume '{'
            let mut fields = Vec::new();

            while self.peek().map(|t| t.kind) != Some(TokenKind::RBrace) {
                let field_name = self.expect(TokenKind::Name)?.literal;
                self.expect(TokenKind::Colon)?;
                let field_ty = self.parse_primary_type(&HashSet::new())?;
                fields.push((field_name.to_string(), field_ty));

                if self.peek().map(|t| t.kind) != Some(TokenKind::Comma) {
                    break;
                }
                self.next(); // consume the comma
            }

            self.expect(TokenKind::RBrace)?;

            Ok(EnumVariant {
                name: name.to_string(),
                payload: EnumVariantPayload::Record(fields),
            })
        } else {
            // Simple identifier variant
            // No additional parsing needed, just return the name
            Ok(EnumVariant {
                name: name.to_string(),
                payload: EnumVariantPayload::None,
            })
        }
    }

    pub(crate) fn parse_record_decl(&mut self) -> parser::Return<Span<ToplevelStmt>> {
        // record name = { field: ty, ... }
        // record name[A] = { field: ty, ... }
        let location = self.peek().map(|t| t.location).unwrap_or_default();

        self.expect(TokenKind::KwRecord)?;
        let name = self.expect(TokenKind::Name)?.literal;

        // Parse type parameters if there are any
        let mut type_params = Vec::new();
        if self.peek().map(|t| t.kind) == Some(TokenKind::LSquare) {
            self.next(); // consume '['
            while let Some(TokenKind::Name) = self.peek().map(|t| t.kind) {
                type_params.push(self.expect(TokenKind::Name)?.literal.to_string());
                if self.peek().map(|t| t.kind) == Some(TokenKind::Comma) {
                    self.next();
                } else {
                    break;
                }
            }
            self.expect(TokenKind::RSquare)?;
        }

        let type_param_ctx: HashSet<String> = type_params.iter().cloned().collect();

        self.expect(TokenKind::Eq)?;

        self.expect(TokenKind::LBrace)?;
        let mut record_fields = Vec::new();

        while self.peek().map(|t| t.kind) != Some(TokenKind::RBrace) {
            let field_name = self.expect(TokenKind::Name)?.literal;
            self.expect(TokenKind::Colon)?;
            let field_ty = self.parse_primary_type(&type_param_ctx)?;

            record_fields.push((field_name.to_string(), field_ty));

            if self.peek().map(|t| t.kind) != Some(TokenKind::Comma) {
                break;
            }
            self.next();
        }

        self.expect(TokenKind::RBrace)?;

        Ok(spanned(
            ToplevelStmt::RecordDecl(Record {
                name: name.to_string(),
                type_params,
                fields: record_fields,
            }),
            location,
        ))
    }
    pub(crate) fn parse_enum_decl(&mut self) -> parser::Return<Span<ToplevelStmt>> {
        // Parse an enum declaration. Enums are alsos known as algebraic data types (ADTs).

        // Supported formats:
        // enum Color = Red | Green | Blue
        // enum Maybe[A] = Just(A) | Nothing
        // enum Either[A, B] = Left(A) | Right(B)
        // enum Animal = { name: String, age: u32 } | { name: String, age: u32, species: String }

        self.skip_comments();

        let location = self.peek().map(|t| t.location).unwrap_or_default();

        self.expect(TokenKind::KwEnum)?;
        let name = self.expect(TokenKind::Name)?.literal;

        let mut type_params = Vec::new();
        println!("Peek token: {:?}", self.peek());

        // Parse type parameters if there are any
        // Type parameters are enclosed in square brackets, e.g. `fn name[T, U]`
        if self.peek().map(|t| t.kind) == Some(TokenKind::LSquare) {
            self.next(); // consume '['
            while let Some(TokenKind::Name) = self.peek().map(|t| t.kind) {
                type_params.push(self.expect(TokenKind::Name)?.literal.to_string());
                if self.peek().map(|t| t.kind) == Some(TokenKind::Comma) {
                    self.next();
                } else {
                    break;
                }
            }
            self.expect(TokenKind::RSquare)?;
        }

        let type_param_ctx: HashSet<String> = type_params.iter().cloned().collect();

        self.expect(TokenKind::Eq)?;

        let mut enum_variants = Vec::new();

        // Parse first variant (required)
        let first_variant = self.parse_enum_variant(&type_param_ctx)?;
        enum_variants.push(first_variant);

        while self.peek().map(|t| t.kind) == Some(TokenKind::Pipe) {
            self.next(); // consume the pipe

            let variant = self.parse_enum_variant(&type_param_ctx)?;
            enum_variants.push(variant);
        }

        Ok(spanned(
            ToplevelStmt::EnumDecl(Enum {
                name: name.to_string(),
                type_params,
                variants: enum_variants,
            }),
            location,
        ))
    }

    pub(crate) fn parse_fn_decl(&mut self) -> parser::Return<Span<ToplevelStmt>> {
        // fn name(args: ty, ...) -> ty =
        // With generics: fn name[T, U](args: ty, ...) -> ty =
        let location = self.peek().map(|t| t.location).unwrap_or_default();

        self.expect(TokenKind::KwFn)?;
        let name = self.expect(TokenKind::Name)?.literal;

        let mut type_params = Vec::new();
        println!("Peek token: {:?}", self.peek());

        // Parse type parameters if there are any
        // Type parameters are enclosed in square brackets, e.g. `fn name[T, U]`
        if self.peek().map(|t| t.kind) == Some(TokenKind::LSquare) {
            self.next(); // consume '['
            while let Some(TokenKind::Name) = self.peek().map(|t| t.kind) {
                type_params.push(self.expect(TokenKind::Name)?.literal.to_string());
                if self.peek().map(|t| t.kind) == Some(TokenKind::Comma) {
                    self.next();
                } else {
                    break;
                }
            }
            self.expect(TokenKind::RSquare)?;
        }

        let type_param_ctx: HashSet<String> = type_params.iter().cloned().collect();

        self.expect(TokenKind::LParen)?;

        // Parse function arguments
        let mut args = Vec::new();
        while self.peek().map(|t| t.kind) != Some(TokenKind::RParen) {
            let name = self.expect(TokenKind::Name)?.literal.to_string();
            self.expect(TokenKind::Colon)?;
            let ty = self.parse_primary_type(&type_param_ctx)?;
            args.push((name, ty));

            if self.peek().map(|t| t.kind) == Some(TokenKind::Comma) {
                self.next(); // consume the comma
            } else {
                break;
            }
        }
        self.expect(TokenKind::RParen)?;

        let return_ty = if self.peek().map(|t| t.kind) == Some(TokenKind::Colon) {
            self.next();
            self.parse_primary_type(&type_param_ctx)?
        } else {
            // If no type is specified, we'll figure it out later
            Ty::Unresolved
        };

        self.expect(TokenKind::Eq)?;

        // Check if the next token is `do`, and if so parse the block. Otherwise
        // we have a single expression function.
        let mut func = if self.peek().map(|t| t.kind) == Some(TokenKind::KwDo) {
            let block = self.parse_block()?;
            Function::new_with_stmts(name.to_string(), args, return_ty, block)
        } else {
            // Single expression function - use body_expr
            let expr = self.parse_expr()?;
            Function::new_with_expr(name.to_string(), args, return_ty, Box::new(expr))
        };

        if !type_params.is_empty() {
            // If we have type parameters, add them to the function
            func.type_params = type_params;
        }

        Ok(spanned(ToplevelStmt::FnDecl(func), location))
    }

    pub(crate) fn parse_extern_decl(&mut self) -> parser::Return<Span<ToplevelStmt>> {
        // extern fn "js_name" name(args: ty, ...) : ty
        let location = self.peek().map(|t| t.location).unwrap_or_default();

        self.expect(TokenKind::KwExtern)?;
        self.expect(TokenKind::KwFn)?;

        let extern_name = self.expect(TokenKind::StringLit)?.literal;

        let name = self.expect(TokenKind::Name)?.literal;

        let mut type_params = Vec::new();
        if self.peek().map(|t| t.kind) == Some(TokenKind::LSquare) {
            self.next(); // consume '['
            while let Some(TokenKind::Name) = self.peek().map(|t| t.kind) {
                type_params.push(self.expect(TokenKind::Name)?.literal.to_string());
                if self.peek().map(|t| t.kind) == Some(TokenKind::Comma) {
                    self.next();
                } else {
                    break;
                }
            }
            self.expect(TokenKind::RSquare)?;
        }

        self.expect(TokenKind::LParen)?;
        let args = self.parse_typed_pair_list()?;
        self.expect(TokenKind::RParen)?;

        let return_ty = if self.peek().map(|t| t.kind) == Some(TokenKind::Colon) {
            self.next();
            self.parse_typename()?
        } else {
            Ty::Unit
        };

        let func = Function::new_extern(extern_name.to_string(), name.to_string(), args, return_ty);

        Ok(spanned(ToplevelStmt::ExternDecl(func), location))
    }

    fn parse_remaining_expr(&mut self, mut lhs: Span<Expr>) -> parser::Return<Span<Expr>> {
        // First, handle any postfix operations (function calls, member access, etc.)
        // Use the existing parse_postfix method from expr.rs
        lhs = self.parse_postfix(lhs)?;

        // Then handle binary operators with precedence
        while let Some(op_token) = self.peek().clone() {
            // Check if the current token is actually a binary operator
            if !op_token.kind.is_binary_operator() {
                break;
            }

            let op = op_token.kind.clone().to_operator();
            let (prec, assoc) = self.get_precedence(op);

            self.next(); // consume the operator
            let rhs = self.parse_expr_prec(if assoc == super::Assoc::Left {
                prec + 1
            } else {
                prec
            })?;

            let location = lhs.loc.clone();
            lhs = spanned(Expr::BinOp(Box::new(lhs), op, Box::new(rhs)), location);
        }

        Ok(lhs)
    }
}
