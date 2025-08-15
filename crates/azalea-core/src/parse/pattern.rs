//! Pattern parsing

use crate::ast::Expr;
use crate::ast::ast_types::{Branch, Pattern, PatternMatch};
use crate::lexer::TokenKind;
use crate::parse::error::ParserError;
use crate::parse::span::{Span, spanned};
use crate::parse::{Parser, base as parser};

impl<'a> Parser<'a> {
    pub fn parse_match(&mut self) -> parser::Return<Span<Expr>> {
        // Parses a match expression
        // match expr
        //   case pattern1 -> expr1,
        //   case pattern2 -> expr2,

        self.expect(TokenKind::KwMatch)?;
        let case = self.parse_expr()?;
        let mut branches = Vec::new();

        // Parse first branch (required)
        let first_branch = self.parse_case_pattern()?;
        branches.push(first_branch);

        while self.peek().map(|t| t.kind) == Some(TokenKind::Pipe) {
            // Parse additional branches
            let branch = self.parse_case_pattern()?;
            branches.push(branch);
        }

        let match_expr = PatternMatch {
            target: Box::new(case),
            branches,
        };

        Ok(spanned(Expr::Match(match_expr), self.get_token_location()))
    }

    fn parse_case_pattern(&mut self) -> parser::Return<Branch> {
        // Parses a pattern in a case expression
        // | []     -> []
        // | [x:xs] -> x :: xs

        self.expect(TokenKind::Pipe)?;
        let pattern = self.parse_pattern()?;
        self.expect(TokenKind::Arrow)?;
        let expr = self.parse_expr()?;

        Ok(Branch {
            pattern,
            expr: Box::new(expr),
        })
    }

    /// Parses a pattern.
    fn parse_pattern(&mut self) -> parser::Return<Pattern> {
        match self.peek().map(|t| t.kind) {
            Some(TokenKind::IntLit | TokenKind::FloatLit | TokenKind::StringLit) => {
                // Parse a literal pattern
                let token = self.peek().unwrap();
                let literal = self.parse_literal(&token)?;
                Ok(Pattern::Literal(literal))
            }

            Some(TokenKind::Name) => {
                // Could be either a capture pattern or an enum variant pattern
                let first_token = self.next().unwrap();

                // Check if this is followed by a dot (enum variant)
                if self.peek().map(|t| t.kind) == Some(TokenKind::Dot) {
                    self.next(); // consume the dot
                    let variant_name = self.expect(TokenKind::Name)?.literal.to_string();

                    // Check if there's a payload in parentheses
                    let payload = if self.peek().map(|t| t.kind) == Some(TokenKind::LParen) {
                        self.next(); // consume '('
                        let pattern = self.parse_pattern()?;
                        self.expect(TokenKind::RParen)?;
                        Some(Box::new(pattern))
                    } else {
                        None
                    };

                    Ok(Pattern::EnumVariant {
                        enum_name: first_token.literal.to_string(),
                        variant_name,
                        payload,
                    })
                } else {
                    // Just a capture pattern
                    Ok(Pattern::Capture(first_token.literal.to_string()))
                }
            }

            Some(TokenKind::LSquare) => {
                // Parse a list partition pattern, e.g. [x:xs]
                let mut seen_colon = false;

                self.next();

                let mut patterns = Vec::new();
                while self.peek().map(|t| t.kind) != Some(TokenKind::RSquare) {
                    let pattern = self.parse_pattern()?;
                    patterns.push(pattern);

                    if self.peek().map(|t| t.kind) == Some(TokenKind::Colon) {
                        seen_colon = true;
                        self.next();
                    } else {
                        break;
                    }
                }

                self.expect(TokenKind::RSquare)?;
                if seen_colon {
                    // If we saw a colon, we expect exactly two patterns
                    if patterns.len() != 2 {
                        return Err(ParserError::InvalidPattern);
                    }

                    Ok(Pattern::Partition(
                        Box::new(patterns.remove(0)),
                        Box::new(patterns.remove(0)),
                    ))
                } else {
                    Ok(Pattern::List(patterns.into_iter().map(Box::new).collect()))
                }
            }

            Some(TokenKind::Underscore) => {
                // Parse a wildcard pattern
                self.next();
                Ok(Pattern::Wildcard)
            }

            _ => todo!(),
        }
    }
}
