use std::collections::HashMap;

use super::syntax_error::SyntaxError;
use crate::lexer::{LexerIter, Op, SourceLoc, Token, TokenKind};

/// Associativity
#[derive(Debug, Copy, Clone, PartialEq, Hash)]
pub enum Assoc {
    Left,
    Right,
}

#[derive(Clone)]
#[allow(unused)]
pub struct Parser<'a> {
    // lifetime 'a is used throughout the entire parser
    token_stream: LexerIter<'a>,
    errors: Vec<SyntaxError>,

    pub precedence_table: HashMap<Op, (u8, Assoc)>,

    // Keep track of the location internally
    line: usize,
    col: usize,
}

pub type Return<'a, T> = Result<T, SyntaxError>;

impl<'a> Parser<'a> {
    pub fn new(token_stream: LexerIter<'a>) -> Self {
        // Get the location of the first token in the stream
        let location = token_stream
            .clone()
            .peek()
            .map_or(SourceLoc::default(), |t| t.clone().location);

        let mut parser = Parser {
            token_stream,
            errors: Vec::new(),
            line: location.line,
            col: location.start,
            precedence_table: HashMap::new(),
        };

        // Setup the precedence table
        parser.setup_precedence_table();

        parser
    }

    pub(crate) fn setup_precedence_table(&mut self) {
        self.precedence_table.insert(Op::Add, (10, Assoc::Left));
        self.precedence_table.insert(Op::Sub, (10, Assoc::Left));
        self.precedence_table.insert(Op::Mul, (20, Assoc::Left));
        self.precedence_table.insert(Op::Div, (20, Assoc::Left));
        self.precedence_table.insert(Op::DoubleEq, (5, Assoc::Left));
        self.precedence_table.insert(Op::NotEq, (5, Assoc::Left));
        self.precedence_table.insert(Op::Less, (5, Assoc::Left));
        self.precedence_table.insert(Op::LessEq, (5, Assoc::Left));
        self.precedence_table.insert(Op::Greater, (5, Assoc::Left));
        self.precedence_table
            .insert(Op::GreaterEq, (5, Assoc::Left));
    }

    pub fn get_precedence(&self, op: Op) -> (u8, Assoc) {
        self.precedence_table
            .get(&op)
            .copied()
            .unwrap_or((0, Assoc::Left))
    }

    pub fn errors(&self) -> &Vec<SyntaxError> {
        &self.errors
    }

    /// Peek ahead by one token in the stream
    pub(crate) fn peek(&mut self) -> Option<Token<'a>> {
        self.token_stream.peek().cloned()
    }

    /// Consume the next token in the stream and return it
    pub(crate) fn next(&mut self) -> Option<Token<'a>> {
        self.col += 1;
        let token = self.token_stream.next();
        token
    }

    /// `check` has the same fundamental behavior as `expect`, but it doesn't advance
    /// the parser or consume the token.
    pub(crate) fn check(&mut self, kind: TokenKind) -> Return<Token<'a>> {
        let token = self.peek().ok_or(SyntaxError::UnexpectedEOF)?;
        if token.kind == kind {
            Ok(token)
        } else if token.kind == TokenKind::Comment {
            // If the token is a comment, skip it and try again
            self.next();
            self.check(kind)
        } else {
            let error = SyntaxError::UnexpectedToken {
                token: token.kind,
                expected_any: vec![kind],
                location: token.location,
            };

            self.errors.push(error.clone());
            Err(error)
        }
    }

    /// Expect a token, consume it and return it if it matches the expected kind.
    /// If it doesn't match, return a `SyntaxError`.
    pub(crate) fn expect(&mut self, kind: TokenKind) -> Return<Token<'a>> {
        let token = self.next().ok_or(SyntaxError::UnexpectedEOF)?;
        if token.kind == kind {
            Ok(token)
        } else if token.kind == TokenKind::Comment {
            // If the token is a comment, skip it and try again
            self.expect(kind)
        } else {
            let error = SyntaxError::UnexpectedToken {
                token: token.kind,
                expected_any: vec![kind],
                location: token.location,
            };

            self.errors.push(error.clone());
            Err(error)
        }
    }

    pub(crate) fn expect_error(&mut self, kind: TokenKind, err: SyntaxError) -> Return<Token<'a>> {
        self.expect(kind).map_err(|_| err)
    }

    /// `expect_one_of` does the same thing as `expect`, but it allows for multiple expected
    /// token kinds.
    /// If the next token is not one of the expected kinds, return a `SyntaxError`.
    pub(crate) fn expect_one_of(&mut self, kinds: Vec<TokenKind>) -> Return<Token<'a>> {
        let token = self.next().ok_or(SyntaxError::UnexpectedEOF)?;
        if kinds.contains(&token.kind) {
            Ok(token)
        } else if token.kind == TokenKind::Comment {
            // If the token is a comment, skip it and try again
            self.expect_one_of(kinds)
        } else {
            let error = SyntaxError::UnexpectedToken {
                token: token.kind,
                expected_any: kinds,
                location: token.location,
            };

            self.errors.push(error.clone());
            Err(error)
        }
    }
}
