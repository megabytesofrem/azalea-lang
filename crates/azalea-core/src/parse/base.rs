use std::collections::HashMap;

use super::error::ParserError;
use crate::ast::Stmt;
use crate::lexer::{self, LexerIter, Op, Token, TokenKind};
use crate::parse::{base, span::Span};

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
    errors: Vec<ParserError>,

    pub ast: Vec<Span<Stmt>>,
    pub precedence_table: HashMap<Op, (u8, Assoc)>,

    // Keep track of the location internally
    line: usize,
    col: usize,
}

pub type Return<'a, T> = Result<T, ParserError>;

#[allow(dead_code)]
impl<'a> Parser<'a> {
    pub fn new(token_stream: LexerIter<'a>) -> Self {
        // Get the location of the first token in the stream
        let location = token_stream
            .clone()
            .peek()
            .and_then(|lex_result| match lex_result {
                Ok(token) => Some(token.location.clone()),
                Err(lex_error) => Some(lex_error.location().clone()),
            })
            .unwrap_or_default();

        let mut parser = Parser {
            token_stream,
            ast: Vec::new(),
            errors: Vec::new(),
            line: location.line,
            col: location.start,
            precedence_table: HashMap::new(),
        };

        // Setup the precedence table
        parser.setup_precedence_table();

        parser
    }

    // Parser driver function
    pub fn parse(token_stream: lexer::LexerIter<'a>) -> base::Return<'a, Parser<'a>> {
        let mut parser = Parser::new(token_stream);

        // Parse multiple statements until EOF
        while parser.peek().is_some() {
            match parser.parse_stmt() {
                Ok(stmt) => {
                    // println!("Parsed statement: {:?}", stmt);
                    parser.ast.push(stmt);
                }
                Err(err) => {
                    println!("Parse error: {:?}", err);
                    break;
                }
            }
        }

        Ok(parser)
    }

    /// Skip tokens until we find a good recovery point for parsing the next statement
    /// Returns true if a recovery point was found, false if EOF was reached
    fn skip_to_recovery_point(&mut self) -> bool {
        // Skip tokens until we find a keyword that likely starts a new statement
        // or until we reach EOF
        while let Some(token) = self.peek() {
            match token.kind {
                TokenKind::KwLet | TokenKind::KwRecord | TokenKind::KwEnum | TokenKind::KwFn => {
                    // Found a likely start of a new statement, stop skipping
                    return true;
                }
                _ => {
                    // Skip this token and continue
                    self.next();
                }
            }
        }
        false // Reached EOF without finding a recovery point
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

    pub fn errors(&self) -> &Vec<ParserError> {
        &self.errors
    }

    /// Peek ahead by one token in the stream
    pub(crate) fn peek(&mut self) -> Option<Token<'a>> {
        match self.token_stream.peek() {
            Some(Ok(token)) => Some(token.clone()),
            Some(Err(lex_error)) => {
                // Convert lexical error to syntax error and store it
                self.errors
                    .push(ParserError::LexicalError(lex_error.clone()));
                None
            }
            None => None,
        }
    }

    /// Consume the next token in the stream and return it
    pub(crate) fn next(&mut self) -> Option<Token<'a>> {
        self.col += 1;
        match self.token_stream.next() {
            Some(Ok(token)) => Some(token),
            Some(Err(lex_error)) => {
                // Convert lexical error to syntax error and store it
                self.errors.push(ParserError::LexicalError(lex_error));
                None
            }
            None => None,
        }
    }

    /// `check` has the same fundamental behavior as `expect`, but it doesn't advance
    /// the parser or consume the token.
    pub(crate) fn check(&mut self, kind: TokenKind) -> Return<Token<'a>> {
        let token = self.peek().ok_or(ParserError::UnexpectedEOF)?;
        if token.kind == kind {
            Ok(token)
        } else if token.kind == TokenKind::Comment {
            // If the token is a comment, skip it and try again
            self.next();
            self.check(kind)
        } else {
            let error = ParserError::UnexpectedToken {
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
        let token = self.next().ok_or(ParserError::UnexpectedEOF)?;
        if token.kind == kind {
            Ok(token)
        } else if token.kind == TokenKind::Comment {
            // If the token is a comment, skip it and try again
            self.expect(kind)
        } else {
            let error = ParserError::UnexpectedToken {
                token: token.kind,
                expected_any: vec![kind],
                location: token.location,
            };

            self.errors.push(error.clone());
            Err(error)
        }
    }

    pub(crate) fn expect_error(&mut self, kind: TokenKind, err: ParserError) -> Return<Token<'a>> {
        self.expect(kind).map_err(|_| err)
    }

    /// `expect_one_of` does the same thing as `expect`, but it allows for multiple expected
    /// token kinds.
    /// If the next token is not one of the expected kinds, return a `SyntaxError`.
    pub(crate) fn expect_one_of(&mut self, kinds: Vec<TokenKind>) -> Return<Token<'a>> {
        let token = self.next().ok_or(ParserError::UnexpectedEOF)?;
        if kinds.contains(&token.kind) {
            Ok(token)
        } else if token.kind == TokenKind::Comment {
            // If the token is a comment, skip it and try again
            self.expect_one_of(kinds)
        } else {
            let error = ParserError::UnexpectedToken {
                token: token.kind,
                expected_any: kinds,
                location: token.location,
            };

            self.errors.push(error.clone());
            Err(error)
        }
    }
}
