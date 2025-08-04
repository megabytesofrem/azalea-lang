use std::{fmt, iter::Peekable};

use logos::{Logos, SpannedIter};

#[derive(Debug, Clone, PartialEq)]
pub struct SourceLoc {
    pub line: usize,
    pub start: usize,
    pub end: usize,
}

#[derive(Debug, Clone, PartialEq, Logos)]
pub enum TokenKind {
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("[")]
    LSquare,
    #[token("]")]
    RSquare,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token(".")]
    Dot,
    #[token(",")]
    Comma,
    #[token(":")]
    Colon,
    #[token("=")]
    Eq,
    #[token("==")]
    DoubleEq,
    #[token("!=")]
    NotEq,
    #[token("<")]
    Less,
    #[token("<=")]
    LessEq,
    #[token(">")]
    Greater,
    #[token(">=")]
    GreaterEq,
    #[token("\\")]
    Lambda,
    #[token("->")]
    Arrow,

    // Keywords
    #[token("pub")]
    KwPub,
    #[token("const")]
    KwConst,
    #[token("where")]
    KwWhere,
    #[token("let")]
    KwLet,
    #[token("fn")]
    KwFn,
    #[token("record")]
    KwRecord,
    #[token("enum")]
    KwEnum,
    #[token("import")]
    KwImport,
    #[token("if")]
    KwIf,
    #[token("else")]
    KwElse,
    #[token("for")]
    KwFor,
    #[token("while")]
    KwWhile,
    #[token("do")]
    KwDo,
    #[token("end")]
    KwEnd,
    #[token("return")]
    KwReturn,
    #[token("true")]
    KwTrue,
    #[token("false")]
    KwFalse,

    #[regex(r"Int|Float|String|Bool|Unit", |lex| lex.slice().to_owned())]
    KwType(String),

    // Literals
    #[regex(r"[_a-zA-Z][_0-9a-zA-Z]*")]
    Name,
    #[regex(r"[-]?[0-9][0-9]*")]
    IntLit,
    #[regex(r"[-]?[0-9]+\.[0-9]+")]
    FloatLit,
    #[regex(r"0[xX][0-9a-fA-F]+")]
    HexIntLit,
    #[regex(r#""(\\[\\"]|[^"])*""#)]
    StringLit,

    #[regex(r"--.*\n?")]
    Comment,

    // We ignore whitespace in the lexer
    #[regex(r"[ \t\f]+", logos::skip, priority = 2)]
    Whitespace,

    // We're skipping newlines because, the enum parser (and probably other parts
    // of the parser) don't know how to deal with newlines yet.
    #[regex(r"[\r\n]+", logos::skip)]
    NewLine,
}

#[derive(Debug, Clone, Copy, Hash, Eq, PartialEq)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
    DoubleEq,
    NotEq,
    Less,
    LessEq,
    Greater,
    GreaterEq,

    Neg,
    Not,
}

#[derive(Debug, Clone)]
pub struct Token<'a> {
    pub kind: TokenKind,
    pub location: SourceLoc,
    pub literal: &'a str,
}

// Wrapper type for a peekable iterator over tokens
pub type LexerIter<'a> = Peekable<Box<TokenIter<'a>>>;

#[derive(Clone)]
pub struct TokenIter<'a> {
    inner: SpannedIter<'a, TokenKind>,
    src: &'a str,
}

impl fmt::Display for SourceLoc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(L{}, C{}:{})", self.line, self.start, self.end)
    }
}

impl Default for SourceLoc {
    fn default() -> Self {
        Self {
            line: 1,
            start: 0,
            end: 0,
        }
    }
}

impl Token<'_> {
    pub fn location(&self) -> SourceLoc {
        self.location.clone()
    }

    pub fn is_int_literal(&self) -> bool {
        matches!(self.kind, TokenKind::IntLit | TokenKind::HexIntLit)
    }

    pub fn to_int_literal(&self) -> i32 {
        match self.kind {
            TokenKind::IntLit => self.literal.parse().unwrap(),
            TokenKind::HexIntLit => i32::from_str_radix(&self.literal[2..], 16).unwrap(),
            _ => panic!("Token is not a valid integer literal"),
        }
    }
}

impl<'a> TokenIter<'a> {
    // Return the correct location of the token accounting for newlines by using the
    // inner span and directly counting the characters in the source string.
    fn get_location(&self) -> SourceLoc {
        let span = self.inner.span();
        let start = span.start;
        let end = span.end;

        let mut line = 1;
        let mut col = 0;

        for (i, c) in self.src.chars().enumerate() {
            if i == start {
                break;
            }

            if c == '\n' {
                line += 1;
                col = 0;
            } else {
                col += 1;
            }
        }

        SourceLoc {
            line,
            start: col,
            end: col + (end - start),
        }
    }
}

impl<'a> Iterator for TokenIter<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner
            .next()
            .map(|(kind, span)| {
                Some(Token {
                    kind: kind.ok()?,
                    location: self.get_location(),
                    literal: &self.src[span],
                })
            })
            .flatten()
    }
}

/// Return an iterator over the tokens in the source string
pub fn lex_tokens(src: &str) -> LexerIter {
    let iter = TokenIter {
        inner: TokenKind::lexer(src).spanned(),
        src,
    };

    Box::new(iter).peekable()
}
