use std::fmt;
use std::iter::Peekable;

use logos::{Logos, SpannedIter};

#[derive(Debug, Clone, PartialEq)]
pub struct SourceLoc {
    pub line: usize,
    pub start: usize,
    pub end: usize,
}

/// Lexical errors that can occur during tokenization
#[derive(Default, Debug, Clone, PartialEq)]
pub enum LexicalError {
    UnterminatedString {
        source: String,
        location: SourceLoc,
    },
    InvalidHexDigit {
        source: String,
        location: SourceLoc,
        found: char,
    },
    InvalidIntegerFormat {
        source: String,
        location: SourceLoc,
        literal: String,
    },
    InvalidFloatFormat {
        source: String,
        location: SourceLoc,
        literal: String,
    },
    BadEscapeSequence {
        source: String,
        location: SourceLoc,
        sequence: String,
    },

    #[default]
    Unknown,
}

impl LexicalError {
    pub fn location(&self) -> SourceLoc {
        match self {
            LexicalError::UnterminatedString { location, .. }
            | LexicalError::InvalidHexDigit { location, .. }
            | LexicalError::InvalidIntegerFormat { location, .. }
            | LexicalError::InvalidFloatFormat { location, .. }
            | LexicalError::BadEscapeSequence { location, .. } => location.clone(),
            LexicalError::Unknown => SourceLoc::default(),
        }
    }
}

fn valid_string_literal(lex: &mut logos::Lexer<TokenKind>) -> Result<(), LexicalError> {
    let slice = lex.slice();

    if !slice.starts_with('"') || !slice.ends_with('"') {
        return Err(LexicalError::UnterminatedString {
            source: slice.to_owned(),
            location: SourceLoc {
                line: 0, // TODO: Calculate the correct line number somehow
                start: lex.span().start,
                end: lex.span().end,
            },
        });
    }

    // Check for valid escape sequences
    let substring = &slice[1..slice.len() - 1];
    let mut chars = substring.chars().peekable();

    while let Some(c) = chars.next() {
        if c == '\\' {
            match chars.next() {
                // Valid escape sequences
                Some('n') | Some('t') | Some('r') | Some('\\') | Some('"') => continue,

                Some(invalid) => {
                    return Err(LexicalError::BadEscapeSequence {
                        source: slice.to_owned(),
                        location: SourceLoc {
                            line: 0, // TODO: Calculate the correct line number somehow
                            start: lex.span().start,
                            end: lex.span().end,
                        },
                        sequence: format!("\\{}", invalid),
                    });
                }
                None => {
                    return Err(LexicalError::BadEscapeSequence {
                        source: slice.to_owned(),
                        location: SourceLoc {
                            line: 0, // TODO: Calculate the correct line number somehow
                            start: lex.span().start,
                            end: lex.span().end,
                        },
                        sequence: String::from("\\"),
                    });
                }
            }
        }
    }

    Ok(())
}

fn valid_hex_literal(lex: &mut logos::Lexer<TokenKind>) -> Result<(), LexicalError> {
    let slice = lex.slice();
    let hex_digits = &slice[2..]; // Skip the "0x" prefix

    if hex_digits.is_empty() {
        return Err(LexicalError::InvalidIntegerFormat {
            source: slice.to_owned(),
            location: SourceLoc {
                line: 0, // TODO: Calculate the correct line number somehow
                start: lex.span().start,
                end: lex.span().end,
            },
            literal: slice.to_owned(),
        });
    }

    for c in hex_digits.chars() {
        if !c.is_ascii_hexdigit() {
            return Err(LexicalError::InvalidHexDigit {
                source: slice.to_owned(),
                location: SourceLoc {
                    line: 0, // TODO: Calculate the correct line number somehow
                    start: lex.span().start,
                    end: lex.span().end,
                },
                found: c,
            });
        }
    }

    Ok(())
}

#[derive(Debug, Clone, PartialEq, Logos)]
#[logos(error = LexicalError)]
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
    #[token("!")]
    Bang,
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
    #[token("extern")]
    KwExtern,
    #[token("mut")]
    KwMut,
    #[token("class")]
    KwClass,
    #[token("impl")]
    KwImpl,
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
    #[token("then")]
    KwThen,
    #[token("else")]
    KwElse,
    #[token("for")]
    KwFor,
    #[token("in")]
    KwIn,
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

    // Literals
    #[regex(r"[_a-zA-Z][_0-9a-zA-Z]*")]
    Name,
    #[regex(r"[-]?[0-9][0-9]*")]
    IntLit,
    #[regex(r"[-]?[0-9]+\.[0-9]+")]
    FloatLit,
    #[regex(r"0[xX][0-9a-fA-F]+", |lex| valid_hex_literal(lex))]
    HexIntLit,
    #[regex(r#""(\\[\\"]|[^"])*""#, |lex| valid_string_literal(lex))]
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

#[derive(Debug, Clone)]
pub struct Token<'a> {
    pub kind: TokenKind,
    pub location: SourceLoc,
    pub literal: &'a str,
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

// Wrapper type for a peekable iterator over lexing results
pub type LexerIter<'a> = Peekable<Box<TokenIter<'a>>>;

/// Result type for lexing operations - either a valid token or a lexical error
pub type LexResult<'a> = Result<Token<'a>, LexicalError>;

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
    type Item = LexResult<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(|(kind, span)| {
            let location = self.get_location();
            match kind {
                Ok(kind) => Ok(Token {
                    kind,
                    location,
                    literal: &self.src[span],
                }),

                // We handle lexical errors in the lexer itself
                Err(lex_error) => Err(lex_error),
            }
        })
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
