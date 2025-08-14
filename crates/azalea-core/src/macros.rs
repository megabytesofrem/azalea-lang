//! Macros used throughout the compiler

#[macro_export]
macro_rules! parse_expr {
    ($src:expr) => {{
        let tokens = crate::lexer::lex_tokens($src);
        let mut parser = crate::parse::Parser::new(tokens);
        parser.parse_expr()
    }};
}

#[macro_export]
macro_rules! parse_stmt {
    ($src:expr) => {{
        let tokens = crate::lexer::lex_tokens($src);
        let mut parser = crate::parse::Parser::new(tokens);
        parser.parse_stmt()
    }};
}

#[macro_export]
macro_rules! parse_toplevel {
    ($src:expr) => {{
        let tokens = crate::lexer::lex_tokens($src);
        let mut parser = crate::parse::Parser::new(tokens);
        parser.parse_toplevel_stmt()
    }};
}
