use insta::assert_snapshot;

/// Macros for assertions
macro_rules! parse_expr {
    ($src:expr) => {{
        let tokens = crate::lexer::lex_tokens($src);
        let mut parser = crate::parse::Parser::new(tokens);
        parser.parse_expr()
    }};
}

macro_rules! assert_success {
    ($src:expr) => {
        let expr = parse_expr!($src).unwrap();
        let output = format!("SOURCE:\n{}\n\nEXPRESSION:\n{:#?}", $src, expr);
        assert_snapshot!(output);
    };
}

macro_rules! assert_error {
    ($src:expr) => {
        let error = crate::parse::parser_tests::expect_error($src);
        let output = format!("SOURCE:\n{}\n\nERROR:\n{}", $src, error);
        assert_snapshot!(output);
    };
}

pub fn expect_error(src: &str) -> String {
    let tokens = crate::lexer::lex_tokens(src);
    let mut parser = crate::parse::Parser::new(tokens);

    let result = parser.parse_expr();

    match result {
        Ok(_) => panic!("Expected an error, but got a valid expression"),
        Err(e) => {
            let mut report = e.report();
            report.source = src.to_string();

            report.show_pretty_source()
        }
    }
}

// Test cases
#[test]
fn invalid_identifier() {
    assert_error!("$florbble");
    assert_error!("#1abcdef");
}

#[test]
fn parse_literal() {
    assert_success!("42");
    assert_success!("0x123");
    assert_success!("3.14");
    assert_success!("\"Hello, world!\"");
}

#[test]
fn parse_binary_expr() {
    assert_success!("1 + 2 * 3");
    // assert_success!("(1 + 2) * 3");
}

#[test]
fn parse_unary_expr() {
    assert_success!("!42");
}

#[test]
fn parse_array() {
    assert_success!("[1, 2, 3]");
}

#[test]
fn parse_if_expr() {
    let expr = parse_expr!("if 1 == 1 then 42 end");
    println!("{:#?}", expr);

    assert_success!("if 1 == 1 then 42 end");
}

#[test]
fn parse_lambda_expr() {
    assert_success!("\\(x) -> x + 1");
}
