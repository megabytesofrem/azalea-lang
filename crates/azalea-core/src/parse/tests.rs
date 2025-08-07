use insta::assert_snapshot;

use crate::ast::pretty::{Pretty, pretty_with_loc};
use crate::{parse_expr, parse_stmt};

macro_rules! assert_success {
    ($src:expr) => {
        let expr = crate::parse_expr!($src).unwrap();
        let output = format!(
            "SOURCE:\n{}\n\nEXPRESSION:\n{}",
            $src,
            pretty_with_loc(&expr.target, &expr.loc)
        );
        assert_snapshot!(output);
    };
}

macro_rules! assert_error {
    ($src:expr) => {
        let error = crate::parse::tests::expect_error($src, None);
        let output = format!("SOURCE:\n{}\n\nERROR:\n{}", $src, error);
        assert_snapshot!(output);
    };
    ($src:expr, $hint:expr) => {
        let error = crate::parse::tests::expect_error($src, Some($hint));
        let output = format!("SOURCE:\n{}\n\nERROR:\n{}", $src, error);
        assert_snapshot!(output);
    };
}

pub fn expect_error(src: &str, hint: Option<&str>) -> String {
    let tokens = crate::lexer::lex_tokens(src);
    let mut parser = crate::parse::Parser::new(tokens);

    let result = parser.parse_expr();

    match result {
        Ok(_) => panic!("Expected an error, but got a valid expression"),
        Err(e) => {
            let mut report = e.report();

            if let Some(hint) = hint {
                report.hint = Some(hint.to_string());
            }
            report.source = src.to_string();

            report.show_pretty_source()
        }
    }
}

// Test cases
#[test]
fn invalid_identifier() {
    assert_error!(
        "$florbble",
        "Identifiers cannot start with a special character"
    );
    assert_error!(
        "#1abcdef",
        "Identifiers cannot start with a special character"
    );
}

#[test]
fn bad_string_literal() {
    assert_error!("\"Hello, world!", "Unterminated string literal");
    assert_error!(
        "\"Hello, \\xworld\"",
        "Invalid escape sequence in string literal"
    );
}

#[test]
fn literals() {
    assert_success!("42");
    assert_success!("0x123");
    assert_success!("3.14");
    assert_success!("\"Hello, world!\"");
}

#[test]
fn binary_and_unary_expr() {
    assert_success!("!42");
    assert_success!("1 + 2 * 3");
    // assert_success!("(1 + 2) * 3");
}

#[test]
fn array() {
    assert_success!("[1, 2, 3]");
}

#[test]
fn if_expr() {
    let expr = parse_expr!("if 1 == 1 then 42 end");
    println!("{:#?}", expr);

    assert_success!("if 1 == 1 then 42 end");
}

#[test]
fn lambda_expr() {
    assert_success!("\\(x) -> x + 1");
    assert_success!("\\(x, y) -> x + y");
}

#[test]
fn complex_literals() {
    assert_success!("[1, 2, 3]");
    assert_success!(".{ a: 1, b: 2 }");
    assert_success!(".{ a: 1, b: [2, 3] }");
}

#[test]
fn let_stmt() {
    let stmts = [
        "let x = 42",
        "let x: Int = 42",
        "let name: String = \"Charlotte\"",
        "let lottery_numbers = [21, 38, 42, 14]",
    ];

    for stmt in stmts {
        let result = parse_stmt!(stmt);
        let output = format!("SOURCE:\n{}\n\nAST:\n{:#?}", stmt, result);
        assert_snapshot!(output);
    }
}

#[test]
fn for_loop() {
    let src = "for x in [1, 2, 3] do let x = 5 end";
    let result = parse_stmt!(src);
    let output = format!("SOURCE:\n{}\n\nAST:\n{:#?}", src, result);
    assert_snapshot!(output);
}
