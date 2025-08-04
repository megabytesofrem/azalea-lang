use crate::ast::ast_types::Ty;
use crate::ast::{Expr, Literal, Stmt};
use crate::lexer::{Op, SourceLoc};
use crate::parse::span::Span;
use insta::assert_snapshot;

#[test]
fn parse_literal() {
    let literal = Literal::Int(42);
    let span = Span::new(literal.clone(), SourceLoc::default());
    assert_snapshot!(format!("{:#?}", span));
}

#[test]
fn parse_binary_expr() {
    let left = Span::new(Expr::Literal(Literal::Int(1)), SourceLoc::default());
    let right = Span::new(Expr::Literal(Literal::Int(2)), SourceLoc::default());
    let bin_expr = Expr::BinOp(Box::new(left), Op::Add, Box::new(right));
    assert_snapshot!(format!("{:#?}", bin_expr));
}

#[test]
fn parse_unary_expr() {
    let operand = Span::new(Expr::Literal(Literal::Int(3)), SourceLoc::default());
    let unary_expr = Expr::UnOp(Op::Neg, Box::new(operand));
    assert_snapshot!(format!("{:#?}", unary_expr));
}

#[test]
fn parse_array() {
    let arr_elements = vec![
        Span::new(Expr::Literal(Literal::Int(1)), SourceLoc::default()),
        Span::new(Expr::Literal(Literal::Int(2)), SourceLoc::default()),
    ];
    let array_expr = Expr::Array {
        elements: arr_elements,
    };
    assert_snapshot!(format!("{:#?}", array_expr));
}

#[test]
fn parse_if_expr() {
    let cond = Span::new(Expr::Literal(Literal::Bool(true)), SourceLoc::default());
    let then_branch = vec![Span::new(
        Stmt::Expr(Span::new(
            Expr::Literal(Literal::Int(1)),
            SourceLoc::default(),
        )),
        SourceLoc::default(),
    )];
    let else_branch = Some(vec![Span::new(
        Stmt::Expr(Span::new(
            Expr::Literal(Literal::Int(0)),
            SourceLoc::default(),
        )),
        SourceLoc::default(),
    )]);

    let if_expr = Expr::If {
        cond: Box::new(cond),
        then: then_branch,
        else_: else_branch,
    };
    assert_snapshot!(format!("{:#?}", if_expr));
}

#[test]
fn parse_lambda() {
    let params = vec![("x".to_string(), Ty::Int)];
    let lam_body = Box::new(Span::new(
        Expr::Literal(Literal::Int(42)),
        SourceLoc::default(),
    ));

    let lambda_expr = Expr::Lam {
        args: params,
        return_ty: Ty::Int,
        body: lam_body,
    };
    assert_snapshot!(format!("{:#?}", lambda_expr));
}
