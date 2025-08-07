use crate::ast::ast_types::{Function, Record, Ty};
use crate::ast::{Expr, Literal, Stmt};
use crate::lexer::SourceLoc;
use crate::parse::span::{Span, spanned};
use crate::typeck::typecheck::{Typechecker, TypingEnv};
use insta::assert_snapshot;

macro_rules! assert_typecheck_success {
    ($src:expr) => {{
        let stmt = crate::parse_stmt!($src).unwrap();
        let mut tc = Typechecker::new();
        let mut env = TypingEnv::new();
        let result = tc.check(&mut env, &stmt.clone(), stmt.loc);
        let output = format!(
            "SOURCE:\n{}\n\nRESULT:\n{:#?}\n\nENV:\n{:#?}",
            $src, result, env
        );
        assert_snapshot!(output);
    }};
}

macro_rules! assert_infer_success {
    ($expr:expr) => {{
        let mut tc = Typechecker::new();
        let mut env = TypingEnv::new();
        let result = tc.infer_type(&mut env, &$expr, SourceLoc::default());
        let output = format!(
            "EXPR:\n{:#?}\n\nRESULT:\n{:#?}\n\nENV:\n{:#?}",
            $expr, result, env
        );
        assert_snapshot!(output);
    }};
}

macro_rules! assert_unify_success {
    ($ty1:expr, $ty2:expr) => {{
        let mut tc = Typechecker::new();
        let result = tc.unify(&$ty1, &$ty2, SourceLoc::default());
        let output = format!(
            "TYPE1:\n{:#?}\n\nTYPE2:\n{:#?}\n\nRESULT:\n{:#?}",
            $ty1, $ty2, result
        );
        assert_snapshot!(output);
    }};
}

macro_rules! assert_occurs_check {
    ($var:expr, $ty:expr) => {{
        let tc = Typechecker::new();
        let result = tc.occurs_check($var, &$ty);
        let output = format!(
            "VAR: {}\n\nTYPE:\n{:#?}\n\nRESULT:\n{:#?}",
            $var, $ty, result
        );
        assert_snapshot!(output);
    }};
}
#[test]
fn does_type_occur_in_var() {
    assert_occurs_check!("a", Ty::Var("a".to_string()));
}

#[test]
fn does_type_occur_in_ctor() {
    assert_occurs_check!(
        "a",
        Ty::TypeCons("a".to_string(), vec![Ty::Var("a".to_string())])
    );
}

#[test]
fn primitive_types_are_not_infinite() {
    assert_occurs_check!("a", Ty::Int);
    assert_occurs_check!("a", Ty::Float);
    assert_occurs_check!("a", Ty::String);
    assert_occurs_check!("a", Ty::Bool);
    assert_occurs_check!("a", Ty::Unit);
}

#[test]
fn can_unify_valid_type_ctor() {
    assert_unify_success!(
        Ty::TypeCons("List".to_string(), vec![Ty::Var("a".to_string())]),
        Ty::TypeCons("List".to_string(), vec![Ty::Int])
    );
}

#[test]
fn cannot_unify_infinite_type_ctor() {
    assert_unify_success!(
        Ty::TypeCons("A".to_string(), vec![Ty::Var("a".to_string())]),
        Ty::TypeCons("a".to_string(), vec![Ty::Var("a".to_string())])
    );
}

#[test]
fn can_unify_arrays() {
    assert_unify_success!(
        Ty::Array(Box::new(Ty::Var("a".to_string()))),
        Ty::Array(Box::new(Ty::Int))
    );
}

#[test]
fn can_unify_records() {
    assert_unify_success!(
        Ty::Record(Box::new(Record {
            name: "a".to_string(),
            fields: vec![],
        })),
        Ty::Record(Box::new(Record {
            name: "b".to_string(),
            fields: vec![],
        }))
    );
}

#[test]
fn cannot_unify_records_with_different_fields() {
    assert_unify_success!(
        Ty::Record(Box::new(Record {
            name: "Person".to_string(),
            fields: vec![("age".to_string(), Ty::Int)],
        })),
        Ty::Record(Box::new(Record {
            name: "Person".to_string(),
            fields: vec![("name".to_string(), Ty::String)],
        }))
    );
}

#[test]
fn typecheck_for() {
    assert_typecheck_success!("for x in [1, 2, 3] do let x = 5 end");
}

#[test]
fn typecheck_while() {
    assert_typecheck_success!("while 1==1 do let x = 5 end");
}

#[test]
fn typecheck_fn_type() {
    let mut tc = Typechecker::new();
    let mut env = TypingEnv::new();

    let body_stmts: Vec<Span<Stmt>> = vec![
        spanned(
            Stmt::Expr(spanned(
                Expr::Literal(Literal::Int(50)),
                SourceLoc::default(),
            )),
            SourceLoc::default(),
        ),
        spanned(
            Stmt::Expr(spanned(
                Expr::Literal(Literal::Int(100)),
                SourceLoc::default(),
            )),
            SourceLoc::default(),
        ),
    ];

    let args = vec![("y".to_string(), Ty::Int)];
    let declared_func = Function::new_with_stmts("f".to_string(), args, Ty::Unresolved, body_stmts);

    let stmt = spanned(Stmt::FnDecl(declared_func), SourceLoc::default());
    let result = tc.check(&mut env, &stmt, SourceLoc::default());

    let output = format!("RESULT:\n{:#?}\n\nENV:\n{:#?}", result, env);
    assert_snapshot!(output);
}

#[test]
fn typecheck_lambda() {
    let lambda_expr = Expr::Lam {
        args: vec![("a".to_string(), Ty::Var("a".to_string()))],
        return_ty: Ty::Var("a".to_string()),
        body: Box::new(spanned(Expr::Ident("a".to_string()), SourceLoc::default())),
    };

    assert_infer_success!(lambda_expr);
}

#[test]
fn infer_most_general_types() {
    let mut tc = Typechecker::new();
    let mut env = TypingEnv::new();

    let f = Function::new_with_expr(
        "id".to_string(),
        vec![("x".to_string(), Ty::Var("x".to_string()))],
        Ty::Unresolved,
        Box::new(spanned(Expr::Ident("x".to_string()), SourceLoc::default())),
    );

    let fn_decl = Stmt::FnDecl(f.clone());
    let result = tc.check(
        &mut env,
        &spanned(fn_decl, SourceLoc::default()),
        SourceLoc::default(),
    );

    let output = format!("RESULT:\n{:#?}\n\nENV:\n{:#?}", result, env);
    assert_snapshot!(output);
}
