#[allow(unused_imports)]
use crate::ast::ast_types::{Function, Ty};
use crate::ast::{Expr, Literal, Stmt};
use crate::parse::span::Span;
use crate::{lexer::SourceLoc, parse::span::spanned};

use crate::typeck::typecheck::{Typechecker, TypingEnv};
use insta::{assert_snapshot, assert_yaml_snapshot};

// Shorthand for creating a new `Typechecker`, since we do it everywhere
// in these tests.
#[allow(dead_code)]
fn tc() -> Typechecker {
    Typechecker::new()
}

#[test]
fn does_type_occur_in_var() {
    let tc = tc();

    // `a` is bound in the type constructor `a`, _and_ the type constructor _is_ `a`
    // so this is an infinite type
    let result = tc.occurs_check("a", &Ty::Var("a".to_string()));
    assert_snapshot!(format!("Result: {:#?}", result));
}

#[test]
fn does_type_occur_in_ctor() {
    let tc = tc();

    // `a` is bound in the type constructor `a`, _and_ the type constructor _is_ `a`
    // so this is an infinite type
    let result = tc.occurs_check(
        "a",
        &Ty::TypeCons("a".to_string(), vec![Ty::Var("a".to_string())]),
    );
    assert_snapshot!(format!("Result: {:#?}", result));
}

#[test]
fn primitive_types_are_not_infinite() {
    let tc = tc();

    // Primitive types can never be infinite
    let results = vec![
        tc.occurs_check("a", &Ty::Int),
        tc.occurs_check("a", &Ty::Float),
        tc.occurs_check("a", &Ty::String),
        tc.occurs_check("a", &Ty::Bool),
        tc.occurs_check("a", &Ty::Unit),
    ];
    assert_snapshot!(format!("Results: {:#?}", results));
}

// Unification tests

#[test]
fn can_unify_valid_type_ctor() {
    let mut tc = tc();

    // Unifying List[a] with List[Int] → a = Int
    let result = tc.unify(
        &Ty::TypeCons("List".to_string(), vec![Ty::Var("a".to_string())]),
        &Ty::TypeCons("List".to_string(), vec![Ty::Int]),
        SourceLoc::default(),
    );
    assert_snapshot!(format!("Result: {:#?}", result));
}

#[test]
fn cannot_unify_infinite_type_ctor() {
    let mut tc = tc();

    // This is an infinite type so unification should fail
    let result = tc.unify(
        &Ty::TypeCons("A".to_string(), vec![Ty::Var("a".to_string())]),
        &Ty::TypeCons("a".to_string(), vec![Ty::Var("a".to_string())]),
        SourceLoc::default(),
    );
    assert_snapshot!(format!("Result: {:#?}", result));
}

#[test]
fn can_unify_arrays() {
    let mut tc = tc();

    // Unifying Array[a] with Array[Int] → a = Int
    let result = tc.unify(
        &Ty::Array(Box::new(Ty::Var("a".to_string()))),
        &Ty::Array(Box::new(Ty::Int)),
        SourceLoc::default(),
    );
    assert_snapshot!(format!("Result: {:#?}", result));
}

#[test]
fn can_unify_records() {
    use crate::ast::ast_types::Record;
    let mut tc = tc();

    // Unifying Record[a] with Record[Int] → a = Int
    let result = tc.unify(
        &Ty::Record(Box::new(Record {
            name: "a".to_string(),
            fields: vec![],
        })),
        &Ty::Record(Box::new(Record {
            name: "b".to_string(),
            fields: vec![],
        })),
        SourceLoc::default(),
    );
    assert_snapshot!(format!("Result: {:#?}", result));
}

#[test]
fn cannot_unify_records_with_different_fields() {
    use crate::ast::ast_types::Record;
    let mut tc = tc();

    let result = tc.unify(
        &Ty::Record(Box::new(Record {
            name: "Person".to_string(),
            fields: vec![("age".to_string(), Ty::Int)],
        })),
        &Ty::Record(Box::new(Record {
            name: "Person".to_string(),
            fields: vec![("name".to_string(), Ty::String)],
        })),
        SourceLoc::default(),
    );
    assert_snapshot!(format!("Result: {:#?}", result));
}

#[test]
fn infer_fn_type_from_body() {
    let mut tc = tc();
    let mut env = TypingEnv::new();

    let body_stmts: Vec<Span<Stmt>> = vec![
        // Perhaps some statement that doesn't produce a value...
        spanned(
            Stmt::Expr(spanned(
                Expr::Literal(Literal::Int(50)),
                SourceLoc::default(),
            )),
            SourceLoc::default(),
        ),
        // The final statement (an expression) that results in 100.
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
    assert_snapshot!(format!("Result: {:#?}\nEnv: {:#?}", result, env));
}

#[test]
fn infer_fn_type_lambda() {
    let mut tc = tc();
    let mut env = TypingEnv::new();

    // The lambda is `id : a -> a`
    let lambda_expr = Expr::Lam {
        args: vec![("a".to_string(), Ty::Var("a".to_string()))],
        return_ty: Ty::Var("a".to_string()),
        body: Box::new(spanned(Expr::Ident("a".to_string()), SourceLoc::default())),
    };

    let result = tc.infer_type(&mut env, &lambda_expr, SourceLoc::default());
    assert_snapshot!(format!("Result: {:#?}\nEnv: {:#?}", result, env));
}

#[test]
fn infer_most_general_types() {
    let mut tc = tc();
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
    assert_snapshot!(format!("Result: {:#?}\nEnv: {:#?}", result, env));
}
