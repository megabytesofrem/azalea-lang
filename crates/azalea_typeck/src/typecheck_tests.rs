use azalea_parse::ast::ast_types::Function;
#[allow(unused_imports)]
use azalea_parse::ast::{Expr, Literal, Stmt};
use azalea_parse::span::Span;
use azalea_parse::{ast::ast_types::Ty, lexer::SourceLoc, span::spanned};

use crate::typecheck::{Typechecker, TypingEnv};

fn tc() -> Typechecker {
    Typechecker::new()
}

#[test]
fn test_type_occurs_in_var() {
    let tc = tc();

    // `a` is bound in the type constructor `a`, _and_ the type constructor _is_ `a`
    // so this is an infinite type
    assert!(tc.occurs_check("a", &Ty::Var("a".to_string())) == true);
}

#[test]
fn test_type_occurs_in_ctor() {
    let tc = tc();

    // `a` is bound in the type constructor `a`, _and_ the type constructor _is_ `a`
    // so this is an infinite type
    assert!(
        tc.occurs_check(
            "a",
            &Ty::Constructed("a".to_string(), vec![Ty::Var("a".to_string())])
        ) == true
    );
}

#[test]
fn test_primitive_types_are_not_infinite() {
    let tc = tc();

    // Primitive types can never be infinite
    assert!(tc.occurs_check("a", &Ty::Int) == false);
    assert!(tc.occurs_check("a", &Ty::Float) == false);
    assert!(tc.occurs_check("a", &Ty::String) == false);
    assert!(tc.occurs_check("a", &Ty::Bool) == false);
    assert!(tc.occurs_check("a", &Ty::Unit) == false);
}

// Unification tests

#[test]
fn test_can_unify_valid_type_ctor() {
    let mut tc = tc();

    assert!(
        tc.unify(
            &Ty::Constructed("List".to_string(), vec![Ty::Var("a".to_string())]),
            &Ty::Constructed("List".to_string(), vec![Ty::Int]),
            SourceLoc::default()
        )
        .is_ok()
    );
}

#[test]
fn test_cannot_unify_infinite_type_ctor() {
    let mut tc = tc();

    // This is an infinite type
    assert!(
        tc.unify(
            &Ty::Constructed("A".to_string(), vec![Ty::Var("a".to_string())]),
            &Ty::Constructed("a".to_string(), vec![Ty::Var("a".to_string())]),
            SourceLoc::default()
        )
        .is_err()
    );
}

#[test]
fn test_can_unify_array() {
    let mut tc = tc();

    assert!(
        tc.unify(
            &Ty::Array(Box::new(Ty::Var("a".to_string()))),
            &Ty::Array(Box::new(Ty::Int)),
            SourceLoc::default()
        )
        .is_ok()
    );
}

#[test]
fn test_can_unify_record() {
    use azalea_parse::ast::ast_types::Record;
    let mut tc = tc();

    assert!(
        tc.unify(
            &Ty::Record(Box::new(Record {
                name: "a".to_string(),
                fields: vec![],
            })),
            &Ty::Record(Box::new(Record {
                name: "b".to_string(),
                fields: vec![],
            })),
            SourceLoc::default()
        )
        .is_ok()
    );
}

#[test]
fn test_cannot_unify_record_with_different_fields() {
    use azalea_parse::ast::ast_types::Record;
    let mut tc = tc();

    assert!(
        tc.unify(
            &Ty::Record(Box::new(Record {
                name: "Person".to_string(),
                fields: vec![("age".to_string(), Ty::Int)],
            })),
            &Ty::Record(Box::new(Record {
                name: "Person".to_string(),
                fields: vec![("name".to_string(), Ty::String)],
            })),
            SourceLoc::default()
        )
        .is_err()
    );
}

#[test]
fn test_infer_fn_type_from_body() {
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
    assert!(tc.check(&mut env, &stmt, SourceLoc::default()).is_ok());

    // Check that the inferred type is correct
    let f_ty = env.get("f").expect("function f not found");
    match f_ty {
        Ty::Fn(func) => {
            assert_eq!(func.args.len(), 1);
            assert_eq!(func.args[0].1, Ty::Int);
            assert_eq!(func.return_ty, Ty::Int);

            println!("Function type: {:?}", func);
        }
        _ => panic!("Expected function type, found {:?}", f_ty),
    }
}

#[test]
fn test_infer_most_general_types() {
    // Test to see if the typechecker can infer the most general types
    // fn id(x) = x, x is a type variable and the most general or polymorphic type

    let mut tc = tc();
    let mut env = TypingEnv::new();

    // `f` is a function that takes an int and returns an int
    let f = Function::new_with_expr(
        "id".to_string(),
        vec![("x".to_string(), Ty::Var("x".to_string()))],
        Ty::Unresolved,
        Box::new(spanned(Expr::Ident("x".to_string()), SourceLoc::default())),
    );

    let fn_decl = Stmt::FnDecl(f.clone());

    // Infer the type of `f`
    tc.check(
        &mut env,
        &spanned(fn_decl, SourceLoc::default()),
        SourceLoc::default(),
    )
    .expect("Failed to check function declaration");

    // Check that the inferred type is correct
    let f_ty = env.get("id").expect("function id not found");
    println!("f_ty: {:?}", f_ty);
}
