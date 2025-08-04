use crate::ast::ast_types::Ty;
use crate::resolver::resolver::Resolver;
use crate::resolver::semantic_error::SemanticError;
use insta::assert_snapshot;

#[test]
fn basic_variable_definition() {
    let mut resolver = Resolver::new();

    // Define a variable in global scope
    resolver.define_variable("x".to_string(), Ty::Int).unwrap();

    // Snapshot the resolver state
    assert_snapshot!(format!("{:#?}", resolver));
}

#[test]
fn errors_on_redefinition() {
    let mut resolver = Resolver::new();

    resolver.define_variable("x".to_string(), Ty::Int).unwrap();

    // Attempt to redefine the variable
    let result = resolver.define_variable("x".to_string(), Ty::String);

    // Snapshot the result and resolver state
    assert_snapshot!(format!("Result: {:#?}\nResolver: {:#?}", result, resolver));
}

#[test]
fn valid_nested_scopes() {
    let mut resolver = Resolver::new();

    // Define variable in global scope
    resolver.define_variable("x".to_string(), Ty::Int).unwrap();

    // Push new scope
    resolver.push_scope();

    // Define new variable in inner scope
    resolver
        .define_variable("y".to_string(), Ty::String)
        .unwrap();

    // Pop scope
    resolver.pop_scope().unwrap();

    // Snapshot the resolver state
    assert_snapshot!(format!("{:#?}", resolver));
}

#[test]
fn variable_shadowing() {
    let mut resolver = Resolver::new();

    // Define variable in global scope
    resolver.define_variable("x".to_string(), Ty::Int).unwrap();

    // Push new scope and shadow the variable
    resolver.push_scope();
    resolver
        .define_variable("x".to_string(), Ty::String)
        .unwrap();

    // Pop scope
    resolver.pop_scope().unwrap();

    // Snapshot the resolver state
    assert_snapshot!(format!("{:#?}", resolver));
}

#[test]
fn does_allow_redefinition() {
    let mut resolver = Resolver::new();

    resolver.define_variable("x".to_string(), Ty::Int).unwrap();

    // Redefine the variable
    resolver.define_or_redefine_variable("x".to_string(), Ty::String);

    // Snapshot the resolver state
    assert_snapshot!(format!("{:#?}", resolver));
}

#[test]
fn errors_on_empty_scope() {
    let mut resolver = Resolver::new();

    // Attempt to pop the global scope
    let result = resolver.pop_scope();

    // Snapshot the result and resolver state
    assert_snapshot!(format!("Result: {:#?}\nResolver: {:#?}", result, resolver));
}

#[test]
fn undefined_variable() {
    let resolver = Resolver::new();

    // Snapshot the lookup result
    assert_snapshot!(format!(
        "Lookup: {:#?}",
        resolver.lookup_variable("undefined")
    ));
}

#[test]
fn function_definition_and_lookup() {
    let mut resolver = Resolver::new();

    // Define a function in global scope
    let func_ty = Ty::Fn(Box::new(crate::ast::ast_types::Function {
        name: "test_func".to_string(),
        args: vec![("x".to_string(), Ty::Int)],
        return_ty: Ty::String,
        body: None,
        body_expr: None,
    }));

    resolver
        .define_function("test_func".to_string(), func_ty.clone())
        .unwrap();

    // Snapshot the resolver state
    assert_snapshot!(format!("{:#?}", resolver));
}

#[test]
fn function_redefinition_error() {
    let mut resolver = Resolver::new();

    let func_ty1 = Ty::Fn(Box::new(crate::ast::ast_types::Function {
        name: "test_func".to_string(),
        args: vec![("x".to_string(), Ty::Int)],
        return_ty: Ty::String,
        body: None,
        body_expr: None,
    }));

    let func_ty2 = Ty::Fn(Box::new(crate::ast::ast_types::Function {
        name: "test_func".to_string(),
        args: vec![("y".to_string(), Ty::String)],
        return_ty: Ty::Int,
        body: None,
        body_expr: None,
    }));

    resolver
        .define_function("test_func".to_string(), func_ty1)
        .unwrap();

    // Attempt to redefine the function
    let result = resolver.define_function("test_func".to_string(), func_ty2);

    // Snapshot the result and resolver state
    assert_snapshot!(format!("Result: {:#?}\nResolver: {:#?}", result, resolver));
}

#[test]
fn valid_function_scoping() {
    let mut resolver = Resolver::new();

    let func_ty = Ty::Fn(Box::new(crate::ast::ast_types::Function {
        name: "global_func".to_string(),
        args: vec![],
        return_ty: Ty::Unit,
        body: None,
        body_expr: None,
    }));

    // Define function in global scope
    resolver
        .define_function("global_func".to_string(), func_ty.clone())
        .unwrap();

    // Push new scope
    resolver.push_scope();

    // Define local function
    let local_func_ty = Ty::Fn(Box::new(crate::ast::ast_types::Function {
        name: "local_func".to_string(),
        args: vec![],
        return_ty: Ty::Int,
        body: None,
        body_expr: None,
    }));

    resolver
        .define_function("local_func".to_string(), local_func_ty.clone())
        .unwrap();

    // Pop scope
    resolver.pop_scope().unwrap();

    // Snapshot the resolver state
    assert_snapshot!(format!("{:#?}", resolver));
}
