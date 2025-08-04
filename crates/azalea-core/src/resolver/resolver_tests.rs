use crate::ast::ast_types::Ty;
use crate::resolver::resolver::Resolver;
use crate::resolver::semantic_error::SemanticError;

#[test]
fn test_basic_variable_definition() {
    let mut resolver = Resolver::new();

    // Define a variable in global scope
    resolver.define_variable("x".to_string(), Ty::Int).unwrap();

    // Should be able to look it up
    assert_eq!(resolver.lookup_variable("x"), Some(&Ty::Int));
    assert!(resolver.is_variable_defined("x"));
}

#[test]
fn test_redefinition_error() {
    let mut resolver = Resolver::new();

    resolver.define_variable("x".to_string(), Ty::Int).unwrap();

    // Should error on redefinition in same scope
    let result = resolver.define_variable("x".to_string(), Ty::String);
    assert!(result.is_err());

    if let Err(SemanticError::RedefinedVariable(name)) = result {
        assert_eq!(name, "x");
    } else {
        panic!("Expected RedefinedVariable error");
    }
}

#[test]
fn test_nested_scopes() {
    let mut resolver = Resolver::new();

    // Define variable in global scope
    resolver.define_variable("x".to_string(), Ty::Int).unwrap();

    // Push new scope
    resolver.push_scope();
    assert_eq!(resolver.scope_depth(), 1);

    // Should still see global variable
    assert_eq!(resolver.lookup_variable("x"), Some(&Ty::Int));

    // Define new variable in inner scope
    resolver
        .define_variable("y".to_string(), Ty::String)
        .unwrap();
    assert_eq!(resolver.lookup_variable("y"), Some(&Ty::String));

    // Pop scope
    resolver.pop_scope().unwrap();
    assert_eq!(resolver.scope_depth(), 0);

    // Should no longer see inner variable
    assert_eq!(resolver.lookup_variable("y"), None);
    assert_eq!(resolver.lookup_variable("x"), Some(&Ty::Int));
}

#[test]
fn test_variable_shadowing() {
    let mut resolver = Resolver::new();

    // Define variable in global scope
    resolver.define_variable("x".to_string(), Ty::Int).unwrap();

    // Push new scope and shadow the variable
    resolver.push_scope();
    resolver
        .define_variable("x".to_string(), Ty::String)
        .unwrap();

    // Should see the inner variable (String)
    assert_eq!(resolver.lookup_variable("x"), Some(&Ty::String));

    // Pop scope
    resolver.pop_scope().unwrap();

    // Should see the outer variable (Int) again
    assert_eq!(resolver.lookup_variable("x"), Some(&Ty::Int));
}

#[test]
fn test_allow_redefinition() {
    let mut resolver = Resolver::new();

    resolver.define_variable("x".to_string(), Ty::Int).unwrap();

    // This should work (for let statements that allow redefinition)
    resolver.define_or_redefine_variable("x".to_string(), Ty::String);

    assert_eq!(resolver.lookup_variable("x"), Some(&Ty::String));
}

#[test]
fn test_empty_scope_error() {
    let mut resolver = Resolver::new();

    // Should not be able to pop the global scope
    let result = resolver.pop_scope();
    assert!(result.is_err());

    if let Err(SemanticError::EmptyScope) = result {
        // Expected
    } else {
        panic!("Expected EmptyScope error");
    }
}

#[test]
fn test_undefined_variable() {
    let resolver = Resolver::new();

    assert_eq!(resolver.lookup_variable("undefined"), None);
    assert!(!resolver.is_variable_defined("undefined"));
}

#[test]
fn test_function_definition_and_lookup() {
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

    // Should be able to look it up
    assert_eq!(resolver.lookup_function("test_func"), Some(&func_ty));
    assert!(resolver.is_function_defined("test_func"));
}

#[test]
fn test_function_redefinition_error() {
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

    // Should error on redefinition in same scope
    let result = resolver.define_function("test_func".to_string(), func_ty2);
    assert!(result.is_err());

    if let Err(SemanticError::RedefinedVariable(name)) = result {
        assert_eq!(name, "test_func");
    } else {
        panic!("Expected RedefinedVariable error");
    }
}

#[test]
fn test_function_scoping() {
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

    // Should still see global function
    assert_eq!(resolver.lookup_function("global_func"), Some(&func_ty));

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
    assert_eq!(resolver.lookup_function("local_func"), Some(&local_func_ty));

    // Pop scope
    resolver.pop_scope().unwrap();

    // Should no longer see local function
    assert_eq!(resolver.lookup_function("local_func"), None);
    // But should still see global function
    assert_eq!(resolver.lookup_function("global_func"), Some(&func_ty));
}
