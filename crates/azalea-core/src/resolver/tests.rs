use crate::ast::ast_types::Ty;
use crate::resolver;
use crate::resolver::error::SemanticError;
use crate::resolver::resolver::Resolver;
use insta::assert_snapshot;

macro_rules! assert_define {
    ($r:expr, $name:expr, $ty:expr) => {{
        if !$r.is_variable_defined($name) {
            $r.define_or_redefine_variable($name.to_string(), $ty.clone());
            let result_str = format!("RESULT:\n\n'{}' defined with type: {:#?}", $name, $ty);
            assert_snapshot!(format!("{}", result_str));
        } else {
            let result_str = format!("ERROR:\n\n'{}' is already defined", $name);
            assert_snapshot!(format!("{}", result_str));
        }
    }};
}

macro_rules! assert_error {
    ($result:expr, $expect_err:pat) => {{
        match $result {
            Ok(_) => panic!("Expected an error, but got success"),
            Err(err) => {
                let report = report_error(err.clone());
                assert!(matches!(err, $expect_err));
                assert_snapshot!(format!("ERROR:\n\n{}", report));
            }
        }
    }};
}

macro_rules! assert_lookup {
    ($r:expr, $name:expr) => {{
        let lookup = $r.lookup_variable($name);
        let result_str = lookup.map_or_else(
            || format!("ERROR:\n\nVariable '{}' not found", $name),
            |ty| format!("RESULT:\n\n'{}' has type: {:#?}", $name, ty),
        );
        let snapshot = format!("{}", result_str);
        assert_snapshot!(snapshot);
        lookup
    }};
}

fn define_function(
    resolver: &mut Resolver,
    name: &str,
    args: Vec<(String, Ty)>,
    return_ty: Ty,
) -> Result<(), SemanticError> {
    let function_ty = Ty::Fn(Box::new(crate::ast::ast_types::Function {
        name: name.to_string(),
        args,
        return_ty,
        body: None,
        body_expr: None,
    }));

    resolver.define_function(name.to_string(), function_ty)?;
    Ok(())
}

fn report_error_source(err: SemanticError) -> String {
    let mut report = err.report();
    report.source = "<source>".to_string();
    report.show_pretty_source()
}

fn report_error(err: SemanticError) -> String {
    let mut report = err.report();
    report.source = "<source>".to_string();
    report.show_pretty_message()
}

#[test]
fn basic_variable_definition() {
    let mut resolver = Resolver::new();

    // Define a variable in global scope
    assert_define!(resolver, "x", Ty::Int);
    assert_lookup!(resolver, "x");
}

#[test]
fn errors_on_redefinition() {
    let mut resolver = Resolver::new();

    assert_define!(resolver, "x", Ty::Int);

    // Attempt to redefine the variable
    assert_define!(resolver, "x", Ty::String);
}

#[test]
fn valid_nested_scopes() {
    let mut resolver = Resolver::new();

    // Define variable in global scope
    assert_define!(resolver, "x", Ty::Int);

    // Push new scope
    resolver.push_scope();
    assert_define!(resolver, "y", Ty::String);

    // Pop scope
    resolver.pop_scope().unwrap();

    assert_snapshot!(format!("STATE: {:#?}", resolver));
}

#[test]
fn variable_shadowing() {
    let mut resolver = Resolver::new();

    // Define variable in global scope
    assert_define!(resolver, "x", Ty::Int);

    // Push new scope and shadow the variable
    resolver.push_scope();
    assert_define!(resolver, "x", Ty::String);

    // Pop scope
    resolver.pop_scope().unwrap();

    assert_snapshot!(format!("STATE: {:#?}", resolver));
}

#[test]
fn errors_on_empty_scope() {
    let mut resolver = Resolver::new();

    // Attempt to pop the global scope
    let result = resolver.pop_scope();
    assert_error!(result, SemanticError::EmptyScope);
}

#[test]
fn undefined_variable() {
    let resolver = Resolver::new();

    // Snapshot the lookup result
    assert_error!(
        resolver
            .lookup_variable("undefined_var")
            .ok_or(SemanticError::UndefinedVariable(
                "undefined_var".to_string()
            )),
        SemanticError::UndefinedVariable(_)
    );
}

#[test]
fn function_definition_and_lookup() {
    let mut resolver = Resolver::new();

    // Define a function in global scope
    let args = vec![("x".to_string(), Ty::Int)];
    let _ = define_function(&mut resolver, "test_func", args, Ty::String);

    // Snapshot the resolver state
    assert_snapshot!(format!("STATE: {:#?}", resolver));
}

#[test]
fn function_redefinition_error() {
    let mut resolver = Resolver::new();

    let args = vec![("x".to_string(), Ty::Int)];
    let _ = define_function(&mut resolver, "test_func", args, Ty::String);

    // Attempt to redefine the function with a different signature
    let args = vec![("y".to_string(), Ty::String)];
    let result = define_function(&mut resolver, "test_func", args, Ty::Int);

    // Snapshot the result and resolver state
    assert_error!(result, SemanticError::RedefinedVariable(_));
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
    assert_snapshot!(format!("STATE:\n---\n: {:#?}", resolver));
}
