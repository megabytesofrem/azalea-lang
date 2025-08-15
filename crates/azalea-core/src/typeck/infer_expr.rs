use std::collections::{HashMap, HashSet};

use crate::ast::ast_types::{EnumVariantPayload, Function, Pattern, Record, RecordExpr, Ty};
use crate::ast::pretty::Pretty;
use crate::ast::{Expr, Literal};
use crate::lexer::{Op, SourceLoc};

use crate::parse::type_;
// Name and scope resolution
use crate::resolver::error::SemanticError;

use crate::resolver::Return;
use crate::typeck::typecheck::{Typechecker, TypingEnv};

impl Typechecker {
    /// Infer the type of an expression within a substitution environment `env`
    pub fn infer_type(
        &mut self,
        env: &mut TypingEnv,
        expr: &Expr,
        location: SourceLoc,
    ) -> Return<Ty> {
        let inferred_ty = match expr {
            Expr::Literal(lit) => self.infer_literal(lit, env, location.clone()),
            Expr::Ident(name) => {
                // First check if the variable exists in the resolver (for scope checking)
                let is_variable = self.resolver.is_variable_defined(name);
                let is_function = self.resolver.is_function_defined(name);

                if !is_variable && !is_function {
                    // Handle extern functions
                    if self.global_env.is_extern_function(name) {
                        let extern_decl = self.global_env.lookup_extern_function(name).unwrap();
                        return Ok(Ty::Fn(Box::new(extern_decl.clone())));
                    }

                    if self.global_env.is_enum_defined(name) {
                        // If it's an enum, we can return the enum type directly
                        let enum_ty = self.global_env.lookup_enum(name).unwrap();
                        return Ok(enum_ty.to_type(&mut self.clone()));
                    }

                    return Err(SemanticError::UndefinedVariable(name.clone()));
                }

                println!(
                    "DEBUG: Looking up type: {} from {}",
                    name,
                    self.resolver.scope_depth()
                );

                if self.global_env.is_record_defined(name) {
                    let record_ty = self.global_env.lookup_record(name).unwrap();
                    return Ok(record_ty.to_type(&mut self.clone()));
                }

                // Then get the type from the typing environment (which tracks current types during inference)
                let ty = env
                    .get(name)
                    .cloned()
                    .ok_or_else(|| SemanticError::UndefinedVariable(name.clone()))?;

                Ok(self.instantiate(&ty))
            }

            Expr::BinOp(lhs, Op::Dollar, rhs) => {
                // Special case for the application operator `$`
                // Desugar f $ g $ h(i) to f(g(h(i)))
                let fn_call = Expr::FnCall {
                    target: lhs.clone(),
                    args: vec![*rhs.clone()],
                };

                self.infer_type(env, &fn_call, location.clone())
            }

            Expr::BinOp(lhs, op, rhs) => {
                let lhs_ty = self.infer_type(env, &lhs.target, lhs.loc.clone())?;
                let rhs_ty = self.infer_type(env, &rhs.target, rhs.loc.clone())?;

                if op.is_comparison() {
                    // Special case: for comparison operators, we expect both sides to be of the same type
                    self.unify(&lhs_ty, &rhs_ty, location.clone())?;
                    Ok(Ty::Bool)
                } else {
                    // For other binary operations, we can assume that the types are compatible
                    // and we will unify them later in the function call.
                    let subst = self.unify(&lhs_ty, &rhs_ty, location.clone())?;
                    env.extend(subst);

                    Ok(lhs_ty)
                }
            }

            Expr::UnOp(_, expr) => {
                let ty = self.infer_type(env, &expr.target, expr.loc.clone())?;
                Ok(ty)
            }

            Expr::Record(record) => self.infer_record_expr(record, env, location.clone()),

            Expr::Array { elements } => {
                let mut element_types = Vec::new();
                for elem in elements {
                    let ty = self.infer_type(env, &elem.target, elem.loc.clone())?;
                    element_types.push(ty);
                }

                // Unify all the element types with the first type
                let first_ty = element_types.first().cloned().unwrap_or(Ty::Unit);

                if elements.len() > 1 {
                    // If there are multiple elements, we need to ensure they all have the same type
                    for ty in &element_types[1..] {
                        self.unify(&first_ty, ty, location.clone())?;
                    }
                }

                Ok(Ty::TypeCons("Array".to_string(), vec![first_ty]))
            }

            Expr::ArrayIndex { target, index } => {
                let target_ty = self.infer_type(env, &target.target, target.loc.clone())?;
                let index_ty = self.infer_type(env, &index.target, index.loc.clone())?;

                // Index should be Int
                self.unify(&index_ty, &Ty::Int, location.clone())?;

                // Extract the inner type of the array
                match target_ty {
                    Ty::TypeCons(name, params) if name == "Array" && params.len() == 1 => {
                        // If the target is an array type, return the inner type
                        Ok(params[0].clone())
                    }

                    // Strings can be treated as arrays of characters
                    Ty::String => Ok(Ty::String),

                    Ty::Array(inner) => {
                        // If the target is an array type, return the inner type
                        Ok(*inner.clone())
                    }
                    _ => {
                        // If the target is not an array type, we can't index it
                        Err(SemanticError::TypeMismatch {
                            expected: Ty::Array(Box::new(Ty::Any)),
                            found: target_ty,
                            location: target.loc.clone(),
                        })
                    }
                }
            }

            Expr::FnCall { target, args } => {
                // Infer the type of the function expression (could be a variable, lambda, or another pipeline)
                let func_ty = self.infer_type(env, &target.target, target.loc.clone())?;

                // For function types, check argument count and types
                match func_ty {
                    Ty::Fn(func) => {
                        if args.len() != func.args.len() {
                            return Err(SemanticError::ArityMismatch {
                                expected: func.args.len(),
                                found: args.len(),
                                location: target.loc.clone(),
                            });
                        }

                        // Instantiate generics if needed
                        let mut local_env = HashMap::new();
                        for ty_param in &func.type_params {
                            let fresh_var = self.fresh();
                            local_env.insert(ty_param.clone(), Ty::Var(fresh_var));
                        }
                        let instantiated_arg_types = func
                            .args
                            .iter()
                            .map(|(_, ty)| self.instantiate_type(ty, &local_env))
                            .collect::<Vec<_>>();
                        let instantiated_return_ty =
                            self.instantiate_type(&func.return_ty, &local_env);

                        for (arg, expected_ty) in args.iter().zip(instantiated_arg_types.iter()) {
                            let (arg_ty, unify_expected_ty) = match &arg.target {
                                Expr::Lam { .. } => {
                                    let subst_expected_ty = self.hydrate_type(expected_ty, env);

                                    let lambda_ty = self.infer_lam_with_type(
                                        env,
                                        &arg.target,
                                        Some(&subst_expected_ty),
                                        arg.loc.clone(),
                                    )?;

                                    // Unify against the substituted expected type, not the original
                                    (lambda_ty, subst_expected_ty)
                                }
                                _ => {
                                    let ty = self.infer_type(env, &arg.target, arg.loc.clone())?;
                                    (ty, expected_ty.clone())
                                }
                            };

                            let subst = self.unify(&unify_expected_ty, &arg_ty, arg.loc.clone())?;
                            env.extend(subst);
                        }

                        Ok(instantiated_return_ty)
                    }

                    _ => panic!(
                        "Expected a function type for function call, found: {}",
                        func_ty.pretty_with_loc(&location),
                    ),
                }
            }

            Expr::If { cond, then, else_ } => {
                // Infer the condition type
                let cond_ty = self.infer_type(env, &cond.target, cond.loc.clone())?;
                self.unify(&cond_ty, &Ty::Bool, cond.loc.clone())?;

                // Infer the types of both branches
                let then_ty = self.infer_block(then, env, location.clone())?;

                if let Some(else_) = else_ {
                    // If there's an else branch, infer its type as well
                    let else_ty = self.infer_block(else_, env, location.clone())?;

                    // Unify the types of both branches
                    self.unify(&then_ty, &else_ty, location.clone())?;
                } else {
                    // If there's no else branch, we can assume the type is Unit
                    let else_ty = Ty::Unit;

                    // Unify the types of both branches
                    self.unify(&then_ty, &else_ty, location.clone())?;
                }

                Ok(then_ty)
            }

            Expr::Match(pmatch) => {
                let scrutinee_ty =
                    self.infer_type(env, &pmatch.target.target, pmatch.target.loc.clone())?;

                let mut branch_result_ty = None;
                for branch in &pmatch.branches {
                    let mut branch_env = env.clone();

                    self.check_pattern(
                        &mut branch_env,
                        &branch.pattern,
                        &scrutinee_ty,
                        location.clone(),
                    )?;

                    // Type check the branch expression
                    let branch_ty = self.infer_type(
                        &mut branch_env,
                        &branch.expr.target,
                        branch.expr.loc.clone(),
                    )?;

                    if let Some(ref ty) = branch_result_ty {
                        // If we already have a branch type, unify it with the new one
                        self.unify(ty, &branch_ty, branch.expr.loc.clone())?;
                    } else {
                        branch_result_ty = Some(branch_ty);
                    }
                }

                branch_result_ty.ok_or(SemanticError::NoMatch)
            }

            Expr::Lam { .. } => self.infer_lam(&expr, env, location.clone()),

            Expr::MemberAccess(member) => {
                // For member access, we need to resolve the type of the target expression
                let target_ty =
                    self.infer_type(env, &member.target.target, member.target.loc.clone())?;

                match target_ty {
                    // Both records and enums are represented nominally as `TypeCons`
                    Ty::TypeCons(name, params) => {
                        println!(
                            "DEBUG: Member access on TypeCons: '{}' with params: {{{}}}",
                            name,
                            params
                                .clone()
                                .into_iter()
                                .map(|p| p.pretty())
                                .collect::<Vec<_>>()
                                .join(", ")
                        );

                        if self.global_env.is_record_defined(&name) {
                            let record = self.global_env.lookup_record(&name).unwrap();
                            println!(
                                "DEBUG: Found record '{}' with fields: {{{}}}",
                                record.name,
                                record
                                    .fields
                                    .iter()
                                    .map(|f| format!("{}: {}", f.0, f.1.pretty()))
                                    .collect::<Vec<_>>()
                                    .join(", ")
                            );

                            // Find the field and field type in the record
                            if let Some((_, field_ty)) = record
                                .fields
                                .iter()
                                .find(|(field_name, _)| field_name == &member.name)
                            {
                                println!(
                                    "DEBUG: Found field '{}' with type: {}",
                                    member.name,
                                    field_ty.pretty()
                                );

                                let mut instantiation_env = HashMap::new();

                                // Extract all type parameters from the record definition
                                let mut type_params_in_record = HashSet::new();
                                for (_, fty) in &record.fields {
                                    self.collect_type_params(fty, &mut type_params_in_record);
                                }

                                let type_params: Vec<String> =
                                    type_params_in_record.into_iter().collect();

                                // Map type parameters to actual types
                                for (i, param) in params.iter().enumerate() {
                                    if let Some(type_param_name) = type_params.get(i) {
                                        instantiation_env
                                            .insert(type_param_name.clone(), param.clone());
                                    }
                                }

                                println!(
                                    "DEBUG: Instantiation env for record '{}': {}",
                                    record.name,
                                    self.pretty_print_env(&instantiation_env)
                                );

                                Ok(self.instantiate_type(field_ty, &instantiation_env))
                            } else {
                                println!(
                                    "DEBUG: Field {} not found in record {}",
                                    member.name, name
                                );
                                println!(
                                    "DEBUG: Available fields: {:?}",
                                    record
                                        .fields
                                        .iter()
                                        .map(|(name, _)| name)
                                        .collect::<Vec<_>>()
                                );
                                Err(SemanticError::UndefinedFieldOrVariant {
                                    field: member.name.clone(),
                                    structure: name,
                                    location: member.target.loc.clone(),
                                })
                            }
                        } else if self.global_env.is_enum_defined(&name) {
                            let enum_decl = self.global_env.lookup_enum(&name).unwrap().clone();

                            if let Some(variant) =
                                enum_decl.variants.iter().find(|v| v.name == member.name)
                            {
                                match &variant.payload {
                                    EnumVariantPayload::None => {
                                        // If the variant has no payload, we can return the enum type directly
                                        return Ok(enum_decl.to_type(&mut self.clone()));
                                    }
                                    EnumVariantPayload::Tuple(arg_tys) => {
                                        // Constructor function like Maybe.Just -> Maybe[A]

                                        let arg_types = arg_tys
                                            .iter()
                                            .enumerate()
                                            .map(|(i, ty)| (format!("arg{}", i), ty.clone()))
                                            .collect();

                                        let func_ty = Function {
                                            name: format!("{}.{}", name, member.name),
                                            args: arg_types,
                                            type_params: enum_decl.type_params.clone(),
                                            return_ty: Ty::TypeCons(name.clone(), params.clone()),
                                            body: None,
                                            body_expr: None,
                                            where_bindings: vec![],
                                            is_extern: false,
                                            extern_name: None,
                                        };

                                        return Ok(Ty::Fn(Box::new(func_ty)));
                                    }
                                    EnumVariantPayload::Record(fields) => {
                                        // Record style constructor like Person { name: String, age: Int }
                                        let func_ty = Function {
                                            name: format!("{}.{}", name, member.name),
                                            args: fields.clone(),
                                            type_params: vec![],
                                            return_ty: Ty::TypeCons(name.clone(), params.clone()),
                                            body: None,
                                            body_expr: None,
                                            where_bindings: vec![],
                                            is_extern: false,
                                            extern_name: None,
                                        };

                                        return Ok(Ty::Fn(Box::new(func_ty)));
                                    }
                                }
                            } else {
                                println!("DEBUG: Record {} not found in resolver", name);
                                Err(SemanticError::UndefinedFieldOrVariant {
                                    field: member.name.clone(),
                                    structure: name,
                                    location: member.target.loc.clone(),
                                })
                            }
                        } else {
                            // If the type is not a record or enum, we can't access members
                            return Err(SemanticError::UndefinedFieldOrVariant {
                                field: member.name.clone(),
                                structure: name,
                                location: member.target.loc.clone(),
                            });
                        }
                    }
                    // TODO: Change to expected one of many
                    _ => Err(SemanticError::TypeMismatch {
                        expected: Ty::Any,
                        found: target_ty,
                        location: member.target.loc.clone(),
                    }),
                }
            }

            _ => todo!("Type inference for expression: {:?}", expr),
        };

        match expr {
            // Only print debug information for certain expressions
            Expr::Lam { .. } => {
                println!(
                    "DEBUG: Infer type for lambda: {} ⟹  {}, Γ: {}",
                    expr.pretty(),
                    inferred_ty.clone()?.pretty_with_loc(&location),
                    self.pretty_print_env(env),
                );
            }

            Expr::FnCall { .. } => {
                println!(
                    "DEBUG: Infer type for expr: {} ⟹  {}, Γ: {}",
                    expr.pretty(),
                    inferred_ty.clone()?.pretty_with_loc(&location),
                    self.pretty_print_env(env),
                );
            }

            _ => print!(""),
        }

        Ok(inferred_ty?)
    }

    /// Infer the type of a literal expression
    fn infer_literal(
        &self,
        lit: &Literal,
        _env: &mut TypingEnv,
        _location: SourceLoc,
    ) -> Return<Ty> {
        match lit {
            Literal::Int(_) => Ok(Ty::Int),
            Literal::Float(_) => Ok(Ty::Float),
            Literal::String(_) => Ok(Ty::String),
            Literal::Bool(_) => Ok(Ty::Bool),
        }
    }

    fn infer_enum_type(
        &mut self,
        env: &mut TypingEnv,
        enum_name: &str,
        variant_name: &str,
        args: &[Expr],
        location: SourceLoc,
    ) -> Return<Ty> {
        // Check if the enum is defined in the global environment
        if !self.global_env.is_enum_defined(enum_name) {
            return Err(SemanticError::UndefinedVariable(enum_name.to_string()));
        }

        // Lookup the enum definition
        let enum_decl = self.global_env.lookup_enum(enum_name).unwrap().clone();

        let variant = enum_decl
            .variants
            .iter()
            .find(|v| v.name == variant_name)
            .ok_or_else(|| SemanticError::UndefinedFieldOrVariant {
                field: variant_name.to_string(),
                structure: enum_name.to_string(),
                location: location.clone(),
            })?;

        // Check payload
        match &variant.payload {
            EnumVariantPayload::None => {
                if !args.is_empty() {
                    return Err(SemanticError::ArityMismatch {
                        expected: 0,
                        found: args.len(),
                        location: location.clone(),
                    });
                }
            }

            EnumVariantPayload::Tuple(tys) => {
                if args.len() != tys.len() {
                    return Err(SemanticError::ArityMismatch {
                        expected: tys.len(),
                        found: args.len(),
                        location: location.clone(),
                    });
                }

                // Infer types of each argument and unify with expected types
                for (arg, expected_ty) in args.iter().zip(tys.iter()) {
                    let arg_ty = self.infer_type(env, arg, location.clone())?;
                    self.unify(&arg_ty, expected_ty, location.clone())?;
                }
            }
            EnumVariantPayload::Record(expected_fields) => {
                if !args.is_empty() {
                    return Err(SemanticError::ArityMismatch {
                        expected: 0,
                        found: args.len(),
                        location: location.clone(),
                    });
                }

                if let Some(Expr::Record(record_expr)) = args.first() {
                    for (field_name, expected_ty) in expected_fields {
                        // Check if the field exists in the record expression
                        let field_expr = record_expr
                            .fields
                            .iter()
                            .find(|(name, _)| name == field_name)
                            .ok_or_else(|| SemanticError::UndefinedFieldOrVariant {
                                field: field_name.clone(),
                                structure: enum_name.to_string(),
                                location: location.clone(),
                            })?
                            .1
                            .clone();

                        let field_ty = self.infer_type(env, &field_expr, location.clone())?;
                        self.unify(&field_ty, &expected_ty, location.clone())?;
                    }
                } else {
                    return Err(SemanticError::TypeMismatch {
                        expected: Ty::TypeCons(
                            enum_name.to_string(),
                            expected_fields.iter().map(|(_, ty)| ty.clone()).collect(),
                        ),
                        found: self.infer_type(env, &args[0], location.clone())?,
                        location: location.clone(),
                    });
                }
            }
        }

        Ok(enum_decl.to_type(&mut self.clone()))
    }

    fn infer_record_expr(
        &mut self,
        record_expr: &RecordExpr,
        env: &mut TypingEnv,
        location: SourceLoc,
    ) -> Return<Ty> {
        // For a record expression like Person { name: "Alice", age: 25 },
        // we need to infer the types of each field expression and construct
        // a record type from those inferred types

        if !self.global_env.is_record_defined(&record_expr.name) {
            return Err(SemanticError::UndefinedVariable(
                record_expr.name.to_string(),
            ));
        }

        let record_decl = self
            .global_env
            .lookup_record(&record_expr.name)
            .unwrap()
            .clone();

        let mut field_types = Vec::new();
        let mut type_instantiations = HashMap::new();

        for (field_name, field_expr) in &record_expr.fields {
            // Infer the type of the field expression
            let field_ty = self.infer_type(env, field_expr, location.clone())?;

            // Validate against record declaration
            if let Some((_, expected_field_ty)) = record_decl
                .fields
                .iter()
                .find(|(name, _)| name == field_name)
            {
                // Unify the inferred type with the expected type
                let field_subst = self.unify(&field_ty, expected_field_ty, location.clone())?;

                // Track type parameters for instantiation
                for (var, ty) in field_subst {
                    if let Some(existing_ty) = type_instantiations.get(&var) {
                        // If we already have a type for this variable, unify it
                        self.unify(existing_ty, &ty, location.clone())?;
                    } else {
                        type_instantiations.insert(var, ty);
                    }
                }
            } else {
                // Undefined field in the record, or variant if an enum
                return Err(SemanticError::UndefinedFieldOrVariant {
                    field: field_name.clone(),
                    structure: record_expr.name.clone(),
                    location: location.clone(),
                });
            }

            field_types.push((field_name.clone(), field_ty));
        }

        // Construct the final type with instantiated type parameters
        let mut instantiated_params = Vec::new();
        let mut type_params_in_record = HashSet::new();

        for (_, field_ty) in &record_decl.fields {
            self.collect_type_params(field_ty, &mut type_params_in_record);
        }

        for param in type_params_in_record {
            if let Some(instantiated_ty) = type_instantiations.get(&param) {
                instantiated_params.push(instantiated_ty.clone());
            } else {
                // If the type parameter was not instantiated, use a fresh type variable
                instantiated_params.push(Ty::Var(self.fresh()));
            }
        }

        Ok(Ty::TypeCons(record_decl.name.clone(), instantiated_params))
    }

    fn check_pattern(
        &mut self,
        env: &mut TypingEnv,
        pattern: &Pattern,
        scrutinee_ty: &Ty,
        location: SourceLoc,
    ) -> Return<()> {
        match pattern {
            Pattern::Literal(lit) => {
                // Infer the literals type
                let literal_ty = self.infer_literal(lit, env, location.clone())?;
                self.unify(&literal_ty, scrutinee_ty, location.clone())?;
                Ok(())
            }

            Pattern::Capture(name) => {
                env.insert(name.clone(), scrutinee_ty.clone());
                self.resolver
                    .define_variable(name.clone(), scrutinee_ty.clone())
                    .map_err(|_| SemanticError::RedefinedVariable(name.clone()))?;

                Ok(())
            }

            Pattern::Wildcard => {
                // Wildcard patterns match anything
                Ok(())
            }

            Pattern::Partition(head, tail) => {
                // For a pattern in the form of [x:xs], this only works on arrays and tuples
                match scrutinee_ty {
                    Ty::TypeCons(name, params) if name == "Array" && params.len() == 1 => {
                        let elem_ty = &params[0];
                        self.check_pattern(env, head, elem_ty, location.clone())?;
                        Ok(self.check_pattern(env, tail, scrutinee_ty, location.clone())?)
                    }

                    Ty::Array(elem_ty) => {
                        self.check_pattern(env, head, &elem_ty, location.clone())?;
                        Ok(self.check_pattern(env, tail, scrutinee_ty, location.clone())?)
                    }
                    // TODO: Add tuple support
                    _ => Err(SemanticError::TypeMismatch {
                        expected: Ty::TypeCons("Array".into(), vec![Ty::Any]),
                        found: scrutinee_ty.clone(),
                        location: location.clone(),
                    }),
                }
            }

            Pattern::List(elems) => {
                // For a pattern in the form of [a, b, c], we expect an array type
                match scrutinee_ty {
                    Ty::TypeCons(name, params) if name == "Array" && params.len() == 1 => {
                        let elem_ty = &params[0];
                        for elem in elems {
                            self.check_pattern(env, elem, elem_ty, location.clone())?;
                        }
                        Ok(())
                    }

                    Ty::Array(elem_ty) => {
                        for elem in elems {
                            self.check_pattern(env, elem, &elem_ty, location.clone())?;
                        }
                        Ok(())
                    }

                    _ => Err(SemanticError::TypeMismatch {
                        expected: Ty::TypeCons("Array".into(), vec![Ty::Any]),
                        found: scrutinee_ty.clone(),
                        location: location.clone(),
                    }),
                }
            }

            Pattern::EnumVariant {
                enum_name,
                variant_name,
                payload,
            } => {
                // Check that the scrutinee type matches the enum
                match scrutinee_ty {
                    Ty::TypeCons(name, _) if name == enum_name => {
                        // Find the enum definition to check the variant
                        let enum_def = self.global_env.lookup_enum(enum_name).cloned();
                        if let Some(enum_def) = enum_def {
                            // Find the specific variant
                            if let Some(variant) =
                                enum_def.variants.iter().find(|v| &v.name == variant_name)
                            {
                                // If there's a payload pattern, check it against the variant's payload type
                                if let Some(payload_pattern) = payload {
                                    match &variant.payload {
                                        EnumVariantPayload::None => {
                                            return Err(SemanticError::TypeMismatch {
                                                expected: Ty::Unit,
                                                found: Ty::Any, // placeholder
                                                location: location.clone(),
                                            });
                                        }
                                        EnumVariantPayload::Tuple(types) if types.len() == 1 => {
                                            let payload_ty = types[0].clone();
                                            self.check_pattern(
                                                env,
                                                payload_pattern,
                                                &payload_ty,
                                                location.clone(),
                                            )?;
                                        }
                                        EnumVariantPayload::Tuple(_) => {
                                            // TODO: Handle multiple tuple elements
                                            return Err(SemanticError::TypeMismatch {
                                                expected: Ty::Any,
                                                found: Ty::Any,
                                                location: location.clone(),
                                            });
                                        }
                                        EnumVariantPayload::Record(_) => {
                                            // TODO: Handle record patterns
                                            return Err(SemanticError::TypeMismatch {
                                                expected: Ty::Any,
                                                found: Ty::Any,
                                                location: location.clone(),
                                            });
                                        }
                                    }
                                }
                                Ok(())
                            } else {
                                Err(SemanticError::UndefinedVariable(format!(
                                    "{}.{}",
                                    enum_name, variant_name
                                )))
                            }
                        } else {
                            Err(SemanticError::UndefinedVariable(enum_name.clone()))
                        }
                    }
                    _ => Err(SemanticError::TypeMismatch {
                        expected: Ty::TypeCons(enum_name.clone(), vec![]),
                        found: scrutinee_ty.clone(),
                        location: location.clone(),
                    }),
                }
            }
        }
    }

    pub fn check_record_against_expected_type(
        &mut self,
        record_expr: &RecordExpr,
        expected_ty: &Ty,
        env: &mut TypingEnv,
        location: SourceLoc,
    ) -> Return<Ty> {
        // Get record definition
        let record_decl = self
            .global_env
            .lookup_record(&record_expr.name)
            .ok_or_else(|| SemanticError::UndefinedVariable(record_expr.name.clone()))?
            .clone();

        // Extract type parameters from expected type: Simple[Int] -> [Int]
        let type_params = match expected_ty {
            Ty::TypeCons(name, params) if name == &record_expr.name => params,
            _ => return self.infer_record_expr(record_expr, env, location), // fallback
        };

        // Build substitution map: A -> Int, B -> String, etc.
        let subst_map = self.build_type_param_substitution(&record_decl, type_params);

        // Check each field with substituted types
        for (field_name, field_expr) in &record_expr.fields {
            let expected_field_ty = record_decl
                .fields
                .iter()
                .find(|(name, _)| name == field_name)
                .map(|(_, ty)| self.hydrate_type(ty, &subst_map))
                .ok_or_else(|| SemanticError::UndefinedFieldOrVariant {
                    field: field_name.clone(),
                    structure: record_expr.name.clone(),
                    location: location.clone(),
                })?;

            let actual_field_ty = self.infer_type(env, field_expr, location.clone())?;
            let _field_subst =
                self.unify(&expected_field_ty, &actual_field_ty, location.clone())?;
            // Don't extend env with field_subst - it should only affect this record check
        }

        Ok(expected_ty.clone())
    }

    fn build_type_param_substitution(
        &self,
        record_decl: &Record,
        type_params: &[Ty],
    ) -> HashMap<String, Ty> {
        // Use the record's declared type parameter order instead of HashSet order
        record_decl
            .type_params
            .iter()
            .enumerate()
            .filter_map(|(i, param_name)| {
                type_params
                    .get(i)
                    .map(|ty| (param_name.clone(), ty.clone()))
            })
            .collect()
    }

    fn infer_lam(&mut self, lam: &Expr, env: &mut TypingEnv, location: SourceLoc) -> Return<Ty> {
        self.infer_lam_with_type(env, lam, None, location)
    }

    pub fn infer_lam_with_type(
        &mut self,
        env: &mut TypingEnv,
        lam: &Expr,
        expected_ty: Option<&Ty>,
        location: SourceLoc,
    ) -> Return<Ty> {
        let Expr::Lam {
            args,
            return_ty,
            body,
        } = lam
        else {
            return Err(SemanticError::ExpectedLambda { location });
        };

        // Enter new scope for lambda parameters
        self.resolver.push_scope();

        // Resolve parameter types
        let resolved_args = self.resolve_lambda_params(args, expected_ty, &location)?;

        // Add parameters to both resolver and typing environment
        for (name, ty) in &resolved_args {
            self.resolver
                .define_variable(name.clone(), ty.clone())
                .map_err(|_| SemanticError::RedefinedVariable(name.clone()))?;
            env.insert(name.clone(), ty.clone());
        }

        // Infer body type
        let body_ty = self.infer_type(env, &body.target, body.loc.clone())?;

        // Resolve return type
        let final_return_ty = self.resolve_return_type(env, return_ty, &body_ty, &location)?;

        self.resolver.pop_scope()?;

        // Build and generalize function type
        let func = Function::new_with_expr(
            "lambda".to_string(),
            resolved_args,
            final_return_ty,
            body.clone(),
            vec![],
        );

        let func_ty = Ty::Fn(Box::new(func));
        Ok(self.generalize(env, &func_ty))
    }

    fn resolve_lambda_params(
        &mut self,
        args: &[(String, Ty)],
        expected_ty: Option<&Ty>,
        location: &SourceLoc,
    ) -> Return<Vec<(String, Ty)>> {
        match expected_ty {
            Some(Ty::Fn(func)) => {
                // If we have an expected function type, use its argument types
                if func.args.len() != args.len() {
                    return Err(SemanticError::ArityMismatch {
                        expected: func.args.len(),
                        found: args.len(),
                        location: location.clone(),
                    });
                }

                // Use expected parameter types
                args.iter()
                    .zip(&func.args)
                    .map(|((name, param_ty), (_, expected_param_ty))| {
                        let resolved_ty = match param_ty {
                            Ty::Unresolved => expected_param_ty.clone(),
                            _ => {
                                self.unify(&param_ty, expected_param_ty, location.clone())?;
                                param_ty.clone()
                            }
                        };
                        Ok((name.clone(), resolved_ty))
                    })
                    .collect()
            }
            _ => {
                // No expected type, infer fresh variables
                args.iter()
                    .map(|(name, param_ty)| {
                        let resolved_ty = match param_ty {
                            Ty::Unresolved => Ty::Var(self.fresh()),
                            _ => param_ty.clone(),
                        };
                        Ok((name.clone(), resolved_ty))
                    })
                    .collect()
            }
        }
    }

    fn resolve_return_type(
        &mut self,
        env: &mut TypingEnv,
        declared_return_ty: &Ty,
        body_ty: &Ty,
        location: &SourceLoc,
    ) -> Return<Ty> {
        match declared_return_ty {
            Ty::Unresolved => Ok(body_ty.clone()),
            _ => {
                // If we have a declared return type, unify it with the body type
                let subst = self.unify(declared_return_ty, body_ty, location.clone())?;
                env.extend(subst);

                Ok(declared_return_ty.clone())
            }
        }
    }
}
