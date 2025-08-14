use std::collections::{HashMap, HashSet};

use crate::ast::ast_types::{EnumVariantPayload, Function, Record, RecordExpr, Ty};
use crate::ast::pretty::Pretty;
use crate::ast::{Expr, Literal};
use crate::lexer::SourceLoc;

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
                        let mut instantiation_env = HashMap::new();
                        for ty_param in &func.type_params {
                            let fresh_var = self.fresh();
                            instantiation_env.insert(ty_param.clone(), Ty::Var(fresh_var));
                        }
                        let instantiated_arg_types = func
                            .args
                            .iter()
                            .map(|(_, ty)| self.instantiate_type(ty, &instantiation_env))
                            .collect::<Vec<_>>();
                        let instantiated_return_ty =
                            self.instantiate_type(&func.return_ty, &instantiation_env);

                        for (arg, expected_ty) in args.iter().zip(instantiated_arg_types.iter()) {
                            let arg_ty = self.infer_type(env, &arg.target, arg.loc.clone())?;
                            let subst = self.unify(expected_ty, &arg_ty, arg.loc.clone())?;
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
        enum_name: &str,
        variant_name: &str,
        args: &[Expr],
        env: &mut TypingEnv,
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
        self.infer_lam_with_type(lam, env, location, None)
    }

    pub fn infer_lam_with_type(
        &mut self,
        lam: &Expr,
        env: &mut TypingEnv,
        location: SourceLoc,
        expected_ty: Option<&Ty>,
    ) -> Return<Ty> {
        if let Expr::Lam {
            args,
            return_ty,
            body,
        } = lam
        {
            // Enter a new scope for the lambda
            self.resolver.push_scope();

            let mut arg_types = Vec::new();

            // Handle parameter type inference
            let resolved_args = if let Some(Ty::Fn(expected_func)) = expected_ty {
                if args.len() != expected_func.args.len() {
                    return Err(SemanticError::ArityMismatch {
                        expected: expected_func.args.len(),
                        found: args.len(),
                        location: location.clone(),
                    });
                }

                // If we have an expected function type, use it to resolve argument types
                args.iter()
                    .zip(expected_func.args.iter())
                    .map(|((name, param_ty), (_, expected_param_ty))| -> Result<(String, Ty), SemanticError> {
                        match param_ty {
                            Ty::Unresolved => Ok((name.clone(), expected_param_ty.clone())),
                            _ => {
                                self.unify(param_ty, expected_param_ty, location.clone())?;
                                Ok((name.clone(), param_ty.clone()))
                            }
                        }
                    })
                    .collect::<Result<Vec<_>, _>>()?
            } else {
                // Handle case where we don't have an expected type
                args.iter()
                    .map(|(name, ty)| {
                        if matches!(ty, Ty::Unresolved) {
                            let var = Ty::Var(self.fresh());
                            Ok((name.clone(), var))
                        } else {
                            Ok((name.clone(), ty.clone()))
                        }
                    })
                    .collect::<Result<Vec<_>, _>>()?
            };

            // Define variables in the typing environment
            for (name, ty) in &resolved_args {
                self.resolver
                    .define_variable(name.to_string(), ty.clone())
                    .map_err(|_| SemanticError::RedefinedVariable(name.clone()))?;

                arg_types.push((name.clone(), ty.clone()));
                env.insert(name.clone(), ty.clone());
            }

            // Infer the body type and unify with the return type
            let body_ty = self.infer_type(env, &body.target, body.loc.clone())?;
            let unified_return_ty = if matches!(return_ty, Ty::Unresolved) {
                // If the return type is unresolved, we can unify it with the body type
                body_ty.clone()
            } else {
                // Otherwise, unify explicit return type with body type
                let subst = self.unify(&return_ty, &body_ty, location.clone())?;
                env.extend(subst);
                return_ty.clone()
            };

            self.resolver.pop_scope()?;

            let func = Function::new_with_expr(
                "lambda".to_string(),
                resolved_args,
                unified_return_ty,
                body.clone(),
            );

            // Generalize the function type if needed
            let func_ty = Ty::Fn(Box::new(func));
            let generalized_ty = self.generalize(env, &func_ty);

            Ok(generalized_ty)
        } else {
            Err(SemanticError::ExpectedLambda { location: location })
        }
    }
}
