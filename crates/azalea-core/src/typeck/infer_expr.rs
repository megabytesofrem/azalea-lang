use std::collections::{HashMap, HashSet};

use crate::ast::ast_types::{Function, Record, RecordExpr, Ty};
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
                if !self.resolver.is_variable_defined(name) {
                    return Err(SemanticError::UndefinedVariable(name.clone()));
                }

                println!(
                    "DEBUG: Looking up type: {} from {}",
                    name,
                    self.resolver.scope_depth()
                );

                if self.type_registry.is_record_defined(name) {
                    let record_ty = self.type_registry.lookup_record(name).unwrap();
                    return Ok(record_ty.to_type());
                }

                // Then get the type from the typing environment (which tracks current types during inference)
                let ty = env
                    .get(name)
                    .cloned()
                    .ok_or_else(|| SemanticError::UndefinedVariable(name.clone()))?;

                self.hydrate_type(env, &ty);
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
                for ty in &element_types[1..] {
                    self.unify(&first_ty, ty, location.clone())?;
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

            Expr::FnCall { target, args } => match &target.target {
                Expr::Ident(name) => {
                    // Check if the function is an `extern` function first
                    if self.type_registry.is_extern_function(name) {
                        // If it's an extern function, we need to look it up in the type registry
                        let extern_decl = self
                            .type_registry
                            .lookup_extern_function(name)
                            .unwrap()
                            .clone();

                        // Type check arguments against the extern function signature
                        if args.len() != extern_decl.args.len() {
                            return Err(SemanticError::ArityMismatch {
                                expected: extern_decl.args.len(),
                                found: args.len(),
                                location: target.loc.clone(),
                            });
                        }

                        // Type check each argument
                        for (arg, (_, expected_ty)) in args.iter().zip(extern_decl.args.iter()) {
                            let arg_ty = self.infer_type(env, &arg.target, arg.loc.clone())?;
                            self.unify(expected_ty, &arg_ty, arg.loc.clone())?;
                        }

                        println!(
                            "DEBUG: Extern function call: {} with args: {:?}",
                            name,
                            extern_decl
                                .args
                                .iter()
                                .map(|(_, ty)| ty.pretty())
                                .collect::<Vec<_>>()
                        );

                        // Return the return type of the extern function
                        return Ok(extern_decl.return_ty.clone());
                    }

                    // Otherwise, handle it as a regular function call

                    let func_ty = env
                        .get(name)
                        .cloned()
                        .ok_or_else(|| SemanticError::UndefinedFunction(name.clone()))?;

                    match func_ty {
                        Ty::Fn(func) => {
                            // Handle generic function instantiation
                            let mut instantiation_env = HashMap::new();
                            for ty_param in &func.type_params {
                                // Create a fresh type variable for each type parameter
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

                            // Type check arguments
                            if args.len() != func.args.len() {
                                return Err(SemanticError::ArityMismatch {
                                    expected: func.args.len(),
                                    found: args.len(),
                                    location: target.loc.clone(),
                                });
                            }

                            for (arg, expected_ty) in args.iter().zip(instantiated_arg_types.iter())
                            {
                                let arg_ty = self.infer_type(env, &arg.target, arg.loc.clone())?;
                                self.unify(expected_ty, &arg_ty, arg.loc.clone())?;
                            }

                            Ok(instantiated_return_ty)
                        }

                        // TODO: Add support for `foo[0](a, b)` syntax
                        _ => todo!("Complex function call handling not implemented yet"),
                    }
                }
                e => todo!(
                    "Complex function call handling not implemented yet: {:?}",
                    e
                ),
            },

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

                // Check if the target type is a record or an enum
                match target_ty {
                    Ty::TypeCons(name, params) => {
                        println!(
                            "DEBUG: Member access on TypeCons: '{}' with params: {:?}",
                            name, params
                        );

                        if self.type_registry.is_record_defined(&name) {
                            let record = self.type_registry.lookup_record(&name).unwrap();
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
                        } else {
                            println!("DEBUG: Record {} not found in resolver", name);
                            Err(SemanticError::UndefinedFieldOrVariant {
                                field: member.name.clone(),
                                structure: name,
                                location: member.target.loc.clone(),
                            })
                        }
                    }

                    // Ty::Enum(enum_ty) => {
                    //     // For enums, we need to check if the field is a variant
                    //     if enum_ty.variants.contains(&member.name) {
                    //         Ok(enum_ty.to_type())
                    //     } else {
                    //         Err(SemanticError::UndefinedFieldOrVariant {
                    //             field: member.name.clone(),
                    //             structure: enum_ty.name.clone(),
                    //             location: member.target.loc.clone(),
                    //         })
                    //     }
                    // }

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

        println!(
            "DEBUG: Infer type for expr: {} => {}",
            expr.pretty(),
            inferred_ty.clone()?.pretty_with_loc(&location)
        );

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

    fn infer_record_expr(
        &mut self,
        record_expr: &RecordExpr,
        env: &mut TypingEnv,
        location: SourceLoc,
    ) -> Return<Ty> {
        // For a record expression like Person { name: "Alice", age: 25 },
        // we need to infer the types of each field expression and construct
        // a record type from those inferred types

        if !self.type_registry.is_record_defined(&record_expr.name) {
            return Err(SemanticError::UndefinedVariable(
                record_expr.name.to_string(),
            ));
        }

        let record_decl = self
            .type_registry
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

    fn infer_lam(&mut self, lam: &Expr, env: &mut TypingEnv, location: SourceLoc) -> Return<Ty> {
        if let Expr::Lam {
            args,
            return_ty,
            body,
        } = lam
        {
            // Enter a new scope for the lambda
            self.resolver.push_scope();

            let mut arg_types = Vec::new();
            for (name, ty) in args {
                self.resolver
                    .define_variable(name.clone(), ty.clone())
                    .map_err(|_| SemanticError::RedefinedVariable(name.clone()))?;
                arg_types.push((name.clone(), ty.clone()));
            }

            for (name, ty) in args {
                env.insert(name.clone(), ty.clone());
            }

            // Unify the return type with the body
            let body_ty = self.infer_type(env, &body.target, body.loc.clone())?;
            let subst = self.unify(&return_ty, &body_ty, location.clone())?;
            let return_ty = self.hydrate_type(env, return_ty);
            env.extend(subst);

            // Exit the lambda scope
            self.resolver.pop_scope()?;

            let func = Function::new_with_expr(
                "lambda".to_string(),
                args.clone(),
                return_ty.clone(),
                body.clone(),
            );

            // Generalize the lambda type
            let func_ty = Ty::Fn(Box::new(func));
            let general_ty = self.generalize(env, &func_ty);

            Ok(general_ty)
        } else {
            Err(SemanticError::ExpectedLambda { location: location })
        }
    }
}
