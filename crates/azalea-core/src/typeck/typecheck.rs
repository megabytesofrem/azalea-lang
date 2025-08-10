use std::collections::{HashMap, HashSet};

use crate::ast::ast_types::{Function, Record, RecordExpr, Ty};
use crate::ast::pretty::Pretty;
use crate::ast::{Expr, Literal, Stmt};
use crate::lexer::SourceLoc;
use crate::parse::span::Span;

// Name and scope resolution
use crate::resolver::error::SemanticError;
use crate::resolver::resolver::Resolver;

use crate::resolver::Return;

/// A typing environment is a mapping from type variables to types.
pub type TypingEnv = HashMap<String, Ty>;

/// The type checker is based on a simplified version of the Hindley-Milner type system.
///
/// It performs the process of both type inference and unification.
/// See:
/// - https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system
/// - https://steshaw.org/hm/hindley-milner.pdf

#[derive(Debug, Clone)]
pub struct Typechecker {
    next_var: usize,

    pub resolver: Resolver,

    /// During type checking, we collect errors and bubble them up
    pub errors: Vec<SemanticError>,
}

impl Typechecker {
    pub fn new() -> Self {
        Self {
            next_var: 0,
            resolver: Resolver::new(),
            errors: Vec::new(),
        }
    }

    /// Return a fresh type variable.
    pub fn fresh(&mut self) -> String {
        let var = format!("t{}", self.next_var);
        println!("DEBUG: Fresh type variable: {}", var);
        self.next_var += 1;
        var
    }

    /// Occurs check. Check for infinite type expansion.
    ///
    /// `A<A<A<...>>>` is classed as an *infinite type*, since `A` occurs in itself.
    pub fn occurs_check(&self, var: &str, ty: &Ty) -> bool {
        match ty {
            Ty::Var(v) => v == var,
            Ty::Array(inner) => self.occurs_check(var, inner),

            Ty::TypeCons(ctor, params) => {
                // Check if the type variable occurs in the constructor name
                ctor == var && params.iter().any(|param| self.occurs_check(var, param))
            }

            // Check if type variable occurs in function arguments or return type
            Ty::Fn(func) => {
                let arg_occurs = func.args.iter().any(|(_, aty)| self.occurs_check(var, aty));
                let ret_occurs = self.occurs_check(var, &func.return_ty);

                arg_occurs || ret_occurs
            }

            // Check record fields for infinite types
            Ty::Record(record) => record
                .fields
                .iter()
                .any(|(_, aty)| self.occurs_check(var, aty)),

            // For now, enums are not allowed to have user-defined types, so
            // we can conclude that logically they can never be infinite types.
            Ty::Enum(_) => false,

            // For all other types, we don't need to check
            _ => false,
        }
    }

    /// Attempt to unify two types `t1` and `t2` within a typing environment `env`.
    pub fn unify(&mut self, t1: &Ty, t2: &Ty, location: SourceLoc) -> Return<TypingEnv> {
        // Thank you @NullifyDev for pointing out these issues!
        //
        // FIXME: Cannot unify TypeCons(\"Loop\", []) with Record(Record { name: \"Loop\", fields: [(\"lstart\", Int), (\"lend\", Int)] })
        //
        // This is a bug in the unification logic, since we should be able to unify a type constructor with a record type of the same name
        // since it is logically sound that `Loop` is a type constructor for the `Loop` record.

        let map = match (t1, t2) {
            (t1, t2) if t1 == t2 => {
                // If both types are the same, return an empty substitution
                Ok(HashMap::new())
            }

            (Ty::Unit, Ty::Unresolved)
            | (Ty::Unresolved, Ty::Unit)
            | (Ty::Unit, Ty::Any)
            | (Ty::Any, Ty::Unit) => {
                // Unit should be unifiable with any void-equivalent type such as `Any` or `Unresolved`.
                Ok(HashMap::new())
            }

            (Ty::Any, _) | (_, Ty::Any) => {
                // If one type is `Any`, we can unify it with any other type
                // This is an _intentional hole_ in the type system
                println!("Unifying with Any: {:?} and {:?}", t1, t2);

                Ok(HashMap::new())
            }

            // If one type is a variable, create a substitution binding that
            // variable to the other type. Need to occurs check.
            (Ty::Var(var), ty) | (ty, Ty::Var(var)) => {
                let mut subst = HashMap::new();

                // Occurs check
                if self.occurs_check(var, ty) {
                    return Err(SemanticError::OccursCheck { location });
                } else {
                    subst.insert(var.clone(), ty.clone());
                }

                Ok(subst)
            }

            (Ty::TypeCons(ty_name1, tys_1), Ty::TypeCons(ty_name2, tys_2))
                if ty_name1 == ty_name2 && tys_1.len() == tys_2.len() =>
            {
                // Occurs check for all the type parameters of both types
                for (ty1, ty2) in tys_1.iter().zip(tys_2.iter()) {
                    if let (Ty::Var(var), ty) | (ty, Ty::Var(var)) = (ty1, ty2) {
                        if self.occurs_check(var, ty) {
                            return Err(SemanticError::OccursCheck { location });
                        }
                    }
                }

                let mut subst = HashMap::new();
                for (ty1, ty2) in tys_1.iter().zip(tys_2.iter()) {
                    let new_subst = self.unify(ty1, ty2, location.clone())?;
                    subst.extend(new_subst);
                }
                Ok(subst)
            }

            (Ty::Fn(f1), Ty::Fn(f2)) => {
                // Unify the argument types of both functions
                let mut subst = HashMap::new();

                for (arg1, arg2) in f1.args.iter().zip(f2.args.iter()) {
                    let new_subst = self.unify(&arg1.1, &arg2.1, location.clone())?;
                    subst.extend(new_subst);
                }

                // Unify the return types of both functions
                let ret_subst = self.unify(&f1.return_ty, &f2.return_ty, location.clone())?;
                subst.extend(ret_subst);

                Ok(subst)
            }

            (Ty::Array(inner1), Ty::Array(inner2)) => {
                // Unify the inner types of both arrays
                let subst = self.unify(inner1, inner2, location.clone())?;
                Ok(subst)
            }

            (Ty::Record(rec1), Ty::Record(rec2)) => {
                let mut subst = HashMap::new();

                // If the lengths of the records are different, we can't unify them. So
                // this should fail since they are not the same type.
                if rec1.fields.len() != rec2.fields.len() {
                    return Err(SemanticError::TypeMismatch {
                        expected: Ty::Record(rec1.clone()),
                        found: Ty::Record(rec2.clone()),
                        location,
                    });
                }

                // Unify the fields of both records. Field names must match for them to unify.
                for (field1, field2) in rec1.fields.iter().zip(rec2.fields.iter()) {
                    let (name1, ty1) = field1;
                    let (name2, ty2) = field2;

                    // Check if the field names are the same
                    if name1 != name2 {
                        return Err(SemanticError::TypeMismatch {
                            expected: Ty::Record(rec1.clone()),
                            found: Ty::Record(rec2.clone()),
                            location,
                        });
                    }

                    // Unify the types of the fields
                    let new_subst = self.unify(ty1, ty2, location.clone())?;
                    subst.extend(new_subst);
                }

                Ok(subst)
            }

            _ => Err(SemanticError::UnificationError {
                message: format!("Cannot unify {t1:?} with {t2:?}"),
                location: location.clone(),
            }),
        };

        println!(
            "DEBUG: Unifying {} with {} => {}",
            t1.pretty(),
            t2.pretty(),
            self.pretty_print_env(&map.clone()?)
        );

        map
    }

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
                    Ty::Record(record) => {
                        // Find the field type in the record
                        if let Some(field_ty) = record.fields.iter().find_map(|(name, ty)| {
                            if name == &member.name {
                                Some(ty.clone())
                            } else {
                                None
                            }
                        }) {
                            Ok(field_ty)
                        } else {
                            Err(SemanticError::UndefinedFieldOrVariant {
                                field: member.name.clone(),
                                structure: record.name.clone(),
                                location: member.target.loc.clone(),
                            })
                        }
                    }
                    Ty::Enum(enum_ty) => {
                        // For enums, we need to check if the field is a variant
                        if enum_ty.variants.contains(&member.name) {
                            Ok(enum_ty.to_type())
                        } else {
                            Err(SemanticError::UndefinedFieldOrVariant {
                                field: member.name.clone(),
                                structure: enum_ty.name.clone(),
                                location: member.target.loc.clone(),
                            })
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

        let mut field_types = Vec::new();

        for (field_name, field_expr) in &record_expr.fields {
            // Infer the type of the field expression
            let field_ty = self.infer_type(env, field_expr, location.clone())?;
            field_types.push((field_name.clone(), field_ty));
        }

        let record_type = Record {
            name: record_expr.name.clone(),
            fields: field_types,
        };

        Ok(record_type.to_type())
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

    fn infer_block(
        &mut self,
        block: &Vec<Span<Stmt>>,
        env: &mut TypingEnv,
        location: SourceLoc,
    ) -> Return<Ty> {
        let mut last_seen_ty = Ty::Unit;

        for stmt in block {
            match &stmt.target {
                Stmt::Expr(expr) => {
                    last_seen_ty = self.infer_type(env, &expr.target, expr.loc.clone())?
                }
                _ => self.check(env, stmt, location.clone())?,
            }
        }

        Ok(last_seen_ty)
    }

    pub fn check(
        &mut self,
        env: &mut TypingEnv,
        stmt: &Span<Stmt>,
        location: SourceLoc,
    ) -> Result<(), SemanticError> {
        match &stmt.target {
            Stmt::Expr(expr) => {
                self.infer_type(env, &expr.target, expr.loc.clone())?;
                Ok(())
            }

            Stmt::Let { name, ty, value } => {
                let value_ty = self.infer_type(env, &value.target, value.loc.clone())?;
                let local_subst = self.unify(&ty, &value_ty, value.loc.clone())?;

                env.extend(local_subst);

                // Add the variable to both the resolver (for scope management) and typing environment (for type tracking)
                self.resolver
                    .define_or_redefine_variable(name.clone(), ty.clone());
                env.insert(name.clone(), ty.clone());
                Ok(())
            }

            Stmt::Mut { name, ty, value } => {
                let value_ty = self.infer_type(env, &value.target, value.loc.clone())?;
                let local_subst = self.unify(&ty, &value_ty, value.loc.clone())?;
                env.extend(local_subst);

                // Add the variable to both the resolver (for scope management) and typing environment (for type tracking)
                self.resolver
                    .define_or_redefine_variable(name.clone(), ty.clone());
                env.insert(name.clone(), ty.clone());
                Ok(())
            }

            Stmt::Assign { name, value } => {
                // For assignment statements, we need to check that the variable exists
                // and that the value type matches the variable type
                let var_ty = env
                    .get(name)
                    .cloned()
                    .ok_or_else(|| SemanticError::UndefinedVariable(name.clone()))?;

                let value_ty = self.infer_type(env, &value.target, value.loc.clone())?;
                self.unify(&var_ty, &value_ty, value.loc.clone())?;

                Ok(())
            }

            Stmt::For {
                name,
                iterable,
                body,
            } => {
                let iterable_ty = self.infer_type(env, &iterable.target, iterable.loc.clone())?;

                // For now, this only supports arrays as iterables
                let iterable_elem_ty = match iterable_ty {
                    Ty::Array(inner) => *inner,
                    _ => {
                        return Err(SemanticError::TypeMismatch {
                            expected: Ty::Array(Box::new(Ty::Unresolved)),
                            found: iterable_ty,
                            location: iterable.loc.clone(),
                        });
                    }
                };

                // Add the loop variable to the typing environment
                env.insert(name.clone(), iterable_elem_ty.clone());
                self.resolver.push_scope();
                self.resolver
                    .define_variable(name.clone(), iterable_elem_ty.clone())
                    .map_err(|_| SemanticError::RedefinedVariable(name.clone()))?;

                // Type check the body of the for loop
                body.iter()
                    .try_for_each(|stmt| self.check(env, stmt, location.clone()))?;

                self.resolver
                    .pop_scope()
                    .map_err(|_| SemanticError::UnificationError {
                        message: "Failed to pop for loop scope".to_string(),
                        location,
                    })?;

                env.remove(name);
                Ok(())
            }

            Stmt::While { cond, body } => {
                // Infer the condition type
                let cond_ty = self.infer_type(env, &cond.target, cond.loc.clone())?;

                // Unify the condition type with boolean
                self.unify(&cond_ty, &Ty::Bool, cond.loc.clone())?;

                // Type check the body of the while loop
                body.iter()
                    .try_for_each(|stmt| self.check(env, stmt, location.clone()))?;

                Ok(())
            }

            Stmt::RecordDecl(record) => {
                let ty = record.to_type();
                let local_subst = self.unify(&ty, &record.to_type(), stmt.loc.clone())?;
                env.extend(local_subst);

                // Add the record to the environment
                env.insert(record.name.clone(), ty.clone());
                Ok(())
            }

            Stmt::EnumDecl(enum_decl) => {
                let ty = enum_decl.to_type();
                let local_env = self.unify(&ty, &enum_decl.to_type(), stmt.loc.clone())?;
                env.extend(local_env);

                // Add the enum to the environment
                env.insert(enum_decl.name.clone(), ty.clone());
                Ok(())
            }

            Stmt::FnDecl(func_decl) => {
                // Create a new scope for the function
                self.resolver.push_scope();

                let mut local_env = env.clone();

                // Add type parameters as fresh type variables to the environment
                for ty_param in &func_decl.type_params {
                    let ty_var = Ty::Var(self.fresh());
                    local_env.insert(ty_param.clone(), ty_var);
                }

                for (arg_name, arg_ty) in &func_decl.args {
                    // Handle types that are unknown for now but can be inferred later
                    // We generate a fresh type variable, marking it as polymorphic
                    let actual_arg_ty = if *arg_ty == Ty::Unresolved {
                        Ty::Var(self.fresh())
                    } else {
                        arg_ty.clone()
                    };

                    // Add function parameters to the resolver
                    self.resolver
                        .define_variable(arg_name.clone(), actual_arg_ty.clone())
                        .map_err(|_| SemanticError::RedefinedVariable(arg_name.clone()))?;

                    local_env.insert(arg_name.clone(), actual_arg_ty);
                }

                // Infer the body type of the function
                let body_ty = if let Some(body_expr) = &func_decl.body_expr {
                    self.infer_type(&mut local_env, &body_expr.target, body_expr.loc.clone())?
                } else if let Some(body_stmts) = &func_decl.body {
                    self.infer_block(&body_stmts, &mut local_env, location.clone())?
                } else {
                    Ty::Unit
                };

                // Unify with any declared return type, if there is one
                let return_ty = if func_decl.return_ty != Ty::Unresolved {
                    self.unify(&func_decl.return_ty, &body_ty, location.clone())?;
                    func_decl.return_ty.clone()
                } else {
                    body_ty
                };

                // Apply our substitutions to the arguments using `hydrate_type`
                // FIXME: Fix so we can infer argument types from the body
                let args: Vec<(String, Ty)> = func_decl
                    .args
                    .iter()
                    .map(|(name, ty)| (name.clone(), self.hydrate_type(&local_env, ty)))
                    .collect();

                // Add the function to the environment
                let func_ty = Ty::Fn(Box::new(Function {
                    name: func_decl.name.clone(),
                    args: args,
                    type_params: func_decl.type_params.clone(),
                    return_ty: return_ty,
                    body: if func_decl.body.is_some() {
                        Some(func_decl.body.clone().unwrap())
                    } else {
                        None
                    },
                    body_expr: if func_decl.body_expr.is_some() {
                        Some(func_decl.body_expr.clone().unwrap())
                    } else {
                        None
                    },
                }));

                // Pop the function scope
                self.resolver
                    .pop_scope()
                    .map_err(|_| SemanticError::UnificationError {
                        message: "Failed to pop function scope".to_string(),
                        location: stmt.loc.clone(),
                    })?;

                // Add the function to both the resolver and typing environment
                self.resolver
                    .define_function(func_decl.name.clone(), func_ty.clone())
                    .map_err(|_| SemanticError::RedefinedVariable(func_decl.name.clone()))?;
                env.insert(func_decl.name.clone(), func_ty);

                Ok(())
            }
        }
    }

    pub fn walk_ast(&mut self, ast: Vec<Span<Stmt>>) -> Result<(), Vec<SemanticError>> {
        let mut subst = TypingEnv::new();

        for stmt in ast {
            self.check(&mut subst, &stmt, stmt.loc.clone())
                .map_err(|e| self.errors.push(e))
                .unwrap_or(());
        }

        if self.errors.is_empty() {
            Ok(())
        } else {
            // Return all the errors, bundled up nice 'n cosy
            Err(self.errors.clone())
        }
    }
}
