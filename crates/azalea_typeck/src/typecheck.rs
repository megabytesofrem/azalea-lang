use std::collections::{HashMap, HashSet};

use azalea_parse::ast::ast_types::{Function, Record, RecordExpr, Ty};
use azalea_parse::ast::{Expr, Literal, Stmt};
use azalea_parse::lexer::SourceLoc;
use azalea_parse::span::Span;

// Name and scope resolution
use azalea_resolver::resolver::Resolver;
use azalea_resolver::semantic_error::SemanticError;

use crate::Return;

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
        match (t1, t2) {
            (t1, t2) if t1 == t2 => {
                // If both types are the same, return an empty substitution
                Ok(HashMap::new())
            }

            (Ty::Any, _) | (_, Ty::Any) => {
                // If one type is `Any`, we can unify it with any other type
                // This is an _intentional hole_ in the type system
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

            _ => Err(SemanticError::UnificationError(format!(
                "Cannot unify {t1:?} with {t2:?}"
            ))),
        }
    }

    /// Infer the type of an expression within a substitution environment `env`
    pub fn infer_type(
        &mut self,
        env: &mut TypingEnv,
        expr: &Expr,
        location: SourceLoc,
    ) -> Return<Ty> {
        match expr {
            Expr::Literal(lit) => self.infer_literal(lit, env, location),
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

            Expr::BinOp(lhs, _, rhs) => {
                let lhs_ty = self.infer_type(env, &lhs.target, lhs.loc.clone())?;
                let rhs_ty = self.infer_type(env, &rhs.target, rhs.loc.clone())?;

                // Unify the types of both sides
                let subst = self.unify(&lhs_ty, &rhs_ty, location.clone())?;
                env.extend(subst);

                Ok(lhs_ty)
            }

            Expr::UnOp(_, expr) => {
                let ty = self.infer_type(env, &expr.target, expr.loc.clone())?;
                Ok(ty)
            }

            Expr::Record(record) => self.infer_record_expr(record, env, location),

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

                Ok(Ty::Array(Box::new(first_ty)))
            }

            Expr::ArrayIndex { target, index } => {
                let target_ty = self.infer_type(env, &target.target, target.loc.clone())?;
                let index_ty = self.infer_type(env, &index.target, index.loc.clone())?;

                self.unify(&index_ty, &Ty::Int, location.clone())?;

                Ok(target_ty)
            }

            Expr::FnCall { .. } => {
                // Not enough information to unify right now, we'll come back to it later
                Ok(Ty::Unresolved)
            }

            Expr::Lam { .. } => self.infer_lam(&expr, env, location.clone()),

            _ => todo!(),
        }
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
                self.resolver.pop_scope().map_err(|_| {
                    SemanticError::UnificationError("Failed to pop function scope".to_string())
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
