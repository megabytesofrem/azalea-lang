use std::collections::HashMap;

use azalea_parse::ast::ast_types::{Function, Record, Ty};
use azalea_parse::ast::{Expr, Literal, Stmt};
use azalea_parse::lexer::SourceLoc;
use azalea_parse::span::Span;

use crate::Return;
use crate::resolver::Resolver;
use crate::type_error::TypeError;

/// A substitution is a mapping from type variables (e.g t0) to types.
pub type Subst = HashMap<String, Ty>;

/// The type checker is based on a simplified version of the Hindley-Milner type system.
/// The only thing it doesn't implement is **let polymorphism**, but we shouldn't really need it.
///
/// It performs the process of both type inference and unification.
/// See: https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system

#[derive(Debug, Clone)]
pub struct Typechecker {
    next_var: usize,

    pub resolver: Resolver,

    /// During type checking, we collect errors and bubble them up
    pub errors: Vec<TypeError>,
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
    fn fresh(&mut self) -> String {
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

    /// Given the types `t1` and `t2`, this function attempts to unify them following the laws.
    ///
    /// - If both types are the same primitive type, they unify.
    ///
    /// - If one type is a (type)variable, we create a substitution binding that binds
    ///   the variable to the other type.
    ///
    /// - If both types are instantiated types with the same name and the same number of type parameters,
    ///  we unify their type parameters.
    ///
    /// A bunch of other cases are handled that I can't be bothered to write down
    /// In all other cases, unification fails.
    pub fn unify(&mut self, t1: &Ty, t2: &Ty, location: SourceLoc) -> Return<Subst> {
        match (t1, t2) {
            (Ty::Int, Ty::Int) => Ok(HashMap::new()),
            (Ty::Float, Ty::Float) => Ok(HashMap::new()),
            (Ty::String, Ty::String) => Ok(HashMap::new()),
            (Ty::Bool, Ty::Bool) => Ok(HashMap::new()),
            (Ty::Unit, Ty::Unit) => Ok(HashMap::new()),

            // If one type is a variable, create a substitution binding that
            // variable to the other type. Need to occurs check.
            (Ty::Var(var), ty) | (ty, Ty::Var(var)) => {
                let mut subst = HashMap::new();

                // Occurs check
                if self.occurs_check(var, ty) {
                    return Err(TypeError::OccursCheck { location });
                } else {
                    subst.insert(var.clone(), ty.clone());
                }

                Ok(subst)
            }

            (Ty::Instantiated(ty_name1, tys_1), Ty::Instantiated(ty_name2, tys_2))
                if ty_name1 == ty_name2 && tys_1.len() == tys_2.len() =>
            {
                // Occurs check for all the type parameters of both types
                for (ty1, ty2) in tys_1.iter().zip(tys_2.iter()) {
                    if let (Ty::Var(var), ty) | (ty, Ty::Var(var)) = (ty1, ty2) {
                        if self.occurs_check(var, ty) {
                            return Err(TypeError::OccursCheck { location });
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
                    return Err(TypeError::TypeMismatch {
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
                        return Err(TypeError::TypeMismatch {
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

            _ => Err(TypeError::UnificationError(format!(
                "Cannot unify {t1:?} with {t2:?}"
            ))),
        }
    }

    /// Infer the type of an expression within a substitution environment `env`
    pub fn infer(&mut self, env: &mut Subst, expr: &Expr, location: SourceLoc) -> Return<Ty> {
        match expr {
            Expr::Literal(lit) => self.infer_literal(lit, env, location),
            Expr::Ident(name) => {
                let ty = env
                    .get(name)
                    .cloned()
                    .ok_or_else(|| TypeError::UndefinedVariable(name.clone()))?;

                self.apply_subst(env, &ty);
                Ok(self.instantiate(&ty))
            }

            Expr::BinOp(lhs, _, rhs) => {
                let lhs_ty = self.infer(env, &lhs.target, lhs.loc.clone())?;
                let rhs_ty = self.infer(env, &rhs.target, rhs.loc.clone())?;

                // Unify the types of both sides
                let subst = self.unify(&lhs_ty, &rhs_ty, location.clone())?;
                env.extend(subst);

                Ok(lhs_ty)
            }

            Expr::UnOp(_, expr) => {
                let ty = self.infer(env, &expr.target, expr.loc.clone())?;
                Ok(ty)
            }

            Expr::Record(record) => self.infer_record_ty(record, env, location),

            Expr::Array { elements } => {
                let mut element_types = Vec::new();
                for elem in elements {
                    let ty = self.infer(env, &elem.target, elem.loc.clone())?;
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
                let target_ty = self.infer(env, &target.target, target.loc.clone())?;
                let index_ty = self.infer(env, &index.target, index.loc.clone())?;

                self.unify(&index_ty, &Ty::Int, location.clone())?;

                Ok(target_ty)
            }

            Expr::FnCall { .. } => {
                // Not enough information to unify right now, we'll come back to it later
                Ok(Ty::UnknownForNow)
            }

            Expr::Lam { .. } => self.infer_lam(&expr, env, location.clone()),

            _ => todo!(),
        }
    }

    /// Infer the type of a literal expression
    fn infer_literal(&self, lit: &Literal, _env: &mut Subst, _location: SourceLoc) -> Return<Ty> {
        match lit {
            Literal::Int(_) => Ok(Ty::Int),
            Literal::Float(_) => Ok(Ty::Float),
            Literal::String(_) => Ok(Ty::String),
            Literal::Bool(_) => Ok(Ty::Bool),
        }
    }

    fn infer_record_ty(
        &mut self,
        record: &Record,
        env: &mut Subst,
        location: SourceLoc,
    ) -> Return<Ty> {
        let ty = record.to_type();

        // Unify the type of the record with the expected type
        let subst = self.unify(&ty, &record.to_type(), location.clone())?;
        env.extend(subst);

        Ok(ty)
    }

    fn infer_lam(&mut self, lam: &Expr, env: &mut Subst, location: SourceLoc) -> Return<Ty> {
        if let Expr::Lam {
            args,
            return_ty,
            body,
        } = lam
        {
            let mut arg_types = Vec::new();
            for (name, ty) in args {
                arg_types.push((name.clone(), ty.clone()));
            }

            // Unify the return type with the body
            let body_ty = self.infer(env, &body.target, body.loc.clone())?;
            let subst = self.unify(&return_ty, &body_ty, location.clone())?;
            env.extend(subst);

            let func = Function::new_with_expr(
                "lambda".to_string(),
                args.clone(),
                return_ty.clone(),
                body.clone(),
            );

            Ok(Ty::Fn(Box::new(func)))
        } else {
            Err(TypeError::ExpectedLambda { location: location })
        }
    }

    fn instantiate(&mut self, ty: &Ty) -> Ty {
        if let Ty::Instantiated(_, type_params) = ty {
            let mut subst = HashMap::new();

            // Replace every parameter with a fresh type variable
            // e.g: Option<A, B> becomes Option<t0, t1>
            //
            // We leave concrete types alone, since `apply_subst` will map them to themselves intuitively
            for tv in type_params {
                let fresh_var = self.fresh();
                subst.insert(fresh_var.clone(), tv.clone());
            }

            self.apply_subst(&subst, ty)
        } else {
            ty.clone()
        }
    }

    fn apply_subst(&self, subst: &Subst, ty: &Ty) -> Ty {
        match &*ty {
            // If the type is a type level variable, we check if there is a substitution for it
            Ty::Var(var) => subst.get(var).cloned().unwrap_or(Ty::Var(var.to_string())),

            // If the type is an array of any type, apply the substitution to the inner type
            Ty::Array(inner) => Ty::Array(Box::new(self.apply_subst(subst, &inner))),

            Ty::Fn(func) => {
                let args: Vec<(String, Ty)> = func
                    .args
                    .iter()
                    .map(|(name, ty)| (name.clone(), self.apply_subst(subst, &ty)))
                    .collect();

                let return_ty = self.apply_subst(subst, &func.return_ty);

                // Check if the function is a lambda expression or has a body consisting of statements
                let is_lambda = func.body_expr.is_some() && func.body.is_none();

                if is_lambda {
                    let func_type = Function::new_with_expr(
                        func.name.clone(),
                        args.clone(),
                        return_ty.clone(),
                        func.body_expr.clone().unwrap(),
                    );

                    return Ty::Fn(Box::new(func_type));
                } else {
                    let func_type = Function::new_with_stmts(
                        func.name.clone(),
                        args.clone(),
                        return_ty.clone(),
                        func.body.clone().unwrap_or_default(),
                    );

                    return Ty::Fn(Box::new(func_type));
                }
            }

            // If the type is a record, apply the substitution to the fields
            Ty::Record(record) => {
                let fields = record
                    .fields
                    .iter()
                    .map(|(name, ty)| (name.clone(), self.apply_subst(subst, &ty)))
                    .collect();

                Ty::Record(Box::new(Record {
                    name: record.name.clone(),
                    fields,
                }))
            }

            // See above: Enums currently cannot have user-defined types, so we can
            // logically conclude that they can never be infinite types.
            Ty::Enum(_) => ty.clone(),

            // All other types are concrete - leave them alone
            _ => ty.clone(),
        }
    }

    pub fn check(&mut self, gamma: &mut Subst, stmt: &Span<Stmt>) -> Result<(), TypeError> {
        match &stmt.target {
            Stmt::Expr(expr) => {
                self.infer(gamma, &expr.target, expr.loc.clone())?;
                Ok(())
            }

            Stmt::Let { name, ty, value } => {
                let value_ty = self.infer(gamma, &value.target, value.loc.clone())?;
                let subst = self.unify(&ty, &value_ty, value.loc.clone())?;
                gamma.extend(subst);

                // Add the variable to the environment
                gamma.insert(name.clone(), ty.clone());
                Ok(())
            }

            Stmt::RecordDecl(record) => {
                let ty = record.to_type();
                let subst = self.unify(&ty, &record.to_type(), stmt.loc.clone())?;
                gamma.extend(subst);

                // Add the record to the environment
                gamma.insert(record.name.clone(), ty.clone());
                Ok(())
            }

            Stmt::EnumDecl(enum_decl) => {
                let ty = enum_decl.to_type();
                let subst = self.unify(&ty, &enum_decl.to_type(), stmt.loc.clone())?;
                gamma.extend(subst);

                // Add the enum to the environment
                gamma.insert(enum_decl.name.clone(), ty.clone());
                Ok(())
            }

            Stmt::FnDecl(func_decl) => {
                let mut arg_types = Vec::new();
                for (arg_name, arg_ty) in &func_decl.args {
                    arg_types.push((arg_name.clone(), arg_ty.clone()));
                }

                // Add the function to the environment
                // let func_ty = Ty::FnDecl(Box::new(FunctionDecl {
                //     name: func_decl.name.clone(),
                //     args: arg_types,
                //     return_ty: func_decl.return_ty.clone(),
                //     body: func_decl.body.clone(),
                // }));

                // gamma.insert(func_decl.name.clone(), func_decl.return_ty.clone());
                Ok(())
            }

            _ => todo!(),
        }
    }

    pub fn check_ast(&mut self, ast: Vec<Span<Stmt>>) -> Result<(), Vec<TypeError>> {
        let mut gamma = Subst::new();

        for stmt in ast {
            self.check(&mut gamma, &stmt)
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
