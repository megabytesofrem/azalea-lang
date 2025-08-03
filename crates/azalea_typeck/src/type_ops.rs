use std::collections::{HashMap, HashSet};

use azalea_parse::ast::ast_types::{Function, Record, Ty};

use crate::typecheck::{Typechecker, TypingEnv};

impl Typechecker {
    /// Find all free type variables in a type.
    pub fn find_free_type_vars(&self, ty: &Ty) -> Vec<String> {
        match ty {
            Ty::Var(v) => vec![v.clone()],
            Ty::Array(inner) => self.find_free_type_vars(inner),
            Ty::Fn(func) => {
                let mut vars = func
                    .args
                    .iter()
                    .flat_map(|(_, ty)| self.find_free_type_vars(ty))
                    .collect::<Vec<_>>();

                vars.extend(self.find_free_type_vars(&func.return_ty));
                vars
            }
            Ty::Record(record) => record
                .fields
                .iter()
                .flat_map(|(_, ty)| self.find_free_type_vars(ty))
                .collect(),
            Ty::TypeCons(_, type_params) => type_params
                .iter()
                .flat_map(|ty| self.find_free_type_vars(ty))
                .collect(),

            // All other types are concrete and have no free variables
            _ => vec![],
        }
    }

    /// Instantiate a polymorphic type, replacing all quantified type variables
    ///
    /// In the case of a `forall` type we replace each type variable with a fresh type variable.
    /// For example, `forall A. A -> Int` becomes `t0 -> Int
    ///
    pub fn instantiate(&mut self, ty: &Ty) -> Ty {
        match ty {
            Ty::ForAll(vars, inner_ty) => {
                // `forall A, B. A -> B` becomes `t0 -> t1` where `t0` and `t1` are fresh type variables
                let mut subst = HashMap::new();

                for var in vars {
                    subst.insert(var.clone(), Ty::Var(self.fresh()));
                }

                // Apply the substitution to the inner type
                return self.hydrate_type(&subst, inner_ty);
            }

            Ty::TypeCons(name, ty_params) => {
                // For type constructors, instantiate each type parameter recursively
                // e.g: `Option<A, B>` becomes `Option<t0, t1> where `t0` and `t1` are fresh type variables
                let instantiated_params: Vec<Ty> =
                    ty_params.iter().map(|ty| self.instantiate(ty)).collect();

                Ty::TypeCons(name.clone(), instantiated_params)
            }

            // For all other cases, we simply return the type as is
            _ => ty.clone(),
        }
    }

    /// Generalize a type by universally quantifying free type variables.
    pub fn generalize(&mut self, env: &TypingEnv, ty: &Ty) -> Ty {
        // Collect all free type variables
        let free_vars = self.find_free_type_vars(ty);

        // Find all free type variables for each type in the environment
        let env_vars: HashSet<String> = env
            .values()
            .flat_map(|ty| self.find_free_type_vars(ty))
            .collect();

        // Filter out any type variables already bound in the environment
        // leaving us with the free variables
        let generalized_vars: Vec<String> = free_vars
            .into_iter()
            .filter(|var| !env_vars.contains(var))
            .collect();

        if generalized_vars.is_empty() {
            ty.clone()
        } else {
            // Return a universally quantified type, a forall type
            Ty::ForAll(generalized_vars, Box::new(ty.clone()))
        }
    }

    /// Hydrate a type by applying substitutions from the typing environment.
    ///
    /// This function resolves type variables based on the provided substition map.
    /// If a type variable has no substitution, it remains unchanged.
    pub fn hydrate_type(&self, subst: &TypingEnv, ty: &Ty) -> Ty {
        match &*ty {
            // If the type is a type level variable, we check if there is a substitution for it
            Ty::Var(var) => subst.get(var).cloned().unwrap_or(Ty::Var(var.to_string())),

            // If the type is an array of any type, apply the substitution to the inner type
            Ty::Array(inner) => Ty::Array(Box::new(self.hydrate_type(subst, &inner))),

            Ty::Fn(func) => {
                let args: Vec<(String, Ty)> = func
                    .args
                    .iter()
                    .map(|(name, ty)| (name.clone(), self.hydrate_type(subst, &ty)))
                    .collect();

                let return_ty = self.hydrate_type(subst, &func.return_ty);

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
                    .map(|(name, ty)| (name.clone(), self.hydrate_type(subst, &ty)))
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
}
