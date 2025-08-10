use std::collections::{HashMap, HashSet};
use std::hash::Hash;

use crate::ast::ast_types::{Function, Record, Ty};

use crate::ast::pretty::Pretty;
use crate::lexer::SourceLoc;
use crate::typeck::typecheck::{Typechecker, TypingEnv};

impl Typechecker {
    /// Find all free type variables in a type.
    pub fn find_free_type_vars(&self, ty: &Ty) -> HashSet<String> {
        match ty {
            Ty::Var(v) => HashSet::from([v.clone()]),
            Ty::Array(inner) => self.find_free_type_vars(inner),
            Ty::Fn(func) => {
                let mut vars = func
                    .args
                    .iter()
                    .flat_map(|(_, ty)| self.find_free_type_vars(ty))
                    .collect::<HashSet<_>>();

                vars.extend(self.find_free_type_vars(&func.return_ty));
                vars
            }

            Ty::TypeCons(_name, type_params) => {
                // TypeCons now represents only actual type constructors like records/enums
                // Type parameters are represented as Ty::Var instead
                type_params
                    .iter()
                    .flat_map(|ty| self.find_free_type_vars(ty))
                    .collect::<HashSet<_>>()
            }

            // All other types are concrete and have no free variables
            _ => HashSet::new(),
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
                return self.hydrate_type(inner_ty, &subst);
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

    /// This function is similar to `instantiate`, but it substitutes type parameters from a
    /// provided typing environment whereas `instantiate` creates fresh type variables
    pub fn instantiate_type(&self, ty: &Ty, instantiation_env: &TypingEnv) -> Ty {
        // Use this when we know what the type parameters should be substituted with
        // whereas `instantiate` creates a new instance of a polymorphic type, with fresh
        // type variables.

        let instantiated = match ty {
            Ty::TypeCons(name, args) if args.is_empty() => {
                // This might be a type parameter, since it looks like `A`
                // foo :: A -> A
                instantiation_env
                    .get(name)
                    .cloned()
                    .unwrap_or_else(|| ty.clone())
            }
            Ty::TypeCons(name, args) => {
                // For type constructors, instantiate each type parameter recursively
                let instantiated_args: Vec<Ty> = args
                    .iter()
                    .map(|arg| self.instantiate_type(arg, instantiation_env))
                    .collect();

                Ty::TypeCons(name.clone(), instantiated_args)
            }

            // For all other types, we simply return the type as is
            _ => ty.clone(),
        };

        println!(
            "DEBUG: Instantiate type '{}' with {} => {}",
            ty.pretty(),
            self.pretty_print_env(instantiation_env),
            instantiated.pretty()
        );

        instantiated
    }

    pub fn collect_type_params(&self, ty: &Ty, type_params: &mut HashSet<String>) {
        match ty {
            Ty::Var(name) => {
                // Type variables represent type parameters
                type_params.insert(name.clone());
            }
            Ty::TypeCons(_, args) => {
                // TypeCons represents actual type constructors, recurse into arguments
                for arg in args {
                    self.collect_type_params(arg, type_params);
                }
            }
            Ty::Array(inner) => {
                self.collect_type_params(inner, type_params);
            }
            Ty::Fn(func) => {
                for (_, arg_ty) in &func.args {
                    self.collect_type_params(arg_ty, type_params);
                }
                self.collect_type_params(&func.return_ty, type_params);
            }
            _ => {}
        }
    }

    pub fn pretty_print_env(&self, map: &TypingEnv) -> String {
        let mut result = String::new();
        result.push_str("Γ{");

        let entries: Vec<String> = map
            .iter()
            .map(|(var, ty)| format!("{} := {}", var, ty.pretty()))
            .collect();

        result.push_str(&entries.join(", "));
        result.push_str("}");
        result
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

        println!("DEBUG: Vars bound in environment Γ: {:?}", env_vars);

        // Filter out any type variables already bound in the environment
        let generalized_vars: HashSet<String> = free_vars
            .into_iter()
            .filter(|var| !env_vars.contains(var))
            .collect();

        if generalized_vars.is_empty() {
            ty.clone()
        } else {
            let result = Ty::ForAll(generalized_vars.clone(), Box::new(ty.clone()));
            result
        }
    }

    /// Hydrate a type by applying substitutions from the typing environment.
    ///
    /// This function resolves type variables based on the provided substition map.
    /// If a type variable has no substitution, it remains unchanged.
    pub fn hydrate_type(&self, ty: &Ty, subst: &TypingEnv) -> Ty {
        match &*ty {
            // If the type is a type level variable, we check if there is a substitution for it
            Ty::Var(var) => subst.get(var).cloned().unwrap_or(Ty::Var(var.to_string())),

            // If the type is an array of any type, apply the substitution to the inner type
            Ty::Array(inner) => Ty::Array(Box::new(self.hydrate_type(&inner, subst))),

            Ty::Fn(func) => {
                let args: Vec<(String, Ty)> = func
                    .args
                    .iter()
                    .map(|(name, ty)| (name.clone(), self.hydrate_type(&ty, subst)))
                    .collect();

                let return_ty = self.hydrate_type(&func.return_ty, subst);

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

            // Handle type constructors with parameters, including records and enums
            // since they are represented nominally as type constructors.
            Ty::TypeCons(name, params) => {
                if params.is_empty() && subst.contains_key(name) {
                    return subst[name].clone();
                }

                let hydrated_params: Vec<Ty> = params
                    .iter()
                    .map(|param| self.hydrate_type(param, subst))
                    .collect();

                Ty::TypeCons(name.clone(), hydrated_params)
            }

            // All other types are concrete - leave them alone
            _ => ty.clone(),
        }
    }
}
