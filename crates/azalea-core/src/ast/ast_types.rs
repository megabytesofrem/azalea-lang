use std::hash::Hash;

use crate::parse::span::Span;

use super::{Expr, Stmt};

#[derive(Debug, Clone)]
pub enum Ty {
    Int,
    Float,
    String,
    Bool,
    Unit,

    /// A hole in the type system
    Any,

    /// Represents a type that we don't know yet, will be resolved later on
    Unresolved,

    /// A type variable is a placeholder for a type that can be replaced with any type.
    /// For example, in the type `List[A]`, `A` is a type variable.
    Var(String),

    /// A type constructor is a type that is _parameterized_ by other types.
    /// The following are valid type constructors:
    /// - `List[A]`
    /// - `List[Int]`
    ///
    /// This is not a type constructor:
    /// - `String` ❌
    TypeCons(String, Vec<Ty>),

    /// A type constrained by a type class.
    /// An example of a constraint is `Num[A]`, which means that `A` must be a numeric type.
    /// This is used to represent type classes in the type system.
    Constraint(String, Vec<Ty>),

    /// Maps directly to JavaScript arrays
    Array(Box<Ty>),

    /// Allows us to pass functions around as values
    Fn(Box<Function>),

    /// Record and enum types
    Record(Box<Record>),
    Enum(Box<Enum>),

    /// A universally quantified polymorphic type, e.g. `forall a. a -> a`
    ForAll(Vec<String>, Box<Ty>),
}

// Implement PartialEq, Eq and Hash for `Ty` since it's needed in
// typecheck.

impl PartialEq for Ty {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Ty::Var(a), Ty::Var(b)) => a == b,
            (Ty::TypeCons(a, _), Ty::TypeCons(b, _)) => a == b,
            (Ty::Array(a), Ty::Array(b)) => a == b,
            (Ty::Fn(a), Ty::Fn(b)) => a == b,
            (Ty::Record(a), Ty::Record(b)) => a == b,
            (Ty::Enum(a), Ty::Enum(b)) => a == b,

            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

impl Eq for Ty {}

impl Hash for Ty {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        core::mem::discriminant(self).hash(state);
    }
}

/// A pair of a name and a type
pub type TypedPair = (String, Ty);

/// Records are semantically equivalent to JavaScript objects
#[derive(Debug, Clone, PartialEq)]
pub struct Record {
    pub name: String,
    pub fields: Vec<(String, Ty)>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Enum {
    pub name: String,
    pub variants: Vec<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub name: String,
    pub args: Vec<(String, Ty)>,

    // Generic type parameters, e.g. `fn foo[T](x: T): T`
    pub type_params: Vec<String>,

    pub return_ty: Ty,

    // Optional body, if this is used in a function declaration
    pub body: Option<Vec<Span<Stmt>>>,
    pub body_expr: Option<Box<Span<Expr>>>, // If this is a lambda expression
}

#[derive(Debug, Clone, PartialEq)]
pub struct RecordExpr {
    pub name: String,
    pub fields: Vec<(String, Expr)>,
}

impl Record {
    pub fn to_type(&self) -> Ty {
        Ty::Record(Box::new(self.clone()))
    }
}

impl Enum {
    pub fn to_type(&self) -> Ty {
        Ty::Enum(Box::new(self.clone()))
    }
}

impl Function {
    pub fn to_type(&self) -> Ty {
        Ty::Fn(Box::new(self.clone()))
    }

    pub fn new_with_empty(name: String, args: Vec<(String, Ty)>, return_ty: Ty) -> Self {
        Self {
            name,
            args,
            type_params: vec![],
            return_ty,
            body: None,
            body_expr: None,
        }
    }

    pub fn new_with_stmts(
        name: String,
        args: Vec<(String, Ty)>,
        return_ty: Ty,
        body: Vec<Span<Stmt>>,
    ) -> Self {
        Self {
            name,
            args,
            type_params: vec![],
            return_ty,
            body: Some(body),
            body_expr: None,
        }
    }

    pub fn new_with_expr(
        name: String,
        args: Vec<(String, Ty)>,
        return_ty: Ty,
        body_expr: Box<Span<Expr>>,
    ) -> Self {
        Self {
            name,
            args,
            type_params: vec![],
            return_ty,
            body: None,
            body_expr: Some(body_expr),
        }
    }
}
