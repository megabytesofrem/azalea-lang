use crate::ast::ast_types::{RecordExpr, Ty};
use crate::ast::{Expr, Literal, Member};
use crate::lexer::{Op, SourceLoc};

/// Trait for pretty-printing AST nodes, annotated with their source location.
///
/// Do note that this trait is only used for debugging the AST and pretty-printing type information.
/// It is not intended for user-facing output.
pub trait Pretty {
    fn pretty(&self) -> String;
    fn pretty_with_loc(&self, loc: &SourceLoc) -> String;
}

fn op_to_string(op: &Op) -> String {
    match op {
        Op::Add => "+",
        Op::Sub => "-",
        Op::Mul => "*",
        Op::Div => "/",
        Op::DoubleEq => "==",
        Op::Not => "!",
        Op::NotEq => "!=",
        Op::Less => "<",
        Op::Greater => ">",
        Op::LessEq => "<=",
        Op::GreaterEq => ">=",
        Op::Neg => "-",
    }
    .into()
}

/// Pretty-print a node
pub fn pretty<T: Pretty>(item: &T) -> String {
    item.pretty()
}

/// Pretty-print a node with its source location attached
pub fn pretty_with_loc<T: Pretty>(item: &T, loc: &SourceLoc) -> String {
    item.pretty_with_loc(loc)
}

impl Pretty for Ty {
    fn pretty(&self) -> String {
        match self {
            Ty::Int => "Int".to_string(),
            Ty::Float => "Float".to_string(),
            Ty::String => "String".to_string(),
            Ty::Bool => "Bool".to_string(),
            Ty::Unit => "Unit".to_string(),
            Ty::Any => "Any".to_string(),
            Ty::Unresolved => "?".to_string(),

            // A type variable
            Ty::Var(name) => format!("ᵗ{}", name),

            // A type constructor with optional type arguments
            Ty::TypeCons(name, args) => {
                if args.is_empty() {
                    name.clone()
                } else {
                    let args_str: Vec<String> = args.iter().map(|arg| arg.pretty()).collect();
                    format!("{}[{}]", name, args_str.join(", "))
                }
            }
            Ty::Array(ty) => format!("[{}]", ty.pretty()),
            Ty::Fn(func) => {
                let args_str: Vec<String> = func
                    .args
                    .iter()
                    .map(|(name, ty)| {
                        format!(
                            "{}: {}",
                            if !name.is_empty() { name } else { "_" },
                            ty.pretty()
                        )
                    })
                    .collect();

                let return_ty_str = func.return_ty.pretty();
                format!("fn({}) -> {}", args_str.join(", "), return_ty_str)
            }

            // A universally quantified polymorphic type
            Ty::ForAll(vars, ty) => {
                let vars_str: Vec<String> = vars.iter().map(|v| v.to_string()).collect();
                format!("∀{}. {}", vars_str.join(", "), ty.pretty())
            }

            ty => todo!("Not implemented: {:?}", ty),
        }
    }

    fn pretty_with_loc(&self, loc: &SourceLoc) -> String {
        format!("{} @ {}", self.pretty(), loc)
    }
}

impl Pretty for Literal {
    fn pretty(&self) -> String {
        match self {
            Literal::Int(i) => format!("{}", i),
            Literal::Float(f) => format!("{}", f),
            Literal::String(s) => format!("{}", s),
            Literal::Bool(b) => format!("{}", b),
        }
    }

    fn pretty_with_loc(&self, loc: &SourceLoc) -> String {
        format!("{} @ {}", self.pretty(), loc)
    }
}

impl Pretty for Member {
    fn pretty(&self) -> String {
        format!("{}.{}", self.target.target.pretty(), self.name)
    }

    fn pretty_with_loc(&self, loc: &SourceLoc) -> String {
        format!("{} @ {}", self.pretty(), loc)
    }
}

impl Pretty for RecordExpr {
    fn pretty(&self) -> String {
        let fields: Vec<String> = self
            .fields
            .iter()
            .map(|(name, expr)| format!("{}: {}", name, expr.pretty()))
            .collect();
        format!("{} {{ {} }}", self.name, fields.join(", "))
    }

    fn pretty_with_loc(&self, loc: &SourceLoc) -> String {
        format!("{} @ {}", self.pretty(), loc)
    }
}

impl Pretty for Expr {
    fn pretty(&self) -> String {
        match self {
            Expr::Literal(lit) => lit.pretty(),
            Expr::Ident(ident) => ident.to_string(),
            Expr::MemberAccess(member) => member.pretty(),
            Expr::BinOp(left, op, right) => format!(
                "({} {} {})",
                left.target.pretty(),
                op_to_string(op),
                right.target.pretty()
            ),
            Expr::UnOp(op, expr) => format!("({} {})", op_to_string(op), expr.target.pretty()),
            Expr::Record(record) => record.pretty(),
            Expr::Array { elements } => {
                let elems: Vec<String> = elements.iter().map(|e| e.target.pretty()).collect();
                format!("[{}]", elems.join(", "))
            }
            Expr::ArrayIndex { target, index } => {
                format!("{}[{}]", target.target.pretty(), index.target.pretty())
            }
            Expr::FnCall { target, args } => {
                let args_str: Vec<String> = args.iter().map(|arg| arg.target.pretty()).collect();
                format!("{}({})", target.target.pretty(), args_str.join(", "))
            }
            Expr::Lam {
                args,
                return_ty,
                body,
            } => {
                let args_str: Vec<String> = args.iter().map(|arg| arg.clone().0).collect();
                let return_ty_str = format!(" -> {}", return_ty.pretty());
                format!(
                    "fn({}){} = {}",
                    args_str.join(", "),
                    return_ty_str,
                    body.target.pretty()
                )
            }

            e => {
                // For any other expression, we just use the debug format
                format!("{:?}", e)
            }
        }
    }

    fn pretty_with_loc(&self, loc: &SourceLoc) -> String {
        format!("{} @ {}", self.pretty(), loc)
    }
}
