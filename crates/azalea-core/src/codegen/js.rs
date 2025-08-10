use crate::ast::ast_types::Ty;
use crate::ast::pretty::Pretty;
use crate::ast::{Expr, Literal, Stmt};
use crate::codegen::pp::PrettyPrinter;
use crate::lexer::Op;
use crate::parse::span::Span;

#[derive(Debug, Clone)]
pub struct JSCodegen {
    ast: Vec<Span<Stmt>>,
    pp: PrettyPrinter,

    /// The generated JavaScript code.
    pub js_code: String,
}

/// Trait for generating code from an AST.
pub trait Emit {
    fn visit_expr(&mut self, expr: &Expr) -> String;
    fn visit_stmt(&mut self, stmt: &Stmt) -> String;

    fn visit_block(&mut self, block: Vec<Span<Stmt>>) -> String;

    fn emit_code(&mut self, ast: Vec<Span<Stmt>>);
}

/// Desugar an if-else expression into a ternary expression.
/// Only works for single-statement branches.
pub fn desugar_to_ternary(
    emit: &mut impl Emit,
    cond: &Span<Expr>,
    then: Vec<Span<Stmt>>,
    else_: Option<Vec<Span<Stmt>>>,
) -> String {
    if then.len() > 1 || else_.as_ref().map_or(false, |e| e.len() > 1) {
        panic!("Cannot handle multi-statement branches in a ternary");
    }

    let then_str = emit.visit_stmt(&then.first().unwrap().target);

    let else_str = else_
        .map(|else_block| emit.visit_stmt(&else_block.first().unwrap().target))
        .unwrap_or_else(|| "undefined".to_string());

    format!(
        "({} ? {} : {})",
        emit.visit_expr(&cond.target).replace("\n", ""),
        then_str.replace("\n", ""),
        else_str.replace("\n", "")
    )
}

fn default_js_value(ty: &Ty) -> String {
    match ty {
        Ty::Int => "(0)|0".to_string(),
        Ty::Float => "0.0".to_string(),
        Ty::String => "\"\"".to_string(),
        Ty::Bool => "false".to_string(),
        Ty::Unit => "undefined".to_string(),
        Ty::Array(inner_ty) => {
            // Default value for an empty array of the given type
            format!("[] /* ARRAY[{}] */", inner_ty.pretty())
        }
        Ty::TypeCons(name, args) => {
            // For user-defined types, we can return a default value if one exists
            // This is a placeholder; in a real implementation, you might want to
            // generate a constructor call or similar.

            if args.is_empty() {
                format!("{}", default_js_value(&Ty::Unit))
            } else {
                "{}".to_string()
            }
        }
        Ty::Record(record) => {
            // For records, we can return an empty record with the fields initialized to their defaults
            let fields: Vec<String> = record
                .fields
                .iter()
                .map(|(name, ty)| format!("{}: {}", name, default_js_value(ty)))
                .collect();
            format!("{{ {} }}", fields.join(", "))
        }
        Ty::Enum(_) => {
            // TODO: Serialize enums to JS objects or something alike
            // For now, we can just return an empty object
            "/* ENUM */ {}".to_string()
        }

        Ty::ForAll(_, _) => {
            // For polymorphic types, we cannot generate a default value
            // since they are not concrete types.
            "/* FORALL */ undefined".to_string()
        }

        _ => panic!("Unsupported type for default value: {:?}", ty),
    }
}

impl JSCodegen {
    pub fn new() -> Self {
        JSCodegen {
            ast: Vec::new(),
            pp: PrettyPrinter::new(),
            js_code: String::new(),
        }
    }

    fn visit_bin_op(&mut self, left: &Expr, op: &Op, right: &Expr) -> String {
        let left_js = self.visit_expr(left);
        let right_js = self.visit_expr(right);
        match op {
            Op::Add => format!("{} + {}", left_js, right_js),
            Op::Sub => format!("{} - {}", left_js, right_js),
            Op::Mul => format!("{} * {}", left_js, right_js),
            Op::Div => format!("{} / {}", left_js, right_js),
            Op::DoubleEq => format!("{} === {}", left_js, right_js),
            Op::NotEq => format!("{} !== {}", left_js, right_js),
            Op::Less => format!("{} < {}", left_js, right_js),
            Op::LessEq => format!("{} <= {}", left_js, right_js),
            Op::Greater => format!("{} > {}", left_js, right_js),
            Op::GreaterEq => format!("{} >= {}", left_js, right_js),
            Op::Neg => format!("-{}", right_js), // Unary negation
            Op::Not => format!("!{}", right_js), // Unary not
            _ => panic!("Unsupported binary operator: {:?}", op),
        }
    }

    fn visit_unary_op(&mut self, op: &Op, expr: &Expr) -> String {
        let expr_js = self.visit_expr(expr);
        match op {
            Op::Neg => format!("-{}", expr_js),
            Op::Not => format!("!{}", expr_js),
            _ => panic!("Unsupported unary operator: {:?}", op),
        }
    }
}

impl Emit for JSCodegen {
    fn visit_expr(&mut self, expr: &Expr) -> String {
        match &expr {
            Expr::Literal(lit) => match lit {
                Literal::Int(i) => format!("{}|0", i.to_string()),
                Literal::Float(f) => f.to_string(),
                Literal::String(s) => format!("{}", s),
                Literal::Bool(b) => b.to_string(),
            },

            Expr::Ident(name) => name.clone(),
            Expr::MemberAccess(member) => {
                // TODO: Will this handle nested member accesses?
                let emit = self.visit_expr(&member.target.target);
                format!("{}.{}", emit, member.name)
            }
            Expr::BinOp(left, op, right) => self.visit_bin_op(&left.target, op, &right.target),
            Expr::UnOp(op, expr) => self.visit_unary_op(op, &expr.target),

            Expr::Record(record) => {
                let fields: Vec<String> = record
                    .fields
                    .iter()
                    .map(|(name, expr)| format!("{}: {}", name, self.visit_expr(&expr)))
                    .collect();
                format!("/*{}*/ {{ {} }}", record.name, fields.join(", "))
            }
            Expr::Array { elements } => {
                let emit: Vec<String> = elements
                    .iter()
                    .map(|elem| self.visit_expr(&elem.target))
                    .collect();
                format!("[{}]", emit.join(", "))
            }
            Expr::ArrayIndex { target, index } => {
                let target_emit = self.visit_expr(&target.target);
                let index_emit = self.visit_expr(&index.target);
                format!("{}[{}]", target_emit, index_emit)
            }
            Expr::FnCall { target, args } => {
                let target_emit = self.visit_expr(&target.target);
                let args_emit: Vec<String> = args
                    .iter()
                    .map(|arg| self.visit_expr(&arg.target))
                    .collect();

                format!("{}({})", target_emit, args_emit.join(", "))
            }
            Expr::Lam {
                args,
                return_ty: _,
                body,
            } => {
                // Discard any type information for JS
                let args_emit: Vec<String> = args.iter().map(|(name, _)| name.clone()).collect();
                let body_emit = self.visit_expr(&body.target);

                self.pp.print(&format!(
                    "function({}) {{ return {}; }}",
                    args_emit.join(", "),
                    body_emit
                ))
            }
            Expr::If { cond, then, else_ } => {
                let cond_emit = self.visit_expr(&cond.target);

                // Check if we can desugar to ternary: both branches must have exactly one Stmt::Expr
                let can_desugar = then.len() == 1
                    && matches!(then[0].target, Stmt::Expr(_))
                    && else_.as_ref().map_or(true, |else_branch| {
                        else_branch.len() == 1 && matches!(else_branch[0].target, Stmt::Expr(_))
                    });

                if can_desugar {
                    desugar_to_ternary(
                        self,
                        &cond,
                        then.to_vec(),
                        else_.as_ref().map(|e| e.to_vec()),
                    )
                } else {
                    // Use block syntax for multi-statement or non-expression branches
                    let mut code = String::new();
                    code.push_str(&format!("if ({}) {{\n", cond_emit));
                    self.pp.indent();
                    code.push_str(&self.visit_block(then.clone()));
                    self.pp.dedent();
                    code.push_str(&self.pp.print("}\n"));

                    if let Some(else_branch) = else_ {
                        code.push_str(&self.pp.print("else {\n"));
                        self.pp.indent();
                        code.push_str(&self.visit_block(else_branch.clone()));
                        self.pp.dedent();
                        code.push_str(&self.pp.print("}\n"));
                    }
                    code
                }
            }

            // Unimplemented expressions
            _ => "// TODO: Unsupported expression".into(),
        }
    }

    fn visit_stmt(&mut self, stmt: &Stmt) -> String {
        match &stmt {
            Stmt::Expr(expr) => self.visit_expr(&expr.target),
            Stmt::Let { name, ty: _, value } => {
                let value_emit = self.visit_expr(&value.target);
                format!("const {} = {};", name, value_emit)
            }

            Stmt::Mut { name, ty: _, value } => {
                let value_emit = self.visit_expr(&value.target);
                format!("let /*MUT*/ {} = {};", name, value_emit)
            }

            Stmt::Assign { name, value } => {
                let value_emit = self.visit_expr(&value.target);
                format!("{} = {};", name, value_emit)
            }

            Stmt::RecordDecl(record) => {
                // FIXME: This should use the default value of the field type, need to get this

                let fields: Vec<String> = record
                    .fields
                    .iter()
                    .map(|(name, ty)| format!("{}: {}", name, default_js_value(ty)))
                    .collect();
                format!(
                    "const /*RECORD*/ {} = {{ {} }};",
                    record.name,
                    fields.join(", ")
                )
            }

            Stmt::EnumDecl(enum_decl) => {
                let variants: Vec<String> = enum_decl
                    .variants
                    .iter()
                    .map(|variant| format!("\"{}\"", variant))
                    .collect();
                format!(
                    "const /*ENUM*/ {} = {{ {} }};",
                    enum_decl.name,
                    variants.join(", ")
                )
            }
            Stmt::FnDecl(func) => {
                // Discard any type information for JS
                let func_args: Vec<String> =
                    func.args.iter().map(|(name, _ty)| name.clone()).collect();

                let body_emit = if func.body.is_none() {
                    // Single expression function
                    if func.body_expr.is_some() {
                        let body_expr = self.visit_expr(&func.body_expr.as_ref().unwrap().target);
                        format!(
                            "function {}({}) {{ return {}; }}",
                            func.name,
                            func_args.join(", "),
                            body_expr
                        )
                    } else {
                        // Empty function body
                        format!("function {}({}) {{}}", func.name, func_args.join(", "))
                    }
                } else {
                    println!("Function with a block body");

                    // Function with a block body
                    let body = self.visit_block(func.body.as_ref().unwrap().clone());

                    format!(
                        "function {}({}) {{\n{}\n}}",
                        func.name,
                        func_args.join(", "),
                        body
                    )
                };

                body_emit
            }

            _ => todo!(),
        }
    }

    fn visit_block(&mut self, block: Vec<Span<Stmt>>) -> String {
        let mut block_code = String::new();

        self.pp.indent();
        for stmt in block {
            let stmt_js = self.visit_stmt(&stmt.target);
            block_code.push_str(&self.pp.print(&stmt_js));
            block_code.push('\n');
        }
        self.pp.dedent();

        block_code
    }

    fn emit_code(&mut self, ast: Vec<Span<Stmt>>) {
        self.ast = ast;

        for stmt in self.ast.clone() {
            let stmt_js = self.visit_stmt(&stmt.target);
            self.js_code.push_str(&self.pp.print(&stmt_js));
            self.js_code.push('\n');
        }
    }
}
