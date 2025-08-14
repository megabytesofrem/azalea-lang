use std::collections::{HashMap, HashSet};

use crate::ast::ast_types::{Enum, EnumVariant, EnumVariantPayload, Function, Ty};
use crate::ast::pretty::Pretty;
use crate::ast::{Expr, Literal, Member, Stmt, ToplevelStmt};
use crate::codegen::pp::PrettyPrinter;
use crate::lexer::{Op, SourceLoc};
use crate::parse::span::{Span, spanned};
use crate::typeck::global_env::GlobalEnv;

#[derive(Debug, Clone)]
pub struct JSCodegen {
    ast: Vec<Span<ToplevelStmt>>,
    pp: PrettyPrinter,

    /// The generated JavaScript code.
    pub js_code: String,

    // Used to map scoped bindings e.g `let x = 5;` to `freeze.x`
    relative_to_binding: Option<String>,
    relative_bindings: HashSet<String>,

    pub functions: HashMap<String, Function>,
    pub global_env: GlobalEnv,
}

/// Trait for generating code from an AST.
pub trait Emit {
    fn visit_expr(&mut self, expr: &Expr) -> String;
    fn visit_toplevel(&mut self, toplevel: &ToplevelStmt) -> String;
    fn visit_stmt(&mut self, stmt: &Stmt) -> String;

    fn visit_block(&mut self, block: Vec<Span<Stmt>>) -> String;

    fn emit_code(&mut self, ast: Vec<Span<ToplevelStmt>>);
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
        _ => "undefined".to_string(), // Fallback for unsupported types
    }
}

impl JSCodegen {
    pub fn new() -> Self {
        JSCodegen {
            ast: Vec::new(),
            pp: PrettyPrinter::new(),
            js_code: String::new(),
            relative_to_binding: None,
            relative_bindings: HashSet::new(),

            functions: HashMap::new(),
            global_env: GlobalEnv::new(),
        }
    }

    fn lookup_function(&self, name: &str) -> Option<Function> {
        self.functions.get(name).cloned()
    }

    fn lookup_enum(&self, name: &str) -> Option<&Enum> {
        self.global_env.lookup_enum(name)
    }

    fn is_enum_variant(&self, member: &Member) -> bool {
        // Split the member name into the enum name and variant name

        // Check if this is a member of an enum variant
        if !member.name.contains('.') {
            return false;
        }

        let member_name = member.name.split(".").collect::<Vec<_>>()[0];
        let enum_variant = member.name.split(".").collect::<Vec<_>>()[1];

        if let Some(enum_decl) = self.lookup_enum(&member_name) {
            return enum_decl.variants.iter().any(|v| v.name == enum_variant);
        }
        false
    }

    fn visit_bin_op(&mut self, left: &Expr, op: &Op, right: &Expr) -> String {
        let left_js = self.visit_expr(left);
        let right_js = self.visit_expr(right);
        match op {
            Op::Add => format!("{} + {}", left_js, right_js),
            Op::Sub => format!("{} - {}", left_js, right_js),
            Op::Mul => format!("{} * {}", left_js, right_js),
            Op::Div => format!("{} / {}", left_js, right_js),
            Op::DoubleEq => {
                // Special case for equality check
                if self.is_enum_variant(&Member {
                    target: Box::new(spanned(left.clone(), SourceLoc::default())),
                    name: right_js.clone(),
                }) {
                    return format!("{}.tag === '{}'", left_js, right_js);
                }

                format!("{} === {}", left_js, right_js)
            }
            Op::NotEq => {
                // Special case for inequality check
                if self.is_enum_variant(&Member {
                    target: Box::new(spanned(left.clone(), SourceLoc::default())),
                    name: right_js.clone(),
                }) {
                    return format!("{}.tag !== '{}'", left_js, right_js);
                }

                format!("{} !== {}", left_js, right_js)
            }
            Op::Less => format!("{} < {}", left_js, right_js),
            Op::LessEq => format!("{} <= {}", left_js, right_js),
            Op::Greater => format!("{} > {}", left_js, right_js),
            Op::GreaterEq => format!("{} >= {}", left_js, right_js),
            Op::Neg => format!("-{}", right_js), // Unary negation
            Op::Not => format!("!{}", right_js), // Unary not

            Op::Dollar => {
                // Special case for curried function calls using dollar
                let target = self.visit_expr(left);
                let arg = self.visit_expr(right);
                format!("{}({})", target, arg)
            }
            _ => panic!("Unsupported binary operator: {:?}", op),
        }
    }

    fn generate_extern_call(&mut self, extern_name: &str, args: &[Span<Expr>]) -> String {
        // Check if this is a prototype method: "Type.prototype.method"
        if let Some(method_name) = self.extract_prototype_method(extern_name) {
            return self.generate_prototype_call(&method_name, args);
        }

        // Handle other extern patterns
        match extern_name {
            "console.log" => {
                let args_str = args
                    .iter()
                    .map(|arg| self.visit_expr(&arg.target))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("console.log({})", args_str)
            }
            extern_name if extern_name.starts_with("Math.") => {
                let args_str = args
                    .iter()
                    .map(|arg| self.visit_expr(&arg.target))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{}({})", extern_name, args_str)
            }
            _ => {
                // Fallback to regular function call
                let args_str = args
                    .iter()
                    .map(|arg| self.visit_expr(&arg.target))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{}({})", extern_name, args_str)
            }
        }
    }

    fn extract_prototype_method(&self, extern_name: &str) -> Option<String> {
        // Match pattern "*.prototype.*"
        if let Some(prototype_pos) = extern_name.find(".prototype.") {
            let method_start = prototype_pos + ".prototype.".len();
            if method_start < extern_name.len() {
                return Some(extern_name[method_start..].to_string());
            }
        }
        None
    }

    fn generate_prototype_call(&mut self, method_name: &str, args: &[Span<Expr>]) -> String {
        if args.is_empty() {
            panic!(
                "Prototype method '{}' expects at least one argument (the receiver)",
                method_name
            );
        }

        // First argument is the receiver (the object to call the method on)
        let receiver = self.visit_expr(&args[0].target);

        // Remaining arguments are the method arguments
        let method_args: Vec<String> = args[1..]
            .iter()
            .map(|arg| self.visit_expr(&arg.target))
            .collect();

        if method_args.is_empty() {
            // HACK: Remove the trailing quote from method names, need a better way
            format!(
                "{}.{}()",
                receiver,
                method_name.strip_suffix('"').unwrap_or(method_name)
            )
        } else {
            format!(
                "{}.{}({})",
                receiver,
                method_name.strip_suffix('"').unwrap_or(method_name),
                method_args.join(", ")
            )
        }
    }

    fn visit_function_call(&mut self, target: &Expr, args: Vec<Span<Expr>>) -> String {
        if let Expr::Ident(name) = target {
            // Check if the function is an extern function
            if let Some(func) = self.lookup_function(name) {
                if func.is_extern {
                    println!("DEBUG: Codegen: Extern function call: {}", name);

                    return self.generate_extern_call(&func.extern_name.unwrap(), &args);
                }
            }

            // Regular function call
            let args_emit: Vec<String> = args
                .iter()
                .map(|arg| self.visit_expr(&arg.target))
                .collect();
            return format!("{}({})", name, args_emit.join(", "));
        } else {
            // If the target is not an identifier, visit it as an expression
            let target_js = self.visit_expr(target);
            let args_emit: Vec<String> = args
                .iter()
                .map(|arg| self.visit_expr(&arg.target))
                .collect();
            format!("{}({})", target_js, args_emit.join(", "))
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

            Expr::Ident(name) => {
                if self.relative_to_binding.is_some() && self.relative_bindings.contains(name) {
                    // If we are in a scoped binding, prepend the relative binding
                    return format!("{}.{}", self.relative_to_binding.as_ref().unwrap(), name);
                }

                name.clone()
            }
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
                println!(
                    "DEBUG: Codegen: Visit Function call: {}",
                    target.target.pretty()
                );
                self.visit_function_call(&target.target, args.to_vec())
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

    fn visit_toplevel(&mut self, toplevel: &ToplevelStmt) -> String {
        match &toplevel {
            ToplevelStmt::Stmt(stmt) => {
                // Visit the statement and return its JS representation
                self.visit_stmt(&stmt.target)
            }

            ToplevelStmt::RecordDecl(record) => {
                // FIXME: This should use the default value of the field type, need to get this

                let fields: Vec<String> = record
                    .fields
                    .iter()
                    .map(|(name, ty)| format!("{}: {}", name, default_js_value(ty)))
                    .collect();

                if !self.global_env.is_record_defined(&record.name) {
                    self.global_env.define_record(record.clone());
                }

                format!(
                    "const /*record*/ {} = {{ {} }};",
                    record.name,
                    fields.join(", ")
                )
            }

            ToplevelStmt::EnumDecl(enum_decl) => {
                let mut constructors = HashMap::new();

                if !self.global_env.is_enum_defined(&enum_decl.name) {
                    self.global_env.define_enum(enum_decl.clone());
                }

                for variant in &enum_decl.variants {
                    match &variant.payload {
                        EnumVariantPayload::None => {
                            constructors.insert(
                                variant.name.clone(),
                                (
                                    format!("{}_{}", enum_decl.name, variant.name),
                                    format!(
                                        "const {}_{} = {{ tag: '{}' }};",
                                        enum_decl.name, variant.name, variant.name,
                                    ),
                                ),
                            );
                        }

                        EnumVariantPayload::Tuple(args) => {
                            let arg_names = (0..args.len())
                                .map(|i| format!("arg{}", i))
                                .collect::<Vec<_>>();

                            constructors.insert(
                                variant.name.clone(),
                                (
                                    format!("{}_{}", enum_decl.name, variant.name),
                                    format!(
                                        "function {}_{}({}) {{ return {{ tag: '{}', values: [{}] }}; }}",
                                        enum_decl.name,
                                        variant.name,
                                        arg_names.join(", "),
                                        variant.name,
                                        arg_names.join(", ")
                                    ),
                                ),
                            );
                        }

                        EnumVariantPayload::Record(fields) => {
                            constructors.insert(variant.name.clone(), (
                                format!("{}_{}", enum_decl.name, variant.name),
                                format!(
                                    "function {}_{}(record) {{ return {{ tag: '{}', ...record }}; }}",
                                    enum_decl.name, variant.name, variant.name
                                ),
                            ));
                        }
                    }
                }

                let mut code = String::new();

                code.push_str(&format!("// Constructors for {}\n\n", enum_decl.name));
                for (_name, (_, body)) in &constructors {
                    code.push_str(&self.pp.print(&format!("{}\n", body)));
                }

                code.push_str(&format!("// Object for enum {}\n", enum_decl.name));
                code.push_str(&format!("const /*enum*/ {} = {{\n", enum_decl.name));

                for (name, (key, _)) in &constructors {
                    self.pp.indent();
                    code.push_str(&self.pp.print(&format!("{}: {},\n", name, key)));
                    self.pp.dedent();
                }
                code.push_str("};\n");

                code
            }

            ToplevelStmt::ExternDecl(func) => {
                // For extern functions, we just emit the function signature
                // without any body, as they are expected to be defined in JS.
                let func_args: Vec<String> =
                    func.args.iter().map(|(name, _ty)| name.clone()).collect();

                if !self.global_env.is_extern_function(&func.name) {
                    self.global_env.define_extern(func.clone());
                }

                format!(
                    "/* extern: function {}({}) */",
                    func.name,
                    func_args.join(", ")
                )
            }

            ToplevelStmt::FnDecl(func) => {
                // Discard any type information for JS
                let func_args: Vec<String> =
                    func.args.iter().map(|(name, _ty)| name.clone()).collect();

                if !self.functions.contains_key(&func.name) {
                    self.functions.insert(func.name.clone(), func.clone());
                }

                let mut bindings_code = String::new();

                if !func.where_bindings.is_empty() {
                    // Handle where bindings
                    let where_bindings = func
                        .where_bindings
                        .iter()
                        .map(|binding| {
                            format!(
                                "{}: {}",
                                binding.name,
                                self.visit_expr(&binding.value.target)
                            )
                        })
                        .collect::<Vec<_>>()
                        .join(", ");

                    // Freeze the object (closest to true immutability in JS)
                    bindings_code =
                        format!("const freeze = Object.freeze({{ {} }})", where_bindings);
                }

                let body_emit = if func.body.is_none() {
                    // Single expression function
                    if func.body_expr.is_some() {
                        self.relative_to_binding = Some("freeze".to_string());
                        self.relative_bindings = func
                            .where_bindings
                            .iter()
                            .map(|binding| binding.name.clone())
                            .collect();

                        let body_expr = self.visit_expr(&func.body_expr.as_ref().unwrap().target);

                        // If we have any bindings, prepend them
                        let to_return = if func.where_bindings.is_empty() {
                            // No where bindings, just return the expression
                            format!(
                                "function {}({}) {{ return {}; }}",
                                func.name,
                                func_args.join(", "),
                                body_expr
                            )
                        } else {
                            // With where bindings, we need to include them
                            format!(
                                "function {}({}) {{ {}; return {}; }}",
                                func.name,
                                func_args.join(", "),
                                bindings_code,
                                body_expr
                            )
                        };

                        // Reset relative binding after use
                        self.relative_to_binding = None;
                        self.relative_bindings.clear();

                        to_return
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
                format!("let /*mut*/ {} = {};", name, value_emit)
            }

            Stmt::Assign { target, value } => {
                let target_emit = self.visit_expr(&target.target);
                let value_emit = self.visit_expr(&value.target);
                format!("{} = {};", target_emit, value_emit)
            }

            Stmt::For {
                target,
                iterable,
                body,
            } => {
                let target_emit = self.visit_expr(&target.target);
                let iterable_emit = self.visit_expr(&iterable.target);
                let body_emit = self.visit_block(body.clone());

                format!(
                    "for (const {} of {}) {{\n{}\n}}",
                    target_emit, iterable_emit, body_emit
                )
            }

            Stmt::While { cond, body } => {
                let cond_emit = self.visit_expr(&cond.target);
                let body_emit = self.visit_block(body.clone());
                format!("while ({}) {{\n{}\n}}", cond_emit, body_emit)
            }
        }
    }

    fn visit_block(&mut self, block: Vec<Span<Stmt>>) -> String {
        let mut block_code = String::new();

        self.pp.indent();
        let len = block.len();
        for (i, stmt) in block.into_iter().enumerate() {
            match &stmt.target {
                Stmt::Expr(expr) if i == len - 1 => {
                    // Last statement and it's an expression: emit as return
                    let expr_js = self.visit_expr(&expr.target);
                    block_code.push_str(&self.pp.print(&format!("return {};", expr_js)));
                }
                _ => {
                    let stmt_js = self.visit_stmt(&stmt.target);
                    block_code.push_str(&self.pp.print(&stmt_js));
                }
            }
            block_code.push('\n');
        }
        self.pp.dedent();

        block_code
    }

    fn emit_code(&mut self, ast: Vec<Span<ToplevelStmt>>) {
        self.ast = ast;

        for toplevel in self.ast.clone() {
            let stmt_js = self.visit_toplevel(&toplevel.target);
            self.js_code.push_str(&self.pp.print(&stmt_js));
            self.js_code.push('\n');
        }
    }
}
