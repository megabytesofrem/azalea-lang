use crate::ast::{Expr, Stmt};
use crate::codegen::js::Emit;
use crate::parse::span::Span;

#[derive(Debug, Clone)]
pub struct PrettyPrinter {
    indent_level: usize,
}

impl PrettyPrinter {
    pub fn new() -> Self {
        PrettyPrinter { indent_level: 0 }
    }

    pub fn indent(&mut self) {
        self.indent_level += 1;
    }

    pub fn dedent(&mut self) {
        if self.indent_level > 0 {
            self.indent_level -= 1;
        }
    }

    pub fn print_indent(&self) -> String {
        "  ".repeat(self.indent_level)
    }

    pub fn print(&self, code: &str) -> String {
        format!("{}{}", self.print_indent(), code)
    }
}
