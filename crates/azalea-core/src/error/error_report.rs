use crate::lexer::{Op, SourceLoc};

/// An error report with pretty printing
#[derive(Debug)]
pub struct ErrorReport {
    pub message: String,
    pub source: String,
    pub hint: Option<String>,
    pub loc: SourceLoc,
}

impl ErrorReport {
    pub fn new(message: String, source: String, loc: SourceLoc) -> Self {
        Self {
            message,
            source,
            hint: None,
            loc,
        }
    }

    pub fn with_hint(mut self, hint: String) -> Self {
        self.hint = Some(hint);
        self
    }

    /// Show a pretty-printed error report with the source code snippet
    pub fn show_pretty_source(&self) -> String {
        let mut report = String::new();
        let lines: Vec<&str> = self.source.lines().collect();

        // println!("DEBUG: SOURCE: {}", self.source);
        // println!("DEBUG: LINES: {:?}", lines);

        // FIXME: Calculate the range of lines to show
        let start_line = self.loc.line;

        for (i, line) in lines.iter().enumerate() {
            let line_number = start_line + i + 1; // +1 for 1-based index
            report.push_str(&format!("{:>3} | {}\n", line_number, line));
            if line_number == self.loc.line {
                // Highlight the error location
                let marker = " ".repeat(self.loc.start - 1) + "^";
                report.push_str(&format!("    | {}\n", marker));
            }
        }

        if self.hint.is_some() {
            report.push_str(&format!("Hint: {}\n", self.hint.as_ref().unwrap()));
        }

        report.push_str(&format!("Error: {}\n", self.message));
        report
    }

    pub fn show_pretty_message(&self) -> String {
        format!(
            "Error (line {}, {}:{}): {}",
            self.loc.line, self.loc.start, self.loc.end, self.message
        )
    }
}

impl Default for ErrorReport {
    fn default() -> Self {
        Self {
            message: String::new(),
            source: String::new(),
            hint: None,
            loc: SourceLoc::default(),
        }
    }
}
