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

        let error_line = self.loc.line.saturating_sub(1);
        let line_count = lines.len();

        report.push_str(&format!("Error: {}\n", self.message));

        // Calculate the range of lines to show around the error
        let start_line = error_line.saturating_sub(3);

        let end_line = (error_line + 3).min(line_count - 1);

        for line_idx in start_line..=end_line {
            if line_idx < line_count {
                let line_num = line_idx + 1; // Convert to 1-based index
                let is_error_line = line_idx == error_line;

                let line_prefix = if is_error_line {
                    format!("{:>3} > | ", line_num)
                } else {
                    format!("{:>3} | ", line_num)
                };

                report.push_str(&format!("{}{}\n", line_prefix, lines[line_idx]));

                // Add error marker if this is the error line
                if is_error_line {
                    let marker = " ".repeat(self.loc.start.saturating_sub(1))
                        + &"^".repeat((self.loc.end - self.loc.start).max(1));

                    report.push_str(&format!("    {}\n", marker));
                }
            }
        }

        if self.hint.is_some() {
            report.push_str(&format!("Hint: {}\n", self.hint.as_ref().unwrap()));
        }

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
