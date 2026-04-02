use crate::compile_error::CompileError;
use crate::config::Config;
use crate::formatter::Formatter;

pub struct Diagnostics {
    pub config: Config,
    errors: Vec<CompileError>,
    formatter: Formatter,
}

impl Diagnostics {
    pub fn new(conf: Config) -> Self {
        Self {
            config: conf,
            errors: Vec::new(),
            formatter: Formatter::new(),
        }
    }
    pub fn src_path(&self) -> &String {
        &self.config.source_path
    }
    pub fn push_error(&mut self, e: CompileError) {
        self.errors.push(e);
    }
    pub fn print_errors(&self) {
        if !self.has_errors() {
            return;
        }

        for err in &self.errors {
            self.formatter
                .print(err, self.source_code(), self.src_path());
        }
    }

    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }
    pub fn errors(&self) -> &[CompileError] {
        &self.errors
    }
    pub fn errors_len(&self) -> usize {
        self.errors.len()
    }

    pub fn source_code_mut(&mut self) -> &mut String {
        &mut self.config.source_code
    }

    pub fn source_code(&self) -> &String {
        &self.config.source_code
    }

    pub fn empty_diagnostics(s: String) -> Self {
        Self {
            config: Config::empty_config(s),
            errors: Vec::new(),
            formatter: Formatter::new(),
        }
    }
}
