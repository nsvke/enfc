mod fmt_default;
mod fmt_miette;

use crate::compile_error::{CompileError, ErrorFormatter};
#[cfg(feature = "miette")]
use crate::formatter::fmt_miette::fmt_miette_impl::MietteFormatter;
use fmt_default::SimpleFormatter;

pub struct Formatter {
    inner: Box<dyn ErrorFormatter>,
}
#[allow(unreachable_code)]
impl Formatter {
    pub fn new() -> Self {
        #[cfg(feature = "miette")]
        return Self {
            inner: Box::new(MietteFormatter),
        };

        Self {
            inner: Box::new(SimpleFormatter),
        }
    }
    pub fn print(&self, err: &CompileError, src: &str, path: &str) {
        self.inner.format(err, src, path);
    }
}
