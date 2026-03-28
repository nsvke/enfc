use crate::driver::codegen::ccodegen::CCodeGen;

use super::IrProgram;

mod ccodegen;

pub(crate) struct CodeGen;
impl CodeGen {
    pub fn compile(ir: &IrProgram) -> CompileOutput {
        CCodeGen::generate(ir)
    }
}

pub trait Backend {
    fn generate(ir: &IrProgram) -> CompileOutput;
}

#[allow(dead_code)]
pub enum CompileOutput {
    SourceCode(String),
    Binary(Vec<u8>),
}

impl From<String> for CompileOutput {
    fn from(value: String) -> Self {
        Self::SourceCode(value)
    }
}
impl From<Vec<u8>> for CompileOutput {
    fn from(value: Vec<u8>) -> Self {
        Self::Binary(value)
    }
}
