#![allow(unused)]
use std::collections::HashSet;

use crate::driver::{
    IrProgram,
    codegen::{Backend, CompileOutput},
    irgen::{Instruction, IrType},
};

trait AsCType {
    fn as_c_type(&self) -> String;
}

impl AsCType for IrType {
    fn as_c_type(&self) -> String {
        match self {
            IrType::I32 => "int".into(), //"int64_t",
            IrType::Bool => "bool".into(),
            IrType::Char => "char".into(),
            IrType::Str => "const char*".into(),
            IrType::Void => "void".into(),
            IrType::Reference(inner_type) => format!("{}*", inner_type.as_c_type()),
        }
    }
}

pub(crate) struct CCodeGen<'a> {
    ir: &'a IrProgram,
    stack: Vec<String>,
    output: String,
    current_params: Vec<String>,
}

impl<'a> CCodeGen<'a> {
    fn new(ir: &'a IrProgram) -> Self {
        CCodeGen {
            ir,
            stack: Vec::new(),
            output: String::new(),
            current_params: Vec::new(),
        }
    }
    fn run(mut self) -> Self {
        let mut jump_targets = HashSet::new();
        for instr in self.ir.instructions() {
            match instr {
                Instruction::Jump(target) | Instruction::JumpIfFalse(target) => {
                    jump_targets.insert(*target);
                }
                _ => {}
            }
        }

        self.output.push_str("#include <stdio.h>\n");
        self.output.push_str("#include <stdint.h>\n");
        self.output.push_str("#include <stdbool.h>\n");

        for (index, instr) in self.ir.instructions().iter().enumerate() {
            if jump_targets.contains(&index) {
                self.output
                    .push_str(&format!("instr_index_{}: // -> {:?}\n", index, instr));
            }

            match instr {
                Instruction::PushInt(i) => {
                    self.stack.push(i.to_string());
                }
                Instruction::PushBool(b) => {
                    self.stack.push(b.to_string());
                }
                Instruction::PushChar(c) => {
                    self.stack.push(format!("'{}'", c));
                }
                Instruction::PushStrId(id) => {
                    let s = self.ir.get_str(*id);
                    self.stack.push(format!("\"{}\"", s));
                }
                Instruction::Load(id) => {
                    self.stack.push(format!("enf_var_{}", id));
                }
                Instruction::Store(id) => {
                    let r = self.stack_pop();
                    self.output.push_str(&format!("enf_var_{} = {};\n", *id, r));
                }
                Instruction::Init(id, typ) => {
                    let r = self.stack_pop();
                    self.output.push_str(&format!(
                        "{} enf_var_{} = {};\n",
                        typ.as_c_type(),
                        *id,
                        r
                    ));
                }
                Instruction::AddressOf(id) => {
                    self.stack.push(format!("&enf_var_{}", id));
                }
                Instruction::LoadIndirect => {
                    let addr = self.stack_pop();
                    self.stack.push(format!("*({})", addr));
                }
                Instruction::StoreIndirect => {
                    let addr = self.stack_pop();
                    let val = self.stack_pop();
                    self.output.push_str(&format!("*({}) = {};\n", addr, val));
                }
                Instruction::Add => {
                    let (l, r) = self.stack_pop_binary();
                    self.stack.push(format!("({} + {})", l, r));
                }
                Instruction::Sub => {
                    let (l, r) = self.stack_pop_binary();
                    self.stack.push(format!("({} - {})", l, r));
                }
                Instruction::Mul => {
                    let (l, r) = self.stack_pop_binary();
                    self.stack.push(format!("({} * {})", l, r));
                }
                Instruction::Div => {
                    let (l, r) = self.stack_pop_binary();
                    self.stack.push(format!("({} / {})", l, r));
                }
                Instruction::Mod => {
                    let (l, r) = self.stack_pop_binary();
                    self.stack.push(format!("({} % {})", l, r));
                }
                Instruction::Eq => {
                    let (l, r) = self.stack_pop_binary();
                    self.stack.push(format!("({} == {})", l, r));
                }
                Instruction::Lt => {
                    let (l, r) = self.stack_pop_binary();
                    self.stack.push(format!("({} < {})", l, r));
                }
                Instruction::Gt => {
                    let (l, r) = self.stack_pop_binary();
                    self.stack.push(format!("({} > {})", l, r));
                }
                Instruction::Neg => {
                    let o = self.stack_pop();
                    self.stack.push(format!("(-({}))", o));
                }
                Instruction::Not => {
                    let o = self.stack_pop();
                    self.stack.push(format!("(!({}))", o));
                }
                Instruction::Jump(index) => {
                    self.output
                        .push_str(&format!("goto instr_index_{};\n", index));
                }
                Instruction::JumpIfFalse(index) => {
                    let cond = self.stack_pop();
                    self.output
                        .push_str(&format!("if (!({})) goto instr_index_{};\n", cond, index));
                }
                Instruction::Call(id, len) => {
                    self.current_params.clear();
                    for i in 0..*len {
                        let param = self.stack_pop();
                        self.current_params.push(param);
                    }
                    self.current_params.reverse();
                    let fun_name = self.ir.get_fun_name(*id);

                    let res = format!("enf_fun_{}({})", fun_name, self.current_params.join(", "));
                    self.stack.push(res);
                }
                Instruction::ExternCall(id, len) => {
                    self.current_params.clear();
                    for i in 0..*len {
                        let param = self.stack_pop();
                        self.current_params.push(param);
                    }
                    self.current_params.reverse();
                    let fun_name = self.ir.get_fun_name(*id);

                    let res = format!("{}({})", fun_name, self.current_params.join(", "));
                    self.stack.push(res);
                }
                Instruction::ExternFunStart(id, typ) => {
                    let name = self.ir.get_fun_name(*id);
                    self.output
                        .push_str(&format!("{} {}(", typ.as_c_type(), name));
                    self.current_params.clear();
                }
                Instruction::ExternFunParam(id, typ) => {
                    self.current_params.push(typ.as_c_type()); // TODO no name mangle for extern fun params
                }
                Instruction::ExternFunEnd => {
                    self.output.push_str(&self.current_params.join(", "));
                    self.output.push_str(");\n");
                }
                Instruction::FunStart(id, typ) => {
                    let name = self.ir.get_fun_name(*id);
                    self.output
                        .push_str(&format!("{} enf_fun_{}(", typ.as_c_type(), name));
                    self.current_params.clear();
                }
                Instruction::FunParam(id, typ) => {
                    self.current_params
                        .push(format!("{} enf_var_{}", typ.as_c_type(), *id));
                }
                Instruction::FunBodyStart => {
                    self.output.push_str(&self.current_params.join(", "));
                    self.output.push_str(") {\n");
                }
                Instruction::FunEnd => {
                    self.output.push_str("}\n");
                }
                Instruction::Discard => {
                    let val = self.stack_pop();
                    self.output.push_str(&format!("{};\n", val));
                }
                Instruction::Ret(has_value) => {
                    if *has_value {
                        let val = self.stack_pop();
                        self.output.push_str(&format!("return {};\n", val));
                    } else {
                        self.output.push_str("return;\n");
                    }
                }
            }
        }

        self.output
            .push_str("int main() { enf_fun_main(); return 0; }");
        self
    }

    fn stack_pop_binary(&mut self) -> (String, String) {
        let r = self.stack.pop().expect("stack underflow");
        let l = self.stack.pop().expect("stack underflow");
        (l, r)
    }

    fn stack_pop(&mut self) -> String {
        self.stack.pop().expect("stack underflow")
    }

    fn into_output(self) -> CompileOutput {
        CompileOutput::SourceCode(self.output)
    }
}

impl<'a> Backend for CCodeGen<'a> {
    fn generate(ir: &IrProgram) -> CompileOutput {
        CCodeGen::new(ir).run().into_output()
    }
}
