#![allow(unused)]
use std::collections::HashSet;

use crate::driver::{
    IrProgram,
    codegen::{Backend, CompileOutput},
    irgen::{Instruction, IrType},
};

trait AsCType {
    fn as_c_type(&self) -> String;
    fn as_c_array_format(&self) -> (String, String);
}

impl AsCType for IrType {
    fn as_c_type(&self) -> String {
        match self {
            IrType::I32 => "int".into(), //"int64_t",
            IrType::F32 => "float".into(),
            IrType::Bool => "bool".into(),
            IrType::Char => "char".into(),
            IrType::Str => "const char*".into(),
            IrType::Void => "void".into(),
            IrType::Data(id) => format!("struct enf_data_{}", id),
            IrType::Reference(inner_type) => format!("{}*", inner_type.as_c_type()),
            IrType::Array(inner_type, _) => {
                format!("{}*", inner_type.as_c_type())
            }
        }
    }
    fn as_c_array_format(&self) -> (String, String) {
        match self {
            Self::Array(inner, size) => {
                let (base, dims) = inner.as_c_array_format();
                (base, format!("[{}]{}", size, dims))
            }
            _ => (self.as_c_type(), String::new()),
        }
    }
}

pub(crate) struct CCodeGen<'a> {
    ir: &'a IrProgram,
    stack: Vec<String>,
    output: String,
    current_params: Vec<String>,
    current_fun_sign: String,
    fun_signs: Vec<String>,
    current_data_decl: String,
    data_decls: Vec<String>,
    current_array: Vec<String>,
    main_return_type: IrType,
}

impl<'a> CCodeGen<'a> {
    fn new(ir: &'a IrProgram) -> Self {
        CCodeGen {
            ir,
            stack: Vec::new(),
            output: String::new(),
            current_params: Vec::new(),
            current_fun_sign: String::new(),
            current_array: Vec::new(),
            fun_signs: Vec::new(),
            current_data_decl: String::new(),
            data_decls: Vec::new(),
            main_return_type: IrType::Void,
        }
    }
    fn run(mut self) -> Self {
        let mut output = String::new();

        let mut jump_targets = HashSet::new();
        for instr in self.ir.instructions() {
            match instr {
                Instruction::Jump(target) | Instruction::JumpIfFalse(target) => {
                    jump_targets.insert(*target);
                }
                _ => {}
            }
        }

        for (index, instr) in self.ir.instructions().iter().enumerate() {
            if jump_targets.contains(&index) {
                output.push_str(&format!("instr_index_{}: // -> {:?}\n", index, instr));
            }

            match instr {
                Instruction::PushInt(i) => self.stack.push(i.to_string()),
                Instruction::PushFloat(fl) => self.stack.push(fl.to_string()),
                Instruction::PushBool(b) => self.stack.push(b.to_string()),
                Instruction::PushChar(c) => self.stack.push(format!("'{}'", c)),
                Instruction::PushStrId(id) => {
                    let s = self.ir.get_str(*id);
                    self.stack.push(format!("\"{}\"", s));
                }
                Instruction::PushValueId(id) => self.stack.push(id.to_string()),
                Instruction::PushNull => self.stack.push("NULL".into()),
                Instruction::PushZeroInit => self.stack.push("{0}".into()),
                Instruction::Load => {
                    let x_id = self.stack_pop();
                    self.stack.push(format!("enf_var_{}", x_id));
                }
                Instruction::Store => {
                    let name = self.stack_pop();
                    let r = self.stack_pop();
                    output.push_str(&format!("{} = {};\n", name, r));
                }
                Instruction::Init(id, typ) => {
                    let r = self.stack_pop();

                    match &typ {
                        IrType::Array(_, _) => {
                            let (base_type, dimensions) = typ.as_c_array_format();
                            if r.starts_with('{') {
                                output.push_str(&format!(
                                    "{} enf_var_{}{} = {};\n",
                                    base_type, id, dimensions, r
                                ));
                            } else {
                                output
                                    .push_str(&format!("{}* enf_var_{} = {};\n", base_type, id, r));
                            }
                        }
                        _ => output.push_str(&format!(
                            "{} enf_var_{} = {};\n",
                            typ.as_c_type(),
                            *id,
                            r
                        )),
                    }
                }
                Instruction::AddressOf(id) => self.stack.push(format!("&enf_var_{}", id)),
                Instruction::LoadIndirect => {
                    let addr = self.stack_pop();
                    self.stack.push(format!("*({})", addr));
                }
                Instruction::StoreIndirect => {
                    let addr = self.stack_pop();
                    let val = self.stack_pop();
                    output.push_str(&format!("*({}) = {};\n", addr, val));
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
                Instruction::BitwiseOr => {
                    let (l, r) = self.stack_pop_binary();
                    self.stack.push(format!("({} | {})", l, r));
                }
                Instruction::BitwiseXor => {
                    let (l, r) = self.stack_pop_binary();
                    self.stack.push(format!("({} ^ {})", l, r));
                }
                Instruction::BitwiseAnd => {
                    let (l, r) = self.stack_pop_binary();
                    self.stack.push(format!("({} & {})", l, r));
                }
                Instruction::BitwiseLeftShift => {
                    let (l, r) = self.stack_pop_binary();
                    self.stack.push(format!("({} << {})", l, r));
                }
                Instruction::BitwiseRightShift => {
                    let (l, r) = self.stack_pop_binary();
                    self.stack.push(format!("({} >> {})", l, r));
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
                Instruction::Tilde => {
                    let o = self.stack_pop();
                    self.stack.push(format!("(~({}))", o));
                }
                Instruction::Jump(index) => {
                    output.push_str(&format!("goto instr_index_{};\n", index));
                }
                Instruction::JumpIfFalse(index) => {
                    let cond = self.stack_pop();
                    output.push_str(&format!("if (!({})) goto instr_index_{};\n", cond, index));
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

                    output.push_str(&format!("{} {}(", typ.as_c_type(), name));

                    self.current_fun_sign
                        .push_str(&format!("{} {}(", typ.as_c_type(), name));

                    self.current_params.clear();
                }
                Instruction::ExternFunParam(id, typ) => {
                    self.current_params.push(typ.as_c_type()); // TODO no name mangle for extern fun params
                }
                Instruction::ExternFunEnd => {
                    let params = self.current_params.join(", ");
                    output.push_str(&params);
                    output.push_str(");\n");

                    self.current_fun_sign.push_str(&params);
                    self.current_fun_sign.push_str(");\n");

                    self.fun_signs.push(self.current_fun_sign.clone());
                    self.current_fun_sign.clear();
                }
                Instruction::FunStart(id, typ) => {
                    let name = self.ir.get_fun_name(*id);

                    if name == "main" {
                        self.main_return_type = typ.clone();
                    }

                    output.push_str(&format!("{} enf_fun_{}(", typ.as_c_type(), name));

                    self.current_fun_sign.push_str(&format!(
                        "{} enf_fun_{}(",
                        typ.as_c_type(),
                        name
                    ));

                    self.current_params.clear();
                }
                Instruction::FunParam(id, typ) => {
                    self.current_params
                        .push(format!("{} enf_var_{}", typ.as_c_type(), *id))
                }
                Instruction::FunBodyStart => {
                    let params = self.current_params.join(", ");
                    output.push_str(&params);
                    output.push_str(") {\n");

                    self.current_fun_sign.push_str(&params);
                    self.current_fun_sign.push_str(");\n");

                    self.fun_signs.push(self.current_fun_sign.clone());
                    self.current_fun_sign.clear();
                }
                Instruction::FunEnd => output.push_str("}\n"),
                Instruction::ArrayStart => self.current_array.push(String::from("{")),
                Instruction::ArrayElem => {
                    let elem = self.stack_pop();

                    if let Some(curr_arr) = self.current_array.last_mut() {
                        curr_arr.push_str(&format!("{},", elem));
                    }
                }
                Instruction::ArrayEnd => {
                    if let Some(mut completed_array) = self.current_array.pop() {
                        completed_array.push('}');
                        self.stack.push(completed_array);
                    }
                }
                Instruction::IndexAccess => {
                    let index = self.stack_pop();
                    let target = self.stack_pop();
                    self.stack.push(format!("{}[{}]", target, index));
                }
                Instruction::DataFieldAccess(id) => {
                    let target = self.stack_pop();
                    self.stack.push(format!("{}.enf_field_{}", target, id));
                }
                Instruction::DataStart(id) => {
                    self.current_data_decl.clear();
                    self.current_data_decl
                        .push_str(&format!("struct enf_data_{} {{", id));
                }
                Instruction::DataField(id, typ) => {
                    let (base_type, dimensions) = typ.as_c_array_format();
                    self.current_data_decl
                        .push_str(&format!("{} enf_field_{}{};", base_type, id, dimensions));
                }
                Instruction::DataEnd => {
                    self.current_data_decl.push_str("};\n");
                    self.data_decls.push(self.current_data_decl.clone());
                }
                Instruction::DataInit(id, len) => {
                    let mut values = Vec::new();
                    for i in 0..(*len) {
                        let val = self.stack_pop();
                        let val_id = self.stack_pop();
                        values.push(format!(".enf_field_{} = {}", val_id, val));
                    }
                    let values_joined = values.join(", ");
                    let full = format!("(struct enf_data_{}){{ {} }}", id, values_joined);
                    self.stack.push(full);
                }
                Instruction::Discard => {
                    let val = self.stack_pop();
                    output.push_str(&format!("{};\n", val));
                }
                Instruction::Ret(has_value) => {
                    if *has_value {
                        let val = self.stack_pop();
                        output.push_str(&format!("return {};\n", val));
                    } else {
                        output.push_str("return;\n");
                    }
                }
            }
        }

        match self.main_return_type {
            IrType::Void => output.push_str("int main(int argc, char** argv) { enf_env_argc = argc; enf_env_argv = argv; enf_fun_main(); return 0; }"),
            IrType::I32 => output.push_str("int main(int argc, char** argv) { enf_env_argc = argc; enf_env_argv = argv; return enf_fun_main(); }"),
            _ => unreachable!(),
        }
        let head = self.generate_head();

        self.output.push_str(&head);
        self.output.push_str(&output);

        self
    }

    fn generate_head(&mut self) -> String {
        let mut head = String::new();
        for included in self.ir.included_headers() {
            head.push_str(&format!("#include<{}>\n", included));
        }
        head.push_str("#include <stdint.h>\n#include <stdbool.h>\n");

        for data in &self.data_decls {
            head.push_str(data);
        }

        for fun in &self.fun_signs {
            head.push_str(fun);
        }

        head.push_str(self.ir.inject());

        head.push('\n');

        head
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
