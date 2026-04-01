mod codegen;
mod irgen;
mod lex;
mod parse;
mod typecheck;

use crate::{
    config::Config,
    diagnostic::Diagnostics,
    driver::{
        codegen::{CodeGen, CompileOutput},
        irgen::IrGenerator,
        lex::Lexer,
        parse::{Parser, Statement},
        typecheck::{TypeChecker, TypedStatement},
    },
};

pub use CompileOutput::{Binary, SourceCode};
pub use irgen::IrProgram;
pub use lex::{Token, TokenKind};
pub use typecheck::TypeKind;

const STD_PRELUDE: &str = include_str!("driver/std.enf");

pub struct Driver {
    packet: Packet,
    inject_std: bool,
}

impl Driver {
    pub fn new(config: Config) -> Self {
        Self {
            packet: Packet::new(Diagnostics::new(config)),
            inject_std: true,
        }
    }

    fn diagnose(&self) -> &Diagnostics {
        &self.packet.diagnostics
    }
    fn diagnose_mut(&mut self) -> &mut Diagnostics {
        &mut self.packet.diagnostics
    }

    pub fn run(&mut self) {
        let no_std_flag = "compile_flag \"no_std\";";
        let src = self.packet.diagnostics.source_code_mut();

        if src.trim_start().starts_with(no_std_flag) {
            self.inject_std = false;
            *src = src.replacen(no_std_flag, "", 1);
        }

        if self.inject_std {
            *src = format!("{}\n{}", STD_PRELUDE, src);
        }

        let lexer = Lexer::new(self.diagnose().source_code().char_indices().peekable());
        let tokens = lexer.tokenize();
        println!("------------------------------------------------------------------------------");
        println!("Token List");
        println!("------------------------------------------------------------------------------");
        tokens.iter().for_each(|t| {
            println!("{:?}", *t);
            println!(
                "\x1b[90m------------------------------------------------------------------------------\x1b[0m"
            );
        });

        let parser = Parser::new(&tokens, self.diagnose_mut());
        let statements = parser.parse();
        println!("------------------------------------------------------------------------------");
        println!("Statement List");
        println!("------------------------------------------------------------------------------");
        statements.iter().for_each(|t| {
            println!("{:?}", *t);
            println!(
                "\x1b[90m------------------------------------------------------------------------------\x1b[0m"
            );
        });
        self.packet.load_tokens(tokens);

        let typechecker = TypeChecker::new(self.diagnose_mut());
        let typed_statements = typechecker.check(&statements);
        println!("------------------------------------------------------------------------------");
        println!("TypedStatement List");
        println!("------------------------------------------------------------------------------");
        typed_statements.iter().for_each(|t| {
            println!("{:?}", *t);
            println!(
                "\x1b[90m------------------------------------------------------------------------------\x1b[0m"
            );
        });
        self.packet.load_asts(statements);

        //println!("------------------------------------------------------------------------------");
        if self.diagnose().has_errors() {
            println!(
                "\x1b[31m------------------------------------------------------------------------------"
            );
            println!("Errors");
            println!(
                "------------------------------------------------------------------------------\x1b[0m"
            );
            self.diagnose().print_errors();
            println!("{} errors occured.", self.diagnose().errors_len());
            return;
        }

        let irgen = IrGenerator::new();
        let ir = irgen.generate_from(&typed_statements);
        println!("------------------------------------------------------------------------------");
        println!("IR List");
        println!("------------------------------------------------------------------------------");
        ir.instructions().iter().enumerate().for_each(|(i,t)| {
            println!("{}: {:?}", i,*t);
            println!(
                "\x1b[90m------------------------------------------------------------------------------\x1b[0m"
            );
        });
        self.packet.load_tasts(typed_statements);

        let output = CodeGen::compile(&ir);
        println!("------------------------------------------------------------------------------");
        println!("C Code");
        println!("------------------------------------------------------------------------------");
        if let CompileOutput::SourceCode(c_code) = &output {
            println!("{}", c_code);
        }

        self.packet.load_ir(ir);
        self.packet.load_output(output.into());
    }

    pub fn result(self) -> Packet {
        self.packet
    }
}

pub struct Packet {
    tokens: Option<Vec<Token>>,
    asts: Option<Vec<Statement>>,
    tasts: Option<Vec<TypedStatement>>,
    ir: Option<IrProgram>,
    pub output: Option<CompileOutput>,
    pub diagnostics: Diagnostics,
}

impl Packet {
    pub fn new(diagnostics: Diagnostics) -> Self {
        Self {
            tokens: None,
            asts: None,
            tasts: None,
            ir: None,
            output: None,
            diagnostics,
        }
    }
    fn load_tokens(&mut self, tokens: Vec<Token>) {
        self.tokens = Some(tokens)
    }
    fn load_asts(&mut self, asts: Vec<Statement>) {
        self.asts = Some(asts)
    }
    fn load_tasts(&mut self, tasts: Vec<TypedStatement>) {
        self.tasts = Some(tasts)
    }
    fn load_ir(&mut self, ir: IrProgram) {
        self.ir = Some(ir)
    }
    fn load_output(&mut self, output: CompileOutput) {
        self.output = Some(output)
    }
}

// use anstyle::{AnsiColor, Color, Style};
// const ERROR_STYLE: Style = Style::new()
//     .fg_color(Some(Color::Ansi(AnsiColor::Red)))
//     .bold();
// anstream::println!("{ERROR_STYLE}merhaba{ERROR_STYLE:#}");
// return;
