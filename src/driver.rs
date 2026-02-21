mod lexer;
mod parser;
mod semantic_analyzer;
// mod ir_gen;

pub use lexer::{Token, TokenKind};

use crate::{
    config::Config,
    diagnostic::Diagnostics,
    driver::{lexer::Lexer, parser::Parser, semantic_analyzer::SemanticAnalyzer},
};

pub struct Driver {
    diagnose: Diagnostics,
}

impl Driver {
    pub fn new(config: Config) -> Self {
        Self {
            diagnose: Diagnostics::new(config),
        }
    }

    pub fn run(&mut self) {
        let lexer = Lexer::new(self.diagnose.source_code().char_indices().peekable());
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

        let mut parser = Parser::new(&tokens, &mut self.diagnose);
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

        let mut sa = SemanticAnalyzer::new(&statements, &mut self.diagnose);
        let typed_statements = sa.analyze();
        println!("------------------------------------------------------------------------------");
        println!("TypedStatement List");
        println!("------------------------------------------------------------------------------");
        typed_statements.iter().for_each(|t| {
            println!("{:?}", *t);
            println!(
                "\x1b[90m------------------------------------------------------------------------------\x1b[0m"
            );
        });

        //println!("------------------------------------------------------------------------------");
        if self.diagnose.has_errors() {
            println!(
                "\x1b[31m------------------------------------------------------------------------------"
            );
            println!("Errors");
            println!(
                "------------------------------------------------------------------------------\x1b[0m"
            );
            self.diagnose.print_errors();
        }
    }
}

// use anstyle::{AnsiColor, Color, Style};
// const ERROR_STYLE: Style = Style::new()
//     .fg_color(Some(Color::Ansi(AnsiColor::Red)))
//     .bold();
// anstream::println!("{ERROR_STYLE}merhaba{ERROR_STYLE:#}");
// return;
