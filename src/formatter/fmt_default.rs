use crate::compile_error::{CompileError, ErrorFormatter, LexicalError, ParsingError};

pub struct SimpleFormatter;
impl ErrorFormatter for SimpleFormatter {
    fn format(&self, err: &CompileError, _: &str, _: &str) {
        match err {
            CompileError::Lexical(LexicalError::UnterminatedLiteral { value, span }) => {
                eprintln!(
                    "Unterminated literal '{}' at {},{}",
                    value, span.start, span.end
                );
            }
            CompileError::Parsing(ParsingError::UnexpectedToken {
                expected,
                found,
                span,
            }) => {
                eprintln!(
                    "Unexpected token. expected: '{:?}', found: '{:?}' at {},{}",
                    expected, found, span.start, span.end
                );
            }
            _ => eprintln!("holder error.."),
        }
    }
}
