use crate::driver::TokenKind;
use crate::structs::Span;

#[derive(Debug)]
pub enum CompileError {
    Lexical(LexicalError),
    Parsing(ParsingError),
    Semantic(SemanticError),
}

#[derive(Debug)]
pub enum SemanticError {}
#[derive(Debug)]
pub enum ParsingError {
    UnexpectedToken {
        expected: TokenKind,
        found: TokenKind,
        span: Span,
    },
}

// maybe write these as semantic error
#[derive(Debug)]
pub enum LexicalError {
    UnterminatedLiteral { value: String, span: Span },
}

pub enum LiteralError {
    ZeroChar,
}
impl CompileError {
    pub fn unterminated_literal(value: String, span: Span) -> Self {
        CompileError::Lexical(LexicalError::UnterminatedLiteral { value, span })
    }
    pub fn unexpected_token(expected: TokenKind, found: TokenKind, span: Span) -> Self {
        CompileError::Parsing(ParsingError::UnexpectedToken {
            expected,
            found,
            span,
        })
    }
}

pub trait ErrorFormatter {
    fn format(&self, err: &CompileError, source_code: &str, path: &str);
}

impl CompileError {
    pub fn span(&self) -> Span {
        match self {
            Self::Lexical(LexicalError::UnterminatedLiteral { span, .. }) => *span,
            Self::Parsing(ParsingError::UnexpectedToken { span, .. }) => *span,
            Self::Semantic(_) => Span::default(),
        }
    }
}
