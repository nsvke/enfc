use crate::driver::TokenKind;
use crate::driver::TypeKind;
use crate::structs::Span;

#[derive(Debug)]
pub enum CompileError {
    Lexical(LexicalError),
    Parsing(ParsingError),
    Semantic(SemanticError),
}

#[derive(Debug)]
pub enum SemanticError {
    SymbolRedefination {
        name: String,
        old_span: Span,
        new_span: Span,
        kind: SymbolKind,
    },
    UnknownType {
        val: String,
        span: Span,
    },
    UnknownSymbol {
        val: String,
        span: Span,
    },
    NotUnaryType {
        val: String,
        span: Span,
    },
    TypeMismatch {
        l_typ: TypeKind,
        r_typ: TypeKind,
        l_span: Span,
        r_span: Span,
        op: String,
    },
}
#[derive(Debug)]
pub enum SymbolKind {
    Function,
    Variable,
}

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
    pub fn symbol_redefination(
        name: String,
        old_span: Span,
        new_span: Span,
        kind: SymbolKind,
    ) -> Self {
        CompileError::Semantic(SemanticError::SymbolRedefination {
            name,
            old_span,
            new_span,
            kind,
        })
    }
    pub fn unknown_type(val: String, span: Span) -> Self {
        CompileError::Semantic(SemanticError::UnknownType { val, span })
    }
    pub fn unknown_symbol(val: String, span: Span) -> Self {
        CompileError::Semantic(SemanticError::UnknownSymbol { val, span })
    }
    pub fn not_unary_type(val: String, span: Span) -> Self {
        CompileError::Semantic(SemanticError::NotUnaryType { val, span })
    }
    pub fn type_mismatch(
        l_typ: TypeKind,
        r_typ: TypeKind,
        l_span: Span,
        r_span: Span,
        op: String,
    ) -> Self {
        CompileError::Semantic(SemanticError::TypeMismatch {
            l_typ,
            r_typ,
            l_span,
            r_span,
            op,
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
            Self::Semantic(SemanticError::SymbolRedefination { new_span, .. }) => *new_span,
            Self::Semantic(SemanticError::UnknownType { span, .. }) => *span,
            Self::Semantic(SemanticError::UnknownSymbol { span, .. }) => *span,
            Self::Semantic(SemanticError::NotUnaryType { span, .. }) => *span,
            Self::Semantic(SemanticError::TypeMismatch { l_span, r_span, .. }) => Span {
                start: l_span.start,
                end: r_span.end,
            },
        }
    }
}
