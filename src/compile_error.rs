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
    SymbolRedefinition {
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
        op: char,
    },
    TypeMismatch {
        l_typ: TypeKind,
        r_typ: TypeKind,
        l_span: Span,
        r_span: Span,
        op: String,
        kind: MismatchKind,
    },
    NotCallable {
        val: TypeKind,
        span: Span,
    },
    MissingArgument {
        expected: usize,
        found: usize,
        span: Span,
    },
    UnexpectedType {
        expected: TypeKind,
        found: TypeKind,
        span: Span,
    },
    DivideByZero {
        span: Span,
    },
    FeatureNotSupported {
        feature_name: String,
        span: Span,
    },
    NotMutable {
        val: String,
        span: Span,
    },
    InvalidReturnLocation {
        span: Span,
    },
    NestedFunction {
        span: Span,
    },
    MissingReturn {
        span: Span,
    },
}
#[derive(Debug)]
pub enum SymbolKind {
    Function,
    Variable,
    Parameter,
    Builtin,
}
#[derive(Debug, PartialEq)]
pub enum MismatchKind {
    Regular,
    Return,
    Binary,
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
    pub fn symbol_redefinition(
        name: String,
        old_span: Span,
        new_span: Span,
        kind: SymbolKind,
    ) -> Self {
        CompileError::Semantic(SemanticError::SymbolRedefinition {
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
    pub fn not_unary_type(val: String, span: Span, op: char) -> Self {
        CompileError::Semantic(SemanticError::NotUnaryType { val, span, op })
    }
    pub fn type_mismatch(
        l_typ: TypeKind,
        r_typ: TypeKind,
        l_span: Span,
        r_span: Span,
        op: String,
        kind: MismatchKind,
    ) -> Self {
        CompileError::Semantic(SemanticError::TypeMismatch {
            l_typ,
            r_typ,
            l_span,
            r_span,
            op,
            kind,
        })
    }
    pub fn not_callable(val: TypeKind, span: Span) -> Self {
        CompileError::Semantic(SemanticError::NotCallable { val, span })
    }
    pub fn missing_argument(expected: usize, found: usize, span: Span) -> Self {
        CompileError::Semantic(SemanticError::MissingArgument {
            expected,
            found,
            span,
        })
    }
    pub fn unexpected_type(expected: TypeKind, found: TypeKind, span: Span) -> Self {
        CompileError::Semantic(SemanticError::UnexpectedType {
            expected,
            found,
            span,
        })
    }
    pub fn divide_by_zero(span: Span) -> Self {
        CompileError::Semantic(SemanticError::DivideByZero { span })
    }
    pub fn feature_not_supported(feature_name: String, span: Span) -> Self {
        CompileError::Semantic(SemanticError::FeatureNotSupported { feature_name, span })
    }
    pub fn not_mutable(val: String, span: Span) -> Self {
        CompileError::Semantic(SemanticError::NotMutable { val, span })
    }
    pub fn invalid_return_location(span: Span) -> Self {
        CompileError::Semantic(SemanticError::InvalidReturnLocation { span })
    }
    pub fn nested_function(span: Span) -> Self {
        CompileError::Semantic(SemanticError::NestedFunction { span })
    }
    pub fn missing_return(span: Span) -> Self {
        CompileError::Semantic(SemanticError::MissingReturn { span })
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
            Self::Semantic(SemanticError::SymbolRedefinition { new_span, .. }) => *new_span,
            Self::Semantic(SemanticError::UnknownType { span, .. }) => *span,
            Self::Semantic(SemanticError::UnknownSymbol { span, .. }) => *span,
            Self::Semantic(SemanticError::NotUnaryType { span, .. }) => *span,
            Self::Semantic(SemanticError::TypeMismatch { l_span, r_span, .. }) => Span {
                start: l_span.start,
                end: r_span.end,
            },
            Self::Semantic(SemanticError::MissingArgument { span, .. }) => *span,
            Self::Semantic(SemanticError::UnexpectedType { span, .. }) => *span,
            Self::Semantic(SemanticError::NotCallable { span, .. }) => *span,
            Self::Semantic(SemanticError::DivideByZero { span }) => *span,
            Self::Semantic(SemanticError::FeatureNotSupported { span, .. }) => *span,
            Self::Semantic(SemanticError::NotMutable { span, .. }) => *span,
            Self::Semantic(SemanticError::InvalidReturnLocation { span, .. }) => *span,
            Self::Semantic(SemanticError::NestedFunction { span }) => *span,
            Self::Semantic(SemanticError::MissingReturn { span }) => *span,
        }
    }
}
