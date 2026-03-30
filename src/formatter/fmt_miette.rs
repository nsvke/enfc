#![allow(unused)]
#[cfg(feature = "miette")]
pub mod fmt_miette_impl {
    use miette::{
        Diagnostic, GraphicalReportHandler, GraphicalTheme, LabeledSpan, NamedSource, Report,
        RgbColors, SourceSpan, ThemeCharacters, ThemeStyles,
    };
    use std::fmt;
    use thiserror::Error;

    use super::super::ErrorFormatter;
    use crate::{
        compile_error::{
            CompileError, LexicalError, MismatchKind, ParsingError, SemanticError, SymbolKind,
        },
        driver::TokenKind,
    };

    #[derive(Debug, Error)]
    pub struct MietteAdapter<'a> {
        error: &'a CompileError,
        source_code: NamedSource<String>,
    }
    impl<'a> MietteAdapter<'a> {
        pub fn new(error: &'a CompileError, source_code: &'a str, path: &'a str) -> Self {
            Self {
                error,
                source_code: NamedSource::new(path, source_code.to_string()),
            }
        }
    }

    impl<'a> Diagnostic for MietteAdapter<'a> {
        fn labels(&self) -> Option<Box<dyn Iterator<Item = LabeledSpan> + '_>> {
            let span = self.error.span();
            let len = if span.end >= span.start {
                span.end - span.start
            } else {
                0
            };

            let label_text = match &self.error {
                CompileError::Parsing(ParsingError::UnexpectedToken { .. }) => {
                    "unexpected token here".into()
                }
                CompileError::Lexical(LexicalError::UnterminatedLiteral { .. }) => {
                    "unterminated literal here".into()
                }
                CompileError::Semantic(SemanticError::SymbolRedefinition {
                    name,
                    old_span,
                    new_span,
                    kind,
                }) => match kind {
                    SymbolKind::Function => "function redefinition here".into(),
                    SymbolKind::Variable => "variable redefinition here".into(),
                    SymbolKind::Parameter => "parameter redefinition here".into(),
                    SymbolKind::Builtin => "builtin redefinition here".into(),
                },
                CompileError::Semantic(SemanticError::UnknownType { .. }) => {
                    "this is not a type".into()
                }
                CompileError::Semantic(SemanticError::DivideByZero { .. }) => {
                    "divide by zero here".into()
                }
                CompileError::Semantic(SemanticError::UnknownType { .. }) => {
                    "unknown type here".into()
                }
                CompileError::Semantic(SemanticError::UnknownSymbol { .. }) => {
                    "unknown symbol here".into()
                }
                CompileError::Semantic(SemanticError::NotUnaryType { .. }) => {
                    "this is not unary type".into()
                }
                CompileError::Semantic(SemanticError::TypeMismatch { .. }) => {
                    "type mismatch here".into()
                }
                CompileError::Semantic(SemanticError::MissingArgument { .. }) => {
                    "missing argument here".into()
                }
                CompileError::Semantic(SemanticError::UnexpectedType { .. }) => {
                    "unexpected type here".into()
                }
                CompileError::Semantic(SemanticError::NotCallable { .. }) => {
                    "this is not callable".into()
                }
                CompileError::Semantic(SemanticError::FeatureNotSupported { .. }) => {
                    "this is not supported".into()
                }
                CompileError::Semantic(SemanticError::NotMutable { .. }) => {
                    "this is not mutable".into()
                }
                CompileError::Semantic(SemanticError::InvalidReturnLocation { .. }) => {
                    "this location is not a function body".into()
                }
                CompileError::Semantic(SemanticError::NestedFunction { .. }) => {
                    "nested function here".into()
                }
                CompileError::Semantic(SemanticError::MissingReturn { .. }) => {
                    "missing return here".into()
                }
                CompileError::Semantic(SemanticError::NotFoundMain { .. }) => {
                    "consider adding a `main` function".into()
                }
                CompileError::Semantic(SemanticError::WrongMain { .. }) => {
                    "wrong main sign here".into()
                }
                _ => "error here".into(),
            };

            // TODO combine this match with match above
            match &self.error {
                CompileError::Semantic(SemanticError::SymbolRedefinition {
                    name,
                    old_span,
                    new_span,
                    kind,
                }) => {
                    let labels = if old_span.start == 0 && old_span.end == 0 {
                        vec![LabeledSpan::new_with_span(
                            Some("redefined here".into()),
                            (new_span.start, new_span.end - new_span.start),
                        )]
                    } else {
                        vec![
                            LabeledSpan::new_with_span(
                                Some("first defined here".into()),
                                (old_span.start, old_span.end - old_span.start),
                            ),
                            LabeledSpan::new_with_span(
                                Some("redefined here".into()),
                                (new_span.start, new_span.end - new_span.start),
                            ),
                        ]
                    };
                    Some(Box::new(labels.into_iter()))
                }
                CompileError::Semantic(SemanticError::TypeMismatch {
                    l_typ,
                    r_typ,
                    l_span,
                    r_span,
                    op,
                    kind,
                }) if *kind == MismatchKind::Return => {
                    let labels = vec![
                        LabeledSpan::new_with_span(
                            Some(format!("ret type defined '{}' here", l_typ)),
                            (l_span.start, l_span.end - l_span.start),
                        ),
                        LabeledSpan::new_with_span(
                            Some(format!("but returned '{}' here", r_typ)),
                            (r_span.start, r_span.end - r_span.start),
                        ),
                    ];

                    Some(Box::new(labels.into_iter()))
                }
                _ => {
                    let label = LabeledSpan::new_with_span(Some(label_text), (span.start, len));
                    Some(Box::new(vec![label].into_iter()))
                }
            }
        }
        fn source_code(&self) -> Option<&dyn miette::SourceCode> {
            Some(&self.source_code)
        }
        fn code<'b>(&'b self) -> Option<Box<dyn fmt::Display + 'b>> {
            match self.error {
                CompileError::Lexical(LexicalError::UnterminatedLiteral { .. }) => {
                    Some(Box::new("UnterminatedLiteral"))
                }
                CompileError::Parsing(ParsingError::UnexpectedToken { .. }) => {
                    Some(Box::new("UnexpectedToken"))
                }
                CompileError::Semantic(_) => Some(Box::new("SemanticError")), // TODO add semantic error codes
                _ => None,
            };
            None
        }
        fn help<'b>(&'b self) -> Option<Box<dyn fmt::Display + 'b>> {
            match self.error {
                CompileError::Parsing(ParsingError::UnexpectedToken {
                    expected, found, ..
                }) => {
                    if *expected == TokenKind::Ident("ret_type".into()) {
                        return Some(Box::new(
                            "ex: fun main(argv str, args int) '\x1b[1mnret\x1b[0m' { ... }",
                        ));
                    }
                }
                CompileError::Semantic(SemanticError::UnknownType { val, .. }) => {
                    return Some(Box::new("use int, chr, str, bool or nret"));
                }
                CompileError::Semantic(SemanticError::WrongMain { .. }) => {
                    return Some(Box::new("try fun main() nret {.."));
                }
                _ => {}
            }
            None
        }
    }

    impl<'a> fmt::Display for MietteAdapter<'a> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self.error {
                CompileError::Lexical(LexicalError::UnterminatedLiteral { value, span }) => {
                    write!(f, "Unterminated literal '{}'", value)
                }
                CompileError::Parsing(ParsingError::UnexpectedToken {
                    expected, found, ..
                }) => {
                    write!(f, "expected '{:?}', found '{:?}'", expected, found)
                }
                CompileError::Semantic(SemanticError::SymbolRedefinition {
                    name, kind, ..
                }) => {
                    // TODO fix this format
                    write!(
                        f,
                        "{} '{}' is already defined",
                        format!("{:?}", kind).to_lowercase(),
                        name
                    )
                }
                CompileError::Semantic(SemanticError::UnknownType { val, .. }) => {
                    write!(f, "cannot find type `{}`", val)
                }
                CompileError::Semantic(SemanticError::UnknownSymbol { val, .. }) => {
                    write!(f, "cannot find value `{}` in this scope", val)
                }
                CompileError::Semantic(SemanticError::NotUnaryType { val, op, .. }) => {
                    write!(f, "cannot apply unary operator `{}` to type `{}`", op, val)
                }
                CompileError::Semantic(SemanticError::TypeMismatch {
                    l_typ,
                    r_typ,
                    op,
                    kind,
                    ..
                }) => match kind {
                    MismatchKind::Binary => {
                        write!(
                            f,
                            "cannot apply operator '{}' to types '{}' and '{}'",
                            op, l_typ, r_typ
                        )
                    }
                    MismatchKind::Regular | MismatchKind::Return => {
                        write!(
                            f,
                            "types mismatched for '{}', expected '{}' but found '{}'",
                            op, l_typ, r_typ
                        )
                    }
                },
                CompileError::Semantic(SemanticError::MissingArgument {
                    expected, found, ..
                }) => {
                    write!(
                        f,
                        "this method takes {} argument but {} arguments were supplied",
                        expected, found
                    )
                }
                CompileError::Semantic(SemanticError::UnexpectedType {
                    span,
                    expected,
                    found,
                }) => {
                    write!(f, "expected type '{}' but found '{}'", expected, found)
                }
                CompileError::Semantic(SemanticError::NotCallable { val, .. }) => {
                    write!(f, "{} is not a callable type", val)
                }
                CompileError::Semantic(SemanticError::DivideByZero { .. }) => {
                    write!(f, "this operation will panic at runtime")
                }
                CompileError::Semantic(SemanticError::FeatureNotSupported {
                    feature_name, ..
                }) => {
                    write!(f, "{} is not supported yet", feature_name)
                }
                CompileError::Semantic(SemanticError::NotMutable { val, .. }) => {
                    write!(f, "cannot assign twice to immutable variable `{}`", val)
                }
                CompileError::Semantic(SemanticError::InvalidReturnLocation { .. }) => {
                    write!(f, "this return is not in a function")
                }
                CompileError::Semantic(SemanticError::NestedFunction { .. }) => {
                    write!(f, "nested functions is not supported")
                }
                CompileError::Semantic(SemanticError::MissingReturn { .. }) => {
                    write!(
                        f,
                        "implicitly returns `nret` as its body has no `ret` expression"
                    )
                }
                CompileError::Semantic(SemanticError::NotFoundMain { .. }) => {
                    write!(f, "`main` function not found")
                }
                CompileError::Semantic(SemanticError::WrongMain { .. }) => {
                    write!(f, "'main' function is not correct")
                }
                CompileError::Semantic(SemanticError::CannotDeref { typ, .. }) => {
                    write!(f, "can not deref '{}'", typ)
                }
                CompileError::Semantic(SemanticError::InvalidLValue { .. }) => {
                    write!(f, "invalid lvalue")
                }
                CompileError::Semantic(SemanticError::CannotAddressable { .. }) => {
                    write!(f, "can not get address for this")
                }
                CompileError::Semantic(SemanticError::ContinueOutsideLoop { .. }) => {
                    write!(f, "can not use continue outside loop")
                }
                CompileError::Semantic(SemanticError::BreakOutsideLoop { .. }) => {
                    write!(f, "can not use break outside loop")
                }
            }
        }
    }

    pub struct MietteFormatter;
    impl ErrorFormatter for MietteFormatter {
        fn format(&self, err: &CompileError, source_code: &str, path: &str) {
            let adapter = MietteAdapter::new(err, source_code, path);
            let theme = GraphicalTheme {
                characters: ThemeCharacters {
                    error: "error:".into(),
                    ..ThemeCharacters::ascii()
                },
                styles: ThemeStyles::ansi(),
            };
            let handler = GraphicalReportHandler::new()
                .with_context_lines(1)
                .with_theme(theme);

            let mut buffer = String::new();

            if let Err(_) = handler.render_report(&mut buffer, &adapter) {
                eprintln!("Error rendering report.");
                return;
            }
            eprintln!("{}", buffer);
        }
    }
}
