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
        compile_error::{CompileError, LexicalError, ParsingError, SemanticError, SymbolKind},
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
                CompileError::Semantic(SemanticError::SymbolRedefination { kind, .. }) => {
                    match kind {
                        SymbolKind::Function => "function redefination here".into(),
                        SymbolKind::Variable => "variable redefination here".into(),
                    }
                }
                CompileError::Semantic(SemanticError::UnknownType { .. }) => {
                    "this is not a type".into()
                }
                _ => "error here".into(),
            };

            let label = LabeledSpan::new_with_span(Some(label_text), (span.start, len));
            Some(Box::new(vec![label].into_iter()))
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
                CompileError::Semantic(_) => Some(Box::new("S001")),
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
                CompileError::Semantic(SemanticError::UnknownType { val, span }) => {
                    return Some(Box::new("use int, chr, str, bool or nret"));
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
                    expected,
                    found,
                    span,
                }) => {
                    write!(f, "expected '{:?}', found '{:?}'", expected, found)
                }
                CompileError::Semantic(_) => {
                    write!(f, "semantic error..")
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
