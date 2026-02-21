use crate::structs::Span;
use phf::{Map, phf_map};
use std::iter::Peekable;
use std::str::CharIndices;

static KEYWORDS: Map<&'static str, TokenKind> = phf_map! {
    "val" => TokenKind::Val,
    "var" => TokenKind::Var,
    "cnst" => TokenKind::Const,
    "fun" => TokenKind::Fun,
    "if" => TokenKind::If,
    "elsf" => TokenKind::ElseIf,
    "else" => TokenKind::Else,
    "whl" => TokenKind::While,
    "for" => TokenKind::For,
    "cntn" => TokenKind::Continue,
    "brk" => TokenKind::Break,
    "ret" => TokenKind::Ret,
    // "nret" => TokenKind::Nret,
    "enum" => TokenKind::Enum,
    "data" => TokenKind::Data,
    "imp" => TokenKind::Impl,
    "nll" => TokenKind::Null,
    "any" => TokenKind::Any,
};
static SYMBOLS: Map<&'static str, TokenKind> = phf_map! {
    "--" => TokenKind::LineComment,
    " " => TokenKind::WhiteSpace,
    ";" => TokenKind::Semi,
    "," => TokenKind::Comma,
    "." => TokenKind::Dot,
    "@" => TokenKind::At,
    "#" => TokenKind::Pound,
    "~" => TokenKind::Tilde,
    "(" => TokenKind::OpenParam,
    ")" => TokenKind::CloseParam,
    "{" => TokenKind::OpenBrace,
    "}" => TokenKind::CloseBrace,
    "[" => TokenKind::OpenBracket,
    "]" => TokenKind::CloseBracket,
    "<" => TokenKind::Lt,
    ">" => TokenKind::Gt,
    "?" => TokenKind::Question,
    ":" => TokenKind::Colon,
    "$" => TokenKind::Dollar,
    "=" => TokenKind::Eq,
    "!" => TokenKind::Bang,
    "&" => TokenKind::And,
    "|" => TokenKind::Or,
    "-" => TokenKind::Minus,
    "+" => TokenKind::Plus,
    "*" => TokenKind::Star,
    "/" => TokenKind::Slash,
    "\\" => TokenKind::BackSlash,
    "^" => TokenKind::Caret,
    "%" => TokenKind::Percent,
    "||" => TokenKind::OrOr,
    "&&" => TokenKind::AndAnd,
    ".." => TokenKind::DotDot,
    ";;" => TokenKind::SemiSemi,
    "::" => TokenKind::ColonColon,
    "->" => TokenKind::Arrow,
    "<-" => TokenKind::BackArrow,
    "//" => TokenKind::SlashSlash,
    "==" => TokenKind::EqEq,
    ">=" => TokenKind::GtEq,
    "<=" => TokenKind::LtEq,
    "!=" => TokenKind::NotEq,
    "?=" => TokenKind::QuestionEq,
};

const EOF_CHAR: char = '\0';

#[allow(unused)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    fn new(kind: TokenKind, start: usize, end: usize) -> Self {
        Token {
            kind,
            span: Span { start, end },
        }
    }
}

#[allow(unused)]
#[derive(Clone, PartialEq, Eq)]
pub enum TokenKind {
    LineComment, // "--"
    WhiteSpace,
    // Comment(String),
    Ident(String),
    InvalidIdent(String),
    Literal(LiteralKind),
    // Operators
    Semi,         // ';'
    Comma,        // ','
    Dot,          // '.'
    At,           // '@'
    Pound,        // '#'
    Tilde,        // '~'
    OpenParam,    // '('
    CloseParam,   // ')'
    OpenBrace,    // '{'
    CloseBrace,   // '}'
    OpenBracket,  // '['
    CloseBracket, // ']'
    Lt,           // '<'
    Question,     // '?'
    Colon,        // ':'
    Dollar,       // '$'
    Eq,           // '='
    Bang,         // '!'
    Gt,           // '>'
    Minus,        // '-'
    Plus,         // '+'
    Star,         // '*'
    Slash,        // '/'
    BackSlash,    // '\'
    Caret,        // '^'
    Percent,      // '%'
    And,          // '&'
    Or,           // '|'

    //Double Operators
    AndAnd,     // "&&"
    OrOr,       // "||"
    DotDot,     // ".."
    SemiSemi,   // ";;"
    ColonColon, // "::"
    Arrow,      // "->"
    BackArrow,  // "<-"
    SlashSlash, // "//"
    EqEq,       // "=="
    GtEq,       // ">="
    LtEq,       // "<="
    NotEq,      // "!="
    QuestionEq, // "?="
    //
    Unknown(String),
    Eof,

    //Keywords
    Val,
    Var,
    Const,
    Fun,
    If,
    ElseIf,
    Else,
    While,
    For,
    Continue,
    Break,
    Ret,
    // Nret,
    Enum,
    Data,
    Impl,
    Null,
    Any,
    // holder
}

#[allow(unused)]
#[derive(Clone, PartialEq, Eq)]
pub enum LiteralKind {
    Int(i32),
    Char { val: char, terminated: bool },
    Str { val: String, terminated: bool },
    Bool(bool),
}
impl std::fmt::Debug for LiteralKind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Int(i) => write!(f, "LiteralInt {i}"),
            Self::Char { val, terminated } => write!(
                f,
                "LiteralChar '{val}' {}",
                if *terminated { 'V' } else { 'X' }
            ),
            Self::Str { val, terminated } => {
                write!(
                    f,
                    "LiteralStr \"{val}\" {}",
                    if *terminated { "ok" } else { "err" }
                )
            }
            Self::Bool(b) => write!(f, "{b:?}"),
        }
    }
}

impl std::fmt::Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let kind_str = format!("{:?}", self.kind);
        // Kind: 60 chars, Len: 6 chars, Start: 6 chars, End: 6 chars
        let kind_col = format!("{:<60}", kind_str);
        let len_col = format!("\x1b[36;1m{:<6}\x1b[0m", self.span.end - self.span.start); // Cyan
        let start_col = format!("\x1b[32;1m{:.<6}\x1b[0m", self.span.start); // Green
        let end_col = format!("\x1b[31;1m{:.>6}\x1b[0m", self.span.end); // Red

        write!(
            f,
            "{} \x1b[90m|\x1b[0m len: {} \x1b[90m|\x1b[0m span: {}\x1b[90;1m..\x1b[0m{}",
            kind_col, len_col, start_col, end_col
        )
    }
}

impl std::fmt::Debug for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Val
            | Self::Var
            | Self::Const
            | Self::Fun
            | Self::If
            | Self::ElseIf
            | Self::Else
            | Self::While
            | Self::For
            | Self::Continue
            | Self::Break
            | Self::Ret
            | Self::Enum
            | Self::Data
            | Self::Impl
            | Self::Null
            | Self::Any => {
                write!(
                    f,
                    "\x1b[38;2;180;100;255m{}\x1b[0m",
                    self.to_string_variant()
                )
            }
            Self::Ident(s) => write!(f, "\x1b[38;2;255;220;100mIdent {}\x1b[0m", s),
            Self::InvalidIdent(s) => {
                write!(f, "\x1b[38;2;255;150;150m{}\x1b[0m", s)
            }
            Self::Literal(l) => write!(f, "\x1b[38;2;100;200;255m{:?}\x1b[0m", l),
            Self::Unknown(s) => write!(f, "\x1b[38;2;255;100;100mUnknown ? \"{}\"\x1b[0m", s),
            Self::Semi => write!(f, "\x1b[38;2;240;240;050m;\x1b[0m"),
            Self::Comma => write!(f, "\x1b[38;2;240;240;050m,\x1b[0m"),
            Self::Dot => write!(f, "\x1b[38;2;240;240;050m.\x1b[0m"),
            Self::At => write!(f, "\x1b[38;2;240;240;050m@\x1b[0m"),
            Self::Pound => write!(f, "\x1b[38;2;240;240;050m#\x1b[0m"),
            Self::Tilde => write!(f, "\x1b[38;2;240;240;050m~\x1b[0m"),
            Self::OpenParam => write!(f, "\x1b[38;2;240;240;050m(\x1b[0m"),
            Self::CloseParam => write!(f, "\x1b[38;2;240;240;050m)\x1b[0m"),
            Self::OpenBrace => write!(f, "\x1b[38;2;240;240;050m{{\x1b[0m"),
            Self::CloseBrace => write!(f, "\x1b[38;2;240;240;050m}}\x1b[0m"),
            Self::OpenBracket => write!(f, "\x1b[38;2;240;240;050m[\x1b[0m"),
            Self::CloseBracket => write!(f, "\x1b[38;2;240;240;050m]\x1b[0m"),
            Self::Lt => write!(f, "\x1b[38;2;240;240;050m<\x1b[0m"),
            Self::Question => write!(f, "\x1b[38;2;240;240;050m>\x1b[0m"),
            Self::Colon => write!(f, "\x1b[38;2;240;240;050m:\x1b[0m"),
            Self::Dollar => write!(f, "\x1b[38;2;240;240;050m$\x1b[0m"),
            Self::Eq => write!(f, "\x1b[38;2;240;240;050m=\x1b[0m"),
            Self::Bang => write!(f, "\x1b[38;2;240;240;050m!\x1b[0m"),
            Self::Gt => write!(f, "\x1b[38;2;240;240;050m>\x1b[0m"),
            Self::Minus => write!(f, "\x1b[38;2;240;240;050m-\x1b[0m"),
            Self::Plus => write!(f, "\x1b[38;2;240;240;050m+\x1b[0m"),
            Self::Star => write!(f, "\x1b[38;2;240;240;050m*\x1b[0m"),
            Self::Slash => write!(f, "\x1b[38;2;240;240;050m/\x1b[0m"),
            Self::BackSlash => write!(f, "\x1b[38;2;240;240;050m\\\x1b[0m"),
            Self::Caret => write!(f, "\x1b[38;2;240;240;050m^\x1b[0m"),
            Self::Percent => write!(f, "\x1b[38;2;240;240;050m%\x1b[0m"),
            Self::And => write!(f, "\x1b[38;2;240;240;050m&\x1b[0m"),
            Self::Or => write!(f, "\x1b[38;2;240;240;050m|\x1b[0m"),
            Self::AndAnd => write!(f, "\x1b[38;2;240;240;050m&&\x1b[0m"),
            Self::OrOr => write!(f, "\x1b[38;2;240;240;050m||\x1b[0m"),
            Self::DotDot => write!(f, "\x1b[38;2;240;240;050m..\x1b[0m"),
            Self::SemiSemi => write!(f, "\x1b[38;2;240;240;050m;;\x1b[0m"),
            Self::ColonColon => write!(f, "\x1b[38;2;240;240;050m::\x1b[0m"),
            Self::Arrow => write!(f, "\x1b[38;2;240;240;050m->\x1b[0m"),
            Self::BackArrow => write!(f, "\x1b[38;2;240;240;050m<-\x1b[0m"),
            Self::SlashSlash => write!(f, "\x1b[38;2;240;240;050m//\x1b[0m"),
            Self::EqEq => write!(f, "\x1b[38;2;240;240;050m==\x1b[0m"),
            Self::GtEq => write!(f, "\x1b[38;2;240;240;050m>=\x1b[0m"),
            Self::LtEq => write!(f, "\x1b[38;2;240;240;050m<=\x1b[0m"),
            Self::NotEq => write!(f, "\x1b[38;2;240;240;050m!=\x1b[0m"),
            Self::QuestionEq => write!(f, "\x1b[38;2;240;240;050m?=\x1b[0m"),
            _ => {
                write!(
                    f,
                    "\x1b[38;2;255;150;150m{}\x1b[0m",
                    self.to_string_variant().to_lowercase()
                )
            }
        }
    }
}

impl TokenKind {
    fn to_string_variant(&self) -> String {
        match self {
            Self::LineComment => "LineComment".into(),
            Self::WhiteSpace => "WhiteSpace".into(),
            Self::Ident(_) => "Ident".into(),
            Self::InvalidIdent(_) => "InvalidIdent".into(),
            Self::Literal(_) => "Literal".into(),
            Self::Semi => "Semi".into(),
            Self::Comma => "Comma".into(),
            Self::Dot => "Dot".into(),
            Self::At => "At".into(),
            Self::Pound => "Pound".into(),
            Self::Tilde => "Tilde".into(),
            Self::OpenParam => "OpenParam".into(),
            Self::CloseParam => "CloseParam".into(),
            Self::OpenBrace => "OpenBrace".into(),
            Self::CloseBrace => "CloseBrace".into(),
            Self::OpenBracket => "OpenBracket".into(),
            Self::CloseBracket => "CloseBracket".into(),
            Self::Lt => "Lt".into(),
            Self::Question => "Question".into(),
            Self::Colon => "Colon".into(),
            Self::Dollar => "Dollar".into(),
            Self::Eq => "Eq".into(),
            Self::Bang => "Bang".into(),
            Self::Gt => "Gt".into(),
            Self::Minus => "Minus".into(),
            Self::Plus => "Plus".into(),
            Self::Star => "Star".into(),
            Self::Slash => "Slash".into(),
            Self::BackSlash => "BackSlash".into(),
            Self::Caret => "Caret".into(),
            Self::Percent => "Percent".into(),
            Self::And => "And".into(),
            Self::Or => "Or".into(),
            Self::AndAnd => "AndAnd".into(),
            Self::OrOr => "OrOr".into(),
            Self::DotDot => "DotDot".into(),
            Self::SemiSemi => "SemiSemi".into(),
            Self::ColonColon => "ColonColon".into(),
            Self::Arrow => "Arrow".into(),
            Self::BackArrow => "BackArrow".into(),
            Self::SlashSlash => "SlashSlash".into(),
            Self::EqEq => "EqEq".into(),
            Self::GtEq => "GtEq".into(),
            Self::LtEq => "LtEq".into(),
            Self::NotEq => "NotEq".into(),
            Self::QuestionEq => "QuestionEq".into(),
            Self::Unknown(_) => "Unknown".into(),
            Self::Eof => "Eof".into(),
            Self::Val => "Val".into(),
            Self::Var => "Var".into(),
            Self::Const => "Const".into(),
            Self::Fun => "Fun".into(),
            Self::If => "If".into(),
            Self::ElseIf => "ElseIf".into(),
            Self::Else => "Else".into(),
            Self::While => "While".into(),
            Self::For => "For".into(),
            Self::Continue => "Continue".into(),
            Self::Break => "Break".into(),
            Self::Ret => "Ret".into(),
            // Self::Nret => "Nret".into(),
            Self::Enum => "Enum".into(),
            Self::Data => "Data".into(),
            Self::Impl => "Impl".into(),
            Self::Null => "Null".into(),
            Self::Any => "Any".into(),
        }
    }
}

#[derive(Default)]
struct LexingBuffer {
    start: usize,
    chars: String,
}

pub(crate) struct Lexer<'a> {
    iter: Peekable<CharIndices<'a>>,
    tokens: Vec<Token>,
    buffer: LexingBuffer,
}

impl<'a> Lexer<'a> {
    pub(crate) fn new(iter: Peekable<CharIndices<'a>>) -> Lexer<'a> {
        Lexer {
            iter,
            tokens: Vec::new(),
            buffer: LexingBuffer::default(),
        }
    }

    pub(crate) fn tokenize(mut self) -> Vec<Token> {
        while let Some((off, c)) = self.iter.next() {
            if c.is_whitespace() || c == '\n' {
                self.push_ident_token(off);
            } else if c == '"' {
                self.push_ident_token(off);
                let literal = self.is_string_literal();
                self.push_literal_token(literal, off);
            } else if c == '\'' {
                self.push_ident_token(off);
                let literal = self.is_char_literal();
                self.push_c_literal_token(literal, off);
            } else if let Some(sym) = is_symbol(c) {
                self.push_ident_token(off);
                if let Some(d_sym) = self.try_double_symbol(c) {
                    let (end, _) = self.iter.next().unwrap();
                    let end2 = self.end_offset().unwrap_or(end + 1);
                    if d_sym == TokenKind::SemiSemi {
                        self.push_ident_token_w_kind(TokenKind::Semi, off, end);
                    } else if d_sym == TokenKind::LineComment {
                        self.eat_line_comment();
                    } else {
                        self.push_ident_token_w_kind(d_sym, off, end2);
                    }
                } else {
                    let end = self.end_offset().unwrap_or(off + 1);
                    self.push_ident_token_w_kind(sym, off, end);
                }
            } else if is_unknown_character(c) {
                self.push_ident_token(off);
                let end = self.end_offset().unwrap_or(off + 1);
                self.push_ident_token_w_kind(TokenKind::Unknown(c.into()), off, end);
            } else {
                if self.buffer.chars.is_empty() {
                    self.buffer.start = off;
                }
                self.buffer.chars.push(c);
            }
        }
        //*******************************
        if !self.buffer.chars.is_empty() {
            let end_pos = self.buffer.start + self.buffer.chars.len();
            self.push_ident_token(end_pos);
        }
        //*******************************
        let end = match self.tokens.last() {
            Some(t) => t.span.end,
            Option::None => 0,
        };
        self.push_ident_token_w_kind(TokenKind::Eof, end, end + 1);
        self.tokens
    }

    fn end_offset(&mut self) -> Option<usize> {
        if let Some((off, _)) = self.iter.peek() {
            Some(*off)
        } else {
            None
        }
    }

    fn push_literal_token(&mut self, (val, terminated): (String, bool), start: usize) {
        let end = self.end_offset().unwrap_or(start + val.len());
        self.tokens.push(Token::new(
            TokenKind::Literal(LiteralKind::Str { val, terminated }),
            start,
            end,
        ));
    }

    fn push_c_literal_token(&mut self, (val, terminated): (char, bool), start: usize) {
        let end = self.end_offset().unwrap_or(start + 3);
        self.tokens.push(Token::new(
            TokenKind::Literal(LiteralKind::Char { val, terminated }),
            start,
            end,
        ));
    }

    fn push_ident_token(&mut self, end: usize) {
        if !self.buffer.chars.is_empty() {
            let kind = self.find_token_kind();
            self.tokens.push(Token::new(kind, self.buffer.start, end));
        }
        self.buffer.start = end;
        self.buffer.chars.clear();
    }

    fn push_ident_token_w_kind(&mut self, kind: TokenKind, start: usize, end: usize) {
        self.tokens.push(Token::new(kind, start, end));
    }

    fn find_token_kind(&mut self) -> TokenKind {
        if let Some(token) = SYMBOLS.get(&self.buffer.chars) {
            token.clone()
        } else if let Some(token) = KEYWORDS.get(&self.buffer.chars) {
            token.clone()
        } else if is_digit(&self.buffer.chars) {
            TokenKind::Literal(LiteralKind::Int(self.buffer.chars.parse().unwrap()))
        } else if is_ident(&self.buffer.chars) {
            TokenKind::Ident(self.buffer.chars.clone())
        } else {
            TokenKind::InvalidIdent(self.buffer.chars.clone())
        }
    }

    fn try_double_symbol(&mut self, c1: char) -> Option<TokenKind> {
        if let Some((_, c2)) = self.iter.peek() {
            if is_symbol(*c2).is_some() {
                let d = format!("{}{}", c1, c2);
                SYMBOLS.get(&d).cloned()
            } else {
                None
            }
        } else {
            None
        }
    }

    fn eat_line_comment(&mut self) {
        // todo! tokenize line comments
        for (_, c) in self.iter.by_ref() {
            match c {
                '\n' => return,
                _ => continue,
            }
        }
    }

    fn is_char_literal(&mut self) -> (char, bool) {
        // 'a' - '\a' - '' - 'a - '
        let val: char;
        if let Some((_, c)) = self.iter.next() {
            match c {
                '\'' => return (EOF_CHAR, true),
                _ => val = c,
            };
        } else {
            return (EOF_CHAR, false);
        }
        if let Some((_, c)) = self.iter.peek() {
            match c {
                '\'' => {
                    self.iter.next();
                    (val, true)
                }
                _ => (val, false),
            }
        } else {
            (val, false)
        }
    }

    fn is_string_literal(&mut self) -> (String, bool) {
        let mut literal_buf = String::new();
        for (_, c) in self.iter.by_ref() {
            match c {
                '"' => return (literal_buf, true),
                '\n' => return (literal_buf, false),
                _ => literal_buf.push(c),
            }
        }
        (literal_buf, false)
    }
}

fn is_id_start(c: char) -> bool {
    c == '_' || c.is_alphabetic()
}

fn is_id_continue(c: char) -> bool {
    c.is_alphanumeric() || c == '_'
}

fn is_ident(string: &str) -> bool {
    let mut chars = string.chars();
    if let Some(start) = chars.next() {
        is_id_start(start) && chars.all(is_id_continue)
    } else {
        false
    }
}

fn is_digit(string: &str) -> bool {
    string.chars().all(|c| c.is_ascii_digit())
}

fn is_symbol(c: char) -> Option<TokenKind> {
    let mut st = [0u8, 0, 0, 0];
    SYMBOLS.get(c.encode_utf8(&mut st)).cloned()
}

fn is_unknown_character(c: char) -> bool {
    !c.is_alphanumeric() && !c.is_control() && c != '_'
}
