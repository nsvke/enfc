#![allow(unused)]
use std::fmt;

use crate::{
    compile_error::CompileError,
    diagnostic::Diagnostics,
    driver::lexer::{
        LiteralKind::*,
        Token,
        TokenKind::{self, *},
    },
    structs::Span,
}; //, error::ParsingError};
// type Result<T> = std::result::Result<T, ParsingError>;

macro_rules! field_name {
    // yukseklik alacak sekilde modifiye edilecek
    ($name:expr) => {
        concat!("\x1b[38;2;128;128;128m", $name, "\x1b[0m")
    };
}

pub(crate) struct Parser<'a> {
    tokens: &'a [Token],
    pos: usize,
    // is_broken: bool,
    diagnose: &'a mut Diagnostics,
}

impl<'a> Parser<'a> {
    pub(crate) fn new(tokens: &'a [Token], diagnose: &'a mut Diagnostics) -> Self {
        Self {
            tokens,
            pos: 0,
            diagnose,
        }
    }

    fn peek(&self) -> &'a Token {
        self.tokens
            .get(self.pos)
            .unwrap_or_else(|| &self.tokens[self.tokens.len() - 1])
    }
    fn peek_at(&self, i: usize) -> &'a Token {
        self.tokens
            .get(self.pos + i)
            .unwrap_or_else(|| &self.tokens[self.tokens.len() - 1])
    }
    fn consume(&mut self) -> &'a Token {
        let token = self.peek();
        self.pos += 1;
        token
    }

    fn consume_quietly(&mut self) {
        self.pos += 1;
    }

    fn expect(&mut self, expected: TokenKind) -> Result<&'a Token, Span> {
        if self.peek().kind == expected {
            Ok(self.consume())
        } else {
            let found = self.peek();
            let err = CompileError::unexpected_token(expected, found.kind.clone(), found.span);
            self.diagnose.push_error(err);
            Err(self.sync_span())
        }
    }

    pub(crate) fn parse(&mut self) -> Vec<Statement> {
        let mut statements = Vec::new();
        while self.pos < self.tokens.len() {
            if let Some(stmt) = self.parse_statement() {
                statements.push(stmt);
            }
        }
        statements
    }

    //TODO add parse_expression_statement()

    fn parse_statement(&mut self) -> Option<Statement> {
        match self.peek().kind {
            Val | Var => Some(self.parse_val()),
            Fun => Some(self.parse_fun()),
            If => Some(self.parse_if()),
            While => Some(self.parse_while()),
            OpenBrace => Some(self.parse_block()),
            Ret => Some(self.parse_ret()),
            Ident(_) => match self.peek_at(1).kind {
                Eq => Some(self.parse_asgn()),
                OpenParam => Some(Statement::Expression(self.parse_expr())),
                _ => {
                    self.diagnose.push_error(CompileError::unexpected_token(
                        Any,
                        self.peek().kind.clone(),
                        self.peek().span,
                    ));
                    self.consume_quietly();
                    Some(self.sync())
                } //_ => Some(Statement::Expression(self.parse_expr())),
            },
            Semi | Eof => {
                self.consume_quietly();
                None
            }
            _ => {
                // ignore other tokens for now
                self.diagnose.push_error(CompileError::unexpected_token(
                    Any,
                    self.peek().kind.clone(),
                    self.peek().span,
                ));
                self.consume_quietly();
                Some(self.sync())
            }
        }
    }

    fn parse_if(&mut self) -> Statement {
        let if_token = self.consume();

        let expr = self.parse_expr();

        let body_stmt = self.parse_block();
        let body_node = match body_stmt {
            Statement::Block(b) => b,
            Statement::Broken(span) => BlockNode {
                body: vec![Statement::Broken(span)],
                span,
            },
            _ => unreachable!("only Block or BrokenStatement"),
        };

        let mut end = body_node.span.end;

        let mut elseifs = Vec::new();
        while let ElseIf = self.peek().kind {
            let elseif_token = self.consume();
            let ei_expr = self.parse_expr();
            let ei_body_stmt = self.parse_block();
            let ei_body_node = match ei_body_stmt {
                Statement::Block(b) => b,
                Statement::Broken(span) => BlockNode {
                    body: vec![Statement::Broken(span)],
                    span,
                },
                _ => unreachable!("only Block or BrokenStatement"),
            };
            end = ei_body_node.span.end;
            elseifs.push(ElseIfNode {
                condition: ei_expr,
                span: Span {
                    start: elseif_token.span.start,
                    end: ei_body_node.span.end,
                },
                then_branch: ei_body_node,
            })
        }

        let else_block = if let Else = self.peek().kind {
            let else_token = self.consume();
            let e_body = self.parse_block();
            let e_body_node = match e_body {
                Statement::Block(b) => b,
                Statement::Broken(span) => BlockNode {
                    body: vec![Statement::Broken(span)],
                    span,
                },
                _ => unreachable!("only Block or BrokenStatement"),
            };
            end = e_body_node.span.end;
            Some(e_body_node)
        } else {
            None
        };

        Statement::If(IfNode {
            condition: expr,
            then_branch: body_node,
            else_if_branches: elseifs,
            else_branch: else_block,
            span: Span {
                start: if_token.span.start,
                end,
            },
        })
    }

    // ret 12;
    fn parse_ret(&mut self) -> Statement {
        let ret_token = self.consume();
        // let expr = match self.peek().kind {
        //     _ => Some(self.parse_expr()),
        // };
        let expr = Some(self.parse_expr());

        let semi_token = match self.expect(Semi) {
            Ok(t) => t,
            Err(broken) => return broken.into(),
        };

        Statement::Return(ReturnNode {
            value: expr,
            span: Span {
                start: ret_token.span.start,
                end: semi_token.span.end,
            },
        })
    }

    // fun ident( i int, i2 int ) $block
    fn parse_fun(&mut self) -> Statement {
        let fun_token = self.consume();

        let name_token = match self.consume() {
            t if matches!(t.kind, Ident(_)) => t,
            found => {
                let err = CompileError::unexpected_token(
                    Ident("".to_string()),
                    found.kind.clone(),
                    found.span,
                );
                self.diagnose.push_error(err);
                return self.sync();
            }
        };
        let name_str = if let Ident(s) = &name_token.kind {
            s.clone()
        } else {
            String::new()
        };
        let name_node = IdentLiteralNode {
            value: name_str,
            span: name_token.span,
        };

        if let Err(broken) = self.expect(OpenParam) {
            return broken.into();
        }

        let mut parameters = Vec::new();
        while !matches!(self.peek().kind, CloseParam | Eof) {
            let p_name_token = match self.consume() {
                t if matches!(t.kind, Ident(_)) => t,
                found => {
                    let err = CompileError::unexpected_token(
                        Ident("param_name".to_string()),
                        found.kind.clone(),
                        found.span,
                    );
                    self.diagnose.push_error(err);
                    return self.sync();
                }
            };
            let p_name_str = if let Ident(s) = &p_name_token.kind {
                s.clone()
            } else {
                String::new()
            };
            let p_name_node = IdentLiteralNode {
                value: p_name_str,
                span: p_name_token.span,
            };

            let p_type_token = match self.consume() {
                t if matches!(t.kind, Ident(_)) => t,
                found => {
                    let err = CompileError::unexpected_token(
                        Ident("param_type".to_string()),
                        found.kind.clone(),
                        found.span,
                    );
                    self.diagnose.push_error(err);
                    return self.sync();
                }
            };
            let p_type_str = if let Ident(s) = &p_type_token.kind {
                s.clone()
            } else {
                String::new()
            };
            let p_type_node = IdentLiteralNode {
                value: p_type_str,
                span: p_type_token.span,
            };

            parameters.push((p_name_node, p_type_node));

            if matches!(self.peek().kind, Comma) {
                self.consume_quietly();
            }
        }

        if let Err(broken) = self.expect(CloseParam) {
            return broken.into();
        }

        let type_token = if let Ident(_) = self.peek().kind {
            self.consume()
        } else {
            let found = self.peek();
            let err = CompileError::unexpected_token(
                Ident("ret_type".to_string()),
                found.kind.clone(),
                found.span,
            );
            self.diagnose.push_error(err);
            return self.sync();
        };
        let type_str = if let Ident(s) = &type_token.kind {
            s.clone()
        } else {
            String::new()
        };
        let type_node = IdentLiteralNode {
            value: type_str,
            span: type_token.span,
        };

        let body_stmt = self.parse_block();
        let body_node = match body_stmt {
            Statement::Block(b) => b,
            Statement::Broken(span) => BlockNode {
                body: vec![Statement::Broken(span)],
                span,
            },
            _ => unreachable!("only Block or BrokenStatement"),
        };

        Statement::FunDefinition(FunDefNode {
            name: name_node,
            parameters,
            span: Span {
                start: fun_token.span.start,
                end: body_node.span.end,
            },
            body: body_node,
            ret_type: type_node,
        })
    }

    // whl $expr $block
    fn parse_while(&mut self) -> Statement {
        let whl = self.consume();
        let expr = self.parse_expr();
        let body_stmt = self.parse_block();

        let body_node = match body_stmt {
            Statement::Block(b) => b,
            Statement::Broken(span) => BlockNode {
                body: vec![Statement::Broken(span)],
                span,
            },
            _ => unreachable!("only Block or BrokenStatement"),
        };

        Statement::While(WhileNode {
            condition: expr,
            span: Span {
                start: whl.span.start,
                end: body_node.span.end,
            },
            body: body_node,
        })
    }

    // { ... }
    fn parse_block(&mut self) -> Statement {
        let start_token = match self.expect(OpenBrace) {
            Ok(t) => t,
            Err(broken) => return broken.into(),
        };

        let mut statements = Vec::new();
        while !matches!(self.peek().kind, CloseBrace | Eof) {
            if let Some(stmt) = self.parse_statement() {
                statements.push(stmt);
            }
        }

        let end_token = match self.expect(CloseBrace) {
            Ok(t) => t,
            Err(broken) => return broken.into(),
        };

        Statement::Block(BlockNode {
            body: statements,
            span: Span {
                start: start_token.span.start,
                end: end_token.span.end,
            },
        })
    }

    // ident = expr;
    fn parse_asgn(&mut self) -> Statement {
        let name_token = match self.consume() {
            t if matches!(t.kind, Ident(_)) => t,
            _ => {
                // error unexpected token, expected identifier
                return self.sync();
            }
        };
        let name_str = if let Ident(s) = &name_token.kind {
            s.clone()
        } else {
            String::new()
        };

        if let Err(broken) = self.expect(Eq) {
            return broken.into();
        }

        let expr = self.parse_expr();

        let semi_token = match self.expect(Semi) {
            Ok(t) => t,
            Err(broken) => return broken.into(),
        };

        Statement::Assignment(AssignmentNode {
            left: IdentLiteralNode {
                value: name_str,
                span: name_token.span,
            },
            right: expr,
            span: Span {
                start: name_token.span.start,
                end: semi_token.span.end,
            },
        })
    }

    // val x int = 50;
    // val x = 50;
    // var x = 50;
    fn parse_val(&mut self) -> Statement {
        let val_or_var = self.consume();
        let name_token = match self.consume() {
            t if matches!(t.kind, (Ident(_))) => t,
            found => {
                let err = CompileError::unexpected_token(
                    InvalidIdent("identifier".into()),
                    found.kind.clone(),
                    found.span,
                );
                self.diagnose.push_error(err);
                return self.sync();
            }
        };

        let name_str = if let Ident(s) = &name_token.kind {
            s.clone()
        } else {
            String::new()
        };
        let name_node = IdentLiteralNode {
            value: name_str,
            span: name_token.span,
        };

        let type_node = match self.peek().kind {
            Eq => None,
            Ident(_) => {
                let t = self.consume();
                let s = if let Ident(val) = &t.kind {
                    val.clone()
                } else {
                    String::new()
                };
                Some(IdentLiteralNode {
                    value: s,
                    span: t.span,
                })
            }
            _ => {
                // expected Eq or Ident(Type) // todo! need multi expected support
                let found = self.peek();
                let err = CompileError::unexpected_token(Eq, found.kind.clone(), found.span); // Approximation
                self.diagnose.push_error(err);
                return self.sync();
            }
        };

        if let Err(broken) = self.expect(Eq) {
            return broken.into();
        }

        let expr = self.parse_expr();

        let semi_token = match self.expect(Semi) {
            Ok(t) => t,
            Err(broken) => return broken.into(),
        }; // todo ! recovery and big broken statement

        Statement::VarDeclaration(VarDecNode {
            name: name_node,
            var_type: type_node.unwrap_or(IdentLiteralNode {
                value: String::new(),
                span: Span::default(),
            }),
            initalizer: expr,
            mutable: val_or_var.kind == TokenKind::Var,
            span: Span {
                start: val_or_var.span.start,
                end: semi_token.span.end,
            },
        })
    }

    fn sync(&mut self) -> Statement {
        Statement::Broken(self.sync_span())
    }

    fn sync_span(&mut self) -> Span {
        let start = self.peek().span.start;
        let mut end = self.peek().span.end;
        while self.pos < self.tokens.len() - 1 {
            let token = self.peek();
            end = token.span.end;
            match token.kind {
                Semi | OpenBrace => {
                    let end = token.span.end;
                    return Span { start, end };
                }
                CloseBrace | CloseBracket | CloseParam | Eof => {
                    return Span {
                        start,
                        end: token.span.start,
                    };
                }
                _ => {
                    self.consume_quietly();
                }
            }
        }
        Span { start, end }
    }

    fn parse_expr(&mut self) -> Expression {
        self.parse_precedence(Precedence::Assignment)
    }

    fn parse_precedence(&mut self, precedence: Precedence) -> Expression {
        let mut left = self.parse_prefix();

        while precedence <= Precedence::precedence_of(&self.peek().kind) {
            left = self.parse_infix(left);
        }

        left
    }

    fn parse_prefix(&mut self) -> Expression {
        let token = self.peek();
        match token.kind {
            Ident(_) => self.parse_ident_expr(),
            Literal(_) => self.parse_literal_expr(),
            OpenParam => self.parse_groupping(),
            Bang | Minus => self.parse_unary(),
            CloseParam | CloseBrace | OpenBrace | Semi | Eof | Comma => {
                let err = CompileError::unexpected_token(
                    InvalidIdent("expr".into()),
                    token.kind.clone(),
                    token.span,
                );
                self.diagnose.push_error(err);
                Expression::Broken(token.span)
            }
            _ => {
                self.consume_quietly();
                let err = CompileError::unexpected_token(
                    InvalidIdent("expr".into()),
                    token.kind.clone(),
                    token.span,
                );
                self.diagnose.push_error(err);
                self.parse_prefix()
            }
        }
    }
    fn parse_ident_expr(&mut self) -> Expression {
        let token = self.consume();
        let name = match &token.kind {
            Ident(s) => s.clone(),
            _ => unreachable!(),
        };
        Expression::Ident(IdentLiteralNode {
            value: name,
            span: token.span,
        })
    }
    fn parse_literal_expr(&mut self) -> Expression {
        let token = self.consume();
        if let Literal(l) = &token.kind {
            let val = match l {
                Int(i) => LiteralValue::Number(*i),
                Str { val, terminated } => {
                    if !terminated {
                        self.diagnose.push_error(CompileError::unterminated_literal(
                            val.clone(),
                            token.span,
                        ));
                    }
                    LiteralValue::Str(val.clone())
                }
                Bool(b) => LiteralValue::Bool(*b),
                Char { val, terminated } => {
                    if !terminated {
                        self.diagnose.push_error(CompileError::unterminated_literal(
                            (*val).into(),
                            token.span,
                        ));
                    }
                    LiteralValue::Char(*val)
                }
            };
            Expression::Literal(LiteralNode {
                value: val,
                span: token.span,
            })
        } else {
            unreachable!()
        }
    }
    fn parse_groupping(&mut self) -> Expression {
        self.consume_quietly();
        let expr = self.parse_expr();
        if self.peek().kind == TokenKind::CloseParam {
            self.consume_quietly();
        } else {
            let t = self.peek();
            let err = CompileError::unexpected_token(CloseParam, t.kind.clone(), t.span);
            self.diagnose.push_error(err);
            // match t.kind {
            //     CloseBrace | CloseBracket => self.consume_quietly(),
            //     _ => {}
            // }
        }
        expr
    }
    fn parse_unary(&mut self) -> Expression {
        let token = self.consume();
        let operator = match token.kind {
            Bang => UnaryOperator::Not,
            Minus => UnaryOperator::Neg,
            _ => unreachable!(),
        };

        let operand = self.parse_precedence(Precedence::Unary);

        Expression::Unary(UnaryExpressionNode {
            operator,
            span: operand.get_span(),
            operand: Box::new(operand),
        })
    }

    fn parse_infix(&mut self, left: Expression) -> Expression {
        let token = self.peek();
        match token.kind {
            OpenParam => self.parse_call(left),
            OpenBracket => self.parse_index(left),
            Dot => self.parse_field_access(left),
            _ => self.parse_binary(left),
        }
    }
    fn parse_call(&mut self, left: Expression) -> Expression {
        self.consume_quietly();
        let mut arguments = Vec::new();
        if !matches!(self.peek().kind, CloseParam) {
            loop {
                let arg = self.parse_expr();
                let is_broken = matches!(arg, Expression::Broken(_));
                arguments.push(arg);
                if matches!(self.peek().kind, Comma) {
                    self.consume_quietly();
                } else if is_broken && matches!(self.peek().kind, Semi) {
                    break;
                } else {
                    break;
                }
            }
        }
        let end_token = if self.peek().kind == CloseParam {
            self.consume()
        } else {
            let t = self.peek();
            let last_was_broken = arguments
                .last()
                .map(|a| matches!(a, Expression::Broken(_)))
                .unwrap_or(false);

            if last_was_broken && matches!(t.kind, Semi) {
                t
            } else {
                let err = CompileError::unexpected_token(CloseParam, t.kind.clone(), t.span);
                self.diagnose.push_error(err);
                t
            }
        };
        Expression::Call(CallNode {
            span: Span {
                start: left.get_span().start,
                end: end_token.span.end,
            },
            primary: Box::new(left),
            arguments,
        })
    }
    fn parse_index(&mut self, left: Expression) -> Expression {
        self.consume_quietly();
        let index = self.parse_expr();
        let end_token = if self.peek().kind == CloseBracket {
            self.consume()
        } else {
            let t = self.peek();
            let err = CompileError::unexpected_token(CloseBracket, t.kind.clone(), t.span);
            self.diagnose.push_error(err);
            // match t.kind {
            //     TokenKind::CloseBrace | TokenKind::CloseParam => self.consume(),
            //     _ => t,
            // }
            t
        };

        Expression::Index(IndexExpressionNode {
            span: Span {
                start: left.get_span().start,
                end: end_token.span.end,
            },
            target: Box::new(left),
            index: Box::new(index),
        })
    }
    fn parse_field_access(&mut self, left: Expression) -> Expression {
        self.consume();
        let field_token = match self.peek().kind {
            Ident(_) => self.consume(),
            _ => {
                let t = self.consume();
                let err =
                    CompileError::unexpected_token(Ident("field".into()), t.kind.clone(), t.span);
                self.diagnose.push_error(err);
                t // todo! recovery check
            }
        };

        let field_val = if let Ident(s) = &field_token.kind {
            s.clone()
        } else {
            String::from("$")
        };

        Expression::FieldAccess(FieldAccessNode {
            span: Span {
                start: left.get_span().start,
                end: field_token.span.end,
            },
            target: Box::new(left),
            field: IdentLiteralNode {
                value: field_val,
                span: field_token.span,
            },
        })
    }
    fn parse_binary(&mut self, left: Expression) -> Expression {
        let token = self.consume();
        let operator = match token.kind {
            OrOr => BinaryOperator::Or,
            AndAnd => BinaryOperator::And,
            EqEq => BinaryOperator::IsEquals,
            NotEq => BinaryOperator::IsNotEquals,
            Lt => BinaryOperator::Less,
            LtEq => BinaryOperator::LessEquals,
            Gt => BinaryOperator::Greater,
            GtEq => BinaryOperator::GreaterEquals,
            Plus => BinaryOperator::Add,
            Minus => BinaryOperator::Sub,
            Star => BinaryOperator::Mul,
            Slash => BinaryOperator::Div,
            Percent => BinaryOperator::Percent,
            _ => unreachable!("Token {:?} is not a binary operator", token.kind),
        };
        let precedence = Precedence::precedence_of(&token.kind);
        let right = self.parse_precedence(precedence.next());
        let span = Span {
            start: left.get_span().start,
            end: right.get_span().end,
        };
        Expression::Binary(BinaryExpressionNode {
            left: Box::new(left),
            op: operator,
            right: Box::new(right),
            span,
        })
    }
}

pub(crate) enum Statement {
    VarDeclaration(VarDecNode), // +
    Assignment(AssignmentNode), // +
    If(IfNode),
    While(WhileNode), // +
    FunDefinition(FunDefNode),
    Block(BlockNode), // +
    Expression(Expression),
    Return(ReturnNode),
    Broken(Span), // parser hata varsa yine de recovery ile bir statement olusturmayi dener ve bu sayede iyi kotu bir ast dizisi linter'a gider. linter da gordugu hatalari raporlar ama zaten brokenstatement oldugundan derleme yapilmaz.
                  //Empty,                 // simdilik derleyici aglamasin diye koyuldu, sonra silinecek
}

impl From<Span> for Statement {
    fn from(value: Span) -> Self {
        Self::Broken(value)
    }
}

impl fmt::Debug for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::VarDeclaration(n) => n.fmt(f),
            Self::Assignment(n) => n.fmt(f),
            Self::If(n) => n.fmt(f),
            Self::While(n) => n.fmt(f),
            Self::FunDefinition(n) => n.fmt(f),
            Self::Block(n) => n.fmt(f),
            Self::Expression(n) => n.fmt(f),
            Self::Return(n) => n.fmt(f),
            Self::Broken(span) => write!(
                f,
                "\x1b[38;2;255;170;0m{}\x1b[90m..\x1b[38;2;255;170;0m{}\x1b[0m \x1b[31mBrokenStatement!\x1b[0m",
                span.start, span.end
            ),
            //Self::Empty => write!(f, "Empty"),
        }
    }
}
pub(crate) struct BlockNode {
    body: Vec<Statement>,
    span: Span,
}
impl fmt::Debug for BlockNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "\x1b[38;2;255;170;0m{}\x1b[90m..\x1b[38;2;255;170;0m{}\x1b[0m \x1b[38;2;100;123;143mBlock\x1b[0m ",
            self.span.start, self.span.end
        )?;
        f.debug_list().entries(&self.body).finish()
    }
}

pub(crate) struct FunDefNode {
    pub name: IdentLiteralNode,
    pub parameters: Vec<(IdentLiteralNode, IdentLiteralNode)>,
    pub ret_type: IdentLiteralNode,
    pub body: BlockNode,
    pub span: Span,
}
impl fmt::Debug for FunDefNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "\x1b[38;2;255;170;0m{}\x1b[90m..\x1b[38;2;255;170;0m{}\x1b[0m \x1b[38;2;123;243;100mFunDef\x1b[0m ",
            self.span.start, self.span.end
        )?;
        f.debug_struct("")
            .field("\n  \x1b[38;2;128;128;128mname\x1b[0m", &self.name)
            .field("\n  \x1b[38;2;128;128;128mparams\x1b[0m", &self.parameters)
            .field("\n  \x1b[38;2;128;128;128mret\x1b[0m", &self.ret_type)
            .field("\n  \x1b[38;2;128;128;128mbody\x1b[0m", &self.body)
            .finish()
    }
}

pub(crate) struct IfNode {
    condition: Expression,
    then_branch: BlockNode,
    else_if_branches: Vec<ElseIfNode>,
    else_branch: Option<BlockNode>,
    span: Span,
}
impl fmt::Debug for IfNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "\x1b[38;2;255;170;0m{}\x1b[90m..\x1b[38;2;255;170;0m{}\x1b[0m \x1b[38;2;100;102;239mIf\x1b[0m ",
            self.span.start, self.span.end
        )?;
        f.debug_struct("")
            .field("\n  \x1b[38;2;128;128;128mcond\x1b[0m", &self.condition)
            .field("\n  \x1b[38;2;128;128;128mthen\x1b[0m", &self.then_branch)
            .field(
                "\n  \x1b[38;2;128;128;128melse_ifs\x1b[0m",
                &self.else_if_branches,
            )
            .field("\n  \x1b[38;2;128;128;128melse\x1b[0m", &self.else_branch)
            .finish()
    }
}

pub(crate) struct ElseIfNode {
    condition: Expression,
    then_branch: BlockNode,
    span: Span,
}
impl fmt::Debug for ElseIfNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "\n    \x1b[38;2;255;170;0m{}\x1b[90m..\x1b[38;2;255;170;0m{}\x1b[0m \x1b[38;2;100;102;210mElseIf\x1b[0m ",
            self.span.start, self.span.end
        )?;
        f.debug_struct("")
            .field("\n      \x1b[38;2;128;128;128mcond\x1b[0m", &self.condition)
            .field(
                "\n      \x1b[38;2;128;128;128mthen\x1b[0m",
                &self.then_branch,
            )
            .finish()
    }
}

pub(crate) struct WhileNode {
    condition: Expression,
    body: BlockNode,
    span: Span,
}
impl fmt::Debug for WhileNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "\x1b[38;2;255;170;0m{}\x1b[90m..\x1b[38;2;255;170;0m{}\x1b[0m \x1b[38;2;250;102;250mWhile\x1b[0m ",
            self.span.start, self.span.end
        )?;
        f.debug_struct("")
            .field("\n  \x1b[38;2;128;128;128mcond\x1b[0m", &self.condition)
            .field("\n  \x1b[38;2;128;128;128mbody\x1b[0m", &self.body)
            .finish()
    }
}

pub(crate) struct VarDecNode {
    name: IdentLiteralNode,
    var_type: IdentLiteralNode,
    initalizer: Expression,
    mutable: bool,
    span: Span,
}
impl fmt::Debug for VarDecNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "\x1b[38;2;255;170;0m{}\x1b[90m..\x1b[38;2;255;170;0m{}\x1b[0m \x1b[38;2;100;200;143mVarDec\x1b[0m ",
            self.span.start, self.span.end
        )?;
        f.debug_struct("")
            .field("\n  \x1b[38;2;128;128;128mname\x1b[0m", &self.name)
            .field("\n  \x1b[38;2;128;128;128mtype\x1b[0m", &self.var_type)
            .field("\n  \x1b[38;2;128;128;128minit\x1b[0m", &self.initalizer)
            .field("\n  \x1b[38;2;128;128;128mmut\x1b[0m", &self.mutable)
            .finish()
    }
}

pub(crate) struct AssignmentNode {
    left: IdentLiteralNode,
    right: Expression,
    span: Span,
}
impl fmt::Debug for AssignmentNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "\x1b[38;2;255;170;0m{}\x1b[90m..\x1b[38;2;255;170;0m{}\x1b[0m \x1b[38;2;223;230;40mAssign\x1b[0m ",
            self.span.start, self.span.end
        )?;
        f.debug_struct("")
            .field("\n  \x1b[38;2;128;128;128mleft\x1b[0m", &self.left)
            .field("\n  \x1b[38;2;128;128;128mright\x1b[0m", &self.right)
            .finish()
    }
}

pub(crate) enum BinaryOperator {
    // Logical
    Or,
    And,
    // Equality
    IsEquals,
    IsNotEquals,
    // Comparison
    Less,
    LessEquals,
    Greater,
    GreaterEquals,
    // Term
    Add,
    Sub,
    // Factor
    Mul,
    Div,
    Percent,
}

impl fmt::Debug for BinaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let fmt: String = match &self {
            Self::Or => "Or".into(),
            Self::And => "And".into(),
            Self::IsEquals => "IsEquals".into(),
            Self::IsNotEquals => "IsNotEquals".into(),
            Self::Less => "IsLess".into(),
            Self::LessEquals => "IsLessEqauls".into(),
            Self::Greater => "IsGreater".into(),
            Self::GreaterEquals => "IsGreaterEquals".into(),
            Self::Add => "Add".into(),
            Self::Sub => "Sub".into(),
            Self::Mul => "Mul".into(),
            Self::Div => "Div".into(),
            Self::Percent => "Percent".into(),
        };
        write!(f, "\x1b[38;2;100;100;250m{}\x1b[0m", fmt)
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord)]
enum Precedence {
    None,
    Assignment, // =
    Or,         // ||
    And,        // &&
    Equality,   // == !=
    Comparison, // < > <= >=
    Term,       // + -
    Factor,     // * / %
    Unary,      // ! -
    Call,       // () [] .
    Primary,    // Literal Ident Grouping
}
impl Precedence {
    fn precedence_of(t: &TokenKind) -> Self {
        match *t {
            Eq => Self::Assignment,
            OrOr => Self::Or,
            AndAnd => Self::And,
            EqEq | NotEq => Self::Equality,
            Lt | Gt | LtEq | GtEq => Self::Comparison,
            Plus | Minus => Self::Term,
            Star | Slash | Percent => Self::Factor,
            OpenParam | Dot | OpenBracket => Self::Call,
            _ => Self::None,
        }
    }
    fn next(&self) -> Self {
        match self {
            Self::None => Self::Assignment,
            Self::Assignment => Self::Or,
            Self::Or => Self::And,
            Self::And => Self::Equality,
            Self::Equality => Self::Comparison,
            Self::Comparison => Self::Term,
            Self::Term => Self::Factor,
            Self::Factor => Self::Unary,
            Self::Unary => Self::Call,
            Self::Call => Self::Primary,
            Self::Primary => Self::Primary,
        }
    }
}

pub(crate) enum Expression {
    Binary(BinaryExpressionNode),
    Unary(UnaryExpressionNode),
    Literal(LiteralNode),
    Ident(IdentLiteralNode),
    Call(CallNode),
    Index(IndexExpressionNode),
    FieldAccess(FieldAccessNode),
    Broken(Span),
}
impl fmt::Debug for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Binary(n) => n.fmt(f),
            Self::Unary(n) => n.fmt(f),
            Self::Literal(n) => n.fmt(f),
            Self::Ident(n) => n.fmt(f),
            Self::Call(n) => n.fmt(f),
            Self::Index(n) => n.fmt(f),
            Self::FieldAccess(n) => n.fmt(f),
            Self::Broken(span) => write!(
                f,
                "\x1b[38;2;255;170;0m{}\x1b[90m..\x1b[38;2;255;170;0m{}\x1b[0m \x1b[31mBrokenExpression!\x1b[0m",
                span.start, span.end
            ),
        }
    }
}

impl Expression {
    fn get_span(&self) -> Span {
        match self {
            Self::Binary(x) => x.span,
            Self::Unary(x) => x.span,
            Self::Literal(x) => x.span,
            Self::Ident(x) => x.span,
            Self::Call(x) => x.span,
            Self::Index(x) => x.span,
            Self::FieldAccess(x) => x.span,
            Self::Broken(x) => *x,
        }
    }
}

pub(crate) struct IndexExpressionNode {
    target: Box<Expression>,
    index: Box<Expression>,
    span: Span,
}
impl fmt::Debug for IndexExpressionNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "\x1b[38;2;255;170;0m{}\x1b[90m..\x1b[38;2;255;170;0m{}\x1b[0m \x1b[38;2;123;243;100mIndex\x1b[0m ",
            self.span.start, self.span.end
        )?;
        f.debug_struct("")
            .field("\n  \x1b[38;2;128;128;128mtarget\x1b[0m", &self.target)
            .field("\n  \x1b[38;2;128;128;128mindex\x1b[0m", &self.index)
            .finish()
    }
}
pub(crate) struct FieldAccessNode {
    target: Box<Expression>,
    field: IdentLiteralNode,
    span: Span,
}
impl fmt::Debug for FieldAccessNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "\x1b[38;2;255;170;0m{}\x1b[90m..\x1b[38;2;255;170;0m{}\x1b[0m \x1b[38;2;123;243;100mFieldAccess\x1b[0m ",
            self.span.start, self.span.end
        )?;
        f.debug_struct("")
            .field("\n  \x1b[38;2;128;128;128mtarget\x1b[0m", &self.target)
            .field("\n  \x1b[38;2;128;128;128mfield\x1b[0m", &self.field)
            .finish()
    }
}

pub(crate) struct BinaryExpressionNode {
    left: Box<Expression>,
    op: BinaryOperator,
    right: Box<Expression>,
    span: Span,
}
impl fmt::Debug for BinaryExpressionNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "\x1b[38;2;255;170;0m{}\x1b[90m..\x1b[38;2;255;170;0m{}\x1b[0m \x1b[38;2;100;123;143mBinary\x1b[0m ",
            self.span.start, self.span.end
        )?;
        f.debug_struct("")
            .field("\n  \x1b[38;2;128;128;128mop\x1b[0m", &self.op)
            .field("\n  \x1b[38;2;128;128;128mleft\x1b[0m", &self.left)
            .field("\n  \x1b[38;2;128;128;128mright\x1b[0m", &self.right)
            .finish()
    }
}

pub(crate) struct UnaryExpressionNode {
    operator: UnaryOperator,
    operand: Box<Expression>,
    span: Span,
}
impl fmt::Debug for UnaryExpressionNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "\x1b[38;2;255;170;0m{}\x1b[90m..\x1b[38;2;255;170;0m{}\x1b[0m \x1b[38;2;100;123;143mUnary\x1b[0m ",
            self.span.start, self.span.end
        )?;
        f.debug_struct("")
            .field("\n  \x1b[38;2;128;128;128moperator\x1b[0m", &self.operator)
            .field("\n  \x1b[38;2;128;128;128moperand\x1b[0m", &self.operand)
            .finish()
    }
}

pub(crate) enum UnaryOperator {
    Not,
    Neg,
}

impl fmt::Debug for UnaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let fmt: String = match self {
            Self::Neg => "neg".into(),
            Self::Not => "not".into(),
        };
        write!(f, "\x1b[38;2;240;100;100m{}\x1b[0m", fmt)
    }
}

pub(crate) struct CallNode {
    primary: Box<Expression>,
    arguments: Vec<Expression>,
    span: Span,
}
impl fmt::Debug for CallNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "\x1b[38;2;255;170;0m{}\x1b[90m..\x1b[38;2;255;170;0m{}\x1b[0m \x1b[38;2;100;243;143mCall\x1b[0m ",
            self.span.start, self.span.end
        )?;
        f.debug_struct("")
            .field("\n  \x1b[38;2;128;128;128mfunc\x1b[0m", &self.primary)
            .field("\n  \x1b[38;2;128;128;128margs\x1b[0m", &self.arguments)
            .finish()
    }
}

// hizli new metodlari
pub(crate) struct LiteralNode {
    value: LiteralValue,
    span: Span,
}
impl fmt::Debug for LiteralNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "\x1b[38;2;50;100;240m{:?}\x1b[0m", self.value)
    }
}

#[derive(Debug)]
pub(crate) enum LiteralValue {
    Number(i32),
    Str(String),
    Bool(bool),
    Char(char),
    Null,
}

#[derive(Clone)]
pub(crate) struct IdentLiteralNode {
    pub value: String,
    pub span: Span,
}
impl fmt::Debug for IdentLiteralNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "\x1b[38;2;200;200;100mIdent \x1b[1m{:?}\x1b[0m",
            self.value
        )
    }
}

pub(crate) struct ReturnNode {
    value: Option<Expression>,
    span: Span,
}
impl fmt::Debug for ReturnNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "\x1b[38;2;255;170;0m{}\x1b[90m..\x1b[38;2;255;170;0m{}\x1b[0m \x1b[38;2;150;223;143mReturn\x1b[0m ",
            self.span.start, self.span.end
        )?;
        match &self.value {
            Some(v) => v.fmt(f),
            Option::None => write!(f, "Void"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::driver::lexer::Lexer;

    fn parse_source(source: &str) -> (Vec<Statement>, Diagnostics) {
        let mut diag = Diagnostics::empty_diagnostics(source.to_string());
        let tokens = {
            let lexer = Lexer::new(diag.source_code().char_indices().peekable());
            lexer.tokenize()
        };
        let mut parser = Parser::new(&tokens, &mut diag);
        let statements = parser.parse();
        (statements, diag)
    }

    #[test]
    fn test_val_declaration() {
        let src = "val x = 10;";
        let (stmts, diag) = parse_source(src);
        assert!(!diag.has_errors());
        assert_eq!(stmts.len(), 1);
        if let Statement::VarDeclaration(node) = &stmts[0] {
            assert_eq!(node.name.value, "x");
            assert!(!node.mutable);
            if let Expression::Literal(l) = &node.initalizer {
                if let LiteralValue::Number(n) = l.value {
                    assert_eq!(n, 10);
                } else {
                    panic!("Expected number literal");
                }
            } else {
                panic!("Expected literal expression");
            }
        } else {
            panic!("Expected VarDeclaration");
        }
    }

    #[test]
    fn test_var_declaration() {
        let src = "var y = 20;";
        let (stmts, diag) = parse_source(src);
        assert!(!diag.has_errors());
        if let Statement::VarDeclaration(node) = &stmts[0] {
            assert_eq!(node.name.value, "y");
            assert!(node.mutable);
        } else {
            panic!("Expected VarDeclaration");
        }
    }

    #[test]
    fn test_fun_definition() {
        let src = "fun add(a int, b int) int { ret a + b; }";
        let (stmts, diag) = parse_source(src);
        assert!(!diag.has_errors());
        if let Statement::FunDefinition(node) = &stmts[0] {
            assert_eq!(node.name.value, "add");
            assert_eq!(node.parameters.len(), 2);
            assert_eq!(node.parameters[0].0.value, "a");
            assert_eq!(node.parameters[1].0.value, "b");
            assert_eq!(node.ret_type.value, "int");
        } else {
            panic!("Expected FunDefinition");
        }
    }

    #[test]
    fn test_binary_precedence() {
        let src = "val x = 1 + 2 * 3;";
        let (stmts, diag) = parse_source(src);
        assert!(!diag.has_errors());
        if let Statement::VarDeclaration(v) = &stmts[0] {
            if let Expression::Binary(b) = &v.initalizer {
                if let BinaryOperator::Add = b.op {
                    // Good
                } else {
                    panic!("Expected Add, got {:?}", b.op);
                }
            }
        }
    }

    #[test]
    fn test_if_else() {
        let src = "if x { } else { }";
        let (stmts, diag) = parse_source(src);
        assert!(!diag.has_errors());
        if let Statement::If(node) = &stmts[0] {
            assert!(node.else_branch.is_some());
        } else {
            panic!("Expected If");
        }
    }

    #[test]
    fn test_complex_expression_syntax_error() {
        let src = "if x == && a[12 == 75) { }";
        let (stmts, diag) = parse_source(src);

        assert!(diag.has_errors(), "Should have syntax errors");
        assert!(!stmts.is_empty());
    }

    #[test]
    fn test_missing_semicolon() {
        let src = "val x = 10"; // 7 - 8
        let (stmts, diag) = parse_source(src);

        assert!(diag.has_errors());
        assert!(!stmts.is_empty());
        if let Statement::Broken(span) = stmts[0] {
            assert_eq!(span.start, 10); // val x = 10
            assert_eq!(span.end, 11); // End of "10"
        } else {
            panic!("Expected BrokenStatement due to missing semicolon");
        }
    }

    #[test]
    fn test_consecutive_errors() {
        let src = "var x; val y = ;";
        let (stmts, diag) = parse_source(src);

        assert!(diag.errors_len() >= 2);

        assert!(stmts.len() >= 1);
    }

    #[test]
    fn test_incomplete_binary_op() {
        // Yarım kalmış işlem
        let src = "val z = 10 + ;";
        let (stmts, diag) = parse_source(src);

        assert!(diag.has_errors());
        assert!(!stmts.is_empty());
        if let Statement::VarDeclaration(v) = &stmts[0] {
            if let Expression::Binary(b_expr) = &v.initalizer {
                if let Expression::Broken(span) = &*b_expr.right {
                    assert_eq!(span.start, 13);
                    assert_eq!(span.end, 14);
                } else {
                    panic!(
                        "Expected right operand of binary expression to be Broken, got {:?}",
                        b_expr.right
                    );
                }
            } else {
                panic!(
                    "Expected binary expression initializer, got {:?}",
                    v.initalizer
                );
            }
        } else {
            panic!(
                "Expected VarDeclaration for incomplete binary op, got {:?}",
                stmts[0]
            );
        }
    }

    #[test]
    fn test_unclosed_block() {
        // Kapanmamış süslü parantez
        let src = "fun foo() void { val x = 1; ";
        let (stmts, diag) = parse_source(src);

        assert!(diag.has_errors());
        assert!(!stmts.is_empty());
        if let Statement::FunDefinition(f) = &stmts[0] {
            if let Statement::Broken(span) = &f.body.body[0] {
                //
            } else {
                // If body has a statement, it should be parsed till the end or be broken
            }
        } else {
            panic!("Expected FunDefinition for unclosed block");
        }
    }

    #[test]
    fn test_call_error_recovery_hello() {
        let src = "hello(;";
        let (stmts, diag) = parse_source(src);

        // Should have errors
        assert!(diag.has_errors());
        // Should only have 1 error: "Unexpected token ';', expected Expr"
        // The "expected ')'" error should be suppressed.
        assert_eq!(
            diag.errors_len(),
            1,
            "Should report exactly 1 error for 'hello(;'"
        );

        if let Statement::Expression(Expression::Call(call_node)) = &stmts[0] {
            assert_eq!(call_node.arguments.len(), 1);
            if let Expression::Broken(_) = &call_node.arguments[0] {
                // Correctly recovered as a Broken argument
            } else {
                panic!("Expected broken argument");
            }
        } else {
            panic!("Expected Call expression statement");
        }
    }

    #[test]
    fn test_stack_overflow_broken_expr() {
        // This test ensures that printing a Broken expression doesn't cause a stack overflow
        // via infinite recursion in Debug impl.
        let broken = Expression::Broken(Span { start: 0, end: 1 });
        let debug_str = format!("{:?}", broken);
        assert!(debug_str.contains("BrokenExpression!"));
    }

    #[test]
    fn test_invalid_val_name_start_digit() {
        let src = "val 12x = 1000;";
        let (stmts, diag) = parse_source(src);

        assert!(diag.has_errors());
        let err_span = diag.errors()[0].span();
        // The error should be on '12' (index 4), not '1000' (index 10).
        assert_eq!(err_span.start, 4, "Error should point to '12'");
        assert!(stmts.len() > 0); // Should produce a broken statement
    }
}
