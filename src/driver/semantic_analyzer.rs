#![allow(unused)]

use crate::compile_error::CompileError;
use crate::diagnostic::Diagnostics;
use crate::driver::parser::{
    BinaryOperator, Expression, IdentLiteralNode, LiteralNode, Statement, UnaryOperator,
};
use crate::structs::Span;
use std::collections::{HashMap, hash_map::Entry};

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Type {
    pub span: Span,
    pub kind: TypeKind,
}

impl Type {
    fn eq(&self, t: &Type) -> bool {
        t.kind == self.kind
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum TypeKind {
    Int,
    Str,
    Bool,
    Char,
    Nret,
    Error,
}

#[derive(Debug)]
enum Symbol {
    Var(VarInfo),
    Fun(FunInfo),
}

#[derive(Debug)]
struct VarInfo {
    // name: String,
    typ: Type,
    mutable: bool,
    span: Span,
}

#[derive(Debug)]
struct FunInfo {
    // name: String,
    params: Vec<(IdentLiteralNode, Type)>, // (param_name, param_type)
    ret_type: Type,
    span: Span,
}

pub(crate) struct SemanticAnalyzer<'a> {
    statements: &'a [Statement],
    scopes: Vec<HashMap<String, Symbol>>,
    diagnose: &'a mut Diagnostics,
    destruct: bool,
}

impl<'a> SemanticAnalyzer<'a> {
    pub(crate) fn new(statements: &'a [Statement], diagnose: &'a mut Diagnostics) -> Self {
        Self {
            statements,
            scopes: vec![HashMap::new()],
            diagnose,
            destruct: false,
        }
    }
    pub(crate) fn analyze(&mut self) -> Result<Vec<TypedStatement>, Vec<TypedStatement>> {
        let v = self.analyze_and_collect();
        if !self.destruct {
            if !v.is_empty() { Ok(v) } else { Err(v) }
        } else {
            Err(v)
        }
    }

    fn analyze_and_collect(&mut self) -> Vec<TypedStatement> {
        let mut typed_statements = Vec::new();
        self.collect_signatures();
        for stmt in self.statements {
            //
        }
        typed_statements
    }

    fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }
    fn exit_scope(&mut self) {
        self.scopes.pop();
    }

    fn resolve_types(&mut self, typ: &IdentLiteralNode) -> Type {
        let kind = match typ.value.as_str() {
            "int" => TypeKind::Int,
            "str" => TypeKind::Str,
            "bool" => TypeKind::Bool,
            "chr" => TypeKind::Char,
            "nret" => TypeKind::Nret,
            val => {
                self.destruct = true;
                self.diagnose
                    .push_error(CompileError::unknown_type(val.into(), typ.span));
                TypeKind::Error
            }
        };
        Type {
            span: typ.span,
            kind,
        }
    }

    fn broken_typed_statement(&mut self, span: Span) -> TypedStatement {
        self.destruct = true;
        TypedStatement {
            kind: TypedStatementKind::Broken,
            span,
        }
    }

    fn broken_typed_expr(&mut self, span: Span) -> TypedExpression {
        self.destruct = true;
        TypedExpression {
            kind: TypedExpressionKind::Broken,
            typ: TypeKind::Error,
            span,
        }
    }

    fn collect_signatures(&mut self) {
        for stmt in self.statements {
            match stmt {
                Statement::FunDefinition(node) => {
                    // TODO check redefination first, resolve types later
                    let params = node
                        .parameters
                        .iter()
                        .map(|(name, typ)| (name.clone(), self.resolve_types(typ)))
                        .collect();
                    let ret_typ = self.resolve_types(&node.ret_type);
                    match self.scopes[0].entry(node.name.value.clone()) {
                        Entry::Occupied(occupied_entry) => {
                            self.destruct = true;
                            self.diagnose.push_error(CompileError::symbol_redefination(
                                node.name.value.clone(),
                                match occupied_entry.get() {
                                    Symbol::Fun(fun_info) => fun_info.span,
                                    Symbol::Var(var_info) => var_info.span,
                                },
                                node.name.span,
                                crate::compile_error::SymbolKind::Function,
                            ))
                        }
                        Entry::Vacant(entry) => {
                            entry.insert(Symbol::Fun(FunInfo {
                                params: params,
                                ret_type: ret_typ,
                                span: node.span,
                            }));
                        }
                    };
                }
                _ => {}
            };
        }
    }

    fn analyze_expression(&mut self, expr: &Expression) -> TypedExpression {
        match expr {
            Expression::Binary(exp) => {}
            Expression::Unary(exp) => {}
            Expression::Literal(exp) => {}
            Expression::Ident(exp) => {}
            Expression::Call(exp) => {}
            Expression::Index(exp) => {}
            Expression::FieldAccess(exp) => {}
            Expression::Broken(span) => {
                self.destruct = true;
                // self.broken_typed_expr(span)
            }
        };

        self.broken_typed_expr(Span::default()) // mock return
    }
}
#[derive(Debug)]
pub(crate) struct TypedStatement {
    kind: TypedStatementKind,
    span: Span,
}
#[derive(Debug)]
pub(crate) enum TypedStatementKind {
    VarDeclaration(TypedVarDecNode),
    Assignment(TypedAssignmentNode),
    If(TypedIfNode),
    While(TypedWhileNode),
    FunDefinition(TypedFunDefNode),
    Block(TypedBlockNode),
    Expression(TypedExpression),
    Return(TypedReturnNode),
    Broken,
}

#[derive(Debug)]
pub(crate) struct TypedBlockNode {
    body: Vec<TypedStatement>,
}

#[derive(Debug)]
pub(crate) struct TypedFunDefNode {
    name: IdentLiteralNode,
    parameters: Vec<(IdentLiteralNode, IdentLiteralNode)>,
    ret_type: Type,
    body: TypedBlockNode,
}
#[derive(Debug)]
pub(crate) struct TypedIfNode {
    condition: TypedExpression,
    then_branch: TypedBlockNode,
    else_if_branches: Vec<TypedElseIfNode>,
    else_branch: Option<TypedBlockNode>,
}
#[derive(Debug)]
pub(crate) struct TypedElseIfNode {
    condition: TypedExpression,
    then_branch: TypedBlockNode,
}
#[derive(Debug)]
pub(crate) struct TypedWhileNode {
    condition: TypedExpression,
    body: TypedBlockNode,
}
#[derive(Debug)]
pub(crate) struct TypedVarDecNode {
    name: IdentLiteralNode,
    var_type: Type,
    initalizer: TypedExpression,
    mutable: bool,
}
#[derive(Debug)]
pub(crate) struct TypedAssignmentNode {
    left: IdentLiteralNode,
    right: TypedExpression,
}

#[derive(Debug)]
pub(crate) struct TypedExpression {
    kind: TypedExpressionKind,
    typ: TypeKind,
    span: Span,
}

#[derive(Debug)]
pub(crate) enum TypedExpressionKind {
    Binary(TypedBinaryExpressionNode),
    Unary(TypedUnaryExpressionNode),
    Literal(LiteralNode),
    Ident(IdentLiteralNode),
    Call(TypedCallNode),
    Index(TypedIndexExpressionNode),
    FieldAccess(TypedFieldAccessNode),
    Broken,
}

#[derive(Debug)]
pub(crate) struct TypedIndexExpressionNode {
    target: Box<TypedExpression>,
    index: Box<TypedExpression>,
}
#[derive(Debug)]
pub(crate) struct TypedFieldAccessNode {
    target: Box<TypedExpression>,
    field: IdentLiteralNode,
}
#[derive(Debug)]
pub(crate) struct TypedBinaryExpressionNode {
    left: Box<TypedExpression>,
    op: BinaryOperator,
    right: Box<TypedExpression>,
}
#[derive(Debug)]
pub(crate) struct TypedUnaryExpressionNode {
    operator: UnaryOperator,
    operand: Box<TypedExpression>,
}

#[derive(Debug)]
pub(crate) struct TypedCallNode {
    primary: Box<TypedExpression>,
    arguments: Vec<TypedExpression>,
}

#[derive(Debug)]
pub(crate) struct TypedReturnNode {
    value: TypedExpression,
}
