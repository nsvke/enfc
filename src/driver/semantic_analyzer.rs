#![allow(unused)]

use crate::compile_error::CompileError;
use crate::diagnostic::Diagnostics;
use crate::driver::parser::{
    BinaryOperator, IdentLiteralNode, LiteralNode, Statement, UnaryOperator,
};
use crate::structs::Span;
use std::collections::{HashMap, hash_map::Entry};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Type {
    pub span: Span,
    pub kind: TypeKind,
}

impl Type {
    fn eq(&self, t: &Type) -> bool {
        t.kind == self.kind
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeKind {
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
}

impl<'a> SemanticAnalyzer<'a> {
    pub(crate) fn new(statements: &'a [Statement], diagnose: &'a mut Diagnostics) -> Self {
        Self {
            statements,
            scopes: vec![HashMap::new()],
            diagnose,
        }
    }
    pub(crate) fn analyze(&mut self) -> Vec<TypedStatement> {
        self.collect_signatures();
        for stmt in self.statements {
            //
        }
        Vec::new()
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
}

#[derive(Debug)]
pub(crate) enum TypedStatement {
    VarDeclaration(TypedVarDecNode),
    Assignment(TypedAssignmentNode),
    If(TypedIfNode),
    While(TypedWhileNode),
    FunDefinition(TypedFunDefNode),
    Block(TypedBlockNode),
    Expression(TypedExpression),
    Return(TypedReturnNode),
    Broken(Span),
}

#[derive(Debug)]
pub(crate) struct TypedBlockNode {
    body: Vec<TypedStatement>,
    span: Span,
}

#[derive(Debug)]
pub(crate) struct TypedFunDefNode {
    name: IdentLiteralNode,
    parameters: Vec<(IdentLiteralNode, IdentLiteralNode)>,
    ret_type: Type,
    body: TypedBlockNode,
    span: Span,
}
#[derive(Debug)]
pub(crate) struct TypedIfNode {
    condition: TypedExpression,
    then_branch: TypedBlockNode,
    else_if_branches: Vec<TypedElseIfNode>,
    else_branch: Option<TypedBlockNode>,
    span: Span,
}
#[derive(Debug)]
pub(crate) struct TypedElseIfNode {
    condition: TypedExpression,
    then_branch: TypedBlockNode,
    span: Span,
}
#[derive(Debug)]
pub(crate) struct TypedWhileNode {
    condition: TypedExpression,
    body: TypedBlockNode,
    span: Span,
}
#[derive(Debug)]
pub(crate) struct TypedVarDecNode {
    name: IdentLiteralNode,
    var_type: Type,
    initalizer: TypedExpression,
    mutable: bool,
    span: Span,
}
#[derive(Debug)]
pub(crate) struct TypedAssignmentNode {
    left: IdentLiteralNode,
    right: TypedExpression,
    span: Span,
}

#[derive(Debug)]
pub(crate) struct TypedExpression {
    kind: TypedExpressionKind,
    typ: Type,
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
    Broken(Span),
}

#[derive(Debug)]
pub(crate) struct TypedIndexExpressionNode {
    target: Box<TypedExpression>,
    index: Box<TypedExpression>,
    span: Span,
}
#[derive(Debug)]
pub(crate) struct TypedFieldAccessNode {
    target: Box<TypedExpression>,
    field: IdentLiteralNode,
    span: Span,
}
#[derive(Debug)]
pub(crate) struct TypedBinaryExpressionNode {
    left: Box<TypedExpression>,
    op: BinaryOperator,
    right: Box<TypedExpression>,
    span: Span,
}
#[derive(Debug)]
pub(crate) struct TypedUnaryExpressionNode {
    operator: UnaryOperator,
    operand: Box<TypedExpression>,
    span: Span,
}

#[derive(Debug)]
pub(crate) struct TypedCallNode {
    primary: Box<TypedExpression>,
    arguments: Vec<TypedExpression>,
    span: Span,
}

#[derive(Debug)]
pub(crate) struct TypedReturnNode {
    value: TypedExpression,
    span: Span,
}
