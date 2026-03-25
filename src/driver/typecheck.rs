#![allow(unused)]

use crate::compile_error::CompileError;
use crate::diagnostic::Diagnostics;
use crate::driver::parse::{
    BinaryExpressionNode, BinaryOperator, CallNode, Expression, FieldAccessNode, IdentLiteralNode,
    IndexExpressionNode, LiteralNode, LiteralValue, Statement, UnaryExpressionNode, UnaryOperator,
};
use crate::structs::Span;
use std::collections::{HashMap, hash_map::Entry};
use std::fmt::Debug;

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
    Function { params: Vec<Type>, ret: Box<Type> },
    Unknown,
}

#[derive(Debug)]
pub(crate) struct Symbol {
    pub typ: Type,
    pub span: Span,
    pub mutable: bool,
}

pub(crate) struct TypeChecker<'a> {
    // statements: &'a [Statement],
    scopes: Vec<HashMap<String, Symbol>>,
    diagnose: &'a mut Diagnostics,
    // destruct: bool,
}

impl<'a> TypeChecker<'a> {
    pub(crate) fn new(diagnose: &'a mut Diagnostics) -> Self {
        Self {
            scopes: vec![HashMap::new()],
            diagnose,
        }
    }
    pub(crate) fn check(&mut self, statements: &'a [Statement]) -> Vec<TypedStatement> {
        self.check_and_collect(statements)
    }

    fn check_and_collect(&mut self, statements: &'a [Statement]) -> Vec<TypedStatement> {
        let mut typed_statements = Vec::new();
        self.collect_signatures(statements);
        for stmt in statements {
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

    fn broken_typed_stmt(&mut self, span: Span) -> TypedStatement {
        TypedStatement {
            kind: TypedStatementKind::Broken,
            span,
        }
    }

    fn broken_typed_expr(&self, span: Span) -> TypedExpression {
        TypedExpression {
            kind: TypedExpressionKind::Broken,
            typ: TypeKind::Error,
            span,
        }
    }

    fn collect_signatures(&mut self, statements: &'a [Statement]) {
        for stmt in statements {
            match stmt {
                Statement::FunDefinition(node) => {
                    // TODO check redefination first, resolve types later
                    let params = node
                        .parameters
                        .iter()
                        .map(|(_, typ)| self.resolve_types(typ))
                        .collect();
                    let ret_typ = self.resolve_types(&node.ret_type);
                    match self.scopes[0].entry(node.name.value.clone()) {
                        Entry::Occupied(occupied_entry) => {
                            self.diagnose.push_error(CompileError::symbol_redefination(
                                node.name.value.clone(),
                                occupied_entry.get().span,
                                node.name.span,
                                crate::compile_error::SymbolKind::Function,
                            ))
                        }
                        Entry::Vacant(entry) => {
                            entry.insert(Symbol {
                                typ: Type {
                                    span: node.span,
                                    kind: TypeKind::Function {
                                        params,
                                        ret: Box::new(ret_typ),
                                    },
                                },
                                span: node.span,
                                mutable: false,
                            });
                        }
                    };
                }
                _ => {}
            };
        }
    }

    fn check_redefinition(&mut self, ident: &IdentLiteralNode) -> bool {
        self.scopes.last().unwrap().contains_key(&ident.value)
    }

    fn resolve_symbol(&mut self, s: &str) -> TypeKind {
        let mut typ = TypeKind::Unknown;
        for scope in self.scopes.iter().rev() {
            if let Some(symbol) = scope.get(s) {
                typ = symbol.typ.kind.clone();
                break;
            }
        }
        typ
    }

    fn check_expr_field(&mut self, node: &FieldAccessNode) -> TypedExpression {
        self.diagnose
            .push_error(CompileError::feature_not_supported(
                "FieldAccess".into(),
                node.span,
            ));
        TypedExpression {
            kind: TypedExpressionKind::FieldAccess(TypedFieldAccessNode {
                target: Box::new(self.check_expr(&node.target)),
                field: node.field.clone(),
            }),
            typ: TypeKind::Unknown,
            span: node.span,
        }
    }

    fn check_expr_index(&mut self, node: &IndexExpressionNode) -> TypedExpression {
        self.diagnose
            .push_error(CompileError::feature_not_supported(
                "IndexAccess".into(),
                node.span,
            ));
        TypedExpression {
            kind: TypedExpressionKind::Index(TypedIndexExpressionNode {
                target: Box::new(self.check_expr(&node.target)),
                index: Box::new(self.check_expr(&node.index)),
            }),
            typ: TypeKind::Unknown,
            span: node.span,
        }
    }

    fn check_expr_call(&mut self, node: &CallNode) -> TypedExpression {
        let typed_primary = self.check_expr(&node.primary);

        let mut typ = TypeKind::Unknown;

        let mut arguments = Vec::new();

        if typed_primary.typ != TypeKind::Unknown {
            match &typed_primary.typ {
                TypeKind::Function { params, ret } => {
                    if node.arguments.len() != params.len() {
                        self.diagnose.push_error(CompileError::missing_argument(
                            params.len(),
                            node.arguments.len(),
                            typed_primary.span,
                        ));
                    }

                    for (i, arg_expr) in node.arguments.iter().enumerate() {
                        let typed_arg = self.check_expr(arg_expr);

                        if let Some(expected_typ) = params.get(i) {
                            if typed_arg.typ != expected_typ.kind {
                                self.diagnose.push_error(CompileError::unexpected_type(
                                    expected_typ.kind.clone(),
                                    typed_arg.typ.clone(),
                                    typed_arg.span,
                                ));
                            }
                        }

                        arguments.push(typed_arg);
                    }

                    typ = ret.kind.clone();
                }
                _ => {
                    self.diagnose.push_error(CompileError::not_callable(
                        typed_primary.typ.clone(),
                        typed_primary.span,
                    ));
                }
            };
        }

        TypedExpression {
            kind: TypedExpressionKind::Call(TypedCallNode {
                primary: Box::new(typed_primary),
                arguments,
            }),
            typ,
            span: node.span,
        }
    }

    fn check_expr_binary(&mut self, node: &BinaryExpressionNode) -> TypedExpression {
        let (expected_op, final_type) = match node.op {
            BinaryOperator::Add
            | BinaryOperator::Sub
            | BinaryOperator::Mul
            | BinaryOperator::Div
            | BinaryOperator::Percent => (TypeKind::Int, TypeKind::Int),
            BinaryOperator::Greater
            | BinaryOperator::Less
            | BinaryOperator::GreaterEquals
            | BinaryOperator::LessEquals => (TypeKind::Int, TypeKind::Bool),
            BinaryOperator::And | BinaryOperator::Or => (TypeKind::Bool, TypeKind::Bool),
            BinaryOperator::IsEquals | BinaryOperator::IsNotEquals => {
                (TypeKind::Unknown, TypeKind::Bool)
            }
        };

        let typed_left = self.check_expr(&node.left);
        let typed_right = self.check_expr(&node.right);

        let mut typ = final_type.clone();

        if typed_left.typ == TypeKind::Unknown || typed_right.typ == TypeKind::Unknown {
            typ = TypeKind::Unknown;
        }

        if typ != TypeKind::Unknown {
            if expected_op != TypeKind::Unknown {
                if typed_left.typ != expected_op || typed_right.typ != expected_op {
                    self.diagnose.push_error(CompileError::type_mismatch(
                        typed_left.typ.clone(),
                        typed_right.typ.clone(),
                        typed_left.span,
                        typed_right.span,
                        format!("{:?}", node.op),
                    ));
                    typ = TypeKind::Unknown;
                }
            } else {
                if typed_left.typ != typed_right.typ {
                    self.diagnose.push_error(CompileError::type_mismatch(
                        typed_left.typ.clone(),
                        typed_right.typ.clone(),
                        typed_left.span,
                        typed_right.span,
                        format!("{:?}", node.op),
                    ));
                    typ = TypeKind::Unknown;
                }
            }
        }

        if node.op == BinaryOperator::Div {
            if let TypedExpressionKind::Literal(lit) = &typed_right.kind {
                if let LiteralValue::Number(0) = lit.value {
                    self.diagnose
                        .push_error(CompileError::divide_by_zero(node.span));
                    typ = TypeKind::Unknown
                }
            }
        }

        TypedExpression {
            kind: TypedExpressionKind::Binary(TypedBinaryExpressionNode {
                left: Box::new(typed_left),
                op: node.op,
                right: Box::new(typed_right),
            }),
            typ,
            span: node.span,
        }
    }

    fn check_expr_unary(&mut self, node: &UnaryExpressionNode) -> TypedExpression {
        let expected_op = match node.operator {
            UnaryOperator::Neg => TypeKind::Int,
            UnaryOperator::Not => TypeKind::Bool,
        };

        let typed_operand = self.check_expr(&node.operand);

        let mut typ = expected_op.clone();

        if typed_operand.typ != expected_op {
            self.diagnose.push_error(CompileError::not_unary_type(
                format!("{:?}", typed_operand.typ),
                typed_operand.span,
            ));
            typ = TypeKind::Unknown;
        }

        TypedExpression {
            kind: TypedExpressionKind::Unary(TypedUnaryExpressionNode {
                operator: node.operator,
                operand: Box::new(typed_operand),
            }),
            typ: typ,
            span: node.span,
        }
    }

    fn check_expr_ident(&mut self, node: &IdentLiteralNode) -> TypedExpression {
        let typ = self.resolve_symbol(&node.value);
        if typ == TypeKind::Unknown {
            self.diagnose
                .push_error(CompileError::unknown_symbol(node.value.clone(), node.span));
        }
        TypedExpression {
            kind: TypedExpressionKind::Ident(node.clone()),
            typ,
            span: node.span,
        }
    }

    fn check_expr_literal(&mut self, node: &LiteralNode) -> TypedExpression {
        let typ = match node.value {
            LiteralValue::Str(_) => TypeKind::Str,
            LiteralValue::Number(_) => TypeKind::Int,
            LiteralValue::Char(_) => TypeKind::Char,
            LiteralValue::Bool(_) => TypeKind::Bool,
        };
        TypedExpression {
            kind: TypedExpressionKind::Literal(LiteralNode {
                value: node.value.clone(),
                span: node.span,
            }),
            typ: typ,
            span: node.span,
        }
    }

    fn check_expr(&mut self, expr: &Expression) -> TypedExpression {
        match expr {
            Expression::Binary(node) => self.check_expr_binary(node),
            Expression::Unary(node) => self.check_expr_unary(node),
            Expression::Literal(node) => self.check_expr_literal(node),
            Expression::Ident(node) => self.check_expr_ident(node),
            Expression::Call(node) => self.check_expr_call(node),
            Expression::Index(node) => self.check_expr_index(node),
            Expression::FieldAccess(node) => self.check_expr_field(node),
            Expression::Broken(span) => self.broken_typed_expr(*span),
        }
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
