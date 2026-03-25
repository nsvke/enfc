#![allow(unused)]

use crate::compile_error::{CompileError, SymbolKind};
use crate::diagnostic::Diagnostics;
use crate::driver::parse::{
    AssignmentNode, BinaryExpressionNode, BinaryOperator, BlockNode, CallNode, Expression,
    FieldAccessNode, FunDefNode, IdentLiteralNode, IfNode, IndexExpressionNode, LiteralNode,
    LiteralValue, ReturnNode, Statement, UnaryExpressionNode, UnaryOperator, VarDecNode, WhileNode,
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
    // Error,
    Function { params: Vec<Type>, ret: Box<Type> },
    Unknown,
}

#[derive(Debug, Clone)]
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
    expected_return_type: Option<TypeKind>,
}

impl<'a> TypeChecker<'a> {
    pub(crate) fn new(diagnose: &'a mut Diagnostics) -> Self {
        Self {
            scopes: vec![HashMap::new()],
            diagnose,
            expected_return_type: None,
        }
    }
    pub(crate) fn check(&mut self, statements: &'a [Statement]) -> Vec<TypedStatement> {
        self.check_and_collect(statements)
    }

    fn check_and_collect(&mut self, statements: &'a [Statement]) -> Vec<TypedStatement> {
        let mut typed_statements = Vec::new();
        self.collect_signatures(statements);
        for stmt in statements {
            typed_statements.push(self.check_stmt(stmt));
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
            "" => TypeKind::Unknown,
            val => {
                self.diagnose
                    .push_error(CompileError::unknown_type(val.into(), typ.span));
                TypeKind::Unknown
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
            terminates: false,
        }
    }

    fn broken_typed_expr(&self, span: Span) -> TypedExpression {
        TypedExpression {
            kind: TypedExpressionKind::Broken,
            typ: TypeKind::Unknown,
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

    fn resolve_symbol_type(&mut self, s: &str) -> TypeKind {
        for scope in self.scopes.iter().rev() {
            if let Some(symbol) = scope.get(s) {
                return symbol.typ.kind.clone();
            }
        }
        TypeKind::Unknown
    }

    fn resolve_symbol(&self, s: &str) -> Option<Symbol> {
        for scope in self.scopes.iter().rev() {
            if let Some(symbol) = scope.get(s) {
                return Some(symbol.clone());
            }
        }
        None
    }

    fn check_stmt_fun(&mut self, node: &FunDefNode) -> TypedStatement {
        if self.expected_return_type.is_some() {
            self.diagnose
                .push_error(CompileError::nested_function(node.name.span));

            return self.broken_typed_stmt(node.span);
        }

        let typ = self.resolve_types(&node.ret_type); // if type could not resolve, this return Unknown
        self.expected_return_type = Some(typ.kind.clone());

        self.enter_scope();

        for param in &node.parameters {
            let symbol = Symbol {
                span: param.0.span,
                mutable: false,
                typ: self.resolve_types(&param.1),
            };

            match self.scopes.last_mut().unwrap().entry(param.0.value.clone()) {
                Entry::Occupied(occupied_entry) => {
                    self.diagnose.push_error(CompileError::symbol_redefination(
                        param.0.value.clone(),
                        occupied_entry.get().span,
                        param.0.span,
                        SymbolKind::Parameter,
                    ))
                }
                Entry::Vacant(entry) => {
                    entry.insert(symbol);
                }
            };
        }

        let typed_body = self.check_stmt_block(&node.body);

        if typ.kind != TypeKind::Nret && typ.kind != TypeKind::Unknown && !typed_body.terminates {
            self.diagnose.push_error(CompileError::missing_return(Span {
                // fun main () { -- fun main () nret {
                //     ^^^^^^^^^        ^^^^^^^^^^^^^^
                start: node.name.span.start,
                end: node.body.span.start,
            }));
        }

        self.exit_scope();

        self.expected_return_type = None;

        TypedStatement {
            kind: TypedStatementKind::FunDefinition(TypedFunDefNode {
                name: node.name.clone(),
                parameters: node.parameters.clone(),
                ret_type: typ,
                body: typed_body.into_block_node(),
            }),
            span: node.span,
            terminates: false,
        }
    }

    fn check_stmt_if(&mut self, node: &IfNode) -> TypedStatement {
        let typed_condition = self.check_expr(&node.condition);

        if typed_condition.typ != TypeKind::Bool && typed_condition.typ != TypeKind::Unknown {
            self.diagnose.push_error(CompileError::unexpected_type(
                TypeKind::Bool,
                typed_condition.typ.clone(),
                typed_condition.span,
            ));
        }

        let typed_then = self.check_stmt_block(&node.then_branch);
        let then_terminates = typed_then.terminates;

        let mut else_if_terminates = true;
        let mut typed_else_ifs = Vec::with_capacity(node.else_if_branches.len());
        for else_if in &node.else_if_branches {
            let typed_elif_cond = self.check_expr(&else_if.condition);
            if typed_elif_cond.typ != TypeKind::Bool && typed_elif_cond.typ != TypeKind::Unknown {
                self.diagnose.push_error(CompileError::unexpected_type(
                    TypeKind::Bool,
                    typed_elif_cond.typ.clone(),
                    typed_elif_cond.span,
                ));
            }
            let typed_elif_then = self.check_stmt_block(&else_if.then_branch);

            else_if_terminates &= typed_elif_then.terminates;

            typed_else_ifs.push(TypedElseIfNode {
                condition: typed_elif_cond,
                then_branch: typed_elif_then.into_block_node(),
            });
        }

        let else_terminates: bool;
        let typed_else = if let Some(els) = &node.else_branch {
            let typed_stmt = self.check_stmt_block(els);
            else_terminates = typed_stmt.terminates;
            Some(typed_stmt.into_block_node())
        } else {
            else_terminates = false;
            None
        };

        TypedStatement {
            kind: TypedStatementKind::If(TypedIfNode {
                condition: typed_condition,
                then_branch: typed_then.into_block_node(),
                else_if_branches: typed_else_ifs,
                else_branch: typed_else,
            }),
            span: node.span,
            terminates: then_terminates && else_if_terminates && else_terminates,
        }
    }

    fn check_stmt_while(&mut self, node: &WhileNode) -> TypedStatement {
        let typed_condition = self.check_expr(&node.condition);

        if typed_condition.typ != TypeKind::Bool && typed_condition.typ != TypeKind::Unknown {
            self.diagnose.push_error(CompileError::unexpected_type(
                TypeKind::Bool,
                typed_condition.typ.clone(),
                typed_condition.span,
            ));
        }

        let typed_body = self.check_stmt_block(&node.body).into_block_node();

        TypedStatement {
            kind: TypedStatementKind::While(TypedWhileNode {
                condition: typed_condition,
                body: typed_body,
            }),
            span: node.span,
            terminates: false,
        }
    }

    fn check_stmt_block(&mut self, node: &BlockNode) -> TypedStatement {
        self.enter_scope();

        let mut body: Vec<TypedStatement> = Vec::with_capacity(node.body.len());

        let mut terminates = false;

        for stmt in &node.body {
            let typed_stmt = self.check_stmt(stmt);

            // TODO add dead code warning
            if typed_stmt.terminates {
                terminates = true;
            }

            body.push(typed_stmt);
        }

        self.exit_scope();

        TypedStatement {
            kind: TypedStatementKind::Block(TypedBlockNode { body }),
            span: node.span,
            terminates,
        }
    }

    fn check_stmt_return(&mut self, node: &ReturnNode) -> TypedStatement {
        let (typed_value, typ) = match &node.value {
            Some(expr) => {
                let typed = self.check_expr(expr);
                let ty = typed.typ.clone();
                (Some(typed), ty)
            }
            None => (None, TypeKind::Nret),
        };

        match &self.expected_return_type {
            Some(expected_typ) => {
                if typ != TypeKind::Unknown && *expected_typ != typ {
                    let value_span = typed_value.as_ref().map(|v| v.span).unwrap_or(node.span);

                    self.diagnose.push_error(CompileError::type_mismatch(
                        expected_typ.clone(),
                        typ,
                        node.span, // TODO add return location state in TypeChecker
                        value_span,
                        "return".into(),
                    ));
                }
            }
            None => {
                self.diagnose
                    .push_error(CompileError::invalid_return_location(node.span));
            }
        }

        TypedStatement {
            kind: TypedStatementKind::Return(TypedReturnNode { value: typed_value }),
            span: node.span,
            terminates: true,
        }
    }

    fn check_stmt_assign(&mut self, node: &AssignmentNode) -> TypedStatement {
        let symbol = self.resolve_symbol(&node.left.value);
        let typed_right = self.check_expr(&node.right);

        if let Some(sym) = symbol {
            if sym.mutable == false {
                self.diagnose.push_error(CompileError::not_mutable(
                    node.left.value.clone(),
                    node.left.span,
                ));
            }
            if typed_right.typ != TypeKind::Unknown {
                if sym.typ.kind != typed_right.typ {
                    let typ = sym.typ.kind.clone();
                    self.diagnose.push_error(CompileError::type_mismatch(
                        typ,
                        typed_right.typ.clone(),
                        node.left.span,
                        typed_right.span,
                        "assign".into(),
                    ));
                }
            }
        } else {
            self.diagnose.push_error(CompileError::unknown_symbol(
                node.left.value.clone(),
                node.left.span,
            ));
        }

        TypedStatement {
            kind: TypedStatementKind::Assignment(TypedAssignmentNode {
                left: node.left.clone(),
                right: typed_right,
            }),
            span: node.span,
            terminates: false,
        }
    }

    fn check_stmt_val(&mut self, node: &VarDecNode) -> TypedStatement {
        let typed_initalizer = self.check_expr(&node.initalizer);

        let mut typ: Type;

        let old_span = self
            .scopes
            .last()
            .unwrap()
            .get(&node.name.value)
            .map(|sym| sym.span);

        if let Some(span) = old_span {
            self.diagnose.push_error(CompileError::symbol_redefination(
                node.name.value.clone(),
                span,
                node.name.span,
                SymbolKind::Variable,
            ));
            typ = Type {
                kind: TypeKind::Unknown,
                span: node.var_type.span,
            }
        } else {
            typ = self.resolve_types(&node.var_type);

            if typ.kind != TypeKind::Unknown {
                if typ.kind != typed_initalizer.typ {
                    self.diagnose.push_error(CompileError::type_mismatch(
                        typ.kind.clone(),
                        typed_initalizer.typ.clone(),
                        Span {
                            // val x int = 12;
                            //     ^^^^^
                            start: node.name.span.start,
                            end: typ.span.end,
                        },
                        typed_initalizer.span,
                        "assign".into(),
                    ));
                }
            } else {
                typ = Type {
                    span: node.name.span,
                    kind: typed_initalizer.typ.clone(),
                }
            }

            self.scopes.last_mut().unwrap().insert(
                node.name.value.clone(),
                Symbol {
                    typ: typ.clone(),
                    span: node.span,
                    mutable: node.mutable,
                },
            );
        }

        TypedStatement {
            kind: TypedStatementKind::VarDeclaration(TypedVarDecNode {
                name: node.name.clone(),
                var_type: typ,
                initalizer: typed_initalizer,
                mutable: node.mutable,
            }),
            span: node.span,
            terminates: false,
        }
    }

    fn check_stmt(&mut self, stmt: &Statement) -> TypedStatement {
        match stmt {
            Statement::VarDeclaration(node) => self.check_stmt_val(node),
            Statement::Assignment(node) => self.check_stmt_assign(node),
            Statement::If(node) => self.check_stmt_if(node),
            Statement::While(node) => self.check_stmt_while(node),
            Statement::FunDefinition(node) => self.check_stmt_fun(node),
            Statement::Block(node) => self.check_stmt_block(node),
            Statement::Expression(expr) => {
                let typed_expr = self.check_expr(expr);
                TypedStatement {
                    span: typed_expr.span,
                    kind: TypedStatementKind::Expression(typed_expr),
                    terminates: false,
                }
            }
            Statement::Return(node) => self.check_stmt_return(node),
            Statement::Broken(span) => self.broken_typed_stmt(*span),
        }
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
        let typ = self.resolve_symbol_type(&node.value);
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
impl TypedStatement {
    fn into_block_node(self) -> TypedBlockNode {
        match self.kind {
            TypedStatementKind::Block(node) => node,
            _ => unreachable!("this method for only block statements!"),
        }
    }
}

#[derive(Debug)]
pub(crate) struct TypedStatement {
    kind: TypedStatementKind,
    span: Span,
    terminates: bool,
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
    value: Option<TypedExpression>,
}
