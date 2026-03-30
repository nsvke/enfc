#![allow(unused)]

use crate::compile_error::{CompileError, MismatchKind, SymbolKind};
use crate::diagnostic::Diagnostics;
use crate::driver::parse::{
    AddressOfNode, AssignmentNode, BinaryExpressionNode, BinaryOperator, BlockNode, CallNode,
    DerefNode, Expression, ExternBlockNode, FieldAccessNode, FunDefNode, IdentLiteralNode, IfNode,
    IndexExpressionNode, LiteralNode, LiteralValue, ReturnNode, Statement, TypeNode,
    UnaryExpressionNode, UnaryOperator, VarDecNode, WhileNode,
};
use crate::structs::Span;
use phf::{Map, phf_map};
use std::collections::{HashMap, hash_map::Entry};
use std::fmt::{self, Debug};

static RESERVED: Map<&'static str, TypeKind> = phf_map! {
    "true" => TypeKind::Bool,
    "false" => TypeKind::Bool,
    "nret" => TypeKind::Unknown,
    "int" => TypeKind::Unknown,
    "str" => TypeKind::Unknown,
    "bool" => TypeKind::Unknown,
    "chr" => TypeKind::Unknown,
};

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
    Function {
        params: Vec<Type>,
        ret: Box<Type>,
        is_extern: bool,
        is_variadic: bool,
    },
    Reference(Box<TypeKind>),
    Unknown,
}

impl std::fmt::Display for TypeKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int => write!(f, "int"),
            Self::Str => write!(f, "str"),
            Self::Bool => write!(f, "bool"),
            Self::Char => write!(f, "chr"),
            Self::Nret => write!(f, "nret"),
            Self::Function { .. } => write!(f, "function"),
            Self::Unknown => write!(f, "unknown"),
            Self::Reference(inner) => {
                let inn = format!("{}", inner);
                write!(f, "&{}", inn)
            }
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Symbol {
    pub typ: Type,
    pub span: Span,
    pub mutable: bool,
    pub id: usize,
}

pub(crate) struct TypeChecker<'a> {
    // statements: &'a [Statement],
    scopes: Vec<HashMap<String, Symbol>>,
    diagnose: &'a mut Diagnostics,
    // destruct: bool,
    expected_return: Option<(TypeKind, Span)>,
    next_ident_id: usize,
    main_loc: Option<Span>,
}

impl<'a> TypeChecker<'a> {
    pub(crate) fn new(diagnose: &'a mut Diagnostics) -> Self {
        let mut new_type_checker = Self {
            scopes: vec![HashMap::new()],
            diagnose,
            expected_return: None,
            next_ident_id: 1,
            main_loc: None,
        };
        // new_type_checker.init();
        new_type_checker
    }

    // fn init(&mut self) {}

    pub(crate) fn check(mut self, statements: &'a [Statement]) -> Vec<TypedStatement> {
        self.check_and_collect(statements)
    }

    fn check_and_collect(&mut self, statements: &'a [Statement]) -> Vec<TypedStatement> {
        let mut typed_statements = Vec::new();
        self.collect_signatures(statements);
        self.check_main_sign();
        for stmt in statements {
            typed_statements.push(self.check_stmt(stmt));
        }
        let eof_span = match typed_statements.last() {
            Some(stmt) => Span {
                start: stmt.span.end,
                end: stmt.span.end,
            },
            None => Span::default(),
        };

        if let None = self.main_loc {
            self.diagnose
                .push_error(CompileError::not_found_main(eof_span));
        }
        typed_statements
    }

    fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }
    fn exit_scope(&mut self) {
        self.scopes.pop();
    }

    fn resolve_types(&mut self, typ: &TypeNode) -> Type {
        let kind = match typ {
            TypeNode::Named(ident_node) => match ident_node.value.as_str() {
                "int" => TypeKind::Int,
                "str" => TypeKind::Str,
                "bool" => TypeKind::Bool,
                "chr" => TypeKind::Char,
                "nret" => TypeKind::Nret,
                "" => TypeKind::Unknown,
                val => {
                    self.diagnose
                        .push_error(CompileError::unknown_type(val.into(), *typ.span()));
                    TypeKind::Unknown
                }
            },
            TypeNode::Reference { inner, .. } => {
                let inner_type = self.resolve_types(&inner);
                TypeKind::Reference(Box::new(inner_type.kind))
            }
        };

        Type {
            span: *typ.span(),
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
            let id = self.give_ident_id();
            match stmt {
                Statement::FunDefinition(node) => {
                    if node.name.value == "main" {
                        self.main_loc = Some(Span {
                            start: node.name.span.start,
                            end: node.ret_type.span().end,
                        });
                    }
                    let is_extern = node.is_extern;
                    // TODO check redefinition first, resolve types later
                    let params = node
                        .parameters
                        .iter()
                        .map(|(_, typ)| self.resolve_types(typ))
                        .collect();
                    let ret_typ = self.resolve_types(&node.ret_type);
                    match self.scopes[0].entry(node.name.value.clone()) {
                        Entry::Occupied(occupied_entry) => {
                            self.diagnose.push_error(CompileError::symbol_redefinition(
                                node.name.value.clone(),
                                occupied_entry.get().span, // TODO only function signature's span
                                node.name.span,
                                SymbolKind::Function,
                            ))
                        }
                        Entry::Vacant(entry) => {
                            entry.insert(Symbol {
                                typ: Type {
                                    span: node.span,
                                    kind: TypeKind::Function {
                                        params,
                                        ret: Box::new(ret_typ),
                                        is_extern,
                                        is_variadic: false,
                                    },
                                },
                                span: node.span,
                                mutable: false,
                                id,
                            });
                        }
                    };
                }
                Statement::ExternBlock(node) => {
                    for sign in &node.signatures {
                        // TODO check redefinition first, resolve types later
                        let params = sign
                            .params
                            .iter()
                            .map(|(_, typ)| self.resolve_types(typ))
                            .collect();
                        let ret_typ = self.resolve_types(&sign.ret_type);
                        match self.scopes[0].entry(sign.name.value.clone()) {
                            Entry::Occupied(occupied_entry) => {
                                self.diagnose.push_error(CompileError::symbol_redefinition(
                                    sign.name.value.clone(),
                                    occupied_entry.get().span, // TODO only function signature's span
                                    sign.name.span,
                                    SymbolKind::Function,
                                ))
                            }
                            Entry::Vacant(entry) => {
                                entry.insert(Symbol {
                                    typ: Type {
                                        span: node.span,
                                        kind: TypeKind::Function {
                                            params,
                                            ret: Box::new(ret_typ),
                                            is_extern: true,
                                            is_variadic: sign.is_variadic,
                                        },
                                    },
                                    span: node.span,
                                    mutable: false,
                                    id,
                                });
                            }
                        };
                    }
                }
                _ => {}
            };
        }
    }

    fn check_main_sign(&mut self) {
        if let Some(loc) = self.main_loc {
            for func in &self.scopes[0] {
                if func.0 == "main" {
                    match &func.1.typ.kind {
                        TypeKind::Function {
                            params,
                            ret,
                            is_extern,
                            is_variadic: false,
                        } => {
                            if params.len() != 0 || ret.kind != TypeKind::Nret || *is_extern {
                                self.diagnose.push_error(CompileError::wrong_main(loc));
                            }
                        }
                        _ => unreachable!("only functions"),
                    }
                }
            }
        }
    }

    fn give_ident_id(&mut self) -> usize {
        let next_id = self.next_ident_id;
        self.next_ident_id += 1;
        next_id
    }

    fn make_ident_id(&mut self, node: &IdentLiteralNode) -> IdentLiteralIdNode {
        IdentLiteralIdNode {
            value_id: self.give_ident_id(),
            span: node.span,
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

    fn resolve_symbol_type_with_id(&mut self, s: &str) -> (TypeKind, usize) {
        for scope in self.scopes.iter().rev() {
            if let Some(symbol) = scope.get(s) {
                return (symbol.typ.kind.clone(), symbol.id);
            }
        }
        (TypeKind::Unknown, 0)
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
        if self.expected_return.is_some() {
            self.diagnose
                .push_error(CompileError::nested_function(Span {
                    start: node.name.span.start,
                    end: node.ret_type.span().end,
                }));

            return self.broken_typed_stmt(node.span);
        }

        let typ = self.resolve_types(&node.ret_type); // if type could not resolve, this return Unknown
        self.expected_return = Some((
            typ.kind.clone(),
            Span {
                // fun main () nret {
                //     ^^^^^^^^^^^^
                start: node.name.span.start,
                end: node.ret_type.span().end,
            },
        ));

        self.enter_scope();

        let mut params_w_id = Vec::new();
        for param in &node.parameters {
            if let Some(reserved) = RESERVED.get(&param.0.value) {
                self.diagnose.push_error(CompileError::symbol_redefinition(
                    param.0.value.clone(),
                    Span::default(),
                    param.0.span,
                    SymbolKind::Builtin,
                ));
                continue;
            }
            let with_id = self.make_ident_id(&param.0);
            let symbol = Symbol {
                span: param.0.span,
                mutable: false,
                typ: self.resolve_types(&param.1),
                id: with_id.value_id,
            };
            match self.scopes.last_mut().unwrap().entry(param.0.value.clone()) {
                Entry::Occupied(occupied_entry) => {
                    self.diagnose.push_error(CompileError::symbol_redefinition(
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
            params_w_id.push((with_id, self.resolve_types(&param.1)));
        }
        let typed_body = if !node.is_extern {
            let typed_body_stmt = self.check_stmt_block(&node.body);
            if typ.kind != TypeKind::Nret
                && typ.kind != TypeKind::Unknown
                && !typed_body_stmt.terminates
            {
                self.diagnose.push_error(CompileError::missing_return(Span {
                    // fun main () nret {
                    //     ^^^^^^^^^^^^
                    start: node.name.span.start,
                    end: node.ret_type.span().end,
                }));
            }
            typed_body_stmt.into_block_node()
        } else {
            TypedBlockNode { body: Vec::new() }
        };

        self.exit_scope();

        self.expected_return = None;

        TypedStatement {
            kind: TypedStatementKind::FunDefinition(TypedFunDefNode {
                name: node.name.clone(),
                parameters: params_w_id,
                ret_type: typ,
                body: typed_body,
                is_extern: node.is_extern,
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

    fn check_stmt_ret(&mut self, node: &ReturnNode) -> TypedStatement {
        let (typed_value, typ) = match &node.value {
            Some(expr) => {
                let typed = self.check_expr(expr);
                let ty = typed.typ.clone();
                (Some(typed), ty)
            }
            None => (None, TypeKind::Nret),
        };

        match &self.expected_return {
            Some((expected_typ, loc_span)) => {
                if typ != TypeKind::Unknown && *expected_typ != typ {
                    let value_span = typed_value.as_ref().map(|v| v.span).unwrap_or(node.span);

                    self.diagnose.push_error(CompileError::type_mismatch(
                        expected_typ.clone(),
                        typ,
                        *loc_span,
                        value_span,
                        "ret".into(),
                        MismatchKind::Return,
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

    fn check_stmt_asgn(&mut self, node: &AssignmentNode) -> TypedStatement {
        let typed_right = self.check_expr(&node.right);
        let typed_left = self.check_expr(&node.left);

        match &typed_left.kind {
            TypedExpressionKind::Ident(ident_node) => {
                if let Some(sym) = self.resolve_symbol(&ident_node.val) {
                    if sym.mutable == false {
                        self.diagnose.push_error(CompileError::not_mutable(
                            ident_node.val.clone(),
                            typed_left.span,
                        ));
                    }
                }
            }
            TypedExpressionKind::Deref(node) => {}
            _ => self
                .diagnose
                .push_error(CompileError::invalid_l_value(typed_left.span)),
        };
        if typed_right.typ != TypeKind::Unknown && typed_left.typ != TypeKind::Unknown {
            if typed_left.typ != typed_right.typ {
                self.diagnose.push_error(CompileError::type_mismatch(
                    typed_left.typ.clone(),
                    typed_right.typ.clone(),
                    typed_left.span,
                    typed_right.span,
                    "=".into(),
                    MismatchKind::Regular,
                ));
            }
        }
        TypedStatement {
            kind: TypedStatementKind::Assignment(TypedAssignmentNode {
                left: typed_left,
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

        let mut id = 0;
        if let Some(reserved) = RESERVED.get(&node.name.value) {
            self.diagnose.push_error(CompileError::symbol_redefinition(
                node.name.value.clone(),
                Span::default(),
                node.name.span,
                SymbolKind::Builtin,
            ));
            typ = Type {
                kind: TypeKind::Unknown,
                span: *node.var_type.span(),
            }
        } else if let Some(span) = old_span {
            self.diagnose.push_error(CompileError::symbol_redefinition(
                node.name.value.clone(),
                span,
                node.name.span,
                SymbolKind::Variable,
            ));
            typ = Type {
                kind: TypeKind::Unknown,
                span: *node.var_type.span(),
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
                        "=".into(),
                        MismatchKind::Regular,
                    ));
                }
            } else {
                typ = Type {
                    span: node.name.span,
                    kind: typed_initalizer.typ.clone(),
                }
            }

            id = self.give_ident_id();
            self.scopes.last_mut().unwrap().insert(
                node.name.value.clone(),
                Symbol {
                    typ: typ.clone(),
                    span: node.span,
                    mutable: node.mutable,
                    id,
                },
            );
        }
        TypedStatement {
            kind: TypedStatementKind::VarDeclaration(TypedVarDecNode {
                name_id: IdentLiteralIdNode {
                    value_id: id,
                    span: node.span,
                },
                var_type: typ,
                initalizer: typed_initalizer,
                mutable: node.mutable,
            }),
            span: node.span,
            terminates: false,
        }
    }

    fn check_stmt_extern(&mut self, node: &ExternBlockNode) -> TypedStatement {
        let mut signatures = Vec::new();
        for sign in &node.signatures {
            let name = IdentLiteralTuple {
                val: sign.name.value.clone(),
                id: self.give_ident_id(),
            };

            let ret_type = self.resolve_types(&sign.ret_type);

            let mut params = Vec::new();
            for param in &sign.params {
                let param_name = IdentLiteralTuple {
                    val: param.0.value.clone(),
                    id: self.give_ident_id(),
                };
                let typ = self.resolve_types(&param.1);
                params.push((param_name, typ));
            }

            signatures.push(TypedExternSignatureNode {
                name,
                params,
                is_variadic: sign.is_variadic,
                ret_type,
            });
        }

        TypedStatement {
            kind: TypedStatementKind::ExternBlock(TypedExternBlockNode {
                header: node.header.clone(),
                signatures,
            }),
            span: node.span,
            terminates: false,
        }
    }

    fn check_stmt(&mut self, stmt: &Statement) -> TypedStatement {
        match stmt {
            Statement::VarDeclaration(node) => self.check_stmt_val(node),
            Statement::Assignment(node) => self.check_stmt_asgn(node),
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
            Statement::ExpressionStatement(expr) => {
                let typed_expr = self.check_expr(expr);
                TypedStatement {
                    span: typed_expr.span,
                    kind: TypedStatementKind::ExpressionStatement(typed_expr),
                    terminates: false,
                }
            }
            Statement::ExternBlock(node) => self.check_stmt_extern(node),
            Statement::Return(node) => self.check_stmt_ret(node),
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

        let mut is_extern_1 = false;
        if typed_primary.typ != TypeKind::Unknown {
            match &typed_primary.typ {
                TypeKind::Function {
                    params,
                    ret,
                    is_extern,
                    is_variadic,
                } => {
                    is_extern_1 = *is_extern;

                    if *is_variadic {
                        if node.arguments.len() < params.len() {
                            self.diagnose.push_error(CompileError::missing_argument(
                                params.len(),
                                node.arguments.len(),
                                typed_primary.span,
                            ));
                        }
                    } else {
                        if node.arguments.len() != params.len() {
                            self.diagnose.push_error(CompileError::missing_argument(
                                params.len(),
                                node.arguments.len(),
                                typed_primary.span,
                            ));
                        }
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
                is_extern: is_extern_1,
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
                        MismatchKind::Binary,
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
                        MismatchKind::Binary,
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
                match node.operator {
                    UnaryOperator::Neg => '-',
                    UnaryOperator::Not => '!',
                },
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
        let (typ, id) = self.resolve_symbol_type_with_id(&node.value);
        if typ == TypeKind::Unknown {
            self.diagnose
                .push_error(CompileError::unknown_symbol(node.value.clone(), node.span));
        }
        TypedExpression {
            kind: TypedExpressionKind::Ident(IdentLiteralTuple {
                val: node.value.clone(),
                id,
            }),
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

    fn check_expr_addressof(&mut self, node: &AddressOfNode) -> TypedExpression {
        let typed_inner = self.check_expr(&node.inner);
        if let TypedExpressionKind::Ident(ident) = &typed_inner.kind {
            if let Some(sym) = self.resolve_symbol(&ident.val) {
                if !sym.mutable {
                    self.diagnose
                        .push_error(CompileError::not_mutable(ident.val.clone(), node.span));
                }
            }
        } else {
            self.diagnose
                .push_error(CompileError::cannot_addressable(typed_inner.span));
        }

        TypedExpression {
            typ: TypeKind::Reference(Box::new(typed_inner.typ.clone())),
            kind: TypedExpressionKind::AddressOf(TypedAddressOfNode {
                inner: Box::new(typed_inner),
            }),
            span: node.span,
        }
    }

    fn check_expr_deref(&mut self, node: &DerefNode) -> TypedExpression {
        let typed_inner = self.check_expr(&node.inner);
        let resolved = match &typed_inner.typ {
            TypeKind::Reference(inner) => *inner.clone(),
            TypeKind::Unknown => TypeKind::Unknown,
            typ => {
                self.diagnose
                    .push_error(CompileError::cannot_deref(typ.clone(), node.span));
                TypeKind::Unknown
            }
        };
        TypedExpression {
            kind: TypedExpressionKind::Deref(TypedDerefNode {
                inner: Box::new(typed_inner),
            }),
            typ: resolved,
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
            Expression::AddressOf(node) => self.check_expr_addressof(node),
            Expression::Deref(node) => self.check_expr_deref(node),
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
    pub kind: TypedStatementKind,
    pub span: Span,
    pub terminates: bool,
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
    ExpressionStatement(TypedExpression),
    ExternBlock(TypedExternBlockNode),
    Return(TypedReturnNode),
    Broken,
}

#[derive(Debug)]
pub(crate) struct TypedBlockNode {
    pub body: Vec<TypedStatement>,
}

#[derive(Debug)]
pub(crate) struct TypedFunDefNode {
    pub name: IdentLiteralNode,
    pub parameters: Vec<(IdentLiteralIdNode, Type)>,
    pub ret_type: Type,
    pub body: TypedBlockNode,
    pub is_extern: bool,
}
#[derive(Debug)]
pub(crate) struct TypedIfNode {
    pub condition: TypedExpression,
    pub then_branch: TypedBlockNode,
    pub else_if_branches: Vec<TypedElseIfNode>,
    pub else_branch: Option<TypedBlockNode>,
}
#[derive(Debug)]
pub(crate) struct TypedElseIfNode {
    pub condition: TypedExpression,
    pub then_branch: TypedBlockNode,
}
#[derive(Debug)]
pub(crate) struct TypedWhileNode {
    pub condition: TypedExpression,
    pub body: TypedBlockNode,
}
#[derive(Debug)]
pub(crate) struct TypedVarDecNode {
    pub name_id: IdentLiteralIdNode,
    pub var_type: Type,
    pub initalizer: TypedExpression,
    pub mutable: bool,
}
#[derive(Debug)]
pub(crate) struct TypedAssignmentNode {
    pub left: TypedExpression,
    pub right: TypedExpression,
}

#[derive(Debug)]
pub(crate) struct TypedExpression {
    pub kind: TypedExpressionKind,
    pub typ: TypeKind,
    pub span: Span,
}

#[derive(Debug)]
pub(crate) enum TypedExpressionKind {
    Binary(TypedBinaryExpressionNode),
    Unary(TypedUnaryExpressionNode),
    Literal(LiteralNode),
    Ident(IdentLiteralTuple),
    Call(TypedCallNode),
    AddressOf(TypedAddressOfNode),
    Deref(TypedDerefNode),
    Index(TypedIndexExpressionNode),
    FieldAccess(TypedFieldAccessNode),
    Broken,
}

#[derive(Debug)]
pub(crate) struct TypedIndexExpressionNode {
    pub target: Box<TypedExpression>,
    pub index: Box<TypedExpression>,
}
#[derive(Debug)]
pub(crate) struct TypedFieldAccessNode {
    pub target: Box<TypedExpression>,
    pub field: IdentLiteralNode,
}
#[derive(Debug)]
pub(crate) struct TypedBinaryExpressionNode {
    pub left: Box<TypedExpression>,
    pub op: BinaryOperator,
    pub right: Box<TypedExpression>,
}
#[derive(Debug)]
pub(crate) struct TypedUnaryExpressionNode {
    pub operator: UnaryOperator,
    pub operand: Box<TypedExpression>,
}

#[derive(Debug)]
pub(crate) struct TypedCallNode {
    pub primary: Box<TypedExpression>,
    pub arguments: Vec<TypedExpression>,
    pub is_extern: bool,
}

#[derive(Debug)]
pub(crate) struct TypedAddressOfNode {
    pub inner: Box<TypedExpression>,
}

#[derive(Debug)]
pub(crate) struct TypedDerefNode {
    pub inner: Box<TypedExpression>,
}
#[derive(Debug)]
pub(crate) struct TypedReturnNode {
    pub value: Option<TypedExpression>,
}

#[derive(Debug)]
pub(crate) struct IdentLiteralIdNode {
    pub value_id: usize,
    pub span: Span,
}

#[derive(Debug)]
pub(crate) struct IdentLiteralTuple {
    pub val: String,
    pub id: usize,
}

#[derive(Debug)]
pub(crate) struct TypedExternBlockNode {
    pub header: String,
    pub signatures: Vec<TypedExternSignatureNode>,
}

#[derive(Debug)]
pub(crate) struct TypedExternSignatureNode {
    pub name: IdentLiteralTuple,
    pub params: Vec<(IdentLiteralTuple, Type)>,
    pub is_variadic: bool,
    pub ret_type: Type,
}
