use crate::driver::{
    parse::{IdentLiteralNode, LiteralNode},
    typecheck::{
        TypedAssignmentNode, TypedBinaryExpressionNode, TypedBlockNode, TypedCallNode,
        TypedExpression, TypedExpressionKind, TypedFieldAccessNode, TypedFunDefNode, TypedIfNode,
        TypedIndexExpressionNode, TypedReturnNode, TypedStatement, TypedStatementKind,
        TypedUnaryExpressionNode, TypedVarDecNode, TypedWhileNode,
    },
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Instruction {
    PushInt(i32),
    PushBool(bool),
    Load(String),
    Store(String),

    Add,
    Sub,
    Mul,
    Div,
    Mod,

    Eq,
    Lt,
    Gt,

    Neg,
    Not,

    BitAnd,
    BitOr,
    BitXor,

    Jump(usize),
    JumpIfFalse(usize),

    Return,
}

pub(crate) struct IrGenerator {
    instructions: Vec<Instruction>,
}

impl IrGenerator {
    pub fn new() -> Self {
        Self {
            instructions: Vec::new(),
        }
    }

    pub fn generate_from(mut self, tasts: &[TypedStatement]) -> Vec<Instruction> {
        for tast in tasts {
            self.gen_from_stmt(tast);
        }
        self.instructions
    }

    fn _current_index(&self) -> usize {
        self.instructions.len()
    }

    fn gen_from_stmt_if(&mut self, node: &TypedIfNode) {}
    fn gen_from_stmt_while(&mut self, node: &TypedWhileNode) {}
    fn gen_from_stmt_fun(&mut self, node: &TypedFunDefNode) {}
    fn gen_from_stmt_block(&mut self, node: &TypedBlockNode) {}
    fn gen_from_stmt_return(&mut self, node: &TypedReturnNode) {}
    fn gen_from_stmt_assign(&mut self, node: &TypedAssignmentNode) {}
    fn gen_from_stmt_val(&mut self, node: &TypedVarDecNode) {}

    fn gen_from_stmt(&mut self, stmt: &TypedStatement) {
        match &stmt.kind {
            TypedStatementKind::VarDeclaration(node) => self.gen_from_stmt_val(node),
            TypedStatementKind::Assignment(node) => self.gen_from_stmt_assign(node),
            TypedStatementKind::If(node) => self.gen_from_stmt_if(node),
            TypedStatementKind::While(node) => self.gen_from_stmt_while(node),
            TypedStatementKind::FunDefinition(node) => self.gen_from_stmt_fun(node),
            TypedStatementKind::Block(node) => self.gen_from_stmt_block(node),
            TypedStatementKind::Expression(expr) => self.gen_from_expr(expr),
            TypedStatementKind::Return(node) => self.gen_from_stmt_return(node),
            TypedStatementKind::Broken => {
                unreachable!("hey typechecker, what the hell is this doing here?")
            }
        };
    }

    fn gen_from_expr_field(&mut self, node: &TypedFieldAccessNode) {}
    fn gen_from_expr_index(&mut self, node: &TypedIndexExpressionNode) {}
    fn gen_from_expr_call(&mut self, node: &TypedCallNode) {}
    fn gen_from_expr_ident(&mut self, node: &IdentLiteralNode) {}
    fn gen_from_expr_literal(&mut self, node: &LiteralNode) {}
    fn gen_from_expr_unary(&mut self, node: &TypedUnaryExpressionNode) {}
    fn gen_from_expr_binary(&mut self, node: &TypedBinaryExpressionNode) {}

    fn gen_from_expr(&mut self, expr: &TypedExpression) {
        match &expr.kind {
            TypedExpressionKind::Binary(node) => self.gen_from_expr_binary(node),
            TypedExpressionKind::Unary(node) => self.gen_from_expr_unary(node),
            TypedExpressionKind::Literal(node) => self.gen_from_expr_literal(node),
            TypedExpressionKind::Ident(node) => self.gen_from_expr_ident(node),
            TypedExpressionKind::Call(node) => self.gen_from_expr_call(node),
            TypedExpressionKind::Index(node) => self.gen_from_expr_index(node),
            TypedExpressionKind::FieldAccess(node) => self.gen_from_expr_field(node),
            TypedExpressionKind::Broken => {
                unreachable!("hey typechecker, what the hell is this doing here?")
            }
        }
    }
}
