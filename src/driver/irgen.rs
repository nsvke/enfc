use crate::driver::{
    parse::{BinaryOperator, IdentLiteralNode, LiteralNode, LiteralValue, UnaryOperator},
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
    PushChar(char),
    PushStrId(usize),

    Load(String),
    Store(String),

    // Binary
    Add,
    Sub,
    Mul,
    Div,
    Mod,

    // Comparison
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

    Call(usize, usize),

    Return,
}

pub(crate) struct IrGenerator {
    instructions: Vec<Instruction>,
    str_pool: Vec<String>,
    fun_pool: Vec<String>,
}

impl IrGenerator {
    pub fn new() -> Self {
        Self {
            instructions: Vec::new(),
            str_pool: Vec::new(),
            fun_pool: Vec::new(),
        }
    }

    pub fn generate_from(mut self, tasts: &[TypedStatement]) -> Vec<Instruction> {
        for tast in tasts {
            self.gen_from_stmt(tast);
        }
        self.instructions
    }

    fn emit(&mut self, instruction: Instruction) {
        self.instructions.push(instruction);
    }

    fn emit_with_index(&mut self, instruction: Instruction) -> usize {
        self.emit(instruction);
        self.last_instr_index()
    }

    fn add_str(&mut self, s: String) -> usize {
        self.str_pool.push(s);
        self.str_pool.len() - 1
    }

    fn add_fun(&mut self, s: String) -> usize {
        self.fun_pool.push(s);
        self.fun_pool.len() - 1
    }

    fn current_index(&self) -> usize {
        self.instructions.len()
    }

    fn last_instr_index(&self) -> usize {
        self.current_index() - 1
    }

    // fn edit_instr(&mut self, index: usize, instruction: Instruction) {
    //     self.instructions[index] = instruction;
    // }

    fn add_jmp_index_here(&mut self, instr_index: usize) {
        let curr = self.current_index();
        if let Some(instr) = self.instructions.get_mut(instr_index) {
            match instr {
                Instruction::Jump(index) => {
                    *index = curr;
                }
                Instruction::JumpIfFalse(index) => {
                    *index = curr;
                }
                _ => unreachable!("use for only jumpers"),
            }
        } else {
            unreachable!("ir: instruction vector bound error")
        }
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

    fn gen_from_expr_field(&mut self, _node: &TypedFieldAccessNode) {
        unimplemented!("ir: field access not supported yet!");
    }

    fn gen_from_expr_index(&mut self, _node: &TypedIndexExpressionNode) {
        unimplemented!("ir: index access not supported yet!")
    }

    fn gen_from_expr_call(&mut self, node: &TypedCallNode) {
        for arg in &node.arguments {
            self.gen_from_expr(arg);
        }

        let fun_name = if let TypedExpressionKind::Ident(node) = &node.primary.kind {
            node.value.clone()
        } else {
            unimplemented!()
        };

        let id = self.add_fun(fun_name);
        self.emit(Instruction::Call(id, node.arguments.len()));
    }

    fn gen_from_expr_unary(&mut self, node: &TypedUnaryExpressionNode) {
        self.gen_from_expr(&node.operand);

        match node.operator {
            UnaryOperator::Neg => self.emit(Instruction::Neg),
            UnaryOperator::Not => self.emit(Instruction::Not),
        }
    }

    fn gen_from_expr_binary(&mut self, node: &TypedBinaryExpressionNode) {
        if matches!(node.op, BinaryOperator::And | BinaryOperator::Or) {
            match node.op {
                BinaryOperator::And => {
                    self.gen_from_expr(&node.left);
                    let jmp_fls_instr_index = self.emit_with_index(Instruction::JumpIfFalse(0));
                    self.gen_from_expr(&node.right);
                    let jmp_instr_index = self.emit_with_index(Instruction::Jump(0));
                    self.add_jmp_index_here(jmp_fls_instr_index);
                    self.emit(Instruction::PushBool(false));
                    self.add_jmp_index_here(jmp_instr_index);
                }
                BinaryOperator::Or => {
                    self.gen_from_expr(&node.left);
                    let jmp_fls_instr_index = self.emit_with_index(Instruction::JumpIfFalse(0));
                    self.emit(Instruction::PushBool(true));
                    let jmp_instr_index = self.emit_with_index(Instruction::Jump(0));
                    self.add_jmp_index_here(jmp_fls_instr_index);
                    self.gen_from_expr(&node.right);
                    self.add_jmp_index_here(jmp_instr_index);
                }
                _ => unreachable!("only for and, or"),
            }
            return;
        }

        self.gen_from_expr(&node.left);
        self.gen_from_expr(&node.right);

        match node.op {
            BinaryOperator::Or => {
                unreachable!("applied shorcut");
            }
            BinaryOperator::And => {
                unreachable!("applied shortcut");
            }
            // Equality
            BinaryOperator::IsEquals => self.emit(Instruction::Eq),
            BinaryOperator::IsNotEquals => {
                self.emit(Instruction::Eq);
                self.emit(Instruction::Not);
            }
            // Comparison
            BinaryOperator::Less => self.emit(Instruction::Lt),
            BinaryOperator::LessEquals => {
                self.emit(Instruction::Gt);
                self.emit(Instruction::Not);
            }
            BinaryOperator::Greater => self.emit(Instruction::Gt),
            BinaryOperator::GreaterEquals => {
                self.emit(Instruction::Lt);
                self.emit(Instruction::Not);
            }
            // Term
            BinaryOperator::Add => self.emit(Instruction::Add),
            BinaryOperator::Sub => self.emit(Instruction::Sub),
            // Factor
            BinaryOperator::Mul => self.emit(Instruction::Mul),
            BinaryOperator::Div => self.emit(Instruction::Div),
            BinaryOperator::Percent => self.emit(Instruction::Mod),
        }
    }

    fn gen_from_expr_ident(&mut self, node: &IdentLiteralNode) {
        self.emit(Instruction::Load(node.value.clone()));
    }

    fn gen_from_expr_literal(&mut self, node: &LiteralNode) {
        match &node.value {
            LiteralValue::Number(v) => self.emit(Instruction::PushInt(*v)),
            LiteralValue::Str(v) => {
                let id = self.add_str(v.clone());
                self.emit(Instruction::PushStrId(id));
            }
            LiteralValue::Bool(v) => self.emit(Instruction::PushBool(*v)),
            LiteralValue::Char(v) => self.emit(Instruction::PushChar(*v)),
        };
    }

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
