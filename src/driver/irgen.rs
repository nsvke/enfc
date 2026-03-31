use std::collections::HashMap;

use crate::driver::{
    TypeKind,
    parse::{BinaryOperator, InjectNode, LiteralNode, LiteralValue, UnaryOperator},
    typecheck::{
        IdentLiteralTuple, TypedAddressOfNode, TypedArrayLiteralNode, TypedAssignmentNode,
        TypedBinaryExpressionNode, TypedBlockNode, TypedCallNode, TypedDataDecNode,
        TypedDataInitNode, TypedDerefNode, TypedExpression, TypedExpressionKind,
        TypedExternBlockNode, TypedFieldAccessNode, TypedFunDefNode, TypedIfNode,
        TypedIndexExpressionNode, TypedReturnNode, TypedStatement, TypedStatementKind,
        TypedUnaryExpressionNode, TypedVarDecNode, TypedWhileNode,
    },
};

#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    PushInt(i32),
    PushFloat(f32),
    PushBool(bool),
    PushChar(char),
    PushStrId(usize),
    PushValueId(usize),
    PushNull,
    PushZeroInit,

    Load,
    Store,
    Init(usize, IrType),
    AddressOf(usize),
    LoadIndirect,
    StoreIndirect,

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
    Tilde,

    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    BitwiseLeftShift,
    BitwiseRightShift,

    Jump(usize),
    JumpIfFalse(usize),

    Call(usize, usize),
    ExternCall(usize, usize),

    ExternFunStart(usize, IrType),
    ExternFunParam(usize, IrType),
    ExternFunEnd,

    FunStart(usize, IrType),
    FunParam(usize, IrType),
    FunBodyStart,
    FunEnd,

    DataStart(usize),
    DataField(usize, IrType),
    DataEnd,

    DataInit(usize, usize),

    DataFieldAccess(usize),

    ArrayStart,
    ArrayElem,
    ArrayEnd,

    IndexAccess,

    Discard,

    Ret(bool),
}

pub(crate) struct IrGenerator {
    instructions: Vec<Instruction>,
    str_pool: Pool,
    fun_pool: Pool,
    included_headers: Vec<String>,
    waiting_brks: Vec<Vec<usize>>,
    waiting_cntns: Vec<Vec<usize>>,
    inject: String,
}

impl IrGenerator {
    pub fn new() -> Self {
        Self {
            instructions: Vec::new(),
            str_pool: Pool::new(),
            fun_pool: Pool::new(),
            included_headers: Vec::new(),
            waiting_brks: Vec::new(),
            waiting_cntns: Vec::new(),
            inject: String::new(),
        }
    }

    pub fn generate_from(mut self, tasts: &[TypedStatement]) -> IrProgram {
        for tast in tasts {
            self.gen_from_stmt(tast);
        }
        IrProgram {
            instructions: self.instructions,
            str_pool: self.str_pool,
            fun_pool: self.fun_pool,
            included_headers: self.included_headers,
            inject: self.inject,
        }
    }

    fn emit(&mut self, instruction: Instruction) {
        self.instructions.push(instruction);
    }

    fn emit_with_index(&mut self, instruction: Instruction) -> usize {
        self.emit(instruction);
        self.last_instr_index()
    }

    fn get_str_id(&mut self, s: String) -> usize {
        self.str_pool.get_id_or_append(s)
    }

    fn get_fun_id(&mut self, s: String) -> usize {
        self.fun_pool.get_id_or_append(s)
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

    fn patch_jump_to(&mut self, instr_index: usize, target_index: usize) {
        if let Some(instr) = self.instructions.get_mut(instr_index) {
            match instr {
                Instruction::Jump(index) => {
                    *index = target_index;
                }
                Instruction::JumpIfFalse(index) => {
                    *index = target_index;
                }
                _ => unreachable!("use for only jumpers"),
            }
        } else {
            unreachable!("ir: instruction vector bound error")
        }
    }

    fn gen_from_expr_stmt(&mut self, expr: &TypedExpression) {
        self.gen_from_expr(expr);
        self.emit(Instruction::Discard);
    }

    fn gen_from_stmt_fun(&mut self, node: &TypedFunDefNode) {
        let fun_id = self.get_fun_id(node.name.value.clone());

        if !node.is_extern {
            self.emit(Instruction::FunStart(fun_id, (&node.ret_type.kind).into()));
            for param in &node.parameters {
                self.emit(Instruction::FunParam(
                    param.0.value_id,
                    (&param.1.kind).into(),
                ));
            }
            self.emit(Instruction::FunBodyStart);
            self.gen_from_stmt_block(&node.body);
            self.emit(Instruction::FunEnd);
        } else {
            self.emit(Instruction::ExternFunStart(
                fun_id,
                (&node.ret_type.kind).into(),
            ));
            for param in &node.parameters {
                self.emit(Instruction::ExternFunParam(
                    param.0.value_id,
                    (&param.1.kind).into(),
                ));
            }
            self.emit(Instruction::ExternFunEnd);
        }
    }

    fn gen_from_stmt_if(&mut self, node: &TypedIfNode) {
        self.gen_from_expr(&node.condition);
        let jmp_fls_instr_index = self.emit_with_index(Instruction::JumpIfFalse(0));
        self.gen_from_stmt_block(&node.then_branch);
        let jmp_instr_index = self.emit_with_index(Instruction::Jump(0));
        self.add_jmp_index_here(jmp_fls_instr_index);

        let mut jumpers = Vec::new();
        for elsf in &node.else_if_branches {
            self.gen_from_expr(&elsf.condition);
            let jmp_fls_instr_index = self.emit_with_index(Instruction::JumpIfFalse(0));
            self.gen_from_stmt_block(&elsf.then_branch);
            jumpers.push(self.emit_with_index(Instruction::Jump(0)));
            self.add_jmp_index_here(jmp_fls_instr_index);
        }

        if let Some(else_node) = &node.else_branch {
            self.gen_from_stmt_block(else_node);
        }
        self.add_jmp_index_here(jmp_instr_index);
        for jmp in jumpers {
            self.add_jmp_index_here(jmp);
        }
    }

    fn gen_from_stmt_whl(&mut self, node: &TypedWhileNode) {
        self.waiting_brks.push(Vec::new());
        self.waiting_cntns.push(Vec::new());

        let start_index = self.current_index();
        self.gen_from_expr(&node.condition);
        let jmp_fls_instr_index = self.emit_with_index(Instruction::JumpIfFalse(0));
        self.gen_from_stmt_block(&node.body);
        self.emit(Instruction::Jump(start_index));
        self.add_jmp_index_here(jmp_fls_instr_index);

        if let Some(brks) = self.waiting_brks.pop() {
            for brk_instr_index in brks {
                self.add_jmp_index_here(brk_instr_index);
            }
        }
        if let Some(cntns) = self.waiting_cntns.pop() {
            for cntn_instr_index in cntns {
                self.patch_jump_to(cntn_instr_index, start_index);
            }
        }
    }

    fn gen_from_stmt_block(&mut self, node: &TypedBlockNode) {
        for stmt in &node.body {
            self.gen_from_stmt(stmt);
        }
    }

    fn gen_from_stmt_return(&mut self, node: &TypedReturnNode) {
        let mut has_value = false;
        if let Some(expr) = &node.value {
            self.gen_from_expr(expr);
            has_value = true;
        }

        self.emit(Instruction::Ret(has_value));
    }

    fn gen_from_stmt_asgn(&mut self, node: &TypedAssignmentNode) {
        self.gen_from_expr(&node.right);

        match &node.left.kind {
            TypedExpressionKind::Ident(ident) => {
                self.emit(Instruction::PushValueId(ident.id));
                self.emit(Instruction::Load);
                self.emit(Instruction::Store);
            }
            TypedExpressionKind::Deref(node) => {
                self.gen_from_expr(&node.inner);
                self.emit(Instruction::StoreIndirect);
            }
            TypedExpressionKind::Index(node) => {
                self.gen_from_expr_index(node);
                self.emit(Instruction::Store);
            }
            _ => unreachable!("only ident, deref, index"),
        }
    }

    fn gen_from_stmt_val(&mut self, node: &TypedVarDecNode) {
        match &node.initializer {
            Some(initializer) => self.gen_from_expr(&initializer),
            None => match &node.var_type.kind {
                TypeKind::Int => self.emit(Instruction::PushInt(0)),
                TypeKind::Float => self.emit(Instruction::PushFloat(0.0)),
                TypeKind::Bool => self.emit(Instruction::PushBool(false)),
                TypeKind::Char => self.emit(Instruction::PushChar('\0')),
                TypeKind::Str => {
                    let id = self.get_str_id("".into());
                    self.emit(Instruction::PushStrId(id));
                }
                TypeKind::Reference(_) => self.emit(Instruction::PushNull),
                TypeKind::Array(_, _) => self.emit(Instruction::PushZeroInit),
                TypeKind::Data(_) => self.emit(Instruction::PushZeroInit),
                TypeKind::Function { .. } => unreachable!(),
                TypeKind::Nret => unreachable!(),
                TypeKind::Unknown => unreachable!(),
            },
        }

        self.emit(Instruction::Init(
            node.name_id.value_id,
            (&node.var_type.kind).into(),
        ));
    }

    fn gen_from_stmt_extern_block(&mut self, node: &TypedExternBlockNode) {
        if !self.included_headers.contains(&node.header) {
            self.included_headers.push(node.header.clone());
        }
    }

    fn gen_from_stmt_brk(&mut self) {
        let instr_index = self.emit_with_index(Instruction::Jump(0));
        if let Some(brks) = self.waiting_brks.last_mut() {
            brks.push(instr_index);
        } else {
            unreachable!("break used outside loop");
        }
    }
    fn gen_from_stmt_cntn(&mut self) {
        let instr_index = self.emit_with_index(Instruction::Jump(0));
        if let Some(cntns) = self.waiting_cntns.last_mut() {
            cntns.push(instr_index);
        } else {
            unreachable!("continue used outside loop");
        }
    }

    fn take_inject(&mut self, node: &InjectNode) {
        self.inject = node.raw_content.clone();
    }

    fn gen_from_stmt_data(&mut self, node: &TypedDataDecNode) {
        self.emit(Instruction::DataStart(node.name.id));
        for field in &node.fields {
            self.emit(Instruction::DataField(field.0.id, (&field.1.kind).into()));
        }
        self.emit(Instruction::DataEnd);
    }

    fn gen_from_stmt(&mut self, stmt: &TypedStatement) {
        match &stmt.kind {
            TypedStatementKind::VarDeclaration(node) => self.gen_from_stmt_val(node),
            TypedStatementKind::Assignment(node) => self.gen_from_stmt_asgn(node),
            TypedStatementKind::If(node) => self.gen_from_stmt_if(node),
            TypedStatementKind::While(node) => self.gen_from_stmt_whl(node),
            TypedStatementKind::FunDefinition(node) => self.gen_from_stmt_fun(node),
            TypedStatementKind::Block(node) => self.gen_from_stmt_block(node),
            TypedStatementKind::ExternBlock(node) => self.gen_from_stmt_extern_block(node),
            TypedStatementKind::DataDeclaration(node) => self.gen_from_stmt_data(node),
            TypedStatementKind::Inject(node) => self.take_inject(node),
            TypedStatementKind::Expression(expr) => self.gen_from_expr(expr),
            TypedStatementKind::ExpressionStatement(expr) => self.gen_from_expr_stmt(expr),
            TypedStatementKind::Return(node) => self.gen_from_stmt_return(node),
            TypedStatementKind::Break => self.gen_from_stmt_brk(),
            TypedStatementKind::Continue => self.gen_from_stmt_cntn(),
            TypedStatementKind::Broken => {
                unreachable!("hey typechecker, what the hell is this doing here?")
            }
        };
    }

    fn gen_from_expr_field(&mut self, node: &TypedFieldAccessNode) {
        self.gen_from_expr(&node.target);
        self.emit(Instruction::DataFieldAccess(node.field.id));
    }

    fn gen_from_expr_index(&mut self, node: &TypedIndexExpressionNode) {
        self.gen_from_expr(&node.target);
        self.gen_from_expr(&node.index);
        self.emit(Instruction::IndexAccess);
    }

    fn gen_from_expr_call(&mut self, node: &TypedCallNode) {
        for arg in &node.arguments {
            self.gen_from_expr(arg);
        }

        let fun_name = if let TypedExpressionKind::Ident(node) = &node.primary.kind {
            node.val.clone()
        } else {
            unimplemented!()
        };

        let id = self.get_fun_id(fun_name);

        if !node.is_extern {
            self.emit(Instruction::Call(id, node.arguments.len()));
        } else {
            self.emit(Instruction::ExternCall(id, node.arguments.len()));
        }
    }

    fn gen_from_expr_unary(&mut self, node: &TypedUnaryExpressionNode) {
        self.gen_from_expr(&node.operand);

        match node.operator {
            UnaryOperator::Neg => self.emit(Instruction::Neg),
            UnaryOperator::Not => self.emit(Instruction::Not),
            UnaryOperator::Tilde => self.emit(Instruction::Tilde),
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
            BinaryOperator::BitwiseOr => self.emit(Instruction::BitwiseOr),
            BinaryOperator::BitwiseXor => self.emit(Instruction::BitwiseXor),
            BinaryOperator::BitwiseAnd => self.emit(Instruction::BitwiseAnd),
            BinaryOperator::BitwiseLeftShift => self.emit(Instruction::BitwiseLeftShift),
            BinaryOperator::BitwiseRightShift => self.emit(Instruction::BitwiseRightShift),
        }
    }

    fn gen_from_expr_ident(&mut self, node: &IdentLiteralTuple) {
        self.emit(Instruction::PushValueId(node.id));
        self.emit(Instruction::Load);
    }

    fn gen_from_expr_literal(&mut self, node: &LiteralNode) {
        match &node.value {
            LiteralValue::Number(v) => self.emit(Instruction::PushInt(*v)),
            LiteralValue::Str(v) => {
                let id = self.get_str_id(v.clone());
                self.emit(Instruction::PushStrId(id));
            }
            LiteralValue::Bool(v) => self.emit(Instruction::PushBool(*v)),
            LiteralValue::Char(v) => self.emit(Instruction::PushChar(*v)),
            LiteralValue::FloatNumber(v) => self.emit(Instruction::PushFloat(*v)),
        };
    }

    fn gen_from_expr_addressof(&mut self, node: &TypedAddressOfNode) {
        if let TypedExpressionKind::Ident(ident) = &node.inner.kind {
            self.emit(Instruction::AddressOf(ident.id));
        } else {
            unimplemented!("address of works for only ident yet")
        }
    }

    fn gen_from_expr_deref(&mut self, node: &TypedDerefNode) {
        self.gen_from_expr(&node.inner);
        self.emit(Instruction::LoadIndirect);
    }

    fn gen_from_expr_array_literal(&mut self, node: &TypedArrayLiteralNode) {
        self.emit(Instruction::ArrayStart);

        for elem in &node.elems {
            self.gen_from_expr(elem);
            self.emit(Instruction::ArrayElem);
        }

        self.emit(Instruction::ArrayEnd);
    }

    fn gen_from_expr_data(&mut self, node: &TypedDataInitNode) {
        for (tuple, expr) in &node.values {
            self.emit(Instruction::PushValueId(tuple.id));
            self.gen_from_expr(expr);
        }
        self.emit(Instruction::DataInit(node.name.id, node.values.len()));
    }

    fn gen_from_expr(&mut self, expr: &TypedExpression) {
        match &expr.kind {
            TypedExpressionKind::Binary(node) => self.gen_from_expr_binary(node),
            TypedExpressionKind::Unary(node) => self.gen_from_expr_unary(node),
            TypedExpressionKind::Literal(node) => self.gen_from_expr_literal(node),
            TypedExpressionKind::Ident(node) => self.gen_from_expr_ident(node),
            TypedExpressionKind::Call(node) => self.gen_from_expr_call(node),
            TypedExpressionKind::AddressOf(node) => self.gen_from_expr_addressof(node),
            TypedExpressionKind::Deref(node) => self.gen_from_expr_deref(node),
            TypedExpressionKind::ArrayLiteral(node) => self.gen_from_expr_array_literal(node),
            TypedExpressionKind::Index(node) => self.gen_from_expr_index(node),
            TypedExpressionKind::DataInit(node) => self.gen_from_expr_data(node),
            TypedExpressionKind::FieldAccess(node) => self.gen_from_expr_field(node),
            TypedExpressionKind::Broken => {
                unreachable!("hey typechecker, what the hell is this doing here?")
            }
        }
    }
}

pub(crate) struct Pool {
    item_map: HashMap<String, usize>,
    item_vec: Vec<String>,
}

impl Pool {
    pub fn new() -> Self {
        Self {
            item_map: HashMap::new(),
            item_vec: Vec::new(),
        }
    }

    pub fn get_id_or_append(&mut self, s: String) -> usize {
        if let Some(&id) = self.item_map.get(&s) {
            return id;
        }
        let id = self.item_vec.len();
        self.item_map.insert(s.clone(), id);
        self.item_vec.push(s);
        id
    }

    pub fn get_by_id(&self, id: usize) -> &str {
        match self.item_vec.get(id) {
            Some(s) => s,
            None => unreachable!("pool: wrong id!"),
        }
    }

    #[allow(unused)]
    pub fn get_id(&self, s: &str) -> usize {
        match self.item_map.get(s) {
            Some(i) => *i,
            None => unreachable!("pool: wrong str!"),
        }
    }
}

pub struct IrProgram {
    instructions: Vec<Instruction>,
    str_pool: Pool,
    fun_pool: Pool,
    included_headers: Vec<String>,
    inject: String,
}
impl IrProgram {
    pub fn get_str(&self, id: usize) -> &str {
        self.str_pool.get_by_id(id)
    }

    pub fn get_fun_name(&self, id: usize) -> &str {
        self.fun_pool.get_by_id(id)
    }

    pub fn instructions(&self) -> &[Instruction] {
        &self.instructions
    }
    pub fn included_headers(&self) -> &[String] {
        &self.included_headers
    }
    pub fn inject(&self) -> &String {
        &self.inject
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IrType {
    I32,
    F32,
    Bool,
    Char,
    Str,
    Void,
    Reference(Box<IrType>),
    Array(Box<IrType>, usize),
    Data(usize),
}

impl From<&TypeKind> for IrType {
    fn from(value: &TypeKind) -> Self {
        match &value {
            TypeKind::Int => Self::I32,
            TypeKind::Float => Self::F32,
            TypeKind::Bool => Self::Bool,
            TypeKind::Char => Self::Char,
            TypeKind::Str => Self::Str,
            TypeKind::Function { ret, .. } => Self::from(&ret.kind),
            TypeKind::Nret => Self::Void,
            TypeKind::Data(tuple) => Self::Data(tuple.id),
            TypeKind::Reference(inner_type) => {
                let inner = Self::from(&(**inner_type));
                IrType::Reference(Box::new(inner))
            }
            TypeKind::Array(inner_type, size) => {
                let inner = Self::from(&(**inner_type));
                IrType::Array(Box::new(inner), *size)
            }
            TypeKind::Unknown => unreachable!("unknown type is imposible here"),
        }
    }
}
