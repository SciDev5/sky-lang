/*




{
    let a = 0 // primitive
    let b = a // primitive, copied

    let c = [1,2,3] // object -> ref 123
    {
        let d = c // ref 123, copied
    }
    let e = c // ref 123, copied

}









*/

use std::collections::HashMap;

use crate::language::{
    ops::SLOperator,
    slir::{
        SLIRArray, SLIRBlock, SLIRExpression, SLIRLiteral, SLIRStatement, SLIRVarAccessExpression,
    },
};

type Identifier = Box<str>;

#[derive(Debug, Clone, Copy)]
pub enum Value {
    Int(i128),
    Float { re: f64, im: f64 },
    Bool(bool),
    Ref(GCObjectId),
}

macro_rules! impl_unary_op {
    ($name: ident; $($pat: pat => $expr: expr),* $(,)?) => {
        fn $name (self) -> Result<Value, IrrecoverableError> {
            match self {
                $($pat => Ok($expr),)*
                _ => Err(IrrecoverableError::IllegalOperator),
            }
        }
    };
    ($name: ident; TODO) => {
        fn $name (self) -> Result<Value, IrrecoverableError> {
            todo!()
        }
    };
}
macro_rules! impl_binary_op {
    ($name: ident; $($pat: pat => $expr: expr),* $(,)?) => {
        fn $name (self, rhs: Self) -> Result<Value, IrrecoverableError> {
            match (self, rhs) {
                $($pat => Ok($expr),)*
                _ => Err(IrrecoverableError::IllegalOperator),
            }
        }
    };
    ($name: ident; TODO) => {
        fn $name (self, rhs: Self) -> Result<Value, IrrecoverableError> {
            todo!()
        }
    };
}
impl Value {
    impl_unary_op!(not;
        Self::Bool(it) => Self::Bool(!it)
    );
    impl_unary_op!(unary_plus;
        Self::Int(n) => Self::Int(n),
        Self::Float { re, im } => Self::Float { re, im },
    );
    impl_unary_op!(unary_minus;
        Self::Int(n) => Self::Int(-n),
        Self::Float { re, im } => Self::Float { re: -re, im: -im },
    );
    impl_unary_op!(hermitian_conjugate; TODO);
    impl_unary_op!(transpose; TODO);
    impl_unary_op!(inverse; TODO);

    fn apply_op_unary(self, op: SLOperator) -> Result<Value, IrrecoverableError> {
        match op {
            SLOperator::Not => self.not(),
            SLOperator::Plus => self.unary_plus(),
            SLOperator::Minus => self.unary_minus(),
            SLOperator::HermitianConjugate => self.hermitian_conjugate(),
            SLOperator::Transpose => self.transpose(),
            SLOperator::Inverse => self.inverse(),
            _ => Err(IrrecoverableError::IllegalOperator),
        }
    }

    impl_binary_op!(plus;
        (Self::Int(n0), Self::Int(n1)) => Self::Int(n0+n1),
        (Self::Float { re, im }, Self::Int(n)) => Self::Float { re: re + n as f64, im },
        (Self::Int(n), Self::Float { re, im }) => Self::Float { re: n as f64 + re, im },
        (Self::Float { re: r0, im: i0 }, Self::Float { re: r1, im: i1 }) => Self::Float { re: r0 + r1, im: i0 + i1 },
    );
    impl_binary_op!(scalar_times;
        (Self::Int(n0), Self::Int(n1)) => Self::Int(n0*n1),
    );
    impl_binary_op!(gt;
        (Self::Int(n0), Self::Int(n1)) => Self::Bool(n0 > n1),
    );
    impl_binary_op!(ge;
        (Self::Int(n0), Self::Int(n1)) => Self::Bool(n0 >= n1),
    );
    impl_binary_op!(lt;
        (Self::Int(n0), Self::Int(n1)) => Self::Bool(n0 < n1),
    );
    impl_binary_op!(le;
        (Self::Int(n0), Self::Int(n1)) => Self::Bool(n0 <= n1),
    );
    impl_binary_op!(eq;
        (Self::Int(n0), Self::Int(n1)) => Self::Bool(n0 == n1),
    );
    impl_binary_op!(ne;
        (Self::Int(n0), Self::Int(n1)) => Self::Bool(n0 != n1),
    );

    fn apply_op_binary(self, rhs: Self, op: SLOperator) -> Result<Value, IrrecoverableError> {
        match op {
            SLOperator::Plus => self.plus(rhs),
            SLOperator::ScalarTimes => self.scalar_times(rhs),
            SLOperator::GreaterThan => self.gt(rhs),
            SLOperator::GreaterEqual => self.ge(rhs),
            SLOperator::LessThan => self.lt(rhs),
            SLOperator::LessEqual => self.le(rhs),
            SLOperator::Equal => self.eq(rhs),
            SLOperator::NotEqual => self.ne(rhs),
            _ => Err(IrrecoverableError::IllegalOperator),
        }
    }
}

#[derive(Debug)]
struct Function {
    params: Vec<Identifier>,
    closure: Scope,
    code: Vec<Instruction>,
}

#[derive(Debug)]
enum Object {
    Function(Box<Function>),
    List(Vec<Value>),
    String(String),
}
impl Object {
    fn index(&self, indices: &Vec<Value>) -> Option<Value> {
        match self {
            Object::Function(_) => None,
            Object::List(values) => {
                if indices.len() == 1 {
                    todo!()
                } else {
                    None
                }
            }
            Object::String(_) => todo!(),
        }
    }
    fn index_mut(&mut self, indices: &Vec<Value>) -> Option<&mut Value> {
        match self {
            Object::Function(_) => None,
            Object::List(values) => {
                if indices.len() == 1 {
                    todo!()
                } else {
                    None
                }
            }
            Object::String(_) => todo!(),
        }
    }
}

#[derive(Debug)]
enum DataRef {
    Index(GCObjectId, Vec<Value>),
    Identifier(Identifier),
    None,
}
impl DataRef {
    fn read(self, scope: &Scope, gc: &GarbageCollector) -> Option<Value> {
        match self {
            Self::None => None,
            Self::Index(obj_id, indices) => gc.borrow(obj_id).index(&indices),
            Self::Identifier(ident) => scope.read(&ident).ok(),
        }
    }
    fn write<'a>(
        self,
        scope: &'a mut Scope,
        gc: &'a mut GarbageCollector,
        value: Value,
    ) -> Option<()> {
        match self {
            Self::None => None,
            Self::Identifier(ident) => {
                scope.get_var_mut(&ident).ok()?.write(value).ok()?;
                Some(())
            }
            Self::Index(obj_id, indices) => {
                let value_ref = gc.borrow_mut(obj_id).index_mut(&indices)?;
                *value_ref = value;
                Some(())
            }
        }
    }
    fn index(self, scope: &Scope, gc: &GarbageCollector, index: Vec<Value>) -> Option<DataRef> {
        match self.read(scope, gc) {
            Some(Value::Ref(obj_id)) => Some(DataRef::Index(obj_id, index)),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct Var {
    writable: bool,
    value: Option<Value>,
}
impl Var {
    fn read(&self) -> Result<Value, IrrecoverableError> {
        self.value
            .ok_or(IrrecoverableError::VarReadBeforeInitialized)
    }
    fn write(&mut self, value: Value) -> Result<(), IrrecoverableError> {
        if !self.writable && self.value.is_some() {
            Err(IrrecoverableError::VarNotWritable)
        } else {
            self.value = Some(value);
            Ok(())
        }
    }
}

/// The instruction index
///
/// For jumps during translation, instruction indices over `usize::MAX/2` are interpreted as `break`/`continue`s.
///
/// For `n` being the number of loops to not break/continue from before performing the break/continue
/// - BREAK: `usize::MAX - 2*n`
/// - CONTINUE: `usize::MAX - 2*n - 1`
type InstructionIndex = usize;
type RelativeInstructionIndex = isize;

/// Low-level Instructions the interpreter interprets.
///
/// These instructions use:
/// - one `Voidable`, called the "working value"
/// - one `DataRef`, called the "working mutable reference"
/// - stack of `Value`s, called the "intermediate value stack"
/// - the `Scope` stack
#[derive(Debug, Clone)]
pub enum Instruction {
    /// Reads the value of the variable with the given identifier to the working value.
    ReadVar(Identifier),
    /// Gets a reference to the current piece of data stored in the working mutable reference
    RefVar(Identifier),
    /// Indexes into the working mutable reference, indices given by popping the given number of values from the intermediate value stack
    IndexRef(usize),
    /// Writes the working value to the working mutable reference.
    WriteRef,
    /// Writes the working value to the variable given by the identifier.
    WriteVar(Identifier),
    /// Creates a new variable with the given identifier and writability status
    CreateVar { ident: Identifier, writable: bool },
    /// Push the working value to the intermediate value stack.
    PushIntermediate,
    /// Applies the given operator to the working value.
    UnaryOp(SLOperator),
    /// Pops a value from the intermediate value stack and applies the given operator to it and the working value.
    BinaryOp(SLOperator),
    /// Calls the callable represented by the working value, arguments given by popping the given number of values from the intermediate value stack.
    Call(usize),
    /// Indexes into the working value, indices given by popping the given number of values from the intermediate value stack.
    Index(usize),
    /// Create a list of the given length by popping [length] values from the intermediate value stack.
    CreateList(usize),
    /// Create a function value, capturing the current scope.
    CreateFunction {
        params: Vec<Identifier>,
        code: Vec<Instruction>,
    },
    /// Push an empty frame to the scope stack (entering a new scope).
    ///
    /// Note: the instruction index is used as an ID, translation tracking used to keep it unique.
    PushScope(u16),
    /// Pop from the scope stack until one with the matching ID is found (exiting a scope).
    PopScope(u16),
    /// Jumps to given instruction index.
    Jump(RelativeInstructionIndex),
    /// Jumps to given instruction index if the working value is boolean false (fails if not boolean).
    JumpFalse(RelativeInstructionIndex),
    /// Guaranteed fail here to allow late failue of invalid code such as top-level breaks.
    Fail(IrrecoverableError),
    /// Put the current value in as the current working value.
    Primitive(SLIRLiteral),
    /// Return the working value
    Return,
}
impl Instruction {
    fn new_jump_break(n_layers_out: usize) -> Self {
        Self::Jump(RelativeInstructionIndex::MAX - n_layers_out as isize * 2)
    }
    fn new_jump_continue(n_layers_out: usize) -> Self {
        Self::Jump(RelativeInstructionIndex::MAX - n_layers_out as isize * 2 - 1)
    }
    fn finalize_loop_flow_controls(
        &mut self,
        i: usize,
        continue_jump_i: usize,
        break_jump_i: usize,
    ) {
        fn t(i: usize, jump_target: isize, continue_jump_i: usize, break_jump_i: usize) -> isize {
            if jump_target < isize::MAX / 2 {
                jump_target
            } else if jump_target <= isize::MAX - 2 {
                jump_target + 2
            } else if jump_target == isize::MAX {
                // break
                break_jump_i as isize - i as isize
            } else {
                // continue
                continue_jump_i as isize - i as isize
            }
        }
        *self = match self {
            Self::Jump(jmp_i) => Self::Jump(t(i, *jmp_i, continue_jump_i, break_jump_i)),
            Self::JumpFalse(jmp_i) => Self::JumpFalse(t(i, *jmp_i, continue_jump_i, break_jump_i)),
            _ => return,
        }
    }
    fn disallow_loop_flow_controls(&mut self, len: usize) {
        fn t(index: isize) -> Instruction {
            if index % 2 != 0 {
                Instruction::Fail(IrrecoverableError::IllegalBreak)
            } else {
                Instruction::Fail(IrrecoverableError::IllegalContinue)
            }
        }
        *self = match self {
            Self::Jump(i) if *i > isize::MAX / 2 => t(*i),
            Self::JumpFalse(i) if *i > isize::MAX / 2 => t(*i),
            _ => return,
        }
    }
}

enum ScopeId {
    Loop { depth: usize },
    NoSkipPop,
}
impl ScopeId {
    const fn u16(self) -> u16 {
        match self {
            ScopeId::NoSkipPop => 0,
            ScopeId::Loop { depth } => 1 + depth as u16,
        }
    }
}

fn concat_instructions<T: IntoIterator<Item = Vec<Instruction>>>(parts: T) -> Vec<Instruction> {
    parts.into_iter().flatten().collect()
}
fn build_loop(mut body: Vec<Instruction>, loop_depth: usize) -> Vec<Instruction> {
    let continue_jump_i = 1 + body.len(); // jump to the first PopScope, before the reset jump
    let break_jump_i = 1 + body.len() + 2; // jump to the second PopScope

    let body_len = body.len();

    for (i, inst) in body.iter_mut().enumerate() {
        inst.finalize_loop_flow_controls(i + 1, continue_jump_i, break_jump_i)
    }

    let scope_id = ScopeId::Loop { depth: loop_depth }.u16();
    concat_instructions([
        vec![Instruction::PushScope(scope_id)],
        body,
        vec![
            // (continue) pop scope and jump back to the start
            Instruction::PopScope(scope_id),
            Instruction::Jump(-(2 + body_len as isize)),
            // (break) pop scope and exit
            Instruction::PopScope(scope_id), // pop after break
        ],
    ])
}
// fn build_for(mut control_init: Vec<Instruction>, mut control_update: Vec<Instruction>, mut body: Vec<Instruction>) -> Vec<Instruction> {
//     const CONTINUE_JUMP_I: usize = 1;
//     let break_jump_i = 1 + control.len() + 1 + body.len() + 2;
//     for inst in &mut control {
//         inst.translate(1);
//         inst.finalize_loop_flow_controls(CONTINUE_JUMP_I, break_jump_i)
//     }
//     let body_offset = 1 + control.len() + 1;
//     for inst in &mut body {
//         inst.translate(body_offset);
//         inst.finalize_loop_flow_controls(CONTINUE_JUMP_I, break_jump_i)
//     }
//     let body_offset = 1 + control.len() + 1;
//     for inst in &mut body {
//         inst.translate(body_offset);
//         inst.finalize_loop_flow_controls(CONTINUE_JUMP_I, break_jump_i)
//     }
//     [
//         vec![Instruction::PushScope], // push init scope
//         control_init,
//         vec![Instruction::PushScope],
//         control_update,
//         body,
//         vec![
//             Instruction::PopScope,
//             Instruction::Jump(CONTINUE_JUMP_I),
//             Instruction::PopScope, // pop after break
//             Instruction::PopScope, // pop init scope
//         ],
//     ]
//     .into_iter()
//     .flatten()
//     .collect()
// }

#[derive(Debug, Clone)]
struct InstructionBuildingContext {
    loop_infos: Vec<LoopInfo>,
}
impl InstructionBuildingContext {
    fn push_loop_info(&mut self, info: LoopInfo) {
        self.loop_infos.push(info);
    }
}
#[derive(Debug, Clone)]
struct LoopInfo {
    label: Option<Identifier>,
    allow_break_value: bool,
}

#[derive(Debug, Clone, Copy)]
pub enum IrrecoverableError {
    IllegalBreak,
    IllegalBreakValue,
    IllegalContinue,
    IllegalReturn,
    IllegalOperator,
    NonBooleanCondition,
    VoidAsValue,
    VarNotFound,
    VarReadBeforeInitialized,
    VarNotWritable,
    VarRedeclaration,
    NotCallable,
    NotIndexable,
    InternalError(&'static str),
}
#[derive(Debug, Clone, Copy)]
pub enum Voidable {
    Void,
    Value(Value),
}
impl Voidable {
    fn as_value(&self) -> Result<Value, IrrecoverableError> {
        match self {
            Self::Value(v) => Ok(*v),
            _ => Err(IrrecoverableError::VarNotFound),
        }
    }
}

fn serialize_expr_instructions(
    code: SLIRExpression,
    context: &InstructionBuildingContext,
) -> Vec<Instruction> {
    match code {
        crate::language::slir::SLIRExpression::Read(ident) => vec![Instruction::ReadVar(ident)],
        crate::language::slir::SLIRExpression::Call {
            callable,
            arguments,
        } => {
            let arguments_len = arguments.len();
            arguments
                .into_iter()
                .flat_map(|it| {
                    serialize_expr_instructions(it, context)
                        .into_iter()
                        .chain(std::iter::once(Instruction::PushIntermediate))
                })
                .chain(serialize_expr_instructions(*callable, context))
                .chain(std::iter::once(Instruction::Call(arguments_len)))
                .collect()
        }
        crate::language::slir::SLIRExpression::Index { expr, indices } => todo!(),
        crate::language::slir::SLIRExpression::PropertyAccess {
            expr,
            property_ident,
        } => todo!(),
        crate::language::slir::SLIRExpression::Literal(literal) => {
            vec![Instruction::Primitive(literal)]
        }
        crate::language::slir::SLIRExpression::Range { start, step, end } => todo!(),
        crate::language::slir::SLIRExpression::Array(array) => match array {
            SLIRArray::List(list) => {
                let len = list.len();
                concat_instructions(
                    list.into_iter()
                        .flat_map(|it| {
                            [
                                serialize_expr_instructions(it, context),
                                vec![Instruction::PushIntermediate],
                            ]
                        })
                        .chain(std::iter::once(vec![Instruction::CreateList(len)])),
                )
            }
            _ => todo!(),
        },
        crate::language::slir::SLIRExpression::AnonymousFunction { params, block } => {
            vec![Instruction::CreateFunction {
                params,
                code: serialize_block_instructions(block, context),
            }]
        }
        crate::language::slir::SLIRExpression::BinaryOp(op, a, b) => concat_instructions([
            serialize_expr_instructions(*a, context),
            vec![Instruction::PushIntermediate],
            serialize_expr_instructions(*b, context),
            vec![Instruction::BinaryOp(op)],
        ]),
        crate::language::slir::SLIRExpression::UnaryOp(op, expr) => concat_instructions([
            serialize_expr_instructions(*expr, context),
            vec![Instruction::UnaryOp(op)],
        ]),
        crate::language::slir::SLIRExpression::Conditional {
            condition,
            block,
            elifs,
            else_block,
        } => {
            let mut condition = serialize_expr_instructions(*condition, context);
            let mut block = serialize_block_instructions(block, context);
            let elifs = elifs
                .into_iter()
                .map(|(condition, block)| {
                    (
                        serialize_expr_instructions(condition, context),
                        serialize_block_instructions(block, context),
                    )
                })
                .collect::<Vec<_>>();
            let else_block = else_block.map(|block| serialize_block_instructions(block, context));

            /*
            >>
                [condition -> bool]
                jmp_false(ELIF_0)
                scope_push(0)
                [block -> T]
                jmp(END)

                ELIF_0:
                [condition_elif_0 -> bool]
                jmp_false(ELSE)
                scope_push(0)
                [block_elif_0 -> T]
                jmp(END)

                ELSE:
                scope_push(0)
                [block_else -> T]
                // jmp(END) // omit because last

                END:
                scope_pop(0)

                */

            let mut condition_locations = vec![];
            let mut total_len = 0;
            total_len += condition.len() + 2 + block.len() + 1;
            for (condition, block) in &elifs {
                condition_locations.push(total_len);
                total_len += condition.len() + 2 + block.len() + 1;
            }
            condition_locations.push(total_len); // add location else block or after the scope_pop
            if let Some(block) = &else_block {
                total_len += 1 + block.len() + 1;
            }

            let mut condition_jump_locations = condition_locations.into_iter();
            let end_location = total_len - 1; // make sure to catch the one last PopScope at the end

            let mut instr = vec![];
            let scope_id = ScopeId::NoSkipPop.u16();

            // positive condition
            instr.append(&mut condition);
            instr.push(Instruction::JumpFalse(
                condition_jump_locations.next().unwrap() as isize - instr.len() as isize,
            ));
            // positive block
            instr.push(Instruction::PushScope(scope_id));
            instr.append(&mut block);

            for (mut condition, mut block) in elifs {
                instr.push(Instruction::Jump(
                    end_location as isize - instr.len() as isize,
                ));
                // elif condition
                instr.append(&mut condition);
                instr.push(Instruction::JumpFalse(
                    condition_jump_locations.next().unwrap() as isize - instr.len() as isize,
                ));
                // elif block
                instr.push(Instruction::PushScope(scope_id));
                instr.append(&mut block);
            }

            if let Some(mut block) = else_block {
                instr.push(Instruction::Jump(
                    end_location as isize - instr.len() as isize,
                ));
                // else block
                instr.push(Instruction::PushScope(scope_id));
                instr.append(&mut block);
            }

            instr.push(Instruction::PopScope(scope_id));

            instr
        }
        crate::language::slir::SLIRExpression::Loop(block) => {
            let mut context = context.clone();
            context.push_loop_info(LoopInfo {
                label: None, // TODO loop labels
                allow_break_value: true,
            });
            let body = serialize_block_instructions(block, &context);
            build_loop(body, context.loop_infos.len() - 1)
        }
        crate::language::slir::SLIRExpression::For {
            loop_var,
            iterable,
            block,
        } => todo!("// TODO for loops"),
        SLIRExpression::Break(value) => {
            if let Some(target_loop) = context.loop_infos.last() {
                // TODO loop labels

                if let Some(value) = value {
                    if target_loop.allow_break_value {
                        [
                            serialize_expr_instructions(*value, context),
                            vec![Instruction::new_jump_break(0)],
                        ]
                        .into_iter()
                        .flatten()
                        .collect()
                    } else {
                        vec![Instruction::Fail(IrrecoverableError::IllegalBreakValue)]
                    }
                } else {
                    vec![Instruction::new_jump_break(0)]
                }
            } else {
                vec![Instruction::Fail(IrrecoverableError::IllegalBreak)]
            }
        }
        SLIRExpression::Continue => {
            if let Some(_) = context.loop_infos.last() {
                // TODO loop labels
                vec![Instruction::new_jump_continue(0)]
            } else {
                vec![Instruction::Fail(IrrecoverableError::IllegalContinue)]
            }
        }
        SLIRExpression::Return(value) => concat_instructions([
            serialize_expr_instructions(*value, context),
            vec![Instruction::Return],
        ]),
    }
}
fn serialize_statement_instructions(
    code: SLIRStatement,
    context: &InstructionBuildingContext,
) -> Vec<Instruction> {
    match code {
        SLIRStatement::FunctionDefinition {
            ident,
            params,
            block,
            ..
        } => {
            vec![
                Instruction::CreateFunction {
                    params,
                    code: serialize_block_instructions(block, context),
                },
                Instruction::CreateVar {
                    ident: ident.clone(),
                    writable: true,
                },
                Instruction::WriteVar(ident),
            ]
        }
        SLIRStatement::VarDeclare {
            ident,
            writable,
            initial_assignment,
            ..
        } => {
            if let Some(initial_assignment) = initial_assignment {
                concat_instructions([
                    serialize_expr_instructions(*initial_assignment, context),
                    vec![
                        Instruction::CreateVar {
                            ident: ident.clone(),
                            writable,
                        },
                        Instruction::WriteVar(ident),
                    ],
                ])
            } else {
                vec![Instruction::CreateVar { ident, writable }]
            }
        }
        SLIRStatement::Assign(accessor, expr) => {
            if let SLIRVarAccessExpression::Read(ident) = accessor {
                concat_instructions([
                    serialize_expr_instructions(*expr, context),
                    vec![Instruction::RefVar(ident), Instruction::WriteRef],
                ])
            } else {
                todo!("// TODO variable data access")
            }
        }
        SLIRStatement::Expr(expr) => serialize_expr_instructions(*expr, context),
    }
}
fn serialize_block_instructions(
    code: SLIRBlock,
    context: &InstructionBuildingContext,
) -> Vec<Instruction> {
    concat_instructions(
        code.0
            .into_iter()
            .map(|it| serialize_statement_instructions(it, context)),
    )
}

pub fn serialize_program(code: SLIRBlock) -> Vec<Instruction> {
    serialize_block_instructions(code, &InstructionBuildingContext { loop_infos: vec![] })
}

type GCObjectId = u64;
#[derive(Debug)]
pub struct GarbageCollector {
    objects: HashMap<GCObjectId, Object>,
    current_id: GCObjectId,

    root_scopes: Vec<ScopeStackFrame>,
}
impl<'data> GarbageCollector {
    pub fn new() -> Self {
        Self {
            objects: HashMap::new(),
            current_id: 0,
            root_scopes: vec![],
        }
    }
    fn next_free_id(&mut self) -> GCObjectId {
        while self.objects.contains_key(&self.current_id) {
            self.current_id += 1;
        }
        let selected_id = self.current_id;
        self.current_id += 1;
        selected_id
    }
    fn alloc(&mut self, object: Object) -> GCObjectId {
        let id = self.next_free_id();
        self.objects.insert(id, object);
        id
    }
    fn borrow(&self, id: GCObjectId) -> &Object {
        self.objects
            .get(&id)
            .expect("reference to object managed to outlive object")
    }
    fn borrow_mut(&mut self, id: GCObjectId) -> &mut Object {
        self.objects
            .get_mut(&id)
            .expect("reference to object managed to outlive object")
    }
    // fn mark_and_sweep(&mut self) {
    //     let encounter: HashMap<u64, bool, RandomState> = HashMap::from_iter(self.objects.keys().map(|it| (*it, false)));
    //     self.root_scopes.

    // }
}

#[derive(Debug, Clone)]
pub struct ScopeStackFrame {
    vars: HashMap<Identifier, Var>,
    id: u16,
    intermediate_value_stack: Vec<Value>,
}
impl ScopeStackFrame {
    pub fn empty(id: u16) -> Self {
        ScopeStackFrame {
            vars: HashMap::new(),
            id,
            intermediate_value_stack: vec![],
        }
    }
    pub fn base() -> Self {
        ScopeStackFrame {
            vars: HashMap::new(),
            id: u16::MAX,
            intermediate_value_stack: vec![],
        }
    }
}

#[derive(Debug, Clone)]
struct Scope {
    stack: Vec<ScopeStackFrame>,
}
impl Scope {
    fn stack_top(&mut self) -> &mut ScopeStackFrame {
        self.stack
            .last_mut()
            .expect("scope stack should never empty")
    }
    fn read(&self, ident: &Identifier) -> Result<Value, IrrecoverableError> {
        for frame in &self.stack {
            if let Some(value) = frame.vars.get(ident) {
                return Ok(value.read()?);
            }
        }
        Err(IrrecoverableError::VarNotFound)
    }
    fn get_var_mut(&mut self, ident: &Identifier) -> Result<&mut Var, IrrecoverableError> {
        for frame in &mut self.stack {
            if let Some(value) = frame.vars.get_mut(ident) {
                return Ok(value);
            }
        }
        Err(IrrecoverableError::VarNotFound)
    }
    fn create_var(&mut self, ident: &Identifier, writable: bool) -> Result<(), IrrecoverableError> {
        let stack_top = self.stack_top();
        if stack_top.vars.contains_key(ident) {
            Err(IrrecoverableError::VarRedeclaration)
        } else {
            stack_top.vars.insert(
                ident.clone(),
                Var {
                    writable,
                    value: None,
                },
            );
            Ok(())
        }
    }
}

struct CallStackFrame {
    scope: Scope,
    code: Vec<Instruction>,
    exec_index: InstructionIndex,
}

impl CallStackFrame {
    fn jump_relative(exec_index: &mut InstructionIndex, delta: isize) {
        *exec_index = exec_index.checked_add_signed(delta).unwrap();
    }
}

pub fn execute_serialized(
    code: Vec<Instruction>,
    root_scope_stack_frame: ScopeStackFrame,
    gc: &mut GarbageCollector,
) -> Result<Voidable, IrrecoverableError> {
    let mut call_stack = vec![CallStackFrame {
        scope: Scope {
            stack: vec![root_scope_stack_frame],
        },
        code,
        exec_index: 0,
    }];

    let mut working_value = Voidable::Void;
    let mut working_mut_ref = DataRef::None;

    while let Some(call) = call_stack.last_mut() {
        if call.exec_index == call.code.len() {
            // reached end of function, return working value
            call_stack.pop();
            continue;
        }
        println!("> [{}] {:?}", call.exec_index, &call.code[call.exec_index]);
        match &call.code[call.exec_index] {
            Instruction::ReadVar(ident) => {
                working_value = Voidable::Value(call.scope.read(ident)?);
            }
            Instruction::RefVar(ident) => {
                working_mut_ref = DataRef::Identifier(ident.clone());
            }
            Instruction::IndexRef(n_args) => {
                let intermediate_value_stack = &mut call.scope.stack_top().intermediate_value_stack;
                let arg_values = intermediate_value_stack
                    .drain(intermediate_value_stack.len() - n_args..)
                    .collect::<Vec<_>>();

                working_mut_ref = working_mut_ref
                    .index(&call.scope, gc, arg_values)
                    .ok_or(IrrecoverableError::NotIndexable)?;
            }
            Instruction::WriteRef => {
                working_mut_ref
                    .write(&mut call.scope, gc, working_value.as_value()?)
                    .ok_or(IrrecoverableError::VarNotWritable)?;
                working_mut_ref = DataRef::None;
                working_value = Voidable::Void;
            }
            Instruction::WriteVar(ident) => call
                .scope
                .get_var_mut(ident)?
                .write(working_value.as_value()?)?,
            Instruction::CreateVar { ident, writable } => {
                call.scope.create_var(ident, *writable)?;
            }
            Instruction::PushIntermediate => {
                let intermediate_value_stack = &mut call.scope.stack_top().intermediate_value_stack;
                intermediate_value_stack.push(working_value.as_value()?);
                working_value = Voidable::Void;
            }
            Instruction::UnaryOp(op) => {
                working_value = Voidable::Value(working_value.as_value()?.apply_op_unary(*op)?);
            }
            Instruction::BinaryOp(op) => {
                let lhs = call
                    .scope
                    .stack_top()
                    .intermediate_value_stack
                    .pop()
                    .ok_or(IrrecoverableError::InternalError(
                        "intermediate value stack exhausted",
                    ))?;
                let rhs = working_value.as_value()?;
                working_value = Voidable::Value(lhs.apply_op_binary(rhs, *op)?);
            }
            Instruction::Call(n_args) => {
                let intermediate_value_stack = &mut call.scope.stack_top().intermediate_value_stack;
                let arg_values = intermediate_value_stack
                    .drain(intermediate_value_stack.len() - n_args..)
                    .collect::<Vec<_>>();
                let func = match working_value {
                    Voidable::Value(Value::Ref(id)) => match gc.borrow(id) {
                        Object::Function(func) => func.as_ref(),
                        _ => return Err(IrrecoverableError::NotCallable),
                    },
                    _ => return Err(IrrecoverableError::NotCallable),
                };

                let mut new_stack_frame = ScopeStackFrame::base();
                for (i, value) in arg_values.into_iter().enumerate() {
                    new_stack_frame.vars.insert(
                        func.params[i].clone(),
                        Var {
                            writable: false,
                            value: Some(value),
                        },
                    );
                }

                call.exec_index += 1;
                call_stack.push(CallStackFrame {
                    scope: Scope {
                        stack: func
                            .closure
                            .stack
                            .iter()
                            .map(|it| it.clone())
                            .chain(std::iter::once(new_stack_frame))
                            .collect(),
                    },
                    code: func.code.clone(),
                    exec_index: 0,
                });
                continue;
            }
            Instruction::Index(_) => todo!(),
            Instruction::CreateList(n) => {
                let intermediate_value_stack = &mut call.scope.stack_top().intermediate_value_stack;
                let values = intermediate_value_stack
                    .drain(intermediate_value_stack.len() - n..)
                    .collect::<Vec<_>>();

                let list_obj = Object::List(values);

                working_value = Voidable::Value(Value::Ref(gc.alloc(list_obj)));
            }
            Instruction::CreateFunction { params, code } => {
                let func_object = Object::Function(Box::new(Function {
                    closure: call.scope.clone(),
                    params: params.clone(),
                    code: code.clone(),
                }));
                working_value = Voidable::Value(Value::Ref(gc.alloc(func_object)))
            }
            Instruction::PushScope(idx) => call.scope.stack.push(ScopeStackFrame::empty(*idx)),
            Instruction::PopScope(idx) => loop {
                let top = call
                    .scope
                    .stack
                    .pop()
                    .ok_or(IrrecoverableError::InternalError(
                        "matching open stack frame missing",
                    ))?;
                if top.id == *idx {
                    break;
                }
            },
            Instruction::Jump(delta) => {
                CallStackFrame::jump_relative(&mut call.exec_index, *delta);
                continue;
            }
            Instruction::JumpFalse(delta) => {
                match working_value {
                    Voidable::Value(Value::Bool(condition)) => {
                        if condition == false {
                            CallStackFrame::jump_relative(&mut call.exec_index, *delta);
                            continue;
                        } else {
                            // do nothing
                        }
                    }
                    _ => return Err(IrrecoverableError::NonBooleanCondition),
                }
            }
            Instruction::Fail(failure) => return Err(*failure),
            Instruction::Primitive(v) => {
                working_value = Voidable::Value(match v {
                    // TODO values and also types in general
                    SLIRLiteral::Int { re, im } => if *im == 0 { Value::Int(*re) } else { Value::Float { re: *re as f64, im: *im as f64 } },
                    SLIRLiteral::Float { re, im } => Value::Float { re: *re, im: *im },
                    SLIRLiteral::Bool(v) => Value::Bool(*v),
                    SLIRLiteral::String(v) => Value::Ref(gc.alloc(Object::String(v.clone()))),
                });
            }
            Instruction::Return => {
                call_stack.pop();
                continue;
            }
        }
        call.exec_index += 1;
    }

    Ok(working_value)
}
