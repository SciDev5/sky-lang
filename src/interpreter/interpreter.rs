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
    slir::{SLIRBlock, SLIRExpression, SLIRLiteral, SLIRStatement},
};

type Identifier = Box<str>;

#[derive(Debug, Clone, Copy)]
enum Value<'gc_data> {
    Int { real: i128, imag: i128 },
    Float { real: f64, imag: i128 },
    Ref(&'gc_data Object<'gc_data>),
}

#[derive(Debug)]
struct Function<'gc_data> {
    params: Vec<Identifier>,
    closure: Scope<'gc_data>,
    code: Vec<Instruction>,
}

#[derive(Debug)]
enum Object<'gc_data> {
    Function(Box<Function<'gc_data>>),
    List(Vec<Value<'gc_data>>),
}

#[derive(Debug, Clone, Copy)]
struct Var<'gc_data> {
    writable: bool,
    value: Value<'gc_data>,
}

struct GarbageCollector<'data> {
    objects: Vec<Box<Object<'data>>>,

    root_scopes: Vec<ScopeStackFrame<'data>>,
}

#[derive(Debug, Clone)]
struct ScopeStackFrame<'gc_data> {
    vars: HashMap<Identifier, Var<'gc_data>>,
}

#[derive(Debug, Clone)]
struct Scope<'gc_data> {
    stack: Vec<ScopeStackFrame<'gc_data>>,
}

struct CallStackFrame<'gc_data> {
    scope: Scope<'gc_data>,
}

/// The instruction index
///
/// For jumps during translation, instruction indices over `usize::MAX/2` are interpreted as `break`/`continue`s.
///
/// For `n` being the number of loops to not break/continue from before performing the break/continue
/// - BREAK: `usize::MAX - 2*n`
/// - CONTINUE: `usize::MAX - 2*n - 1`
type InstructionIndex = usize;

/// Low-level Instructions the interpreter interprets.
///
/// These instructions use:
/// - one `Voidable`, called the "working value"
/// - stack of `Value`s, called the "intermediate value stack"
/// - the `Scope` stack
#[derive(Debug, Clone)]
pub enum Instruction {
    /// Reads the value of the variable with the given identifier to the working value.
    ReadVar(Identifier),
    /// Writes the working value to the variable with the given identifier.
    WriteVar(Identifier),
    /// Creates a new variable with the given identifier and writability status
    CreateVar { ident: Identifier, is_const: bool },
    /// Push the working value to the intermediate value stack.
    PushIntermediate,
    /// Applies the given operator to the working value.
    UnaryOp(SLOperator),
    /// Pops a value from the intermediate value stack and applies the given operator to it and the working value.
    BinaryOp(SLOperator),
    /// Calls the callable represented by the working value, arguments given by popping the given number of values from the intermediate value stack.
    Call(usize),
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
    PushScope(InstructionIndex),
    /// Pop from the scope stack until one with the matching ID is found (exiting a scope).
    PopScope(InstructionIndex),
    /// Jumps to given instruction index.
    Jump(InstructionIndex),
    /// Jumps to given instruction index if the working value is boolean false (fails if not boolean).
    JumpFalse(InstructionIndex),
    /// Guaranteed fail here to allow late failue of invalid code such as top-level breaks.
    Fail(IrrecoverableError),
    /// Put the current value in as the current working value.
    Primitive(SLIRLiteral),
    /// Return the working value
    Return,
}
impl Instruction {
    fn new_jump_break(n_layers_out: usize) -> Self {
        Self::Jump(InstructionIndex::MAX - n_layers_out * 2)
    }
    fn new_jump_continue(n_layers_out: usize) -> Self {
        Self::Jump(InstructionIndex::MAX - n_layers_out * 2 - 1)
    }
    fn translate(&mut self, delta: usize) {
        fn t(index: InstructionIndex, delta: usize) -> usize {
            if index < InstructionIndex::MAX / 2 {
                index + delta
            } else {
                // break/continue
                index
            }
        }
        *self = match self {
            Self::Jump(i) => Self::Jump(t(*i, delta)),
            Self::JumpFalse(i) => Self::JumpFalse(t(*i, delta)),
            Self::PushScope(i) => Self::PushScope(t(*i, delta)),
            Self::PopScope(i) => Self::PopScope(t(*i, delta)),
            _ => return,
        }
    }
    fn finalize_loop_flow_controls(&mut self, continue_jump_i: usize, break_jump_i: usize) {
        fn t(index: InstructionIndex, continue_jump_i: usize, break_jump_i: usize) -> usize {
            if index < InstructionIndex::MAX / 2 {
                index
            } else if index <= InstructionIndex::MAX - 2 {
                index + 2
            } else if index == InstructionIndex::MAX {
                // break
                break_jump_i
            } else {
                // continue
                continue_jump_i
            }
        }
        *self = match self {
            Self::Jump(i) => Self::Jump(t(*i, continue_jump_i, break_jump_i)),
            Self::JumpFalse(i) => Self::JumpFalse(t(*i, continue_jump_i, break_jump_i)),
            _ => return,
        }
    }
    fn disallow_loop_flow_controls(&mut self, len: usize) {
        fn t(index: InstructionIndex, this: Instruction) -> Instruction {
            if index < InstructionIndex::MAX / 2 {
                this
            } else if index % 2 != 0 {
                // break
                Instruction::Fail(IrrecoverableError::IllegalBreak)
            } else {
                Instruction::Fail(IrrecoverableError::IllegalContinue)
            }
        }
        *self = match self {
            Self::Jump(i) => t(*i, Self::Jump(*i)),
            Self::JumpFalse(i) => t(*i, Self::JumpFalse(*i)),
            _ => return,
        }
    }
}

fn inst_translate(instructions: &mut Vec<Instruction>, delta: usize) {
    for inst in instructions {
        inst.translate(delta);
    }
}

fn concat_instructions<const T: usize>(parts: [Vec<Instruction>; T]) -> Vec<Instruction> {
    let mut out = Vec::with_capacity(parts.iter().map(Vec::len).sum());
    let mut offset = 0;
    for mut part in parts {
        let len = part.len();
        for inst in &mut part {
            inst.translate(offset);
        }
        out.append(&mut part);
        offset += len;
    }
    out
}
fn concat_instructions_no_translate<const T: usize>(
    parts: [Vec<Instruction>; T],
) -> Vec<Instruction> {
    parts.into_iter().flatten().collect()
}
fn build_loop(mut body: Vec<Instruction>) -> Vec<Instruction> {
    const CONTINUE_JUMP_I: usize = 1;
    let break_jump_i = 1 + body.len() + 2;
    let body_offset = 1 + 1;
    for inst in &mut body {
        inst.translate(body_offset);
        inst.finalize_loop_flow_controls(CONTINUE_JUMP_I, break_jump_i)
    }
    [
        vec![Instruction::PushScope(0)],
        body,
        vec![
            Instruction::PopScope(0),
            Instruction::Jump(CONTINUE_JUMP_I),
            Instruction::PopScope(0), // pop after break
        ],
    ]
    .into_iter()
    .flatten()
    .collect()
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
    VoidAsValue,
    VarNotFound,
    VarNotWritable,
    VarNotCallable,
}
enum Voidable<'gc_data> {
    Void,
    Value(Value<'gc_data>),
}
enum ExecOutput<'gc_data> {
    Return(Voidable<'gc_data>),
    Complete(Voidable<'gc_data>),
    Irrecoverable(IrrecoverableError),
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
        crate::language::slir::SLIRExpression::Array(_) => todo!(),
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
        } => todo!("// TODO conditionals"),
        crate::language::slir::SLIRExpression::Loop(block) => {
            let mut context = context.clone();
            context.push_loop_info(LoopInfo {
                label: None, // TODO loop labels
                allow_break_value: true,
            });
            let body = serialize_block_instructions(block, &context);
            build_loop(body)
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
                            vec![Instruction::Jump(InstructionIndex::MAX)],
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
        SLIRExpression::Return(value) => concat_instructions_no_translate([
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
                    is_const: true,
                },
                Instruction::WriteVar(ident),
            ]
        }
        SLIRStatement::VarDeclare {
            ident,
            is_const,
            initial_assignment,
            ..
        } => {
            if let Some(initial_assignment) = initial_assignment {
                concat_instructions_no_translate([
                    serialize_expr_instructions(*initial_assignment, context),
                    vec![
                        Instruction::CreateVar {
                            ident: ident.clone(),
                            is_const,
                        },
                        Instruction::WriteVar(ident),
                    ],
                ])
            } else {
                vec![Instruction::CreateVar { ident, is_const }]
            }
        }
        SLIRStatement::Assign(accessor, expr) => todo!("// TODO variable data access"),
        SLIRStatement::Expr(expr) => serialize_expr_instructions(*expr, context),
    }
}
fn serialize_block_instructions(
    code: SLIRBlock,
    context: &InstructionBuildingContext,
) -> Vec<Instruction> {
    code.0
        .into_iter()
        .flat_map(|it| serialize_statement_instructions(it, context))
        .collect()
}

pub fn serialize_program(code: SLIRBlock) -> Vec<Instruction> {
    serialize_block_instructions(code, &InstructionBuildingContext { loop_infos: vec![] })
}
