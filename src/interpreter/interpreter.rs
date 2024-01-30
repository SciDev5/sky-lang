use std::iter;

use crate::common::IdentInt;

use super::{
    bytecode::{BytecodeModule, Instr, Literal},
    gc::GarbageCollector,
    intrinsics::{apply_intrinsic, apply_intrinsic1, apply_intrinsic2},
    value::Value,
};

#[derive(Debug)]
enum CallStackFrameId {
    TopLevel,
    Function(IdentInt),
}

pub struct CallStackFrame<'a> {
    id: CallStackFrameId,
    code: &'a Vec<Instr>,
    instruction_index: usize,
    scope_ivlen_stack: Vec<usize>,
    locals: Vec<Option<Value>>,
    /// Intermediate value stack (for intermediate values)
    iv_stack: Vec<Value>,
}
impl<'a> CallStackFrame<'a> {
    fn jump(&mut self, delta: isize) {
        self.instruction_index = self
            .instruction_index
            .checked_add_signed(delta)
            .expect("jump out of bounds");
        if self.instruction_index > self.code.len() {
            // not using >= here is intentional. if self.instruction_index == self.code.len(), then
            // that signifies we jump to the end of the block without running the last instruction.
            panic!("jump out of bounds");
        }
    }
    fn pop_iv(&mut self) -> Value {
        self.iv_stack
            .pop()
            .expect("intermediate value stack was exhausted")
    }
    fn pop_iv_many(&mut self, len: usize) -> impl Iterator<Item = Value> + '_ {
        self.iv_stack.drain(self.iv_stack.len() - len..)
    }
}

pub struct PanicGen;
impl PanicGen {
    fn to_panic(self, call_stack: Vec<CallStackFrame>) -> Panic {
        Panic::new(call_stack)
    }
}

#[derive(Debug)]
pub struct Panic {
    stack: Vec<(CallStackFrameId, usize)>,
}
impl Panic {
    pub fn new(call_stack: Vec<CallStackFrame>) -> Self {
        Panic {
            stack: call_stack
                .into_iter()
                .map(|frame| (frame.id, frame.instruction_index))
                .collect(),
        }
    }
}

pub fn execute(module: &BytecodeModule, gc: &mut GarbageCollector) -> Result<Value, Panic> {
    let mut call_stack = vec![CallStackFrame {
        id: CallStackFrameId::TopLevel,
        code: &module.top_level.0,
        instruction_index: 0,
        iv_stack: vec![],
        scope_ivlen_stack: vec![],
        locals: iter::repeat_with(|| None)
            .take(module.top_level.1.len())
            .collect(),
    }];

    loop {
        let call_stack_top = call_stack
            .last_mut()
            .expect("unnaturally exhausted call stack");

        // Check if we've hit the end of the call's instructions.
        if call_stack_top.instruction_index == call_stack_top.code.len() {
            let call_eval_value = call_stack_top
                .iv_stack
                .pop()
                .expect("missing call block eval value somehow");
            call_stack.pop();
            match call_stack.last_mut() {
                Some(call_stack_top) => {
                    // function return, push return value to the iv stack and increment instruction index
                    call_stack_top.iv_stack.push(call_eval_value);
                    call_stack_top.instruction_index += 1;
                }
                None => {
                    // top level finished
                    return Ok(call_eval_value);
                }
            }
            continue;
        }

        // Run the next instruction.
        match &call_stack_top.code[call_stack_top.instruction_index] {
            Instr::Call { function_id } => {
                let func = &module.functions[*function_id];

                let mut locals: Vec<Option<Value>> =
                    iter::repeat_with(|| None).take(func.locals.len()).collect();
                for (i, v) in call_stack_top.pop_iv_many(func.params.len()).enumerate() {
                    locals[i] = Some(v);
                }
                call_stack.push(CallStackFrame {
                    id: CallStackFrameId::Function(*function_id),
                    code: &func.code,
                    instruction_index: 0,
                    scope_ivlen_stack: vec![],
                    iv_stack: vec![],
                    locals,
                });
                continue;
            }
            Instr::CallIntrinsic1 { function_id } => {
                let value = call_stack_top.pop_iv();
                let ret = apply_intrinsic1(*function_id, value);
                call_stack_top.iv_stack.push(ret);
            }
            Instr::CallIntrinsic2 { function_id } => {
                // rhs is popped first because it is created second
                let value_rhs = call_stack_top.pop_iv();
                let value_lhs = call_stack_top.pop_iv();
                let ret = match apply_intrinsic2(*function_id, value_lhs, value_rhs) {
                    Ok(v) => v,
                    Err(panic_gen) => return Err(panic_gen.to_panic(call_stack)),
                };
                call_stack_top.iv_stack.push(ret);
            }
            Instr::CallIntrinsicN { function_id } => {
                let args = call_stack_top.pop_iv_many(function_id.n_args()).collect();
                let ret = match apply_intrinsic(*function_id, args) {
                    Ok(v) => v,
                    Err(panic_gen) => return Err(panic_gen.to_panic(call_stack)),
                };
                call_stack_top.iv_stack.push(ret);
            }
            Instr::LiteralFunctionRef { function_id } => todo!(),
            Instr::CallDyn => todo!(),
            Instr::Return => {
                let return_value = call_stack_top.pop_iv();
                call_stack.pop();
                match call_stack.last_mut() {
                    Some(call_stack_top) => {
                        // setup the return
                        call_stack_top.iv_stack.push(return_value);
                        call_stack_top.instruction_index += 1;
                    }
                    None => {
                        panic!("illegal top-level return");
                    }
                }
                continue;
            }
            Instr::PushScope => {
                call_stack_top
                    .scope_ivlen_stack
                    .push(call_stack_top.iv_stack.len());
            }
            Instr::PopScope => {
                let top = call_stack_top.pop_iv();
                let reset_iv_stack_to_len = call_stack_top
                    .scope_ivlen_stack
                    .pop()
                    .expect("scope stack was drained");
                call_stack_top.iv_stack.drain(reset_iv_stack_to_len..);
                call_stack_top.iv_stack.push(top);
            }
            Instr::DiscardScope => {
                let reset_iv_stack_to_len = call_stack_top
                    .scope_ivlen_stack
                    .pop()
                    .expect("scope stack was drained");
                call_stack_top.iv_stack.drain(reset_iv_stack_to_len..);
            }
            Instr::ResetScope => {
                let reset_iv_stack_to_len = *call_stack_top
                    .scope_ivlen_stack
                    .last()
                    .expect("scope stack was drained");
                call_stack_top.iv_stack.drain(reset_iv_stack_to_len..);
            }
            Instr::ReadProp(id) => {
                let obj = call_stack_top.pop_iv();
                let accessed = match obj {
                    Value::Object(props) => props[*id].clone(),
                    _ => panic!("ReadProp on non-struct Value")
                };
                call_stack_top.iv_stack.push(accessed);
            }
            Instr::WriteProp(_) => todo!(),
            Instr::ReadLocal(id) => {
                call_stack_top.iv_stack.push(
                    call_stack_top.locals[*id]
                        .clone()
                        .expect("var read before assignment"),
                );
            }
            Instr::WriteLocal(id) => {
                call_stack_top.locals[*id] = Some(call_stack_top.pop_iv());
            }
            Instr::Jump(delta) => {
                call_stack_top.jump(*delta);
                continue;
            }
            Instr::JumpIfFalse(delta) => {
                if match call_stack_top.pop_iv() {
                    Value::Bool(v) => v == false,
                    _ => panic!("non boolean jump condition"),
                } {
                    call_stack_top.jump(*delta);
                    continue;
                }
            }
            Instr::PushVoid => {
                call_stack_top.iv_stack.push(Value::Void);
            }
            Instr::Discard => {
                call_stack_top.pop_iv();
            }
            Instr::Fail => {
                return Err(Panic {
                    stack: call_stack
                        .into_iter()
                        .rev()
                        .map(|frame| (frame.id, frame.instruction_index))
                        .collect(),
                });
            }
            Instr::Literal(literal) => call_stack_top.iv_stack.push(match literal {
                Literal::Int(v) => Value::Int(*v),
                Literal::Float(v) => Value::Float(*v),
                Literal::Complex(v) => Value::Complex(*v),
                Literal::Bool(v) => Value::Bool(*v),
                Literal::String(v) => Value::String(v.clone()),
            }),
            Instr::LiteralInitStruct(assign_to) => {
                // assign_to is expected to be correct beforehand
                let mut values = std::iter::repeat_with(|| Value::Void)
                    .take(assign_to.len())
                    .collect::<Vec<_>>();
                for (value_in, assign_to_i) in call_stack_top
                    .pop_iv_many(assign_to.len())
                    .zip(assign_to.iter())
                {
                    values[*assign_to_i] = value_in;
                }
                call_stack_top.iv_stack.push(Value::Object(values));
            }
        }

        call_stack_top.instruction_index += 1;
    }
}
