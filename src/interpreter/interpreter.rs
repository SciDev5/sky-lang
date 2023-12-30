use std::iter;

use crate::common::IdentInt;

use super::{
    bytecode::{BytecodeModule, Instr, Literal},
    gc::GarbageCollector,
    value::Value,
};

#[derive(Debug)]
enum CallStackFrameId {
    TopLevel,
    Function(IdentInt),
}

struct CallStackFrame<'a> {
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
        if self.instruction_index >= self.code.len() {
            panic!("jump out of bounds");
        }
    }
    fn pop_iv(&mut self) -> Value {
        self.iv_stack
            .pop()
            .expect("intermediate value stack was exhausted")
    }
}

#[derive(Debug)]
pub struct Panic {
    stack: Vec<(CallStackFrameId, usize)>,
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

        match &call_stack_top.code[call_stack_top.instruction_index] {
            Instr::Call { function_id } => {
                let func = &module.functions[*function_id];

                let mut locals: Vec<Option<Value>> =
                    iter::repeat_with(|| None).take(func.locals.len()).collect();
                for (i, v) in call_stack_top
                    .iv_stack
                    .drain(call_stack_top.iv_stack.len() - func.params.len()..)
                    .enumerate()
                {
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
            Instr::ReadProp(_) => todo!(),
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
                call_stack_top.iv_stack.pop();
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
        }

        call_stack_top.instruction_index += 1;
        if call_stack_top.instruction_index == call_stack_top.code.len() {
            // hit end of call code
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
        }
    }
}
