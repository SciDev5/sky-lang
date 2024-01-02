use crate::{
    common::common_module::{CMClass, CMExpression, CMFunction, CMLiteralValue, CommonModule},
    interpreter::bytecode::Literal,
    parse::fn_lookup::FnRef,
};

use super::bytecode::{BClass, BFunction, BytecodeModule, Instr};

struct InstrList {
    instructions: Vec<Instr>,
    /// `[(location, n_layers_out)]`
    break_locations: Vec<(usize, u8)>,
    /// `[(location, n_layers_out)]`
    continue_locations: Vec<(usize, u8)>,
}
impl InstrList {
    fn new() -> Self {
        Self {
            instructions: vec![],
            break_locations: vec![],
            continue_locations: vec![],
        }
    }
    fn unwrap_instruction_list(self) -> Vec<Instr> {
        if !self.break_locations.is_empty() || !self.continue_locations.is_empty() {
            panic!("illegal top level break/contine not caught and removed when generating CommonModule");
        } else {
            self.instructions
        }
    }
    /// Add one instruction to the end of this.
    fn push(&mut self, instruction: Instr) {
        self.instructions.push(instruction);
    }
    /// Current number of instructions.
    fn len(&self) -> usize {
        self.instructions.len()
    }
    /// Add one placeholder instruction and necessary metadata to the end of this.
    /// Marks this instruction as somewhere a loop will break from.
    fn push_break(&mut self, n_layers_out: u8) {
        self.break_locations
            .push((self.instructions.len(), n_layers_out));
        // fail if we can't get rid of it
        self.instructions.push(Instr::Fail);
    }
    /// Add one placeholder instruction and necessary metadata to the end of this.
    /// Marks this instruction as somewhere a loop will restart from.
    fn push_continue(&mut self, n_layers_out: u8) {
        self.continue_locations
            .push((self.instructions.len(), n_layers_out));
        // fail if we can't get rid of it
        self.instructions.push(Instr::Fail);
    }
    /// Push a `Jump` instruction, but automatically calculate relative jump index
    /// from an absolute position.
    fn push_jmp_absolute(&mut self, to: usize) {
        let insert_loc = self.instructions.len() as isize;
        self.instructions
            .push(Instr::Jump(to as isize - insert_loc))
    }
    /// Push a `JumpFalse` instruction, but automatically calculate relative jump index
    /// from an absolute position.
    fn push_jmpiffalse_absolute(&mut self, to: usize) {
        let insert_loc = self.instructions.len() as isize;
        self.instructions
            .push(Instr::JumpIfFalse(to as isize - insert_loc))
    }
    /// Move all instructions and metadata from `other` to `self`, leaving `other` empty.
    fn append(&mut self, other: &mut Self) {
        let index_offset = self.instructions.len();
        self.instructions.append(&mut other.instructions);
        for (loc, _) in &mut other.break_locations {
            *loc += index_offset;
        }
        for (loc, _) in &mut other.continue_locations {
            *loc += index_offset;
        }
        self.break_locations.append(&mut other.break_locations);
        self.continue_locations
            .append(&mut other.continue_locations);
    }
    /// Process metadata of this to add `break` and `continue` jumps corresponding to the loop we are building.
    fn unwrap_loop_layer(&mut self, break_jump_target: isize, continue_jump_target: isize) {
        fn get_locations_to_modify(locations: &mut Vec<(usize, u8)>) -> Vec<usize> {
            let (depth_zero, depth_nonzero): (Vec<_>, Vec<_>) =
                locations.into_iter().partition(|(_, depth)| *depth == 0);
            let to_modify = depth_zero.into_iter().map(|(loc, _)| *loc).collect();
            *locations = depth_nonzero
                .into_iter()
                .map(|(loc, depth)| (*loc, *depth - 1))
                .collect::<Vec<_>>();
            return to_modify;
        }

        let break_locations = get_locations_to_modify(&mut self.break_locations);
        let continue_locations = get_locations_to_modify(&mut self.continue_locations);

        for break_loc in break_locations {
            self.instructions[break_loc] = Instr::Jump(break_jump_target - break_loc as isize);
        }
        for continue_loc in continue_locations {
            self.instructions[continue_loc] =
                Instr::Jump(continue_jump_target - continue_loc as isize);
        }
    }
}

pub fn compile_interpreter_bytecode_module(common: CommonModule) -> BytecodeModule {
    BytecodeModule {
        functions: common.functions.into_iter().map(compile_fn).collect(),
        classes: common.classes.into_iter().map(compile_class).collect(),
        top_level: (
            compile_block_top(common.top_level.0).unwrap_instruction_list(),
            common
                .top_level
                .1
                .into_iter()
                .map(|var_info| var_info.ty)
                .collect(),
        ),
    }
}

fn compile_fn(func: CMFunction) -> BFunction {
    BFunction {
        params: func.params,
        locals: func.locals.into_iter().map(|it| it.ty).collect(),
        code: compile_block_top(func.block).unwrap_instruction_list(),
    }
}
fn compile_class(class: CMClass) -> BClass {
    BClass {
        fields: class.fields.into_values().collect(),
        functions: class.functions.into_values().flatten().collect(),
    }
}
fn compile_block_top(block: Vec<CMExpression>) -> InstrList {
    let mut out = InstrList::new();
    compile_block(block, &mut out);
    out
}
fn compile_block(mut block: Vec<CMExpression>, instructions: &mut InstrList) -> CompileExprResult {
    use CompileExprResult::*;

    if block.is_empty() {
        instructions.push(Instr::PushVoid);
        return Value;
    }
    let last_line = block.pop().unwrap();
    for line in block {
        match compile_expr(line, instructions) {
            Value => {
                instructions.push(Instr::Discard);
            }
            Never => {
                return Never;
            }
        }
    }
    compile_expr(last_line, instructions)
    // do not discard value, forward everything from the last line
}

enum CompileExprResult {
    Value,
    Never,
}
impl CompileExprResult {
    fn is_never(self) -> bool {
        matches!(self, Self::Never)
    }
}
fn compile_expr(expr: CMExpression, instructions: &mut InstrList) -> CompileExprResult {
    use CompileExprResult::*;

    // ValueOrVoid -> should always end with one additional value in the iv stack coressponding
    // Never -> doesn't matter because the iv stack gets discarded
    match expr {
        CMExpression::Void => {
            instructions.push(Instr::PushVoid);
            Value
        }
        CMExpression::Fail => {
            instructions.push(Instr::Fail);
            Never
        }
        CMExpression::FailAfter(exprs) => {
            for expr in exprs {
                match compile_expr(expr, instructions) {
                    Never => return Never,
                    _ => { /* ok */ }
                };
            }
            instructions.push(Instr::Fail);
            Never
        }
        CMExpression::AssignProperty {
            object,
            property,
            value,
        } => {
            match compile_expr(*object, instructions) {
                Never => return Never,
                _ => { /* ok (+1 iv) */ }
            };
            match compile_expr(*value, instructions) {
                Never => return Never,
                _ => { /* ok (+1 iv) */ }
            };
            instructions.push(Instr::WriteProp(property));
            Value
        }
        CMExpression::AssignVar { ident, value } => {
            match compile_expr(*value, instructions) {
                Never => return Never,
                _ => { /* ok (+1 iv) */ }
            };
            instructions.push(Instr::WriteLocal(ident));
            Value
        }
        CMExpression::ReadProperty {
            expr,
            property_ident,
        } => {
            match compile_expr(*expr, instructions) {
                Never => return Never,
                _ => { /* ok (+1 iv) */ }
            };
            instructions.push(Instr::ReadProp(property_ident));
            Value
        }
        CMExpression::ReadVar { ident } => {
            instructions.push(Instr::ReadLocal(ident));
            Value
        }
        CMExpression::Call {
            function_id,
            arguments,
            always_inline,
            inlined_lambdas,
        } => {
            if always_inline || inlined_lambdas.is_some() {
                todo!();
            }
            match function_id {
                FnRef::Identity => {
                    let [argument] = <[_; 1]>::try_from(arguments).expect("there was not 1 argument for FnRef::Identity, this means fn_lookup has an issue");
                    compile_expr(argument, instructions)
                }
                FnRef::ModuleFunction(function_id) => {
                    for expr in arguments {
                        match compile_expr(expr, instructions) {
                            Never => return Never,
                            _ => { /* ok (+1 iv) */ }
                        };
                    }
                    instructions.push(Instr::Call { function_id });
                    Value
                }
                FnRef::Intrinsic1(function_id) => {
                    let [argument] = <[_; 1]>::try_from(arguments).expect("there was not 1 argument for FnRef::Intrinsic1, this means fn_lookup has an issue");
                    match compile_expr(argument, instructions) {
                        Never => return Never,
                        _ => { /* ok (+1 iv) */ }
                    };
                    instructions.push(Instr::CallIntrinsic1 { function_id });
                    Value
                }
                FnRef::Intrinsic2(function_id) => {
                    let arguments = <[_; 2]>::try_from(arguments).expect("there were not 2 arguments for FnRef::Intrinsic2, this means fn_lookup has an issue");
                    for expr in arguments {
                        match compile_expr(expr, instructions) {
                            Never => return Never,
                            _ => { /* ok (+1 iv) */ }
                        };
                    }
                    instructions.push(Instr::CallIntrinsic2 { function_id });
                    Value
                }
                FnRef::Intrinsic(function_id) => {
                    for expr in arguments {
                        match compile_expr(expr, instructions) {
                            Never => return Never,
                            _ => { /* ok (+1 iv) */ }
                        };
                    }
                    instructions.push(Instr::CallIntrinsicN { function_id });
                    Value
                }
            }
        }
        CMExpression::LiteralValue(literal) => {
            instructions.push(Instr::Literal(match literal {
                CMLiteralValue::Int(v) => Literal::Int(v),
                CMLiteralValue::Float(v) => Literal::Float(v),
                CMLiteralValue::Complex(v) => Literal::Complex(v),
                CMLiteralValue::Bool(v) => Literal::Bool(v),
                CMLiteralValue::String(v) => Literal::String(v),
            })); // +1 iv
            Value
        }
        CMExpression::LiteralArray(_) => todo!(),
        CMExpression::LiteralFunctionRef { function_id } => todo!(),
        CMExpression::Closure {
            closure_function_id,
        } => todo!(),
        CMExpression::Conditional {
            condition,
            block,
            elifs,
            else_block,
        } => {
            let mut condition_never = false;

            fn assemble(
                instructions: &mut InstrList,
                condition: &mut InstrList,
                block: &mut InstrList,
                elifs: &mut Vec<(InstrList, InstrList)>,
                else_block: &mut Option<InstrList>,
            ) {
                /*
                Compiled if statement structure:
                        [condition]
                        jmp_false( [next_condition] or else END )
                        [block]
                        { for (elif_condition, elif_block) in elifs ->
                            jmp(END)
                            [elif_condition]
                            jmp_false( [next_condition] or else END )
                            [elif_block]
                        }
                        { if Some(else_block) ->
                            jmp(END)
                            [else_block]
                        }
                END:    ...
                */

                let mut skip_to_locations = vec![];
                let mut end_location = instructions.len();
                {
                    // main if
                    end_location += condition.len() + 1 /* conditional jump */ + block.len();
                }
                for (elif_condition, elif_block) in elifs.iter_mut() {
                    end_location += 1 /* preceeding jmp(END) */;
                    skip_to_locations.push(end_location);
                    end_location +=
                        elif_condition.len() + 1 /* conditional jump */  + elif_block.len();
                }
                if let Some(else_block) = else_block {
                    end_location += 1 /* preceeding jmp(END) */;
                    skip_to_locations.push(end_location);
                    end_location += else_block.len();
                }
                skip_to_locations.push(end_location);
                let mut skip_to_locations = skip_to_locations.into_iter();

                // do actual instruction serialization now
                {
                    // main if
                    instructions.append(condition);
                    instructions.push_jmpiffalse_absolute(skip_to_locations.next().unwrap());
                    instructions.append(block);
                }
                for (elif_condition, elif_block) in elifs {
                    instructions.push_jmp_absolute(end_location);
                    instructions.append(elif_condition);
                    instructions.push_jmpiffalse_absolute(skip_to_locations.next().unwrap());
                    instructions.append(elif_block);
                }
                if let Some(else_block) = else_block {
                    instructions.push_jmp_absolute(end_location);
                    instructions.append(else_block);
                }
            }

            let mut condition_out = InstrList::new();
            let mut block_out = InstrList::new();
            let mut blocks_all_never = true;
            condition_never = compile_expr(*condition, &mut condition_out).is_never();
            if condition_never {
                instructions.append(&mut condition_out);
                return Never;
            } else {
                match compile_block(block, &mut block_out) {
                    Value => {
                        blocks_all_never = false;
                    }
                    Never => {}
                }
            }
            let mut elifs_out = vec![];
            for (condition, block) in elifs {
                let mut elif_condition_out = InstrList::new();
                let mut elif_block_out = InstrList::new();

                condition_never = compile_expr(condition, &mut elif_condition_out).is_never();
                if condition_never {
                    let mut else_block = Some(elif_condition_out);
                    // ^ don't worry about the return value because we know it won't return
                    assemble(
                        instructions,
                        &mut condition_out,
                        &mut block_out,
                        &mut elifs_out,
                        &mut else_block,
                    );
                    return Never;
                }
                match compile_block(block, &mut elif_block_out) {
                    Value => {
                        blocks_all_never = false;
                    }
                    Never => {}
                }
                elifs_out.push((elif_condition_out, elif_block_out));
            }

            let mut else_block_out = if let Some(else_block) = else_block {
                let mut else_out = InstrList::new();
                match compile_block(else_block, &mut else_out) {
                    Value => {
                        blocks_all_never = false;
                    }
                    Never => {}
                }
                Some(else_out)
            } else {
                None
            };

            assemble(
                instructions,
                &mut condition_out,
                &mut block_out,
                &mut elifs_out,
                &mut else_block_out,
            );

            if blocks_all_never {
                Never
            } else {
                Value
            }
        }
        CMExpression::Loop { block } => todo!(),
        CMExpression::LoopFor {
            loop_var,
            iterable,
            block,
        } => todo!(),
        CMExpression::LoopBreak(_) => todo!(),
        CMExpression::LoopContinue => todo!(),
        CMExpression::Return(value) => {
            match value {
                Some(value) => match compile_expr(*value, instructions) {
                    Value => { /* ok (+1 iv) */ }
                    Never => return Never,
                },
                None => {
                    instructions.push(Instr::PushVoid); // +1 iv
                }
            };
            // net +1 iv
            instructions.push(Instr::Return); // consumes 1 iv
            Never
        }
    }
}
