use crate::{
    common::common_module::{CMExpression, CMFunction, CMLiteralValue, CMStruct, CommonModule},
    interpreter::bytecode::Literal,
    parse::fn_lookup::FnRef,
};

use super::bytecode::{BFunction, BStruct, BytecodeModule, Instr};

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

        // placeholder for the proper ScopePop and Jump instructions
        self.instructions.push(Instr::Fail);
        self.instructions.push(Instr::Fail);
    }
    /// Add one placeholder instruction and necessary metadata to the end of this.
    /// Marks this instruction as somewhere a loop will restart from.
    fn push_continue(&mut self, n_layers_out: u8) {
        self.continue_locations
            .push((self.instructions.len(), n_layers_out));

        // placeholder for the proper ScopeReset and Jump instructions
        self.instructions.push(Instr::Fail);
        self.instructions.push(Instr::Fail);
    }
    /// Push a `Jump` instruction, but automatically calculate relative jump index
    /// from an absolute position.
    fn push_jmp_absolute(&mut self, to: usize) {
        let insert_loc = self.instructions.len() as isize;
        self.instructions
            .push(Instr::Jump(to as isize - insert_loc))
    }
    /// Push a `Jump` instruction, but automatically calculate relative jump index
    /// from an absolute position.
    fn push_jmp_absolute_isize(&mut self, to: isize) {
        let insert_loc = self.instructions.len() as isize;
        self.instructions.push(Instr::Jump(to - insert_loc))
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
    fn unwrap_loop_layer(
        &mut self,
        break_jump_target: isize,
        continue_jump_target: isize,
        yield_value: bool,
    ) {
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
            self.instructions[break_loc] = if yield_value {
                Instr::PopScope
            } else {
                Instr::DiscardScope
            };
            self.instructions[break_loc + 1] =
                Instr::Jump(break_jump_target - (break_loc + 1) as isize);
        }
        for continue_loc in continue_locations {
            self.instructions[continue_loc] = Instr::ResetScope;
            self.instructions[continue_loc + 1] =
                Instr::Jump(continue_jump_target - (continue_loc + 1) as isize);
        }
    }
}

pub fn compile_interpreter_bytecode_module(common: CommonModule) -> BytecodeModule {
    BytecodeModule {
        functions: common.functions.into_iter().map(compile_fn).collect(),
        structs: common.structs.into_iter().map(compile_struct).collect(),
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
fn compile_struct(st: CMStruct) -> BStruct {
    BStruct {
        fields: st.fields,
        functions: st.functions,
    }
}
fn compile_block_top(block: Vec<CMExpression>) -> InstrList {
    let mut out = InstrList::new();
    compile_block(block, &mut out, true);
    out
}
fn compile_block(
    mut block: Vec<CMExpression>,
    instructions: &mut InstrList,
    yield_value: bool,
) -> CompileExprResult {
    use CompileExprResult::*;

    if block.is_empty() {
        instructions.push(Instr::PushVoid);
        return Value;
    }
    let last_line = block.pop().unwrap();
    for line in block {
        match compile_expr(line, instructions, false) {
            Value => {}
            Never => {
                return Never;
            }
        }
    }
    compile_expr(last_line, instructions, yield_value)
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
fn compile_expr(
    expr: CMExpression,
    instructions: &mut InstrList,
    yield_value: bool,
) -> CompileExprResult {
    use CompileExprResult::*;

    // ValueOrVoid -> should always end with one additional value in the iv stack coressponding
    // Never -> doesn't matter because the iv stack gets discarded
    match expr {
        CMExpression::Void => {
            if yield_value {
                instructions.push(Instr::PushVoid);
            }
            Value
        }
        CMExpression::Fail => {
            instructions.push(Instr::Fail);
            Never
        }
        CMExpression::FailAfter(exprs) => {
            for expr in exprs {
                match compile_expr(expr, instructions, yield_value) {
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
            match compile_expr(*object, instructions, true) {
                Never => return Never,
                _ => { /* ok (+1 iv) */ }
            };
            match compile_expr(*value, instructions, true) {
                Never => return Never,
                _ => { /* ok (+1 iv) */ }
            };
            instructions.push(Instr::WriteProp(property));
            if yield_value {
                instructions.push(Instr::PushVoid);
            }
            Value
        }
        CMExpression::AssignVar { ident, value } => {
            match compile_expr(*value, instructions, true) {
                Never => return Never,
                _ => { /* ok (+1 iv) */ }
            };
            instructions.push(Instr::WriteLocal(ident));
            if yield_value {
                instructions.push(Instr::PushVoid);
            }
            Value
        }
        CMExpression::ReadProperty {
            expr,
            property_ident,
        } => {
            match compile_expr(*expr, instructions, true) {
                Never => return Never,
                _ => { /* ok (+1 iv) */ }
            };
            instructions.push(Instr::ReadProp(property_ident));
            if !yield_value {
                instructions.push(Instr::Discard);
            }
            Value
        }
        CMExpression::ReadVar { ident } => {
            instructions.push(Instr::ReadLocal(ident));
            if !yield_value {
                instructions.push(Instr::Discard);
            }
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
                    return compile_expr(argument, instructions, yield_value);
                }
                FnRef::ModuleFunction(function_id) => {
                    for expr in arguments {
                        match compile_expr(expr, instructions, true) {
                            Never => return Never,
                            _ => { /* ok (+1 iv) */ }
                        };
                    }
                    instructions.push(Instr::Call { function_id });
                }
                FnRef::Intrinsic1(function_id) => {
                    let [argument] = <[_; 1]>::try_from(arguments).expect("there was not 1 argument for FnRef::Intrinsic1, this means fn_lookup has an issue");
                    match compile_expr(argument, instructions, true) {
                        Never => return Never,
                        _ => { /* ok (+1 iv) */ }
                    };
                    instructions.push(Instr::CallIntrinsic1 { function_id });
                }
                FnRef::Intrinsic2(function_id) => {
                    let arguments = <[_; 2]>::try_from(arguments).expect("there were not 2 arguments for FnRef::Intrinsic2, this means fn_lookup has an issue");
                    for expr in arguments {
                        match compile_expr(expr, instructions, true) {
                            Never => return Never,
                            _ => { /* ok (+1 iv) */ }
                        };
                    }
                    instructions.push(Instr::CallIntrinsic2 { function_id });
                }
                FnRef::Intrinsic(function_id) => {
                    for expr in arguments {
                        match compile_expr(expr, instructions, true) {
                            Never => return Never,
                            _ => { /* ok (+1 iv) */ }
                        };
                    }
                    instructions.push(Instr::CallIntrinsicN { function_id });
                }
            }
            Value
        }
        CMExpression::LiteralValue(literal) => {
            if yield_value {
                // yield nothing if no yield because literals can't have side effects.
                instructions.push(Instr::Literal(match literal {
                    CMLiteralValue::Int(v) => Literal::Int(v),
                    CMLiteralValue::Float(v) => Literal::Float(v),
                    CMLiteralValue::Complex(v) => Literal::Complex(v),
                    CMLiteralValue::Bool(v) => Literal::Bool(v),
                    CMLiteralValue::String(v) => Literal::String(v),
                })); // +1 iv
            }
            Value
        }
        CMExpression::LiteralArray(_) => todo!(),
        CMExpression::LiteralFunctionRef { function_id } => todo!(),
        CMExpression::LiteralStructInit {
            ident: _,
            data,
            assign_to,
        } => {
            for expr in data {
                match compile_expr(expr, instructions, yield_value) {
                    Never => return Never,
                    _ => { /* ok (+1 iv if yield_value, else +0 iv) */ }
                }
            }
            if yield_value {
                instructions.push(Instr::LiteralInitStruct(assign_to));
            } else {
                // do nothing, nothing to consume
            }
            Value
        }
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
            condition_never = compile_expr(*condition, &mut condition_out, true).is_never();
            if condition_never {
                instructions.append(&mut condition_out);
                return Never;
            } else {
                match compile_block(block, &mut block_out, yield_value) {
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

                condition_never |=
                    compile_expr(condition, &mut elif_condition_out, true).is_never();
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
                match compile_block(block, &mut elif_block_out, yield_value) {
                    Value => {
                        blocks_all_never = false;
                    }
                    Never => {}
                }
                elifs_out.push((elif_condition_out, elif_block_out));
            }

            let mut else_block_out = if let Some(else_block) = else_block {
                let mut else_out = InstrList::new();
                match compile_block(else_block, &mut else_out, yield_value) {
                    Value => {
                        blocks_all_never = false;
                    }
                    Never => {}
                }
                Some(else_out)
            } else if yield_value {
                let mut else_out = InstrList::new();
                else_out.push(Instr::PushVoid);
                blocks_all_never = false;
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

            if blocks_all_never && else_block_out.is_some() {
                Never
            } else {
                Value
            }
        }
        CMExpression::Loop { block, is_infinite } => {
            let mut loop_code = InstrList::new();
            compile_block(block, &mut loop_code, false);

            // pre-loop push scope to allow break/continue not to leak memory into the iv stack
            instructions.push(Instr::PushScope);

            // loop end buffer to reset the loop
            // (start is at the beginning of the block (after the PushScope))
            loop_code.push_jmp_absolute(0);

            // write the loop code to the instructions list
            loop_code.unwrap_loop_layer(loop_code.len() as isize, 0, yield_value);
            instructions.append(&mut loop_code);

            if is_infinite {
                Never
            } else {
                Value
            }
        }
        CMExpression::LoopFor {
            loop_var,
            iterable,
            block,
            else_block,
        } => todo!(),
        CMExpression::LoopWhile {
            condition,
            block,
            else_block,
        } => {
            let mut condition_code = InstrList::new();
            compile_expr(*condition, &mut condition_code, true);
            let mut loop_code = InstrList::new();
            compile_block(block, &mut loop_code, false);
            let mut else_code = if let Some((else_block, _)) = else_block {
                let mut else_code = InstrList::new();
                compile_block(else_block, &mut else_code, yield_value);
                Some(else_code)
            } else {
                None
            };

            // break if the condition is false
            condition_code.push_jmpiffalse_absolute(condition_code.len() + loop_code.len() + 2); // jump to point C

            // reset the loop to the condition at the end
            loop_code.push_jmp_absolute_isize(-(condition_code.len() as isize)); // jump to point B

            // write the loop code to the instructions list
            loop_code.unwrap_loop_layer(
                loop_code.len() as isize
                    + if let Some(else_code) = &else_code {
                        else_code.len() as isize
                    } else if yield_value {
                        1
                    } else {
                        0
                    }, // jump to point D
                -(condition_code.len() as isize), // jump to point B
                yield_value,
            );

            //// Arrange all the instructions into the output instruction list
            // A
            // pre-loop push scope to allow break/continue not to leak memory into the iv stack
            instructions.push(Instr::PushScope);
            // B
            instructions.append(&mut condition_code);
            instructions.append(&mut loop_code);
            // C
            // instructions.push(Instr::DiscardScope);

            if let Some(mut else_code) = else_code {
                // instructions.push(Instr::Jump((else_code.len() + 1) as isize)); // jump to point E
                //                                                                 // D
                instructions.append(&mut else_code);
            } else if yield_value {
                instructions.push(Instr::PushVoid);
            }
            // D

            Value
        }
        CMExpression::LoopBreak(value) => {
            let res = match value {
                Some(value) => compile_expr(*value, instructions, true),
                None => {
                    instructions.push(Instr::PushVoid);
                    Value
                }
            };
            if !res.is_never() {
                instructions.push_break(0);
            }
            Never
        }
        CMExpression::LoopContinue => {
            instructions.push_continue(0);
            Never
        }
        CMExpression::Return(value) => {
            match value {
                Some(value) => match compile_expr(*value, instructions, true) {
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
