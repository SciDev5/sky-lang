use crate::{
    common::common_module::{CMClass, CMExpression, CMFunction, CMLiteralValue, CommonModule, CMCfg},
    interpreter::bytecode::Literal,
};

use super::{bytecode::{BClass, BFunction, BytecodeModule, Instr}, intrinsics::InterpreterIntrinsicFn};

#[derive(Debug, Clone, Copy)]
pub struct InterpreterCfg;
impl CMCfg for InterpreterCfg {
    type IntrinsicFnRef = InterpreterIntrinsicFn;
}

pub fn compile_interpreter_bytecode_module(common: CommonModule<InterpreterCfg>) -> BytecodeModule {
    BytecodeModule {
        functions: common.functions.into_iter().map(compile_fn).collect(),
        classes: common.classes.into_iter().map(compile_class).collect(),
        top_level: (
            compile_block_top(common.top_level.0),
            common
                .top_level
                .1
                .into_iter()
                .map(|var_info| var_info.ty)
                .collect(),
        ),
    }
}

fn compile_fn(func: CMFunction<InterpreterCfg>) -> BFunction {
    BFunction {
        params: func.params,
        locals: func.locals.into_iter().map(|it| it.ty).collect(),
        code: compile_block_top(func.block),
    }
}
fn compile_class(class: CMClass) -> BClass {
    BClass {
        fields: class.fields.into_values().collect(),
        functions: class.functions.into_values().flatten().collect(),
    }
}
fn compile_block_top(block: Vec<CMExpression<InterpreterCfg>>) -> Vec<Instr> {
    let mut out = vec![];
    compile_block(block, &mut out);
    out
}
fn compile_block(mut block: Vec<CMExpression<InterpreterCfg>>, instructions: &mut Vec<Instr>) -> CompileExprResult {
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
fn compile_expr(block: CMExpression<InterpreterCfg>, instructions: &mut Vec<Instr>) -> CompileExprResult {
    use CompileExprResult::*;

    // ValueOrVoid -> should always end with one additional value in the iv stack coressponding
    // Never -> doesn't matter because the iv stack gets discarded
    match block {
        CMExpression::Void => {
            instructions.push(Instr::PushVoid);
            Value
        }
        CMExpression::Fail => {
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
        } => todo!(),
        CMExpression::CallIntrinsic {
            function_ref: function_id,
            arguments,
        } => todo!(),
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
        } => todo!(),
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
