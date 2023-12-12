use std::collections::HashMap;

use crate::language::{ast::{ASTBlock, ASTExpression, ASTTypesFull, ASTTypesIncomplete}, ops::SLOperator};

use super::{data::Type, interpreter::Identifier};

#[derive(Debug, Clone)]
struct ScopeContext {
    // var_lookup: 
}

pub fn solve_types_block(ast: ASTBlock<ASTTypesIncomplete>) -> ASTBlock<ASTTypesFull> {
    if ast.0.len() == 1 {
        let expr = solve_types_expr(ast.0[0].clone());
        let ty = expr.eval_ty();
        ASTBlock(vec![expr], ty)
    } else {
        todo!("// TODO type elision with blocks with more than one expression")
    }
}
pub fn solve_types_expr(ast: ASTExpression<ASTTypesIncomplete>) -> ASTExpression<ASTTypesFull> {
    match ast {
        ASTExpression::VarDeclare {
            doc_comment,
            ident,
            writable,
            initial_assignment,
            ty,
        } => todo!(),
        ASTExpression::Assign(_, _) => todo!(),
        ASTExpression::Read(_, _) => todo!(),
        ASTExpression::ReadFunc(_, _, _) => todo!(),
        ASTExpression::Call {
            callable,
            arguments,
            output_ty,
        } => todo!(),
        ASTExpression::Index {
            expr,
            indices,
            output_ty,
        } => todo!(),
        ASTExpression::PropertyAccess {
            expr,
            property_ident,
            property_id,
            output_ty,
        } => todo!(),
        ASTExpression::CallAssociated {
            expr,
            property_ident,
            property_id,
            arguments,
            output_ty,
        } => todo!(),
        ASTExpression::Literal(v) => ASTExpression::Literal(v),
        ASTExpression::Range { start, step, end } => todo!(),
        ASTExpression::Array(_) => todo!(),
        ASTExpression::AnonymousFunction { params, block } => todo!(),
        
        ASTExpression::BinaryOp(SLOperator::Plus, a, b, _) => {
            // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! PLACEHOLDER !!!!!!!!!!!!!!!!! //
            println!("WARNING: USING PLACEHOLDER CODE!");
            let a = Box::new(solve_types_expr(*a));
            let b = Box::new(solve_types_expr(*b));
            let ty = a.eval_ty().expect("// TODO handle invalid voids in types");
            ASTExpression::BinaryOp(SLOperator::Plus, a, b, ty)
        },
        ASTExpression::BinaryOp(SLOperator::Equal, a, b, _) => {
            // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! PLACEHOLDER !!!!!!!!!!!!!!!!! //
            println!("WARNING: USING PLACEHOLDER CODE!");
            let a = Box::new(solve_types_expr(*a));
            let b = Box::new(solve_types_expr(*b));
            ASTExpression::BinaryOp(SLOperator::Equal, a, b, Type::Bool)
        },
        ASTExpression::BinaryOp(_, _, _, _) => todo!(),

        ASTExpression::UnaryOp(_, _, _) => todo!(),
        ASTExpression::Conditional {
            condition,
            block,
            elifs,
            else_block,
            output_ty,
        } => {
            let condition = Box::new(solve_types_expr(*condition));
            let block = solve_types_block(block);
            let elifs = elifs
                .into_iter()
                .map(|(condition, block)| (solve_types_expr(condition), solve_types_block(block)))
                .collect::<Vec<_>>();
            let else_block = else_block.map(|it| solve_types_block(it));

            let mut output_ty = block.1.clone();
            if !matches!(condition.eval_ty(), Some(Type::Bool)) {
                todo_type_error();
            }
            for (condition, block) in &elifs {
                if !matches!(condition.eval_ty(), Some(Type::Bool)) {
                    todo_type_error();
                }
                output_ty = Type::voidable_or(output_ty, block.1.clone(), true); // TODO contextual non-void requirement for better error localization.
            }
            if let Some(else_block) = &else_block {
                output_ty = Type::voidable_or(output_ty, else_block.1.clone(), true);
                // TODO contextual non-void requirement for better error localization.
            }

            ASTExpression::Conditional {
                condition,
                block,
                elifs,
                else_block,
                output_ty,
            }
        }
        ASTExpression::Loop(_, _) => todo!(),
        ASTExpression::For {
            loop_var,
            iterable,
            block,
        } => todo!(),
        ASTExpression::Break(_) => todo!(),
        ASTExpression::Continue => ASTExpression::Continue,
        ASTExpression::Return(_) => todo!(),
        ASTExpression::FunctionDefinition {
            doc_comment,
            discriminant,
            ident,
            params,
            block,
            return_ty,
        } => todo!(),
    }
}

fn todo_type_error() -> ! {
    todo!("// TODO handle type errors");
}
