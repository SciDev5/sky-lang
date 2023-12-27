use super::{
    ast::{ASTArray, ASTBlock, ASTExpression, ASTLiteral, ASTVarAccessExpression},
    raw_module::{RMClass, RMExpression, RMFunction, RMLiteralArray, RMLiteralNumber, RawModule},
};

pub fn ast_2_raw(ast: ASTBlock) -> RawModule {
    let mut classes = vec![];
    let mut functions = vec![];
    let top_level = transform_expr_vec(ast, &mut classes, &mut functions);
    RawModule {
        classes,
        functions,
        top_level,
    }
}

fn transform_expr_option_box(
    expr: Option<Box<ASTExpression>>,
    classes: &mut Vec<RMClass>,
    functions: &mut Vec<RMFunction>,
) -> Option<Box<RMExpression>> {
    Some(Box::new(transform_expr(*expr?, classes, functions)))
}
fn transform_expr_box(
    expr: Box<ASTExpression>,
    classes: &mut Vec<RMClass>,
    functions: &mut Vec<RMFunction>,
) -> Box<RMExpression> {
    Box::new(transform_expr(*expr, classes, functions))
}
fn transform_expr_vec(
    exprs: Vec<ASTExpression>,
    classes: &mut Vec<RMClass>,
    functions: &mut Vec<RMFunction>,
) -> Vec<RMExpression> {
    let mut out = vec![];
    for expr in exprs {
        out.push(transform_expr(expr, classes, functions));
    }
    out
}

fn transform_expr(
    expr: ASTExpression,
    classes: &mut Vec<RMClass>,
    functions: &mut Vec<RMFunction>,
) -> RMExpression {
    match expr {
        ////////////////////////////////////////////
        // move function/class definitions to static list

        ASTExpression::FunctionDefinition {
            doc_comment,
            ident,
            params,
            return_ty,
            block,
        } => {
            let function = RMFunction {
                doc_comment,
                params,
                return_ty,
                block: transform_expr_vec(block, classes, functions),
            };
            let id = functions.len();
            functions.push(function);

            RMExpression::DeclareFunction { id, ident }
        },

        // TODO classes/enums in ast_2_raw

        ////////////////////////////////////////////
        // do literal translation

        ASTExpression::VarDeclare {
            doc_comment,
            ident,
            writable,
            initial_value,
            ty,
        } => RMExpression::DeclareVar {
            doc_comment,
            ident,
            writable,
            initial_value: transform_expr_option_box(initial_value, classes, functions),
            ty,
        },
        ASTExpression::Assign { target, value } => {
            let value = transform_expr_box(value, classes, functions);
            match target {
                ASTVarAccessExpression::Var { ident } => RMExpression::AssignVar { ident, value },
                ASTVarAccessExpression::Index { object, indices } => RMExpression::AssignIndex {
                    object: transform_expr_box(object, classes, functions),
                    indices: transform_expr_vec(indices, classes, functions),
                    value,
                },
                ASTVarAccessExpression::PropertyAccess {
                    object,
                    property_ident,
                } => RMExpression::AssignProperty {
                    object: transform_expr_box(object, classes, functions),
                    property: property_ident,
                    value,
                },
            }
        }
        ASTExpression::Read(ident) => RMExpression::Read { ident },
        ASTExpression::Call {
            callable,
            arguments,
        } => RMExpression::Call {
            callable: transform_expr_box(callable, classes, functions),
            arguments: transform_expr_vec(arguments, classes, functions),
        },
        ASTExpression::Index { expr, indices } => RMExpression::ReadIndex {
            expr: transform_expr_box(expr, classes, functions),
            indices: transform_expr_vec(indices, classes, functions),
        },
        ASTExpression::PropertyAccess {
            expr,
            property_ident,
        } => RMExpression::ReadProperty {
            expr: transform_expr_box(expr, classes, functions),
            property_ident,
        },
        ASTExpression::Literal(literal) => match literal {
            ASTLiteral::Int(v) => RMExpression::LiteralNumber(RMLiteralNumber::Int(v)),
            ASTLiteral::Float(v) => RMExpression::LiteralNumber(RMLiteralNumber::Float(v)),
            ASTLiteral::Complex(v) => RMExpression::LiteralNumber(RMLiteralNumber::Complex(v)),
            ASTLiteral::Bool(v) => RMExpression::LiteralBool(v),
            ASTLiteral::String(v) => RMExpression::LiteralString(v),
        },
        ASTExpression::Range { start, step, end } => RMExpression::LiteralRange {
            start: transform_expr_option_box(start, classes, functions),
            step: transform_expr_option_box(step, classes, functions),
            end: transform_expr_option_box(end, classes, functions),
        },
        ASTExpression::Array(literal) => RMExpression::LiteralArray(match literal {
            ASTArray::List(v) => RMLiteralArray::List(transform_expr_vec(v, classes, functions)),
            ASTArray::Matrix(_) => todo!("// TODO transform matrix/tensor literals in `ast_2_raw`"),
            ASTArray::Tensor(_) => todo!("// TODO transform matrix/tensor literals in `ast_2_raw`"),
        }),
        ASTExpression::AnonymousFunction { params, block } => RMExpression::AnonymousFunction {
            params,
            block: transform_expr_vec(block, classes, functions),
        },
        ASTExpression::BinaryOp { op, lhs, rhs } => RMExpression::OpBinary {
            op,
            lhs: transform_expr_box(lhs, classes, functions),
            rhs: transform_expr_box(rhs, classes, functions),
        },
        ASTExpression::UnaryOp { op, value } => RMExpression::OpUnary {
            op,
            value: transform_expr_box(value, classes, functions),
        },
        ASTExpression::Conditional {
            condition,
            block,
            elifs,
            else_block,
        } => RMExpression::Conditional {
            condition: transform_expr_box(condition, classes, functions),
            block: transform_expr_vec(block, classes, functions),
            elifs: {
                let mut elifs_out = vec![];
                for (condition, block) in elifs {
                    elifs_out.push((
                        transform_expr(condition, classes, functions),
                        transform_expr_vec(block, classes, functions),
                    ));
                }
                elifs_out
            },
            else_block: if let Some(block) = else_block {
                Some(transform_expr_vec(block, classes, functions))
            } else {
                None
            },
        },
        ASTExpression::Loop { block } => RMExpression::Loop { block: transform_expr_vec(block, classes, functions) },
        ASTExpression::For {
            loop_var,
            iterable,
            block,
        } => RMExpression::LoopFor { loop_var, iterable: transform_expr_box(iterable, classes, functions), block: transform_expr_vec(block, classes, functions) },
        ASTExpression::Break(value) => RMExpression::LoopBreak(transform_expr_option_box(value, classes, functions)),
        ASTExpression::Continue => RMExpression::LoopContinue,
        ASTExpression::Return(value) => RMExpression::Return(transform_expr_option_box(value, classes, functions)),
    }
}
