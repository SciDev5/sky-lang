use std::collections::HashMap;

use crate::language::{
    ast::{ASTBlock, ASTExpression, ASTTypesFull, ASTTypesIncomplete},
    ops::SLOperator,
};

use super::{
    data::Type,
    interpreter::Identifier,
    phase2::{P2Expression, P2SPTypedResolved, P2SPUntyped, P2Statics},
};

#[derive(Debug, Clone)]
struct ScopeContext {
    // var_lookup:
}

pub fn solve_types(
    statics: P2Statics<P2SPUntyped>,
    top_level_block: Vec<P2Expression<P2SPUntyped>>,
) -> (
    P2Statics<P2SPTypedResolved>,
    Vec<P2Expression<P2SPTypedResolved>>,
) {
    //
    todo!()
}

struct LocalVarInfo {
    ty: Option<Type>,
    writable: bool,
    assigned: bool,
}

fn solve_types_block<const ALLOW_RETURN: bool>(
    statics: &P2Statics<P2SPUntyped>,
    block: Vec<P2Expression<P2SPUntyped>>,
) -> Vec<P2Expression<P2SPTypedResolved>> {
    let mut locals = vec![];

    // forward type solving only, for now. // TODO closure parameter inference.

    let mut out = Vec::with_capacity(block.len());
    for expr in block {
        out.push(solve_types_expr::<ALLOW_RETURN>(statics, expr, &mut locals));
    }
    out
}
fn solve_types_expr<const ALLOW_RETURN: bool>(
    statics: &P2Statics<P2SPUntyped>,
    expr: P2Expression<P2SPUntyped>,
    locals: &mut Vec<LocalVarInfo>,
) -> P2Expression<P2SPTypedResolved> {
    match expr {
        P2Expression::NoOp => P2Expression::NoOp,
        P2Expression::DeclareVar { id, ty, writable } => {
            locals.push(LocalVarInfo { assigned: false, ty, writable });
            P2Expression::NoOp // declaring variables doesn't actually do anything at runtime, only during compile time.
        },
        P2Expression::AssignVar { id, value } => {
            let value = Box::new(solve_types_expr::<ALLOW_RETURN>(statics, *value, locals));
            let value_ty = value.eval_ty().to_value().expect("// TODO handle voidable value in expr");

            let var = &mut locals[id];
            if var.assigned && !var.writable {
                todo!("// TODO handle illegal writes to constants");
            }
            if let Some(var_ty) = &var.ty {
                // check types
                if value_ty != *var_ty {
                    todo!("// TODO handle var assignment type mismatch");
                }
            } else {
                // assert types
                var.ty = Some(value_ty);
            }
            
            P2Expression::AssignVar { id, value }
        },
        P2Expression::AssignValIndexed {
            this,
            indices,
            value,
        } => todo!(),
        P2Expression::AssignValProperty {
            this,
            property,
            value,
        } => todo!(),
        P2Expression::ReadVar { id, eval_ty } => {
            let var = &locals[id];
            if !var.assigned {
                todo!("// TODO handle read before assign");
            }
            P2Expression::ReadVar { id, eval_ty: var.ty.clone().unwrap() }
        },
        P2Expression::CallFunc { ids, args, eval_ty } => todo!(),
        P2Expression::CallAssociated {
            this,
            id,
            args,
            eval_ty,
        } => todo!(),
        P2Expression::ReadFunc { ids, eval_ty } => todo!(),
        P2Expression::ReadFuncAnon { id, eval_ty } => todo!(),
        P2Expression::CallVal {
            this,
            args,
            eval_ty,
        } => todo!(),
        P2Expression::ReadValProperty {
            this,
            property,
            eval_ty,
        } => todo!(),
        P2Expression::ReadValIndexed {
            this,
            indices,
            eval_ty,
        } => todo!(),
        P2Expression::Loop { body, eval_ty } => todo!(),
        P2Expression::Break { result_val } => todo!(),
        P2Expression::Return { result_val } => todo!(),
        P2Expression::Continue => P2Expression::Continue,
        P2Expression::If {
            body,
            condition,
            elifs,
            else_body,
            eval_ty,
        } => todo!(),
        P2Expression::Literal { literal } => P2Expression::Literal { literal },
        P2Expression::BinaryOp {
            op,
            lhs,
            rhs,
            eval_ty,
        } => {
            let lhs = Box::new(solve_types_expr::<ALLOW_RETURN>(statics, *lhs, locals));
            let rhs = Box::new(solve_types_expr::<ALLOW_RETURN>(statics, *rhs, locals));

            solve_op_binary(op, lhs, rhs).expect("// TODO op resolution failed")
        }
        P2Expression::UnaryOp { op, v, eval_ty } => {
            let v = Box::new(solve_types_expr::<ALLOW_RETURN>(statics, *v, locals));

            solve_op_unary(op, v).expect("// TODO op resolution failed")
        },
        P2Expression::PrimitiveCast { v, eval_ty } => todo!(),
    }
}

fn todo_type_error() -> ! {
    todo!("// TODO handle type errors");
}

fn solve_op_binary(
    op: SLOperator,
    lhs: Box<P2Expression<P2SPTypedResolved>>,
    rhs: Box<P2Expression<P2SPTypedResolved>>,
) -> Option<P2Expression<P2SPTypedResolved>> {
    let lhs_ty = lhs.eval_ty().to_value()?;
    let rhs_ty = rhs.eval_ty().to_value()?;
    Some(if lhs_ty.is_primitive() && rhs_ty.is_primitive() {
        if lhs_ty.is_numeric() && rhs_ty.is_numeric() {
            let (lhs, rhs) = match (lhs_ty, rhs_ty) {
                (Type::Int(_), Type::Int(_)) => (lhs, rhs),
                (Type::Int(u), Type::Float(_)) => (
                    Box::new(P2Expression::PrimitiveCast {
                        v: lhs,
                        eval_ty: Type::Float(u),
                    }),
                    rhs,
                ),
                (Type::Int(u), Type::Complex(_)) => (
                    Box::new(P2Expression::PrimitiveCast {
                        v: lhs,
                        eval_ty: Type::Complex(u),
                    }),
                    rhs,
                ),
                (Type::Float(_), Type::Int(u)) => (
                    lhs,
                    Box::new(P2Expression::PrimitiveCast {
                        v: rhs,
                        eval_ty: Type::Float(u),
                    }),
                ),
                (Type::Float(_), Type::Float(_)) => (lhs, rhs),
                (Type::Float(u), Type::Complex(_)) => (
                    Box::new(P2Expression::PrimitiveCast {
                        v: lhs,
                        eval_ty: Type::Complex(u),
                    }),
                    rhs,
                ),
                (Type::Complex(_), Type::Int(u)) => (
                    lhs,
                    Box::new(P2Expression::PrimitiveCast {
                        v: rhs,
                        eval_ty: Type::Complex(u),
                    }),
                ),
                (Type::Complex(_), Type::Float(u)) => (
                    lhs,
                    Box::new(P2Expression::PrimitiveCast {
                        v: rhs,
                        eval_ty: Type::Complex(u),
                    }),
                ),
                (Type::Complex(_), Type::Complex(_)) => (lhs, rhs),
                _ => panic!("not possible"),
            };
            let lhs_ty = lhs.eval_ty().to_value().unwrap();
            let rhs_ty = rhs.eval_ty().to_value().unwrap();
            P2Expression::BinaryOp {
                op,
                lhs,
                rhs,
                eval_ty: match op {
                    (
                        SLOperator::Plus | 
                        SLOperator::Minus | 
                        SLOperator::ScalarTimes | 
                        SLOperator::ScalarDiv | 
                        SLOperator::ScalarExp | 
                        SLOperator::ScalarModulo
                    ) => match (lhs_ty, rhs_ty) {
                        (Type::Int(ua), Type::Int(ub)) => Type::Int(None), // TODO units
                        (Type::Float(ua), Type::Float(ub)) => Type::Float(None),
                        (Type::Complex(ua), Type::Complex(ub)) => Type::Complex(None),
                        _ => panic!("should not be possible")
                    }
                    (
                        SLOperator::BitXor | 
                        SLOperator::BitAnd | 
                        SLOperator::BitOr | 
                        SLOperator::Shl | 
                        SLOperator::Shr
                    ) => match (lhs_ty, rhs_ty) {
                        (Type::Int(ua), Type::Int(ub)) => Type::Int(None),
                        _ => return None,
                    }
                    (
                        SLOperator::Equal | 
                        SLOperator::NotEqual | 
                        SLOperator::LessEqual | 
                        SLOperator::LessThan | 
                        SLOperator::GreaterEqual | 
                        SLOperator::GreaterThan
                    ) => match (lhs_ty, rhs_ty) {
                        (Type::Int(ua), Type::Int(ub)) => Type::Bool, // TODO check unit equality
                        _ => return None,
                    }
                    _ => return None,
                },
            }
        } else {
            match (lhs_ty, rhs_ty) {
                (Type::Bool, Type::Bool) => P2Expression::BinaryOp {
                    op,
                    lhs,
                    rhs,
                    eval_ty: match op {
                        SLOperator::Xor
                        | SLOperator::And
                        | SLOperator::Or
                        | SLOperator::Equal
                        | SLOperator::NotEqual => Type::Bool,
                        _ => return None,
                    },
                },
                (Type::String, Type::String) => {
                    return None;
                }
                _ => return None,
            }
        }
    } else {
        todo!("// TODO class property resolution")
    })
}

fn solve_op_unary(
    op: SLOperator,
    v: Box<P2Expression<P2SPTypedResolved>>,
) -> Option<P2Expression<P2SPTypedResolved>> {
    let v_ty = v.eval_ty().to_value()?;
    Some(if v_ty.is_primitive() {
        P2Expression::UnaryOp { op, v, eval_ty:
        match op {
            SLOperator::HermitianConjugate |
            SLOperator::Transpose |
            SLOperator::Minus |
            SLOperator::Plus |
            SLOperator::Inverse => match v_ty {
                Type::Int(u) => Type::Int(u), // TODO units
                Type::Float(u) => Type::Float(u),
                Type::Complex(u) => Type::Complex(u),
                _ => return None,
            }
            SLOperator::Not => match v_ty {
                Type::Bool => Type::Bool,
                _ => return None
            }
            _ => return None,
        }
    }
    } else {
        todo!("// TODO class property resolution")
    })
}
