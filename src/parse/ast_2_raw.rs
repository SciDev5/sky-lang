use std::collections::HashMap;

use crate::common::{IdentInt, IdentStr};

use super::{
    ast::{ASTArray, ASTBlock, ASTExpression, ASTLiteral, ASTVarAccessExpression},
    raw_module::{
        RMBlock, RMClass, RMExpression, RMFunction, RMLiteralArray, RMLiteralValue, RawModule,
        ScopedStatics,
    },
};

struct StaticsGlobalState {
    classes: Vec<RMClass>,
    functions: Vec<RMFunction>,
}
struct StaticsCurrentScope {
    classes: HashMap<IdentStr, IdentInt>,
    functions: HashMap<IdentStr, Vec<IdentInt>>,
}
impl StaticsCurrentScope {
    fn new() -> Self {
        Self {
            classes: HashMap::new(),
            functions: HashMap::new(),
        }
    }
    fn apply(self, state: &mut StaticsGlobalState) -> ScopedStatics {
        // for each function, add all references
        for (fn_name, overloads) in &self.functions {
            let fns = self.functions.get(fn_name).unwrap();
            for fn_id_i in fns {
                // add function refs
                state.functions[*fn_id_i]
                    .all_scoped
                    .functions
                    .entry(fn_name.clone())
                    .and_modify(|it| {
                        for fn_id_j in fns {
                            it.push(*fn_id_j);
                        }
                    })
                    .or_insert_with(|| fns.clone());
                // add class refs
                for (class_name, class_id) in &self.classes {
                    state.functions[*fn_id_i]
                        .all_scoped
                        .classes
                        .entry(class_name.clone())
                        // Note that we do not want to overwrite any existing scoped class because
                        // that would mean giving precedence to classes declared in a wider scope.
                        .or_insert(*class_id);
                }
            }
        }
        for id_i in &self.classes {}
        ScopedStatics {
            classes: self.classes,
            functions: self.functions,
        }
    }
}

pub fn ast_2_raw(ast: ASTBlock) -> RawModule {
    let mut state = StaticsGlobalState {
        classes: vec![],
        functions: vec![],
    };
    let top_level = transform_expr_block_inner_scoped(ast, &mut state);

    RawModule {
        classes: state.classes,
        functions: state.functions,
        top_level,
    }
}

/// `transform_expr`, but deals with the `Option<Box<...>>`
fn transform_expr_option_box<'a, 'b>(
    expr: Option<Box<ASTExpression>>,
    state: &mut StaticsGlobalState,
    scope: &mut StaticsCurrentScope,
) -> Option<Box<RMExpression>> {
    Some(Box::new(transform_expr(*expr?, state, scope)))
}
/// `transform_expr`, but deals with the `Box<...>`
fn transform_expr_box(
    expr: Box<ASTExpression>,
    state: &mut StaticsGlobalState,
    scope: &mut StaticsCurrentScope,
) -> Box<RMExpression> {
    Box::new(transform_expr(*expr, state, scope))
}
/// Transform a list of expressions. Distinct from `transform_expr_block_inner_scoped`, which is for code blocks.
fn transform_expr_vec(
    exprs: Vec<ASTExpression>,
    state: &mut StaticsGlobalState,
    scope: &mut StaticsCurrentScope,
) -> Vec<RMExpression> {
    let mut out = vec![];
    for expr in exprs {
        out.push(transform_expr(expr, state, scope));
    }
    out
}
/// Transform a code block that bounds a scope. (eg. function declarations inside this block cannot be referenced from outside).
fn transform_expr_block_inner_scoped(
    exprs: Vec<ASTExpression>,
    state: &mut StaticsGlobalState,
) -> RMBlock {
    let mut scope = StaticsCurrentScope::new();
    let mut block = vec![];
    for expr in exprs {
        block.push(transform_expr(expr, state, &mut scope));
    }
    RMBlock {
        block,
        inner_scoped: scope.apply(state),
    }
}

fn transform_expr(
    expr: ASTExpression,
    state: &mut StaticsGlobalState,
    scope: &mut StaticsCurrentScope,
) -> RMExpression {
    match expr {
        ////////////////////////////////////////////
        // move function/class definitions to static list
        //
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
                block: transform_expr_block_inner_scoped(block, state),
                all_scoped: ScopedStatics::empty(),
            };

            let id = state.functions.len();
            state.functions.push(function);
            scope
                .functions
                .entry(ident)
                .or_insert_with(|| vec![])
                .push(id);

            RMExpression::Void
        }

        // TODO classes/enums in ast_2_raw

        ////////////////////////////////////////////
        // do literal translation
        //
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
            initial_value: transform_expr_option_box(initial_value, state, scope),
            ty,
        },
        ASTExpression::Assign { target, value } => {
            let value = transform_expr_box(value, state, scope);
            match target {
                ASTVarAccessExpression::Var { ident } => RMExpression::AssignVar { ident, value },
                ASTVarAccessExpression::Index { object, indices } => RMExpression::AssignIndex {
                    object: transform_expr_box(object, state, scope),
                    indices: transform_expr_vec(indices, state, scope),
                    value,
                },
                ASTVarAccessExpression::PropertyAccess {
                    object,
                    property_ident,
                } => RMExpression::AssignProperty {
                    object: transform_expr_box(object, state, scope),
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
            callable: transform_expr_box(callable, state, scope),
            arguments: transform_expr_vec(arguments, state, scope),
        },
        ASTExpression::Index { expr, indices } => RMExpression::ReadIndex {
            expr: transform_expr_box(expr, state, scope),
            indices: transform_expr_vec(indices, state, scope),
        },
        ASTExpression::PropertyAccess {
            expr,
            property_ident,
        } => RMExpression::ReadProperty {
            expr: transform_expr_box(expr, state, scope),
            property_ident,
        },
        ASTExpression::Literal(literal) => match literal {
            ASTLiteral::Int(v) => RMExpression::LiteralValue(RMLiteralValue::Int(v)),
            ASTLiteral::Float(v) => RMExpression::LiteralValue(RMLiteralValue::Float(v)),
            ASTLiteral::Complex(v) => RMExpression::LiteralValue(RMLiteralValue::Complex(v)),
            ASTLiteral::Bool(v) => RMExpression::LiteralValue(RMLiteralValue::Bool(v)),
            ASTLiteral::String(v) => RMExpression::LiteralValue(RMLiteralValue::String(v)),
        },
        ASTExpression::Range { start, step, end } => RMExpression::LiteralRange {
            start: transform_expr_option_box(start, state, scope),
            step: transform_expr_option_box(step, state, scope),
            end: transform_expr_option_box(end, state, scope),
        },
        ASTExpression::Array(literal) => RMExpression::LiteralArray(match literal {
            ASTArray::List(v) => RMLiteralArray::List(transform_expr_vec(v, state, scope)),
            ASTArray::Matrix(_) => todo!("// TODO transform matrix/tensor literals in `ast_2_raw`"),
            ASTArray::Tensor(_) => todo!("// TODO transform matrix/tensor literals in `ast_2_raw`"),
        }),
        ASTExpression::AnonymousFunction { params, block } => RMExpression::AnonymousFunction {
            params,
            block: transform_expr_block_inner_scoped(block, state),
        },
        ASTExpression::BinaryOp { op, lhs, rhs } => RMExpression::OpBinary {
            op,
            lhs: transform_expr_box(lhs, state, scope),
            rhs: transform_expr_box(rhs, state, scope),
        },
        ASTExpression::UnaryOp { op, value } => RMExpression::OpUnary {
            op,
            value: transform_expr_box(value, state, scope),
        },
        ASTExpression::Conditional {
            condition,
            block,
            elifs,
            else_block,
        } => RMExpression::Conditional {
            condition: transform_expr_box(condition, state, scope),
            block: transform_expr_block_inner_scoped(block, state),
            elifs: {
                let mut elifs_out = vec![];
                for (condition, block) in elifs {
                    elifs_out.push((
                        transform_expr(condition, state, scope),
                        transform_expr_block_inner_scoped(block, state),
                    ));
                }
                elifs_out
            },
            else_block: if let Some(block) = else_block {
                Some(transform_expr_block_inner_scoped(block, state))
            } else {
                None
            },
        },
        ASTExpression::Loop { block } => RMExpression::Loop {
            block: transform_expr_block_inner_scoped(block, state),
        },
        ASTExpression::For {
            loop_var,
            iterable,
            block,
        } => RMExpression::LoopFor {
            loop_var,
            iterable: transform_expr_box(iterable, state, scope),
            block: transform_expr_block_inner_scoped(block, state),
        },
        ASTExpression::Break(value) => {
            RMExpression::LoopBreak(transform_expr_option_box(value, state, scope))
        }
        ASTExpression::Continue => RMExpression::LoopContinue,
        ASTExpression::Return(value) => {
            RMExpression::Return(transform_expr_option_box(value, state, scope))
        }
    }
}
