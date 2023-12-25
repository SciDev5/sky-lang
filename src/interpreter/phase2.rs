use std::{collections::HashMap, fmt::Debug};

use crate::{
    interpreter::{
        data::{Type, Value, VoidableType},
        interpreter::Identifier,
    },
    language::{
        ast::{ASTBlock, ASTExpression, ASTLiteral, ASTVarAccessExpression},
        ops::SLOperator,
    },
};

use super::data::{Class, Enum, Var};

type NativeFn = fn();

pub enum P2Function<P: P2SubPhase> {
    Native(NativeFn),
    Interpreted(
        Vec<P2Expression<P>>,
        Vec<Type>,
        P::VoidableType,
        P2FunctionClosure,
    ),
    Placeholder,
}
pub struct P2AnonFunction<P: P2SubPhase> {
    body: Vec<P2Expression<P>>,
    captures: Vec<P::PropertyId>,
    params_ty: Vec<P::ValueType>,
    ret_ty: P::VoidableType,
}

pub struct P2FunctionClosure {
    ref_functions: Vec<IdentId>,
    // vars: Vec<IdentId]>,
}

pub struct P2Statics<P: P2SubPhase> {
    fn_arr: Vec<(P2Function<P>, Vec<Type>, P::VoidableType)>,
    class_arr: Vec<Class>,
    enum_arr: Vec<Enum>,
    anon_fn_arr: Vec<(P2Function<P>, Vec<P::ValueType>, P::VoidableType)>,
}

pub trait P2SubPhase {
    type ValueType: Debug + Clone;
    type VoidableType: Debug + Clone;
    type PropertyId: Debug + Clone;
    type FunctionRef: Debug + Clone;
}

pub struct P2SPUntyped;
impl P2SubPhase for P2SPUntyped {
    type ValueType = Option<Type>;
    type VoidableType = Option<VoidableType>;
    type PropertyId = Identifier;
    type FunctionRef = Vec<IdentId>;
}
pub struct P2SPTypedResolved;
impl P2SubPhase for P2SPTypedResolved {
    type ValueType = Type;
    type VoidableType = VoidableType;
    type PropertyId = IdentId;
    type FunctionRef = IdentId;
}

pub enum P2OperationFunction<const N: usize> {
    Primitive(fn([Value; N]) -> Value),
    Function(IdentId),
}

pub type IdentId = usize;
pub enum P2Expression<P: P2SubPhase> {
    NoOp,
    DeclareVar {
        id: IdentId,
        ty: P::ValueType,
        writable: bool,
    },
    AssignVar {
        id: IdentId,
        value: Box<P2Expression<P>>,
    },
    AssignValIndexed {
        this: Box<P2Expression<P>>,
        indices: Vec<P2Expression<P>>,
        value: Box<P2Expression<P>>,
    },
    AssignValProperty {
        this: Box<P2Expression<P>>,
        property: P::PropertyId,
        value: Box<P2Expression<P>>,
    },
    ReadVar {
        id: IdentId,
        eval_ty: P::ValueType,
    },
    CallFunc {
        // all scoped overloads
        ids: P::FunctionRef,
        args: Vec<P2Expression<P>>,
        eval_ty: P::VoidableType,
    },
    CallAssociated {
        this: Box<P2Expression<P>>,
        id: P::PropertyId,
        args: Vec<P2Expression<P>>,
        eval_ty: P::VoidableType,
    },
    ReadFunc {
        ids: P::FunctionRef,
        eval_ty: P::ValueType,
    },
    ReadFuncAnon {
        id: IdentId,
        eval_ty: P::ValueType,
    },
    CallVal {
        this: Box<P2Expression<P>>,
        args: Vec<P2Expression<P>>,
        eval_ty: P::VoidableType,
    },
    ReadValProperty {
        this: Box<P2Expression<P>>,
        property: P::PropertyId,
        eval_ty: P::ValueType,
    },
    ReadValIndexed {
        this: Box<P2Expression<P>>,
        indices: Vec<P2Expression<P>>,
        eval_ty: P::ValueType,
    },
    Loop {
        body: Vec<P2Expression<P>>,
        eval_ty: P::VoidableType,
    },
    Break {
        result_val: Option<Box<P2Expression<P>>>,
    },
    Return {
        result_val: Option<Box<P2Expression<P>>>,
    },
    Continue,
    If {
        body: Vec<P2Expression<P>>,
        condition: Box<P2Expression<P>>,
        elifs: Vec<(P2Expression<P>, Vec<P2Expression<P>>)>,
        else_body: Option<Vec<P2Expression<P>>>,
        eval_ty: P::VoidableType,
    },
    Literal {
        literal: ASTLiteral,
    },
    BinaryOp {
        op: SLOperator,
        lhs: Box<P2Expression<P>>,
        rhs: Box<P2Expression<P>>,
        eval_ty: P::ValueType,
    },
    UnaryOp {
        op: SLOperator,
        v: Box<P2Expression<P>>,
        eval_ty: P::ValueType,
    },
    PrimitiveCast {
        v: Box<P2Expression<P>>,
        eval_ty: P::ValueType,
    },
}

impl P2Expression<P2SPTypedResolved> {
    pub fn eval_ty(&self) -> VoidableType {
        match self {
            P2Expression::NoOp => VoidableType::Void,
            P2Expression::DeclareVar { id, ty, writable } => VoidableType::Void,
            P2Expression::AssignVar { id, value } => value.eval_ty(),
            P2Expression::AssignValIndexed {
                this,
                indices,
                value,
            } => value.eval_ty(),
            P2Expression::AssignValProperty {
                this,
                property,
                value,
            } => value.eval_ty(),
            P2Expression::ReadVar { id, eval_ty } => VoidableType::Value(eval_ty.clone()),
            P2Expression::CallFunc { ids, args, eval_ty } => eval_ty.clone(),
            P2Expression::CallAssociated {
                this,
                id,
                args,
                eval_ty,
            } => eval_ty.clone(),
            P2Expression::ReadFunc { ids, eval_ty } => VoidableType::Value(eval_ty.clone()),
            P2Expression::ReadFuncAnon { id, eval_ty } => VoidableType::Value(eval_ty.clone()),
            P2Expression::CallVal {
                this,
                args,
                eval_ty,
            } => eval_ty.clone(),
            P2Expression::ReadValProperty {
                this,
                property,
                eval_ty,
            } => VoidableType::Value(eval_ty.clone()),
            P2Expression::ReadValIndexed {
                this,
                indices,
                eval_ty,
            } => VoidableType::Value(eval_ty.clone()),
            P2Expression::Loop { body, eval_ty } => eval_ty.clone(),
            P2Expression::Break { result_val } => VoidableType::Value(Type::Never),
            P2Expression::Return { result_val } => VoidableType::Value(Type::Never),
            P2Expression::Continue => VoidableType::Value(Type::Never),
            P2Expression::If {
                body,
                condition,
                elifs,
                else_body,
                eval_ty,
            } => eval_ty.clone(),
            P2Expression::Literal { literal } => match literal {
                ASTLiteral::Bool(_) => VoidableType::Value(Type::Bool),
                ASTLiteral::Int(_) => VoidableType::Value(Type::Int(None)),
                ASTLiteral::Float(_) => VoidableType::Value(Type::Float(None)),
                ASTLiteral::Complex(_) => VoidableType::Value(Type::Complex(None)),
                ASTLiteral::String(_) => VoidableType::Value(Type::String),
            },
            P2Expression::BinaryOp {
                op,
                lhs,
                rhs,
                eval_ty,
            } => VoidableType::Value(eval_ty.clone()),
            P2Expression::UnaryOp { op, v, eval_ty } => VoidableType::Value(eval_ty.clone()),
            P2Expression::PrimitiveCast { v, eval_ty } => VoidableType::Value(eval_ty.clone()),
        }
    }
}

struct VarLUTEntry {
    doc_comment: Option<String>,
    id: IdentId,
    ty: Option<Type>,
    writable: bool,
    freed_to_heap: bool,
}
struct ScopeStackFrame {
    pub fn_lut: HashMap<Identifier, Vec<IdentId>>,

    pub local_top_level: bool,
    pub local_var_lut: HashMap<Identifier, VarLUTEntry>,
}

struct A2P2UnbuiltFunction<'a> {
    doc_comment: &'a Option<String>,
    ident: &'a Identifier,
    params: &'a Vec<(Identifier, Type)>,
    return_ty: &'a Option<VoidableType>,
    block: &'a Vec<ASTExpression>,
}

pub fn module_translate(ast: ASTBlock) -> (P2Statics<P2SPUntyped>, Vec<P2Expression<P2SPUntyped>>) {
    let mut scope_stack = vec![ScopeStackFrame {
        fn_lut: HashMap::new(),
        local_var_lut: HashMap::new(),
        local_top_level: true,
    }];
    let mut statics_dump = P2Statics {
        fn_arr: vec![],
        anon_fn_arr: vec![],
        class_arr: vec![],
        enum_arr: vec![],
    };

    let mut next_local_var_id = 0;
    let transformed_top_level = translate_block(
        ast,
        &mut scope_stack,
        &mut statics_dump,
        &mut next_local_var_id,
    );

    (statics_dump, transformed_top_level)
}

fn translate_block(
    ast: ASTBlock,
    scope_stack: &mut Vec<ScopeStackFrame>,
    statics_dump: &mut P2Statics<P2SPUntyped>,
    next_local_var_id: &mut IdentId,
) -> Vec<P2Expression<P2SPUntyped>> {
    let local_functions = resolve_current_scope_statics(&ast);

    let scope_stack_top = get_scope_stack_top(scope_stack);

    // Register all functions in this block
    let function_id_first = statics_dump.fn_arr.len();
    for (i, local_function) in local_functions.iter().enumerate() {
        let id = function_id_first + i;

        statics_dump.fn_arr.push((
            P2Function::Placeholder,
            local_function
                .params
                .iter()
                .map(|(_, ty)| ty.clone())
                .collect(),
            local_function.return_ty.clone(),
        ));
        scope_stack_top
            .fn_lut
            .entry(local_function.ident.clone())
            .and_modify(|it| it.push(id))
            .or_insert(vec![id]);
    }
    // Translate all functions in this block
    for (i, local_function) in local_functions.into_iter().enumerate() {
        let id = function_id_first + i;
        statics_dump.fn_arr[id].0 =
            translate_function(id, &local_function.params, scope_stack, statics_dump);
    }

    // Translate line by line
    let mut out = Vec::with_capacity(ast.len());
    for ast_expr in ast {
        match ast_expr {
            ASTExpression::VarDeclare {
                doc_comment,
                ident,
                writable,
                initial_assignment,
                ty,
            } => {
                let id = *next_local_var_id;
                *next_local_var_id += 1;
                let existing_var = get_scope_stack_top(scope_stack).local_var_lut.insert(
                    ident,
                    VarLUTEntry {
                        doc_comment,
                        id,
                        ty,
                        writable,
                        freed_to_heap: false,
                    },
                );

                if existing_var.is_some() {
                    todo!("handle variable redeclaration");
                }
            }
            ASTExpression::FunctionDefinition { .. } => {
                // do nothing, already handled
            }
            ast_expr => out.push(translate_expr_inline(
                ast_expr,
                scope_stack,
                statics_dump,
                next_local_var_id,
            )),
        }
    }
    out
}

fn translate_expr_inline(
    ast_expr: ASTExpression,
    scope_stack: &mut Vec<ScopeStackFrame>,
    statics_dump: &mut P2Statics<P2SPUntyped>,
    next_local_var_id: &mut IdentId,
) -> P2Expression<P2SPUntyped> {
    match ast_expr {
        // TODO this is temporary
        ASTExpression::Assign(ASTVarAccessExpression::Var(ident), value_expr) => {
            P2Expression::AssignVar {
                id: scope_stack_lookup_var(scope_stack, &ident)
                    .expect("// TODO handle variable not found")
                    .id,
                value: Box::new(translate_expr_inline(
                    *value_expr,
                    scope_stack,
                    statics_dump,
                    next_local_var_id,
                )),
            }
        }
        ASTExpression::Assign(access_expr, value_expr) => todo!(),
        ASTExpression::Read(ident) => {
            if let Some(var) = scope_stack_lookup_var(scope_stack, &ident) {
                P2Expression::ReadVar {
                    id: var.id,
                    eval_ty: var.ty.clone(),
                }
            } else {
                todo!("// TODO handle variable not found")
            }
        }
        ASTExpression::Call {
            callable,
            arguments,
        } => {
            let mut args = Vec::with_capacity(arguments.len());
            for argument in arguments {
                args.push(translate_expr_inline(
                    argument,
                    scope_stack,
                    statics_dump,
                    next_local_var_id,
                ));
            }
            match *callable {
                ASTExpression::Read(ident) => {
                    if let Some(var) = scope_stack_lookup_var(scope_stack, &ident) {
                        // call local var
                        P2Expression::CallVal {
                            this: Box::new(P2Expression::ReadVar {
                                id: var.id,
                                eval_ty: var.ty.clone(),
                            }),
                            args,
                            eval_ty: None,
                        }
                    } else {
                        // call function
                        let func_overloads = scope_stack_lookup_func(scope_stack, &ident);
                        if func_overloads.is_empty() {
                            todo!("could not resolve callable")
                        } else {
                            P2Expression::CallFunc {
                                ids: func_overloads,
                                args,
                                eval_ty: None,
                            }
                        }
                    }
                }
                ASTExpression::PropertyAccess {
                    expr,
                    property_ident,
                } => {
                    // call property
                    P2Expression::CallAssociated {
                        this: Box::new(translate_expr_inline(
                            *expr,
                            scope_stack,
                            statics_dump,
                            next_local_var_id,
                        )),
                        id: property_ident,
                        args,
                        eval_ty: None,
                    }
                }
                callable => {
                    // call working value
                    P2Expression::CallVal {
                        this: Box::new(translate_expr_inline(
                            callable,
                            scope_stack,
                            statics_dump,
                            next_local_var_id,
                        )),
                        args,
                        eval_ty: None,
                    }
                }
            }
        }
        ASTExpression::Index { expr, indices } => todo!(),
        ASTExpression::PropertyAccess {
            expr,
            property_ident,
        } => P2Expression::ReadValProperty {
            this: Box::new(translate_expr_inline(
                *expr,
                scope_stack,
                statics_dump,
                next_local_var_id,
            )),
            property: property_ident,
            eval_ty: None,
        },
        ASTExpression::Literal(literal) => P2Expression::Literal { literal },
        ASTExpression::Range { start, step, end } => todo!(),
        ASTExpression::Array(array) => todo!(),
        ASTExpression::AnonymousFunction { params, block } => todo!(),
        ASTExpression::BinaryOp(op, lhs, rhs) => P2Expression::BinaryOp {
            op,
            lhs: Box::new(translate_expr_inline(
                *lhs,
                scope_stack,
                statics_dump,
                next_local_var_id,
            )),
            rhs: Box::new(translate_expr_inline(
                *rhs,
                scope_stack,
                statics_dump,
                next_local_var_id,
            )),
            eval_ty: None,
        },
        ASTExpression::UnaryOp(op, v) => P2Expression::UnaryOp {
            op,
            v: Box::new(translate_expr_inline(
                *v,
                scope_stack,
                statics_dump,
                next_local_var_id,
            )),
            eval_ty: None,
        },
        ASTExpression::Conditional {
            condition,
            block,
            elifs,
            else_block,
        } => todo!(),
        ASTExpression::Loop(body) => todo!(),
        ASTExpression::For {
            loop_var,
            iterable,
            block,
        } => todo!(),
        ASTExpression::Break(Some(value_expr)) => P2Expression::Break {
            result_val: Some(Box::new(translate_expr_inline(
                *value_expr,
                scope_stack,
                statics_dump,
                next_local_var_id,
            ))),
        },
        ASTExpression::Break(None) => P2Expression::Break { result_val: None },
        ASTExpression::Continue => P2Expression::Continue,
        ASTExpression::Return(Some(value_expr)) => P2Expression::Return {
            result_val: Some(Box::new(translate_expr_inline(
                *value_expr,
                scope_stack,
                statics_dump,
                next_local_var_id,
            ))),
        },
        ASTExpression::Return(None) => P2Expression::Return { result_val: None },

        ASTExpression::FunctionDefinition { .. } => {
            todo!("illegal inline function definition")
        }
        ASTExpression::VarDeclare { .. } => {
            todo!("illegal inline variable declaration")
        }
    }
}

fn get_scope_stack_top(scope_stack: &mut Vec<ScopeStackFrame>) -> &mut ScopeStackFrame {
    scope_stack.last_mut().expect("scope_stack top missing")
}
fn scope_stack_lookup_var<'a>(
    scope_stack: &'a Vec<ScopeStackFrame>,
    ident: &Identifier,
) -> Option<&'a VarLUTEntry> {
    for frame in scope_stack.iter().rev() {
        if let Some(var) = frame.local_var_lut.get(ident) {
            // Found it.
            return Some(var);
        }

        // This was the outermost scope for which the scope is valid, break.
        if frame.local_top_level {
            break;
        }
    }
    return None;
}
fn scope_stack_lookup_func(scope_stack: &Vec<ScopeStackFrame>, ident: &Identifier) -> Vec<IdentId> {
    let mut out = Vec::with_capacity(1);
    for frame in scope_stack.iter().rev() {
        if let Some(var) = frame.fn_lut.get(ident) {
            for id in var {
                out.push(*id);
            }
        }
    }
    return out;
}

fn translate_function(
    id: IdentId,
    params: &[(Identifier, Type)],
    scope_stack: &mut Vec<ScopeStackFrame>,
    statics_dump: &mut P2Statics<P2SPUntyped>,
) -> P2Function<P2SPUntyped> {
    todo!()
}

fn resolve_current_scope_statics<'a>(ast: &'a ASTBlock) -> Vec<A2P2UnbuiltFunction<'a>> {
    let mut fns = vec![];
    for item in ast {
        match item {
            // TODO classes
            ASTExpression::FunctionDefinition {
                doc_comment,
                ident,
                params,
                return_ty,
                block,
            } => {
                fns.push(A2P2UnbuiltFunction {
                    doc_comment,
                    ident,
                    params,
                    return_ty,
                    block,
                });
            }
            _ => {}
        }
    }
    fns
}
