use std::{collections::HashMap, fmt::Debug};

use crate::common::{
    common_module::{
        CMCfg, CMClass, CMExpression, CMFunction, CMLiteralValue, CMLocalVarInfo, CMType,
        CMValueType, CommonModule,
    },
    IdentInt, IdentStr,
};

use super::{
    fn_lookup::IntrinsicAssociatedFnLuts,
    raw_module::{
        RMBlock, RMClass, RMExpression, RMLiteralValue, RMType, RMValueType, RawModule,
        ScopedStatics,
    },
};

#[derive(Debug, Clone)]
struct FunctionThing<Cfg: CMCfg> {
    doc_comment: Option<String>,
    params: Vec<CMValueType>,
    param_idents: Vec<IdentStr>,
    body: FunctionBody<Cfg>,
}
impl<Cfg: CMCfg> FunctionThing<Cfg> {
    fn unwrap_translated_return_ty(&self) -> &CMType {
        match &self.body {
            FunctionBody::Translated { ty_return, .. } => ty_return,
            _ => panic!(),
        }
    }
}
#[derive(Debug, Clone)]
enum FunctionBody<Cfg: CMCfg> {
    Untranslated {
        ty_return: WithResolutionStatus<CMType>,
        all_scoped: ScopedStatics,
        block: RMBlock,
    },
    Translated {
        ty_return: CMType,
        locals: Vec<CMLocalVarInfo>,
        block: Vec<CMExpression<Cfg>>,
    },
}
impl<Cfg: CMCfg> FunctionBody<Cfg> {
    fn unwrap_translated(self) -> (CMType, Vec<CMExpression<Cfg>>, Vec<CMLocalVarInfo>) {
        match self {
            FunctionBody::Translated {
                ty_return,
                block,
                locals,
            } => (ty_return, block, locals),
            _ => panic!(),
        }
    }
    fn unwrap_untranslated(self) -> (WithResolutionStatus<CMType>, ScopedStatics, RMBlock) {
        match self {
            FunctionBody::Untranslated {
                ty_return,
                all_scoped,
                block,
            } => (ty_return, all_scoped, block),
            _ => panic!(),
        }
    }
    fn is_translated(&self) -> bool {
        matches!(self, FunctionBody::Translated { .. })
    }
}

struct ResolverGlobalState<Cfg: CMCfg> {
    functions: Vec<FunctionThing<Cfg>>,
    diagnostics: Vec<Raw2CommonDiagnostic>,
}
impl<Cfg: CMCfg> ResolverGlobalState<Cfg> {
    fn add_diagnostic(&mut self, diagnostic: Raw2CommonDiagnostic) -> Raw2CommonDiagnostic {
        self.diagnostics.push(diagnostic.clone());
        dbg!(diagnostic)
    }
}

#[derive(Debug, Clone)]
enum WithResolutionStatus<T: Debug + Clone> {
    Unknown,
    Failed,
    Resolved(T),
}
impl<T: Debug + Clone> WithResolutionStatus<T> {
    /// Translates `Some` to `Resolved` and `None` to `Unknown`
    fn from_option_none_unknown(from: Option<T>) -> Self {
        match from {
            Some(v) => WithResolutionStatus::Resolved(v),
            None => WithResolutionStatus::Unknown,
        }
    }
    fn unwrap_resolved(self) -> T {
        match self {
            Self::Resolved(v) => v,
            _ => panic!("unwrap_resolve failed"),
        }
    }
    fn as_ref(&self) -> WithResolutionStatus<&T> {
        match self {
            WithResolutionStatus::Unknown => WithResolutionStatus::Unknown,
            WithResolutionStatus::Failed => WithResolutionStatus::Failed,
            WithResolutionStatus::Resolved(v) => WithResolutionStatus::Resolved(v),
        }
    }
}

#[derive(Debug, Clone)]
struct LocalVar {
    doc_comment: Option<String>,
    ty: WithResolutionStatus<CMValueType>,
    writable: bool,
    assigned: bool,
}

#[derive(Debug, Clone)]
struct LoopContext {
    returnable: bool,
    return_value: CMType,
}

#[derive(Debug, Clone)]
struct Raw2CommonDiagnostic {
    text: String,
    // TODO pointing out location
}

pub fn raw_2_common<Cfg: CMCfg>(
    raw_module: RawModule,
    intrinsics: &IntrinsicAssociatedFnLuts<Cfg::IntrinsicFnRef>,
) -> CommonModule<Cfg> {
    let mut state = ResolverGlobalState {
        functions: raw_module
            .functions
            .into_iter()
            .map(|func| FunctionThing {
                doc_comment: func.doc_comment,
                params: func
                    .params
                    .iter()
                    .map(|(_, ty)| static_resolve_value_type(&[func.all_scoped.clone()], ty))
                    .collect(),
                param_idents: func.params.into_iter().map(|(ident, _)| ident).collect(),
                body: FunctionBody::Untranslated {
                    ty_return: match &func.return_ty {
                        Some(ty) => WithResolutionStatus::Resolved(static_resolve_type(
                            &[func.all_scoped.clone()],
                            ty,
                        )),
                        None => WithResolutionStatus::Unknown,
                    },
                    all_scoped: func.all_scoped,
                    block: func.block,
                },
            })
            .collect(),
        diagnostics: vec![],
    };

    // translate top-level block
    let mut current_fn_stack = vec![];
    let mut statics_stack = vec![];
    let mut locals = vec![];
    let mut locals_lookup = HashMap::new();
    let mut loop_context_stack = vec![];
    let mut ty_return = None;
    let (top_level, ty_top_level) = resolve_block(
        raw_module.top_level,
        &mut state,
        &mut current_fn_stack,
        &mut statics_stack,
        &mut locals,
        &mut locals_lookup,
        &mut loop_context_stack,
        &mut ty_return,
    );

    // translate all functions (automatically skips reresolving functions we've already resolved)
    for current_fn_id in 0..state.functions.len() {
        resolve_fn(&mut state, current_fn_id, &mut current_fn_stack);
    }

    CommonModule {
        top_level: (
            top_level,
            locals
                .into_iter()
                .map(
                    |LocalVar {
                         doc_comment,
                         ty,
                         writable,
                         ..
                     }| CMLocalVarInfo {
                        doc_comment,
                        ty: match ty {
                            WithResolutionStatus::Resolved(ty) => CMType::Value(ty),
                            _ => CMType::Never,
                        },
                        writable,
                    },
                )
                .collect(),
            ty_top_level,
        ),
        functions: state
            .functions
            .into_iter()
            .map(
                |FunctionThing {
                     doc_comment,
                     params,
                     param_idents,
                     body,
                 }| {
                    let (ty_return, block, locals) = body.unwrap_translated();
                    CMFunction {
                        doc_comment,
                        locals,
                        params,
                        block,
                        ty_return,
                    }
                },
            )
            .collect(),
        classes: raw_module
            .classes
            .into_iter()
            .map(
                |RMClass {
                     doc_comment,
                     fields,
                     functions,
                 }| {
                    CMClass {
                        doc_comment,
                        fields: HashMap::new(), // TODO classes
                        functions,
                    }
                },
            )
            .collect(),
        closure_functions: vec![], // TODO
    }
}

fn static_resolve_type(statics_scope_stack: &[ScopedStatics], ty: &RMType) -> CMType {
    match ty {
        RMType::Void => CMType::Void,
        RMType::Never => CMType::Never,
        RMType::Value(ty) => CMType::Value(static_resolve_value_type(statics_scope_stack, ty)),
    }
}
fn static_resolve_value_type(
    statics_scope_stack: &[ScopedStatics],
    ty: &RMValueType,
) -> CMValueType {
    match ty {
        RMValueType::Identified(ident) => {
            // ref to prioritize higher scope stack frames, which correspond to the innermost scopes.
            if let Some(id) = statics_scope_stack
                .iter()
                .rev()
                .find_map(|statics_elt| statics_elt.classes.get(ident))
            {
                CMValueType::ClassInstance(*id)
            } else {
                todo!("// TODO handle invalid types.")
            }
        }
        RMValueType::Int => CMValueType::Int,
        RMValueType::Float => CMValueType::Float,
        RMValueType::Complex => CMValueType::Complex,
        RMValueType::Bool => CMValueType::Bool,
        RMValueType::String => CMValueType::String,
        RMValueType::FunctionRef { params, return_ty } => CMValueType::FunctionRef {
            params: params
                .iter()
                .map(|ty| static_resolve_value_type(statics_scope_stack, ty))
                .collect(),
            return_ty: Box::new(static_resolve_type(statics_scope_stack, return_ty)),
        },
        RMValueType::Tuple(v) => CMValueType::Tuple(
            v.iter()
                .map(|ty| static_resolve_value_type(statics_scope_stack, ty))
                .collect(),
        ),
    }
}

struct ResolvedFnInfo<'a> {
    ty_return: &'a CMType,
}
fn resolve_fn<'a, Cfg: CMCfg>(
    state: &'a mut ResolverGlobalState<Cfg>,
    current_fn_id: IdentInt,
    current_fn_stack: &mut Vec<IdentInt>,
) -> ResolvedFnInfo<'a> {
    if state.functions[current_fn_id].body.is_translated() {
        // already resolved, get return type
        ResolvedFnInfo {
            ty_return: state.functions[current_fn_id].unwrap_translated_return_ty(),
        }
    } else {
        if current_fn_stack.contains(&current_fn_id) {
            todo!("// TODO find a way to deal with recursive functions");
        }

        let current_fn_ref = state.functions[current_fn_id].clone();
        let (ty_return, all_scoped, block) = current_fn_ref.body.unwrap_untranslated();

        let mut statics_scope_stack = vec![all_scoped.clone()];

        let mut locals = vec![];
        let mut locals_lookup = HashMap::new();
        for (id, (ty_param, ident)) in current_fn_ref
            .params
            .into_iter()
            .zip(current_fn_ref.param_idents.into_iter())
            .enumerate()
        {
            locals.push(LocalVar {
                doc_comment: None,
                ty: WithResolutionStatus::Resolved(ty_param),
                writable: false,
                assigned: true,
            });
            locals_lookup.insert(ident, id);
        }

        let mut loop_context_stack = vec![];

        let mut ty_return_opt = Some(ty_return);

        current_fn_stack.push(current_fn_id);
        let (mut translated, ty_eval) = resolve_block(
            block.clone(),
            state,
            current_fn_stack,
            &mut statics_scope_stack,
            &mut locals,
            &mut locals_lookup,
            &mut loop_context_stack,
            &mut ty_return_opt,
        );
        current_fn_stack.pop();

        let mut translation_failed = false;
        let ty_return = match ty_return_opt.unwrap() {
            WithResolutionStatus::Unknown => ty_eval,
            WithResolutionStatus::Failed => {
                todo!("i'm actually not sure if this case is even possible ¯\\_(ツ)_/¯ (if you see it then it is possible)")
            }
            WithResolutionStatus::Resolved(ty) => {
                // if the types are mismatched and ty_eval is not never (for consistency and debugging), then fail
                if ty_eval.is_never() {
                    // use the return statements if any to infer return type
                    ty
                } else if ty_eval != ty {
                    // typeerror!!
                    state.add_diagnostic(Raw2CommonDiagnostic {
                        text: format!(
                            "type mismatch in function return type, expected {:?}, found {:?}",
                            &ty, &ty_eval
                        ),
                    });
                    translated.push(CMExpression::Fail);
                    // return the expected return type, before the failure
                    ty
                } else {
                    // normal, no mismatches
                    ty
                }
            }
        };
        let current_fn_ref = &mut state.functions[current_fn_id];
        current_fn_ref.body = FunctionBody::Translated {
            block: translated,
            ty_return,
            locals: locals
                .into_iter()
                .map(
                    |LocalVar {
                         ty,
                         writable,
                         doc_comment,
                         ..
                     }| CMLocalVarInfo {
                        doc_comment,
                        ty: match ty {
                            WithResolutionStatus::Resolved(v) => CMType::Value(v),
                            WithResolutionStatus::Failed | WithResolutionStatus::Unknown => {
                                CMType::Never
                            }
                        },
                        writable,
                    },
                )
                .collect(),
        };

        ResolvedFnInfo {
            ty_return: current_fn_ref.unwrap_translated_return_ty(),
        }
    }
}

fn resolve_block<Cfg: CMCfg>(
    block: RMBlock,

    state: &mut ResolverGlobalState<Cfg>,
    current_fn_stack: &mut Vec<IdentInt>,
    statics_stack: &mut Vec<ScopedStatics>,

    locals: &mut Vec<LocalVar>,
    locals_lookup: &mut HashMap<IdentStr, IdentInt>,

    loop_context_stack: &mut Vec<LoopContext>,

    ty_return: &mut Option<WithResolutionStatus<CMType>>,
) -> (Vec<CMExpression<Cfg>>, CMType) {
    // The value that this entire block will evaluate to.
    // It's determined by the last element in the block unless
    // the block evaluates to `never` in the middle, which signifies
    // that the code will never return.
    let mut ty_eval = CMType::Void;
    let mut exprs_out = Vec::with_capacity(block.block.len());

    statics_stack.push(block.inner_scoped);
    let mut locals_lookup_inner = locals_lookup.clone();
    for expr in block.block {
        let (expr, ty_eval_) = resolve_expr(
            expr,
            state,
            current_fn_stack,
            statics_stack,
            locals,
            &mut locals_lookup_inner,
            loop_context_stack,
            ty_return,
        );
        if !ty_eval.is_never() {
            ty_eval = ty_eval_;
            exprs_out.push(expr);
        } else {
            // we don't have to output this code because it's impossible to get to,
            // but we still did the processing up to this point to get the compiler diagnostics.
        }
    }
    statics_stack.pop();

    (exprs_out, ty_eval)
}

fn resolve_expr<Cfg: CMCfg>(
    expr: RMExpression,

    state: &mut ResolverGlobalState<Cfg>,
    current_fn_stack: &mut Vec<IdentInt>,
    statics_stack: &mut Vec<ScopedStatics>,

    locals: &mut Vec<LocalVar>,
    locals_lookup: &mut HashMap<IdentStr, IdentInt>,

    loop_context_stack: &mut Vec<LoopContext>,

    ty_return: &mut Option<WithResolutionStatus<CMType>>,
) -> (CMExpression<Cfg>, CMType) {
    match expr {
        // TODO use CMType::Never to detect dead code
        RMExpression::Void => (CMExpression::Void, CMType::Void),
        RMExpression::DeclareVar {
            doc_comment,
            ident,
            writable,
            initial_value,
            ty,
        } => {
            let ty_var = match ty {
                Some(ty) => Some(static_resolve_value_type(&statics_stack, &ty)),
                None => None,
            };
            let (initial_value, ty_value) = if let Some((initial_value, ty_iv)) =
                initial_value.map(|expr| {
                    resolve_expr(
                        *expr,
                        state,
                        current_fn_stack,
                        statics_stack,
                        locals,
                        locals_lookup,
                        loop_context_stack,
                        ty_return,
                    )
                }) {
                (Some(initial_value), Some(ty_iv))
            } else {
                (None, None)
            };

            let mut is_never = false;
            let mut error = None;

            let ty = match (ty_value, ty_var) {
                (None, ty) => WithResolutionStatus::from_option_none_unknown(ty),
                (Some(CMType::Void), ty_var) => {
                    error = Some(Raw2CommonDiagnostic { text: format!("type mismatch at variable assignment at /* TODO location */! found void, it should never be void")});
                    WithResolutionStatus::from_option_none_unknown(ty_var)
                }
                (Some(CMType::Never), ty_var) => {
                    is_never = true;
                    WithResolutionStatus::from_option_none_unknown(ty_var)
                }
                (Some(CMType::Value(ty_value)), None) => WithResolutionStatus::Resolved(ty_value),
                (Some(CMType::Value(ty_value)), Some(ty_var)) => {
                    if ty_value != ty_var {
                        // type mismatch
                        error = Some(Raw2CommonDiagnostic { text: format!("type mismatch at variable assignment at /* TODO location */! expected {:?}, found {:?}", &ty_var, ty_value)});
                        WithResolutionStatus::Resolved(ty_var)
                    } else {
                        WithResolutionStatus::Resolved(ty_var)
                    }
                }
            };

            let id = locals.len();
            locals.push(LocalVar {
                doc_comment,
                assigned: initial_value.is_some(),
                writable,
                ty,
            });
            locals_lookup.insert(ident, id);

            let expr = if let Some(error) = error {
                CMExpression::Fail
            } else if let Some(initial_value) = initial_value {
                CMExpression::AssignVar {
                    ident: id,
                    value: Box::new(initial_value),
                }
            } else {
                CMExpression::Void
            };

            (
                expr,
                if is_never {
                    CMType::Never
                } else {
                    CMType::Void
                },
            )
        }

        RMExpression::AssignIndex {
            object,
            indices,
            value,
        } => todo!(),
        RMExpression::AssignProperty {
            object,
            property,
            value,
        } => todo!(),
        RMExpression::AssignVar { ident, value } => {
            if let Some(var_id) = locals_lookup.get(&ident).copied() {
                let (value, ty_value) = resolve_expr(
                    *value,
                    state,
                    current_fn_stack,
                    statics_stack,
                    locals,
                    locals_lookup,
                    loop_context_stack,
                    ty_return,
                );
                let var = &mut locals[var_id];
                let mut is_never = false;
                let mut error = None;
                match (&var.ty, ty_value) {
                    (WithResolutionStatus::Failed, _) => { /* already failed, don't bother */ }
                    (WithResolutionStatus::Unknown, CMType::Value(ty_value)) => {
                        // take new type
                        var.ty = WithResolutionStatus::Resolved(ty_value);
                    }
                    (WithResolutionStatus::Resolved(ty_var), CMType::Value(ty_value)) => {
                        // check type
                        if ty_var != &ty_value {
                            error = Some(state.add_diagnostic(Raw2CommonDiagnostic { text: format!("type mismatch at variable assignment at /* TODO location */! expected {:?}, found {:?}", ty_var, ty_value) }));
                        }
                    }
                    (_, CMType::Never) => {
                        is_never = true;
                    }
                    (_, CMType::Void) => {
                        // guaranteed failure
                        error = Some(state.add_diagnostic(Raw2CommonDiagnostic {
                            text: format!("attempt to assign '{ident}' a void value"),
                        }));
                    }
                }
                let eval_ty = if is_never {
                    CMType::Never
                } else {
                    CMType::Void
                };
                if let Some(error) = error {
                    (CMExpression::Fail, eval_ty)
                } else if !var.writable && var.assigned {
                    state.add_diagnostic(Raw2CommonDiagnostic {
                        text: format!("attempt to reassign immutable var '{ident}'"),
                    });
                    (CMExpression::Fail, eval_ty)
                } else {
                    (
                        CMExpression::AssignVar {
                            ident: var_id,
                            value: Box::new(value),
                        },
                        eval_ty,
                    )
                }
            } else {
                state.add_diagnostic(Raw2CommonDiagnostic {
                    text: format!(
                        "attempt to assign to var '{ident}', which has not been declared"
                    ),
                });
                (CMExpression::Fail, CMType::Void)
            }
        }

        RMExpression::ReadIndex { expr, indices } => todo!(),
        RMExpression::ReadProperty {
            expr,
            property_ident,
        } => todo!(),
        RMExpression::Read { ident } => match locals_lookup.get(&ident) {
            Some(id) => {
                let var = &locals[*id];
                let (eval_ty, ty_ok) = match &var.ty {
                    WithResolutionStatus::Resolved(ty) => (CMType::Value(ty.clone()), true),
                    _ => (CMType::Void, false),
                };
                if !var.assigned {
                    state.add_diagnostic(Raw2CommonDiagnostic {
                        text: format!("attempt var '{ident}' before assignment"),
                    });
                    (CMExpression::Fail, eval_ty)
                } else if !ty_ok {
                    todo!("// TODO make a thing that points to the original dianostic (in the meantime this case is impossible)");
                } else {
                    (CMExpression::ReadVar { ident: *id }, eval_ty)
                }
            }
            None => {
                state.add_diagnostic(Raw2CommonDiagnostic {
                    text: format!("attempt to read nonexistant var '{ident}'"),
                });
                (CMExpression::Fail, CMType::Never)
            }
        },

        RMExpression::Call {
            callable,
            arguments,
        } => todo!(),

        RMExpression::LiteralValue(literal) => {
            let (expr, ty) = match literal {
                RMLiteralValue::Int(v) => (CMLiteralValue::Int(v), CMValueType::Int),
                RMLiteralValue::Float(v) => (CMLiteralValue::Float(v), CMValueType::Float),
                RMLiteralValue::Complex(v) => (CMLiteralValue::Complex(v), CMValueType::Complex),
                RMLiteralValue::Bool(v) => (CMLiteralValue::Bool(v), CMValueType::Bool),
                RMLiteralValue::String(v) => (CMLiteralValue::String(v), CMValueType::String),
            };
            (CMExpression::LiteralValue(expr), CMType::Value(ty))
        }
        RMExpression::LiteralRange { start, step, end } => todo!("// TODO range constructor"),
        RMExpression::LiteralArray(_) => todo!(),

        RMExpression::AnonymousFunction { params, block } => todo!(),

        RMExpression::OpBinary { op, lhs, rhs } => todo!(),
        RMExpression::OpUnary { op, value } => todo!(),

        RMExpression::Conditional {
            condition,
            block,
            elifs,
            else_block,
        } => todo!(),
        RMExpression::Loop { block } => {
            // return_value defaults to never because if there are no breaks it won't ever end. it's just that shrimple, such a sofishticated solution, mantastic.
            loop_context_stack.push(LoopContext {
                returnable: true,
                return_value: CMType::Never,
            });
            todo!()
        }
        RMExpression::LoopFor {
            loop_var,
            iterable,
            block,
        } => todo!(),
        RMExpression::LoopContinue => {
            if loop_context_stack.is_empty() {
                state.add_diagnostic(Raw2CommonDiagnostic {
                    text: format!("use of continue outside a loop"),
                });
                (CMExpression::Fail, CMType::Void)
            } else {
                (CMExpression::LoopContinue, CMType::Never)
            }
        }

        RMExpression::LoopBreak(value) => {
            if loop_context_stack.is_empty() {
                state.add_diagnostic(Raw2CommonDiagnostic {
                    text: format!("use of break outside a loop"),
                });
                // no loop, nothing breaks except the code
                (CMExpression::Fail, CMType::Void)
            } else {
                match value {
                    Some(value) => {
                        let (value, ty_value) = resolve_expr(
                            *value,
                            state,
                            current_fn_stack,
                            statics_stack,
                            locals,
                            locals_lookup,
                            loop_context_stack,
                            ty_return,
                        );
                        let loop_context = loop_context_stack.last_mut().unwrap();
                        let error = if loop_context.returnable {
                            match ty_value {
                                CMType::Never => {
                                    // do nothing because this won't ever be reachable executed so we don't have to care
                                    // though we should // TODO warn of unused code
                                    None
                                }
                                CMType::Void => Some(state.add_diagnostic(Raw2CommonDiagnostic {
                                    text: format!("cannot use void as value (in break)"),
                                })),
                                CMType::Value(ty_value) => {
                                    match &loop_context.return_value {
                                        CMType::Never => {
                                            // set loop return value
                                            loop_context.return_value = CMType::Value(ty_value);
                                            None
                                        }
                                        CMType::Void => {
                                            Some(state.add_diagnostic(Raw2CommonDiagnostic {
                                                text: format!(
                                                    "no value expected for break / return"
                                                ),
                                            }))
                                        }
                                        CMType::Value(ty_ret) => {
                                            if &ty_value != ty_ret {
                                                Some(state.add_diagnostic(Raw2CommonDiagnostic {
                                                    text: format!(
                                                        "type mismatch in return / break. expected {:?}, found {:?}", ty_ret, ty_value
                                                    ),
                                                }))
                                            } else {
                                                None
                                            }
                                        }
                                    }
                                }
                            }
                        } else {
                            Some(state.add_diagnostic(Raw2CommonDiagnostic {
                                text: format!("no value expected for break in this kind of loop"),
                            }))
                        };

                        if let Some(error) = error {
                            (CMExpression::Fail, CMType::Never)
                        } else {
                            (
                                CMExpression::LoopBreak(Some(Box::new(value))),
                                CMType::Never,
                            )
                        }
                    }
                    None => {
                        let loop_context = loop_context_stack.last_mut().unwrap();
                        // loop ret should be void or not have return val
                        let error = if loop_context.returnable {
                            match &loop_context.return_value {
                                CMType::Never => {
                                    loop_context.return_value = CMType::Void;
                                    None
                                }
                                CMType::Void => {
                                    // everything checks out
                                    None
                                }
                                CMType::Value(v) => {
                                    Some(state.add_diagnostic(Raw2CommonDiagnostic {
                                        text: format!("expected value in return / break statement"),
                                    }))
                                }
                            }
                        } else {
                            // everything checks out
                            None
                        };
                        if let Some(error) = error {
                            (CMExpression::Fail, CMType::Never)
                        } else {
                            (CMExpression::LoopBreak(None), CMType::Never)
                        }
                    }
                }
            }
        }
        RMExpression::Return(value) => {
            let value = value.map(|expr| {
                resolve_expr(
                    *expr,
                    state,
                    current_fn_stack,
                    statics_stack,
                    locals,
                    locals_lookup,
                    loop_context_stack,
                    ty_return,
                )
            });
            if let Some(ty_return) = ty_return {
                match value {
                    Some((value, ty_value)) => {
                        let error = match ty_return {
                            WithResolutionStatus::Failed => {
                                // already failed, don't bother
                                Some(state.add_diagnostic(Raw2CommonDiagnostic {
                                    text: format!("return type inference failed"),
                                }))
                            }
                            WithResolutionStatus::Unknown => {
                                *ty_return = WithResolutionStatus::Resolved(ty_value);
                                None
                            }
                            WithResolutionStatus::Resolved(ty_return) => {
                                if &ty_value != ty_return {
                                    // type mismatch
                                    Some(state.add_diagnostic(Raw2CommonDiagnostic {
                                        text: format!(
                                            "type mismatch in return, expected {:?}, found {:?}",
                                            ty_return, &ty_value
                                        ),
                                    }))
                                } else {
                                    // correct types, go ahaead
                                    None
                                }
                            }
                        };
                        if let Some(error) = error {
                            (CMExpression::Fail, CMType::Never)
                        } else {
                            (CMExpression::Return(Some(Box::new(value))), CMType::Never)
                        }
                    }
                    None => {
                        let error = match ty_return {
                            WithResolutionStatus::Failed => {
                                // already failed, don't bother
                                Some(state.add_diagnostic(Raw2CommonDiagnostic {
                                    text: format!("return type inference failed"),
                                }))
                            }
                            WithResolutionStatus::Unknown => {
                                *ty_return = WithResolutionStatus::Resolved(CMType::Void);
                                None
                            }
                            WithResolutionStatus::Resolved(ty_return) => {
                                if ty_return != &CMType::Void {
                                    // type mismatch
                                    Some(state.add_diagnostic(Raw2CommonDiagnostic {
                                        text: format!(
                                            "type mismatch in return, expected {:?}, found void",
                                            ty_return
                                        ),
                                    }))
                                } else {
                                    // correct types, go ahaead
                                    None
                                }
                            }
                        };
                        if let Some(error) = error {
                            (CMExpression::Fail, CMType::Never)
                        } else {
                            (CMExpression::Return(None), CMType::Never)
                        }
                    }
                }
            } else {
                state.add_diagnostic(Raw2CommonDiagnostic {
                    text: format!("illegal return outside function"),
                });
                (CMExpression::Fail, CMType::Void)
            }
        }
    }
}
