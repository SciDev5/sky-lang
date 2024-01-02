use std::{collections::HashMap, fmt::Debug};

use crate::{
    common::{
        common_module::{
            CMClass, CMExpression, CMFunction, CMLiteralValue, CMLocalVarInfo, CMType, CMValueType,
            CommonModule,
        },
        IdentInt, IdentStr,
    },
    parse::fn_lookup::get_fn_lut,
};

use super::{
    fn_lookup::{lookup_fallback, AssociatedFnLut, FnRef},
    raw_module::{
        RMBlock, RMClass, RMExpression, RMLiteralValue, RMType, RMValueType, RawModule,
        ScopedStatics,
    },
};

#[derive(Debug, Clone)]
struct FunctionThing {
    doc_comment: Option<String>,
    params: Vec<CMValueType>,
    param_idents: Vec<IdentStr>,
    body: FunctionBody,
}
impl FunctionThing {
    fn unwrap_translated_return_ty(&self) -> &CMType {
        match &self.body {
            FunctionBody::Translated { ty_return, .. } => ty_return,
            _ => panic!(),
        }
    }
}
#[derive(Debug, Clone)]
enum FunctionBody {
    Untranslated {
        ty_return: WithResolutionStatus<CMType>,
        all_scoped: ScopedStatics,
        block: RMBlock,
    },
    Translated {
        ty_return: CMType,
        locals: Vec<CMLocalVarInfo>,
        block: Vec<CMExpression>,
    },
}
impl FunctionBody {
    fn unwrap_translated(self) -> (CMType, Vec<CMExpression>, Vec<CMLocalVarInfo>) {
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

struct ResolverGlobalState {
    functions: Vec<FunctionThing>,
    diagnostics: Vec<Raw2CommonDiagnostic>,
}
impl ResolverGlobalState {
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

pub fn raw_2_common(raw_module: RawModule) -> CommonModule {
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
fn resolve_fn<'a>(
    state: &'a mut ResolverGlobalState,
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

fn resolve_block(
    block: RMBlock,

    state: &mut ResolverGlobalState,
    current_fn_stack: &mut Vec<IdentInt>,
    statics_stack: &mut Vec<ScopedStatics>,

    locals: &mut Vec<LocalVar>,
    locals_lookup: &mut HashMap<IdentStr, IdentInt>,

    loop_context_stack: &mut Vec<LoopContext>,

    ty_return: &mut Option<WithResolutionStatus<CMType>>,
) -> (Vec<CMExpression>, CMType) {
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

fn lookup_fn_ref_ty_return(
    state: &mut ResolverGlobalState,
    current_fn_stack: &mut Vec<IdentInt>,
    fn_ref: FnRef,
    params: &[CMValueType],
) -> CMType {
    match fn_ref {
        FnRef::ModuleFunction(id) => resolve_fn(state, id, current_fn_stack).ty_return.clone(),
        FnRef::Identity => CMType::Value(params[0].clone()),
        FnRef::Intrinsic1(id) => id.ty_ret(),
        FnRef::Intrinsic2(id) => id.ty_ret(),
        FnRef::Intrinsic(id) => id.ty_ret(),
    }
}

fn resolve_expr(
    expr: RMExpression,

    state: &mut ResolverGlobalState,
    current_fn_stack: &mut Vec<IdentInt>,
    statics_stack: &mut Vec<ScopedStatics>,

    locals: &mut Vec<LocalVar>,
    locals_lookup: &mut HashMap<IdentStr, IdentInt>,

    loop_context_stack: &mut Vec<LoopContext>,

    ty_return: &mut Option<WithResolutionStatus<CMType>>,
) -> (CMExpression, CMType) {
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
                    error = Some(state.add_diagnostic(Raw2CommonDiagnostic { text: format!("type mismatch at variable assignment at /* TODO location */! found void, it should never be void")}));
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
                        error = Some(state.add_diagnostic(Raw2CommonDiagnostic { text: format!("type mismatch at variable assignment at /* TODO location */! expected {:?}, found {:?}", &ty_var, ty_value)}));
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
                    var.assigned = true;
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
        } => {
            let n_args = arguments.len();
            let mut resolved_arguments = Vec::with_capacity(n_args);
            let mut ty_arguments = Vec::with_capacity(n_args);
            let mut args_ok = true;
            for arg in arguments {
                let (resolved, ty) = resolve_expr(
                    arg,
                    state,
                    current_fn_stack,
                    statics_stack,
                    locals,
                    locals_lookup,
                    loop_context_stack,
                    ty_return,
                );
                resolved_arguments.push(resolved);
                match ty {
                    CMType::Value(ty) => {
                        ty_arguments.push(Some(ty));
                    }
                    _ => {
                        args_ok = false;
                        ty_arguments.push(None);
                        break;
                    }
                }
            }

            if args_ok {
                let ty_arguments = ty_arguments
                    .into_iter()
                    .map(Option::unwrap)
                    .collect::<Vec<_>>();
                match callable.as_ref() {
                    RMExpression::Read { ident } => {
                        // function call
                        let mut best_fallback = None;
                        let mut had_semimatch = false;
                        for function_id in statics_stack
                            .iter()
                            .rev()
                            .flat_map(|frame| frame.functions.get(ident))
                            .flatten()
                            .copied()
                        {
                            let k = &state.functions[function_id];
                            if &k.params == &ty_arguments {
                                let ResolvedFnInfo { ty_return } =
                                    resolve_fn(state, function_id, current_fn_stack);
                                return (
                                    CMExpression::Call {
                                        function_id: FnRef::ModuleFunction(function_id),
                                        arguments: resolved_arguments,
                                        always_inline: false,
                                        inlined_lambdas: None,
                                    },
                                    ty_return.clone(),
                                );
                            } else if k.params.len() == ty_arguments.len() {
                                if had_semimatch {
                                    best_fallback = None;
                                } else {
                                    best_fallback = Some(function_id)
                                }
                                had_semimatch = true;
                            }
                        }
                        // did not find lny suitable call signatures
                        state.add_diagnostic(Raw2CommonDiagnostic {
                            text: format!("did not find any suitable call signatures"),
                        });
                        // try to salvage a return value to try to yield as much useful results to the user as possible
                        let return_ty = if let Some(fallback_id) = best_fallback {
                            let ResolvedFnInfo { ty_return } =
                                resolve_fn(state, fallback_id, current_fn_stack);
                            ty_return.clone()
                        } else {
                            CMType::Never
                        };
                        (CMExpression::FailAfter(resolved_arguments), return_ty)
                    }
                    RMExpression::ReadProperty {
                        expr,
                        property_ident,
                    } => {
                        todo!("named associated functions");
                    }
                    _ => {
                        // fail
                        state.add_diagnostic(Raw2CommonDiagnostic {
                            text: format!("object is not callable"),
                        });
                        (CMExpression::Fail, CMType::Never)
                    }
                }
            } else {
                state.add_diagnostic(Raw2CommonDiagnostic {
                    text: format!("illegal void parameter"),
                });
                match callable.as_ref() {
                    RMExpression::Read { ident } => {
                        let fallback_id = lookup_fallback(
                            &ty_arguments,
                            statics_stack
                                .iter()
                                .rev()
                                .flat_map(|frame| frame.functions.get(ident))
                                .flatten()
                                .map(|i| (&state.functions[*i].params, FnRef::ModuleFunction(*i))),
                        );
                        if let Some(FnRef::ModuleFunction(fallback_id)) = fallback_id {
                            // we have an assumption for what real function this is, use it to fill in later type annotations.
                            let ResolvedFnInfo { ty_return } =
                                resolve_fn(state, fallback_id, current_fn_stack);

                            (
                                CMExpression::FailAfter(resolved_arguments),
                                ty_return.clone(),
                            )
                        } else {
                            (CMExpression::FailAfter(resolved_arguments), CMType::Void)
                        }
                    }
                    RMExpression::ReadProperty {
                        expr,
                        property_ident,
                    } => {
                        todo!("named associated functions");
                        // (CMExpression::Fail, CMType::Void)
                    }
                    _ => {
                        // fail
                        state.add_diagnostic(Raw2CommonDiagnostic {
                            text: format!("object is not callable"),
                        });
                        (CMExpression::Fail, CMType::Void)
                    }
                }
            }
        }

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

        RMExpression::OpBinary { op, lhs, rhs } => {
            let (lhs, ty_lhs) = resolve_expr(
                *lhs,
                state,
                current_fn_stack,
                statics_stack,
                locals,
                locals_lookup,
                loop_context_stack,
                ty_return,
            );
            let (rhs, ty_rhs) = resolve_expr(
                *rhs,
                state,
                current_fn_stack,
                statics_stack,
                locals,
                locals_lookup,
                loop_context_stack,
                ty_return,
            );
            match (ty_lhs, ty_rhs) {
                (CMType::Never, _) => {
                    // just do the first thing, it will never finish anyway so dont bother wrapping it anyway
                    state.add_diagnostic(Raw2CommonDiagnostic {
                        text: format!("cannot operate on never"),
                    });
                    (lhs, CMType::Never)
                }
                (CMType::Void | CMType::Value(_), CMType::Never) => {
                    state.add_diagnostic(Raw2CommonDiagnostic {
                        text: format!("cannot operate on never"),
                    });
                    (CMExpression::FailAfter(vec![lhs, rhs]), CMType::Never)
                }
                (CMType::Void, CMType::Void | CMType::Value(_)) => {
                    state.add_diagnostic(Raw2CommonDiagnostic {
                        text: format!("cannot operate on void"),
                    });
                    (CMExpression::FailAfter(vec![lhs, rhs]), CMType::Never)
                }
                (CMType::Value(_), CMType::Void) => {
                    state.add_diagnostic(Raw2CommonDiagnostic {
                        text: format!("cannot operate on void"),
                    });
                    (CMExpression::FailAfter(vec![lhs, rhs]), CMType::Never)
                }
                (CMType::Value(ty_lhs), CMType::Value(ty_rhs)) => {
                    let lut_lhs = get_fn_lut(&ty_lhs);
                    let lut_rhs = get_fn_lut(&ty_rhs);
                    if let Some(function_id) = AssociatedFnLut::lookup_binary(
                        (lut_lhs, ty_lhs.clone()),
                        (lut_rhs, ty_rhs.clone()),
                        op,
                    ) {
                        let ty_return =
                            lookup_fn_ref_ty_return(state, current_fn_stack, function_id, &vec![]);
                        (
                            CMExpression::Call {
                                function_id,
                                arguments: vec![lhs, rhs],
                                always_inline: false,
                                inlined_lambdas: None,
                            },
                            ty_return,
                        )
                    } else {
                        state.add_diagnostic(Raw2CommonDiagnostic { text: format!("no valid operation found for types {:?} and {:?} and operator {:?}", ty_lhs, ty_rhs, op) });
                        (CMExpression::FailAfter(vec![lhs, rhs]), CMType::Never)
                    }
                }
            }
        }
        RMExpression::OpUnary { op, value } => {
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
            match ty_value {
                CMType::Value(ty) => {
                    let lut = get_fn_lut(&ty);
                    if let Some(function_id) = lut.lookup_unary(op) {
                        let ty_return =
                            lookup_fn_ref_ty_return(state, current_fn_stack, function_id, &[ty]);
                        (
                            CMExpression::Call {
                                function_id,
                                arguments: vec![value],
                                always_inline: false,
                                inlined_lambdas: None,
                            },
                            ty_return,
                        )
                    } else {
                        state.add_diagnostic(Raw2CommonDiagnostic {
                            text: format!(
                                "no valid operation found for type {:?} and operator {:?}",
                                ty, op
                            ),
                        });
                        (CMExpression::FailAfter(vec![value]), CMType::Never)
                    }
                }
                CMType::Void => {
                    state.add_diagnostic(Raw2CommonDiagnostic {
                        text: format!("cannot operate on void"),
                    });
                    (CMExpression::FailAfter(vec![value]), CMType::Never)
                }
                CMType::Never => {
                    // just do the first thing, it will never finish anyway so dont bother wrapping it anyway
                    state.add_diagnostic(Raw2CommonDiagnostic {
                        text: format!("cannot operate on never"),
                    });
                    (value, CMType::Never)
                }
            }
        }

        RMExpression::Conditional {
            condition,
            block,
            elifs,
            else_block,
        } => {
            fn check_block(
                state: &mut ResolverGlobalState,
                ty_eval: &mut CMType,
                (mut block, ty_block): (Vec<CMExpression>, CMType),
            ) -> Vec<CMExpression> {
                let is_type_mismatch = match (&ty_eval, ty_block) {
                    (_, CMType::Never) => None, /* nothing changes */
                    (CMType::Never, ty_block) => {
                        *ty_eval = ty_block;
                        None
                    }
                    (CMType::Void, CMType::Void) => None, /* ok */
                    (CMType::Value(a), CMType::Value(b)) => {
                        /* ok if no type mismatch */
                        if a != &b {
                            Some(CMType::Value(b))
                        } else {
                            None
                        }
                    }
                    (_, ty_block) => {
                        /* type mismatch */
                        Some(ty_block)
                    }
                };
                if let Some(ty_found_incorrect) = is_type_mismatch {
                    state.add_diagnostic(Raw2CommonDiagnostic {
                        text: format!(
                            "type mismatch in conditional block, expected {:?}, found {:?}",
                            ty_eval, ty_found_incorrect
                        ),
                    });
                    block.push(CMExpression::Fail);
                }
                block
            }
            /// Check condition so if it doesn't evaluate to a boolean it fails.
            fn check_condition(
                state: &mut ResolverGlobalState,
                (condition, ty_condition): (CMExpression, CMType),
            ) -> CMExpression {
                match ty_condition {
                    CMType::Value(CMValueType::Bool) | CMType::Never => condition,
                    _ => {
                        state.add_diagnostic(Raw2CommonDiagnostic {
                            text: format!("condition was not boolean"),
                        });
                        CMExpression::FailAfter(vec![condition])
                    }
                }
            }

            let mut ty_eval = if else_block.is_none() {
                CMType::Void
            } else {
                CMType::Never
            };
            let condition = resolve_expr(
                *condition,
                state,
                current_fn_stack,
                statics_stack,
                locals,
                locals_lookup,
                loop_context_stack,
                ty_return,
            );
            let condition = check_condition(state, condition);
            let block = resolve_block(
                block,
                state,
                current_fn_stack,
                statics_stack,
                locals,
                locals_lookup,
                loop_context_stack,
                ty_return,
            );
            let block = check_block(state, &mut ty_eval, block);

            let mut elifs_ = vec![];
            for (condition, block) in elifs {
                let condition = resolve_expr(
                    condition,
                    state,
                    current_fn_stack,
                    statics_stack,
                    locals,
                    locals_lookup,
                    loop_context_stack,
                    ty_return,
                );
                let condition = check_condition(state, condition);
                let block = resolve_block(
                    block,
                    state,
                    current_fn_stack,
                    statics_stack,
                    locals,
                    locals_lookup,
                    loop_context_stack,
                    ty_return,
                );
                let block = check_block(state, &mut ty_eval, block);

                elifs_.push((condition, block))
            }
            let elifs = elifs_;

            let else_block = if let Some(else_block) = else_block {
                let else_block = resolve_block(
                    else_block,
                    state,
                    current_fn_stack,
                    statics_stack,
                    locals,
                    locals_lookup,
                    loop_context_stack,
                    ty_return,
                );
                let else_block = check_block(state, &mut ty_eval, else_block);

                Some(else_block)
            } else {
                // None
                Some(vec![CMExpression::Void])
            };

            (
                CMExpression::Conditional {
                    condition: Box::new(condition),
                    block,
                    elifs,
                    else_block,
                },
                ty_eval,
            )
        }
        RMExpression::Loop { block } => {
            // return_value defaults to never because if there are no breaks it won't ever end. it's just that shrimple, such a sofishticated solution, mantastic.
            loop_context_stack.push(LoopContext {
                returnable: true,
                return_value: CMType::Never,
            });
            let (block, _) = resolve_block(
                block,
                state,
                current_fn_stack,
                statics_stack,
                locals,
                locals_lookup,
                loop_context_stack,
                ty_return,
            );
            let LoopContext { return_value, .. } = loop_context_stack
                .pop()
                .expect("loop_context_stack somehow got drained");
            (
                CMExpression::Loop {
                    block,
                    is_infinite: return_value.is_never(),
                },
                return_value,
            )
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
