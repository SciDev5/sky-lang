use std::{
    collections::{HashMap, HashSet},
    fmt::Debug,
};

use crate::{
    build::module_tree::{
        FullId, ModuleTree, ModuleTreeLookup, ModuleTreeLookupError, MultiplatformFullId,
    },
    common::{
        backend::PlatformInfo,
        common_module::{
            CMAssociatedFunction, CMExpression, CMFunction, CMFunctionInfo, CMLiteralValue,
            CMLocalVarInfo, CMStruct, CMTopLevelBlock, CMType, CommonModule, DocComment,
        },
        IdentInt, IdentStr,
    },
    parse::fn_lookup::get_fn_lut,
};

use super::{
    fn_lookup::{gen_struct_fn_lut, AssociatedFnLut, FnRef},
    macros::MacroCall,
    raw_module::{
        LiteralStructInit, RMBlock, RMExpression, RMFunction, RMFunctionInfo, RMLiteralValue,
        RMStruct, RMTrait, RMTraitImpl, RMType, RawModule, RawModuleTopLevel, ScopedStatics,
    },
};

#[derive(Debug, Clone)]
struct PartiallyResolvedFunction {
    attrs: Vec<MacroCall<CMExpression>>,
    doc_comment: DocComment,
    params: Vec<CMType>,
    param_idents: Vec<IdentStr>,
    body: PartiallyResolvedFunctionBody,
}
impl PartiallyResolvedFunction {
    fn return_ty(&self) -> Option<&CMType> {
        self.body.return_ty()
    }
}
#[derive(Debug, Clone)]
enum PartiallyResolvedFunctionBody {
    Disembodied {
        ty_return: CMType,
    },
    Untranslated {
        ty_return: Option<CMType>,
        all_scoped: ScopedStatics,
        block: RMBlock,
    },
    Translated {
        ty_return: CMType,
        locals: Vec<CMLocalVarInfo>,
        block: Vec<CMExpression>,
    },
}
impl PartiallyResolvedFunctionBody {
    fn unwrap_translated(self) -> (CMType, Vec<CMExpression>, Vec<CMLocalVarInfo>) {
        match self {
            PartiallyResolvedFunctionBody::Disembodied { ty_return } => {
                let fail_expr = CMExpression::Fail;
                (ty_return, vec![fail_expr], vec![])
            }
            PartiallyResolvedFunctionBody::Translated {
                ty_return,
                block,
                locals,
            } => (ty_return, block, locals),
            _ => panic!(),
        }
    }
    // fn unwrap_untranslated(self) -> (CMType, ScopedStatics, RMBlock) {
    //     match self {
    //         PartiallyResolvedFunctionBody::Untranslated {
    //             ty_return,
    //             all_scoped,
    //             block,
    //         } => (ty_return, all_scoped, block),
    //         _ => panic!(),
    //     }
    // }
    // fn is_translated(&self) -> bool {
    //     matches!(self, PartiallyResolvedFunctionBody::Translated { .. })
    // }
    fn return_ty(&self) -> Option<&CMType> {
        match self {
            Self::Disembodied { ty_return } => Some(ty_return),
            Self::Translated { ty_return, .. } => Some(ty_return),
            Self::Untranslated { ty_return, .. } => ty_return.as_ref(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct PartiallyResolvedStruct {
    attrs: Vec<MacroCall<CMExpression>>,
    doc_comment: DocComment,
    fields: Vec<CMType>,
    fields_info: HashMap<IdentStr, (IdentInt, DocComment)>,

    pub impl_functions: HashMap<IdentStr, CMAssociatedFunction>,
    pub impl_traits: HashMap<IdentInt, RMTraitImpl>,

    pub fn_lut_inst: AssociatedFnLut,
    pub fn_lut_clss: AssociatedFnLut,
}
#[derive(Debug, Clone)]
pub struct PartiallyResolvedTrait {
    attrs: Vec<MacroCall<CMExpression>>,
    doc_comment: DocComment,

    functions: HashMap<String, CMAssociatedFunction>,
}
pub struct ResolverGlobalState {
    submodule_tree: ModuleTree,
    functions: Vec<PartiallyResolvedFunction>,
    pub structs: Vec<PartiallyResolvedStruct>,
    pub traits: Vec<PartiallyResolvedTrait>,
    diagnostics: Vec<Raw2CommonDiagnostic>,
}
impl ResolverGlobalState {
    fn add_diagnostic(&mut self, diagnostic: Raw2CommonDiagnostic) -> Raw2CommonDiagnostic {
        self.diagnostics.push(diagnostic.clone());
        dbg!(diagnostic)
    }
}

#[derive(Debug, Clone)]
struct LocalVar {
    doc_comment: DocComment,
    ty: CMType,
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

pub fn raw_2_common(
    RawModule {
        functions,
        structs,
        traits,
        top_level,
        mut submodule_tree,
        base_supported_backend,
    }: RawModule,
) -> CommonModule {
    let mut diagnostics = vec![];
    let mut state = ResolverGlobalState {
        functions: functions
            .into_iter()
            .map(
                |RMFunction {
                     info:
                         RMFunctionInfo {
                             attrs,
                             doc_comment,
                             params,
                             local_template_defs,
                             return_ty,
                             all_scoped,
                             can_be_disembodied,
                         },
                     block,
                 }| {
                    let all_scoped = all_scoped.finish_imports(&submodule_tree);
                    PartiallyResolvedFunction {
                        attrs: resolve_attrs(attrs),
                        doc_comment,
                        params: params
                            .iter()
                            .map(|(_, ty)| static_resolve_type(&[all_scoped.clone()], ty))
                            .collect(),
                        param_idents: params.into_iter().map(|(ident, _)| ident).collect(),
                        body: match block {
                            Some(block) => PartiallyResolvedFunctionBody::Untranslated {
                                ty_return: match &return_ty {
                                    Some(ty) => {
                                        Some(static_resolve_type(&[all_scoped.clone()], ty))
                                    }
                                    None => None,
                                },
                                all_scoped,
                                block,
                            },
                            None if can_be_disembodied.is_true() => {
                                PartiallyResolvedFunctionBody::Disembodied {
                                    ty_return: match &return_ty {
                                        Some(ty) => static_resolve_type(&[all_scoped.clone()], ty),
                                        None => {
                                            diagnostics.push(Raw2CommonDiagnostic {
                                                text: format!(
                                                    "disembodied function must have return value"
                                                ),
                                            });
                                            CMType::Unknown
                                        }
                                    },
                                }
                            }
                            None => {
                                diagnostics.push(Raw2CommonDiagnostic {
                                    text: format!("function must have body"),
                                });
                                PartiallyResolvedFunctionBody::Translated {
                                    ty_return: return_ty.map_or(CMType::Unknown, |ty| {
                                        static_resolve_type(&[all_scoped.clone()], &ty)
                                    }),
                                    locals: vec![],
                                    block: vec![CMExpression::Fail],
                                }
                            }
                        },
                    }
                },
            )
            .collect(),
        structs: structs
            .into_iter()
            .map(
                |RMStruct {
                     attrs,
                     doc_comment,
                     fields,
                     impl_functions,
                     impl_traits,
                     all_scoped,
                 }| {
                    let all_scoped = all_scoped.finish_imports(&submodule_tree);
                    let (fields, fields_info) = fields
                        .into_iter()
                        .enumerate()
                        .map(|(i, (ident, (ty, doc_comment)))| {
                            (
                                static_resolve_type(&[all_scoped.clone()], &ty),
                                (ident, (i, doc_comment)),
                            )
                        })
                        .unzip();
                    PartiallyResolvedStruct {
                        attrs: resolve_attrs(attrs),
                        doc_comment,
                        fields,
                        fields_info,
                        fn_lut_inst: gen_struct_fn_lut(&impl_functions, true),
                        fn_lut_clss: gen_struct_fn_lut(&impl_functions, false),
                        impl_functions,
                        impl_traits: impl_traits
                            .into_iter()
                            .map(|(trait_id, trait_impl)| {
                                (
                                    static_resolve_trait(&[all_scoped.clone()], &trait_id),
                                    trait_impl,
                                )
                            })
                            .collect(),
                    }
                },
            )
            .collect(),
        traits: traits
            .into_iter()
            .map(
                |RMTrait {
                     attrs,
                     doc_comment,
                     functions,
                     all_scoped,
                 }| {
                    PartiallyResolvedTrait {
                        attrs: resolve_attrs(attrs),
                        doc_comment,
                        functions,
                    }
                },
            )
            .collect(),
        submodule_tree,
        diagnostics,
    };

    // translate top-level block
    let mut current_fn_stack = vec![];
    let mut statics_stack = vec![];
    let mut loop_context_stack = vec![];
    let mut ty_return = None;
    let top_level = top_level
        .into_iter()
        .map(|RawModuleTopLevel { block, mod_type }| {
            let mut locals = vec![];
            let mut locals_lookup = HashMap::new();
            let (top_level_code, ty_eval) = resolve_block(
                block,
                &mut state,
                &mut current_fn_stack,
                &mut statics_stack,
                &mut locals,
                &mut locals_lookup,
                &mut loop_context_stack,
                &mut ty_return,
            );
            let locals = locals
                .into_iter()
                .map(
                    |LocalVar {
                         doc_comment,
                         ty,
                         writable,
                         ..
                     }| CMLocalVarInfo {
                        doc_comment,
                        ty,
                        writable,
                    },
                )
                .collect();
            CMTopLevelBlock {
                code: top_level_code,
                locals,
                ty_eval,

                mod_type,
            }
        })
        .collect::<Vec<_>>();

    // translate all functions (automatically skips reresolving functions we've already resolved)
    for current_fn_id in 0..state.functions.len() {
        resolve_fn(&mut state, current_fn_id, &mut current_fn_stack, true);
    }

    CommonModule {
        top_level,
        functions: state
            .functions
            .into_iter()
            .map(
                |PartiallyResolvedFunction {
                     attrs,
                     doc_comment,
                     params,
                     param_idents,
                     body,
                 }| {
                    let (ty_return, block, locals) = body.unwrap_translated();
                    CMFunction {
                        info: CMFunctionInfo {
                            attrs,
                            doc_comment,
                            params,
                            ty_return,
                        },
                        locals,
                        block,
                    }
                },
            )
            .collect(),
        structs: state
            .structs
            .into_iter()
            .map(
                |PartiallyResolvedStruct {
                     attrs,
                     doc_comment,
                     fields,
                     fields_info,
                     impl_functions,
                     impl_traits,
                     fn_lut_clss: _,
                     fn_lut_inst: _,
                 }| {
                    CMStruct {
                        attrs,
                        doc_comment,
                        fields,
                        fields_info,
                        // impl_functions,
                        // impl_traits,
                    }
                },
            )
            .collect(),
        // traits: state.traits
        closure_functions: vec![], // TODO
        submodule_tree: state.submodule_tree,

        base_supported_backend,
    }
}

struct StaticLookupFail<'a> {
    resolved_part: &'a [IdentStr],
}
type StaticLookup<'a, T> = Result<T, StaticLookupFail<'a>>;

fn statics_stack_resolve<F: Fn(&ScopedStatics) -> Option<T>, T>(
    statics_scope_stack: &[ScopedStatics],
    f: F,
) -> Option<T> {
    // reverse to prioritize higher scope stack frames, which correspond to the innermost scopes.
    statics_scope_stack.iter().rev().find_map(f)
}
fn static_lookup_fn<'a>(
    submodule_tree: &ModuleTree,
    statics_scope_stack: &[ScopedStatics],
    resolution_chain: &'a [IdentStr],
    platform: &PlatformInfo,
) -> StaticLookup<'a, MultiplatformFullId> {
    if resolution_chain.len() == 1 {
        let ident = &resolution_chain[0];

        eprintln!("// TODO handle overloads or disallow them");
        statics_stack_resolve(statics_scope_stack, |scope| scope.functions.get(ident))
            .ok_or(StaticLookupFail { resolved_part: &[] })
    } else {
        // reverse to prioritize higher scope stack frames, which correspond to the innermost scopes.
        for scoped_statics in statics_scope_stack.iter().rev() {
            if let Some(lookup) =
                static_lookup_helper_lookup(submodule_tree, scoped_statics, &resolution_chain[0])
            {
                return match submodule_tree
                    .get(lookup, &resolution_chain[1..])
                    .map_err(|ok_count| ok_count + 1)
                {
                    Ok(ModuleTreeLookup::Function(id)) => StaticLookupFn::Function(id),
                    Ok(
                        ModuleTreeLookup::Struct(id)
                        | ModuleTreeLookup::Trait(id)
                        | ModuleTreeLookup::Module(id),
                    ) => StaticLookupFn::Fail {
                        resolved_part: &resolution_chain[..resolution_chain.len() - 1],
                    },
                    Err(ModuleTreeLookupError {
                        n_matched_before_fail,
                    }) => StaticLookupFn::Fail {
                        resolved_part: &resolution_chain[..n_matched_before_fail],
                    },
                };
            }
        }

        StaticLookupFn::Fail {
            resolved_part: &resolution_chain[..0],
        }
    }
}

fn static_resolve_type(statics_scope_stack: &[ScopedStatics], ty: &RMType) -> CMType {
    match ty {
        RMType::Void => CMType::Void,
        RMType::Never => CMType::Never,

        RMType::Identified(ident) => {
            // reverse to prioritize higher scope stack frames, which correspond to the innermost scopes.
            if let Some(id) = statics_scope_stack
                .iter()
                .rev()
                .find_map(|statics_elt| statics_elt.structs.get(ident))
            {
                CMType::StructInstance(*id)
            } else {
                todo!("// TODO handle invalid types.")
            }
        }
        RMType::Int => CMType::Int,
        RMType::Float => CMType::Float,
        RMType::Complex => CMType::Complex,
        RMType::Bool => CMType::Bool,
        RMType::String => CMType::String,
        RMType::FunctionRef { params, return_ty } => CMType::FunctionRef {
            params: params
                .iter()
                .map(|ty| static_resolve_type(statics_scope_stack, ty))
                .collect(),
            return_ty: Box::new(static_resolve_type(statics_scope_stack, return_ty)),
        },
        RMType::Tuple(v) => CMType::Tuple(
            v.iter()
                .map(|ty| static_resolve_type(statics_scope_stack, ty))
                .collect(),
        ),
    }
}
fn static_resolve_trait(statics_scope_stack: &[ScopedStatics], id: &IdentStr) -> IdentInt {
    if let Some(id) = statics_scope_stack
        .iter()
        .rev()
        .find_map(|statics_elt| statics_elt.traits.get(id))
    {
        *id
    } else {
        todo!("handle trait not found");
    }
}

fn resolve_macro(macro_call: MacroCall<RMExpression>) -> MacroCall<CMExpression> {
    macro_call.lazy_map(|_| todo!("// TODO resolve_attrs handle exprs"))
}
fn resolve_attrs(attrs: Vec<MacroCall<RMExpression>>) -> Vec<MacroCall<CMExpression>> {
    attrs
        .into_iter()
        .map(|macro_call| resolve_macro(macro_call))
        .collect()
}

struct ResolvedFnInfo<'a> {
    ty_return: &'a CMType,
    ty_args: &'a [CMType],
}

fn resolve_fn_full_id<'a>(
    state: &'a mut ResolverGlobalState,
    function_id: FullId,
    current_fn_stack: &mut Vec<IdentInt>,
) -> ResolvedFnInfo<'a> {
    match function_id {
        FullId::Local(id) => resolve_fn(state, id, current_fn_stack, false),
        FullId::NonLocal { dependency_id, id } => {
            let func = &state.submodule_tree.get_dependency(dependency_id).functions[id];
            ResolvedFnInfo {
                ty_return: &func.info.ty_return,
                ty_args: &func.info.params,
            }
        }
    }
}
fn resolve_fn<'a>(
    state: &'a mut ResolverGlobalState,
    current_fn_id: IdentInt,
    current_fn_stack: &mut Vec<IdentInt>,
    force_translate: bool,
) -> ResolvedFnInfo<'a> {
    match &state.functions[current_fn_id].body {
        PartiallyResolvedFunctionBody::Disembodied { .. }
        | PartiallyResolvedFunctionBody::Translated { .. } => {
            let func = &state.functions[current_fn_id];
            return ResolvedFnInfo {
                ty_return: func.return_ty().unwrap(),
                ty_args: &func.params,
            };
        }
        PartiallyResolvedFunctionBody::Untranslated {
            ty_return: Some(_), ..
        } if force_translate => {
            let func = &state.functions[current_fn_id];
            return ResolvedFnInfo {
                ty_return: func.return_ty().unwrap(),
                ty_args: &func.params,
            };
        }
        _ => {}
    }
    if current_fn_stack.contains(&current_fn_id) {
        todo!("// TODO find a way to deal with recursive functions");
    }

    let current_fn_ref = state.functions[current_fn_id].clone();
    let PartiallyResolvedFunctionBody::Untranslated { ty_return, all_scoped, block } = current_fn_ref.body else {
        unreachable!()
    };

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
            ty: ty_param,
            writable: false,
            assigned: true,
        });
        locals_lookup.insert(ident, id);
    }

    let mut loop_context_stack = vec![];

    let mut ty_return_opt = Some(ty_return.unwrap_or(CMType::Unknown));

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
        CMType::Unknown => ty_eval,
        CMType::Never => unreachable!("// TODO check that this can't happen"),
        ty => {
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
    current_fn_ref.body = PartiallyResolvedFunctionBody::Translated {
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
                    ty,
                    writable,
                },
            )
            .collect(),
    };

    ResolvedFnInfo {
        ty_return: current_fn_ref.return_ty().unwrap(),
        ty_args: &current_fn_ref.params,
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

    ty_return: &mut Option<CMType>,
) -> (Vec<CMExpression>, CMType) {
    // The value that this entire block will evaluate to.
    // It's determined by the last element in the block unless
    // the block evaluates to `never` in the middle, which signifies
    // that the code will never return.
    let mut ty_eval = CMType::Void;
    let mut exprs_out = Vec::with_capacity(block.block.len());

    statics_stack.push(block.inner_scoped.finish_imports(&state.submodule_tree));
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
    params: &[CMType],
) -> CMType {
    match fn_ref {
        FnRef::ModuleFunction(id) => resolve_fn(state, id, current_fn_stack, false)
            .ty_return
            .clone(),
        FnRef::Identity => params[0].clone(),
        FnRef::Intrinsic1(id) => id.ty_ret(),
        FnRef::Intrinsic2(id) => id.ty_ret(),
        FnRef::Intrinsic(id) => id.ty_ret(),
    }
}

/// Attempt to match expressions of the form `a.b.c.d`
fn trace_ident_chain(mut expr: &RMExpression) -> Option<Vec<&IdentStr>> {
    let mut path = vec![];
    loop {
        expr = match expr {
            RMExpression::Ident { ident } => {
                path.push(ident);
                break;
            }
            RMExpression::ReadProperty {
                expr,
                property_ident,
            } => {
                path.push(property_ident);
                expr.as_ref()
            }
            _ => return None,
        }
    }
    path.reverse();
    if path.len() > 0 {
        Some(path)
    } else {
        None
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

    ty_return: &mut Option<CMType>,
) -> (CMExpression, CMType) {
    match expr {
        RMExpression::CallIntrinsic { id, arguments } => {
            let (arguments, ty_args): (Vec<_>, Vec<_>) = arguments
                .into_iter()
                .map(|expr| {
                    resolve_expr(
                        expr,
                        state,
                        current_fn_stack,
                        statics_stack,
                        locals,
                        locals_lookup,
                        loop_context_stack,
                        ty_return,
                    )
                })
                .unzip();
            let Some(ty_ret) = id.ty_ret(ty_args) else {
                state.add_diagnostic(Raw2CommonDiagnostic { text: format!("intrinsic funciton call received wrong number of arguments") });
                return (CMExpression::FailAfter(arguments), CMType::Unknown)
            };
            (CMExpression::CallIntrinsic { id, arguments }, ty_ret)
        }

        // TODO use CMType::Never to detect dead code
        RMExpression::Void => (CMExpression::Void, CMType::Void),
        RMExpression::DeclareVar {
            doc_comment,
            ident,
            writable,
            initial_value,
            ty,
        } => {
            let ty_var = ty
                .map(|ty| static_resolve_type(&statics_stack, &ty))
                .unwrap_or(CMType::Unknown);
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

            let ty = match (ty_var, ty_value) {
                (CMType::Never, _) => unreachable!("local variable type cannot be never"),

                (ty_var, Some(CMType::Unknown) | None) => ty_var,
                (ty_var, Some(CMType::Never)) => {
                    is_never = true;
                    ty_var
                }
                (CMType::Unknown, Some(ty_value)) => ty_value,
                (ty_var, Some(ty_value)) => {
                    if ty_value != ty_var {
                        // type mismatch
                        error = Some(state.add_diagnostic(Raw2CommonDiagnostic { text: format!("type mismatch at variable assignment at /* TODO location */! expected {:?}, found {:?}", &ty_var, ty_value)}));
                        ty_var
                    } else {
                        ty_var
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
            op,
        } => todo!(),
        RMExpression::AssignProperty {
            object,
            property,
            value,
            op,
        } => todo!(),
        RMExpression::AssignVar { ident, value, op } => {
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
                    (_, CMType::Never) => {
                        is_never = true;
                    }
                    // (_, CMType::Void) => {
                    //     // guaranteed failure
                    //     error = Some(state.add_diagnostic(Raw2CommonDiagnostic {
                    //         text: format!("attempt to assign '{ident}' a void value"),
                    //     }));
                    // }
                    (CMType::Unknown, ty_value) => {
                        // take new type
                        var.ty = ty_value;
                    }
                    (ty_var, ty_value) => {
                        // check type
                        if ty_var != &ty_value {
                            error = Some(state.add_diagnostic(Raw2CommonDiagnostic { text: format!("type mismatch at variable assignment at /* TODO location */! expected {:?}, found {:?}", ty_var, ty_value) }));
                        }
                    }
                }
                let eval_ty = if is_never {
                    CMType::Never
                } else {
                    CMType::Void
                };
                if let Some(error) = error {
                    (CMExpression::Fail, eval_ty)
                } else {
                    if let Some(op) = op {
                        if !var.writable {
                            state.add_diagnostic(Raw2CommonDiagnostic {
                                text: format!("attempt to modify immutable var '{ident}'"),
                            });
                            (CMExpression::Fail, eval_ty)
                        } else if !var.assigned {
                            state.add_diagnostic(Raw2CommonDiagnostic {
                                text: format!("attempt to modify unassigned var '{ident}'"),
                            });
                            (CMExpression::Fail, eval_ty)
                        } else {
                            todo!("// TODO this requires mutable references to exist");
                        }
                    } else {
                        if !var.writable && var.assigned {
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
                    }
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
        } => {
            let (expr, ty) = resolve_expr(
                *expr,
                state,
                current_fn_stack,
                statics_stack,
                locals,
                locals_lookup,
                loop_context_stack,
                ty_return,
            );

            match &ty {
                CMType::StructInstance(id) => {
                    if let Some((i, _)) = state.structs[*id].fields_info.get(&property_ident) {
                        let ty_return = state.structs[*id].fields[*i].clone();

                        (
                            CMExpression::ReadProperty {
                                expr: Box::new(expr),
                                property_ident: *i,
                            },
                            ty_return,
                        )
                    } else {
                        (CMExpression::FailAfter(vec![expr]), CMType::Unknown)
                    }
                }
                _ => todo!("// TODO property resolution of non-struct things"),
            }
        }
        RMExpression::Ident { ident } => match locals_lookup.get(&ident) {
            Some(id) => {
                let var = &locals[*id];
                let (eval_ty, ty_ok) = match &var.ty {
                    ty => (ty.clone(), true),
                    _ => (CMType::Unknown, false),
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
                ty_arguments.push(ty);
            }

            macro_rules! fails {
                ($message: expr) => {{
                    // > fail
                    state.add_diagnostic(Raw2CommonDiagnostic { text: $message });
                    return (CMExpression::FailAfter(resolved_arguments), CMType::Unknown);
                }};
            }

            if let Some(path) = trace_ident_chain(&callable) {
                let path = path.into_iter().cloned().collect::<Vec<_>>();
                if !locals_lookup.contains_key(&path[0]) {
                    // calling a function by name `do_something( ... )` and `module_a.module_b.do_something( ... )`
                    let function_id =
                        match static_lookup_fn(&state.submodule_tree, &statics_stack, &path[..]) {
                            StaticLookupFn::Function(id) => id,
                            StaticLookupFn::Fail { resolved_part } => fails!(format!(
                                "could not resolve lookup [{} !! {} !!]",
                                resolved_part.iter().cloned().collect::<Vec<_>>().join("."),
                                path[resolved_part.len()..]
                                    .iter()
                                    .cloned()
                                    .collect::<Vec<_>>()
                                    .join("."),
                            )),
                        };

                    let info = resolve_fn_full_id(state, function_id, current_fn_stack);

                    if info.ty_args != ty_arguments {
                        fails!(format!("argument types mismatched // TODO template args"));
                    }

                    return (
                        CMExpression::Call {
                            function_id,
                            arguments: resolved_arguments,
                            always_inline: false,
                            inlined_lambdas: None,
                        },
                        info.ty_return.clone(),
                    );
                }
            }

            match *callable {
                RMExpression::ReadProperty {
                    expr,
                    property_ident,
                } => {
                    // calling an associated function `( ... ).do_something( ... )`
                    let (callable, ty_callable) = resolve_expr(
                        *expr,
                        state,
                        current_fn_stack,
                        statics_stack,
                        locals,
                        locals_lookup,
                        loop_context_stack,
                        ty_return,
                    );

                    let fn_lut = get_fn_lut(&ty_callable, state);

                    let Some(function_id) = fn_lut.named.get(&property_ident).copied() else {
                        fails!(format!("property '{}' is not callable", &property_ident));
                    };

                    let info = resolve_fn_full_id(state, function_id, current_fn_stack);

                    if info.ty_args.len() == 0 || info.ty_args[0] != ty_callable {
                        panic!("associated function somehow had first argument as something other than `self` (this should not be allowed yet).");
                    }
                    if &info.ty_args[1..] != ty_arguments {
                        fails!(format!("argument types mismatched // TODO template args"));
                    }

                    (
                        CMExpression::Call {
                            function_id,
                            arguments: std::iter::once(callable)
                                .chain(resolved_arguments.into_iter())
                                .collect(),
                            always_inline: false,
                            inlined_lambdas: None,
                        },
                        info.ty_return.clone(),
                    )
                }
                _ => {
                    // don't know what this is and the callable trait either hasn't been invented yet or will never exist.
                    fails!(format!("object is not callable"));
                }
            }

            // match *callable {
            //     RMExpression::Ident { ident } => {
            //         // function call
            //         let mut best_fallback = None;
            //         let mut had_semimatch = false;
            //         for function_id in statics_stack
            //             .iter()
            //             .rev()
            //             .flat_map(|frame| frame.functions.get(&ident))
            //             .flatten()
            //             .copied()
            //         {
            //             let k = &state.functions[function_id];
            //             if &k.params == &ty_arguments {
            //                 let ResolvedFnInfo { ty_return } =
            //                     resolve_fn(state, function_id, current_fn_stack, false);
            //                 return (
            //                     CMExpression::Call {
            //                         function_id: FnRef::ModuleFunction(function_id),
            //                         arguments: resolved_arguments,
            //                         always_inline: false,
            //                         inlined_lambdas: None,
            //                     },
            //                     ty_return.clone(),
            //                 );
            //             } else if k.params.len() == ty_arguments.len() {
            //                 if had_semimatch {
            //                     best_fallback = None;
            //                 } else {
            //                     best_fallback = Some(function_id)
            //                 }
            //                 had_semimatch = true;
            //             }
            //         }
            //         // did not find lny suitable call signatures
            //         state.add_diagnostic(Raw2CommonDiagnostic {
            //             text: format!("did not find any suitable call signatures"),
            //         });
            //         // try to salvage a return value to try to yield as much useful results to the user as possible
            //         let return_ty = if let Some(fallback_id) = best_fallback {
            //             let ResolvedFnInfo { ty_return } =
            //                 resolve_fn(state, fallback_id, current_fn_stack, false);
            //             ty_return.clone()
            //         } else {
            //             CMType::Never
            //         };
            //         (CMExpression::FailAfter(resolved_arguments), return_ty)
            //     }
            //     RMExpression::ReadProperty {
            //         expr,
            //         property_ident,
            //     } => {
            //         let (expr, ty) = resolve_expr(
            //             *expr,
            //             state,
            //             current_fn_stack,
            //             statics_stack,
            //             locals,
            //             locals_lookup,
            //             loop_context_stack,
            //             ty_return,
            //         );

            //         let fn_lut = get_fn_lut(&ty, state);
            //         dbg!(fn_lut);

            //         let base_impl_fn = fn_lut.named.get(&property_ident).and_then(|id| {
            //             let args = &state.functions[*id].params;
            //             dbg!(&args[1..], &ty_arguments, &args[0], &ty);
            //             if args[1..] == ty_arguments && &args[0] == &ty {
            //                 Some(*id)
            //             } else {
            //                 None
            //             }
            //         });
            //         if let Some(base_impl_fn) = base_impl_fn {
            //             let ResolvedFnInfo { ty_return } =
            //                 resolve_fn(state, base_impl_fn, current_fn_stack, false);
            //             (
            //                 CMExpression::Call {
            //                     function_id: FnRef::ModuleFunction(base_impl_fn),
            //                     arguments: std::iter::once(expr)
            //                         .chain(resolved_arguments.into_iter())
            //                         .collect(),
            //                     always_inline: false,
            //                     inlined_lambdas: None,
            //                 },
            //                 ty_return.clone(),
            //             )
            //         } else {
            //             todo!("named associated functions (trait impls and such)");
            //         }
            //     }
            //     _ => {}
            // }
        }

        RMExpression::LiteralValue(literal) => {
            let (expr, ty) = match literal {
                RMLiteralValue::Int(v) => (CMLiteralValue::Int(v), CMType::Int),
                RMLiteralValue::Float(v) => (CMLiteralValue::Float(v), CMType::Float),
                RMLiteralValue::Complex(v) => (CMLiteralValue::Complex(v), CMType::Complex),
                RMLiteralValue::Bool(v) => (CMLiteralValue::Bool(v), CMType::Bool),
                RMLiteralValue::String(v) => (CMLiteralValue::String(v), CMType::String),
            };
            (CMExpression::LiteralValue(expr), ty)
        }
        RMExpression::LiteralRange { start, step, end } => todo!("// TODO range constructor"),
        RMExpression::LiteralArray(_) => todo!(),
        RMExpression::LiteralStructInit {
            ident: ident_str,
            properties,
        } => {
            let ident = statics_stack
                .iter()
                .find_map(|frame| frame.structs.get(&ident_str))
                .copied();
            let (mut data, types, names): (Vec<_>, Vec<_>, Option<Vec<_>>) = match properties {
                LiteralStructInit::Tuple(vars) => {
                    let (data, types): (Vec<_>, Vec<_>) = vars
                        .into_iter()
                        .map(|var| {
                            resolve_expr(
                                var,
                                state,
                                current_fn_stack,
                                statics_stack,
                                locals,
                                locals_lookup,
                                loop_context_stack,
                                ty_return,
                            )
                        })
                        .unzip();
                    (data, types, None)
                }
                LiteralStructInit::Struct(vars) => {
                    let (data_and_types, names): (Vec<_>, Vec<_>) = vars
                        .into_iter()
                        .map(|(property_ident, expr)| {
                            (
                                resolve_expr(
                                    expr,
                                    state,
                                    current_fn_stack,
                                    statics_stack,
                                    locals,
                                    locals_lookup,
                                    loop_context_stack,
                                    ty_return,
                                ),
                                property_ident,
                            )
                        })
                        .unzip();
                    let (data, types): (Vec<_>, Vec<_>) = data_and_types.into_iter().unzip();
                    (data, types, Some(names))
                }
            };

            if let Some(ident) = ident {
                let st = &state.structs[ident];
                // TODO all structs are dict structs rn

                let names = names.unwrap();

                let mut diagnostics = vec![];
                let mut fail_at = data.len();

                let assign_to = names.iter().enumerate().map(|(i, name)| {
                    let Some((id, _)) = st.fields_info.get(name) else {
                        diagnostics.push(Raw2CommonDiagnostic { text: format!("could not find property '{}' in '{}'", name, ident_str) });
                        fail_at = fail_at.min(i);
                        return None;
                    };
                    Some(*id)
                }).collect::<Vec<_>>();

                for (i, (assign_to, eval_ty)) in
                    assign_to.iter().copied().zip(types.into_iter()).enumerate()
                {
                    if let Some(assign_to) = assign_to {
                        if eval_ty != st.fields[assign_to] {
                            diagnostics.push(Raw2CommonDiagnostic { text: format!("type mismatch setting property '{}' in '{}', expected {:?}, found {:?}", names[i], ident_str, st.fields[assign_to], eval_ty ) });
                            fail_at = fail_at.min(i + 1)
                        }
                    }
                }

                let mut assign_to_set = HashSet::new();
                for (i, assign_to) in assign_to.iter().enumerate() {
                    if let Some(assign_to) = assign_to {
                        if !assign_to_set.insert(*assign_to) {
                            diagnostics.push(Raw2CommonDiagnostic {
                                text: format!("duplicate property key '{}'", names[i]),
                            });
                            fail_at = fail_at.min(i)
                        }
                    }
                }
                let all = HashSet::from_iter(0..st.fields.len());
                let diff = all.difference(&assign_to_set).collect::<Vec<_>>();
                if !diff.is_empty() {
                    diagnostics.push(Raw2CommonDiagnostic {
                        text: format!(
                            "missing property keys {:?}",
                            diff.into_iter()
                                .map(|i| st
                                    .fields_info
                                    .iter()
                                    .find(|(_, (id, _))| id == i)
                                    .map(|(ident, _)| ident)
                                    .unwrap())
                                .collect::<Vec<_>>()
                        ),
                    })
                }
                if !diagnostics.is_empty() {
                    for diagnostic in diagnostics {
                        state.add_diagnostic(diagnostic);
                    }
                    return (
                        CMExpression::FailAfter(data.drain(..fail_at).collect()),
                        CMType::StructInstance(ident),
                    );
                }

                (
                    CMExpression::LiteralStructInit {
                        ident,
                        data,
                        assign_to: assign_to.into_iter().map(Option::unwrap).collect(),
                    },
                    CMType::StructInstance(ident),
                )
            } else {
                state.add_diagnostic(Raw2CommonDiagnostic {
                    text: format!("could not find struct with name '{}'", ident_str),
                });
                (CMExpression::FailAfter(data), CMType::Void)
            }
        }

        RMExpression::AnonymousFunction { params, block } => todo!(),

        RMExpression::OpUnary { .. } | RMExpression::OpBinary { .. } => {
            todo!("// TODO [raw_2_common] operator overloading as trait implementation")
        }
        /*
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
            eprintln!("// TODO operator overloading as traits");
            match (ty_lhs, ty_rhs) {
                (CMType::Unknown, _) | (_, CMType::Unknown) => {
                    (CMExpression::FailAfter(vec![lhs, rhs]), CMType::Unknown)
                }
                (CMType::Never, _) => {
                    // just do the first thing, it will never finish anyway so dont bother wrapping it anyway
                    state.add_diagnostic(Raw2CommonDiagnostic {
                        text: format!("cannot operate on never"),
                    });
                    (lhs, CMType::Never)
                }
                (_, CMType::Never) => {
                    state.add_diagnostic(Raw2CommonDiagnostic {
                        text: format!("cannot operate on never"),
                    });
                    (CMExpression::FailAfter(vec![lhs, rhs]), CMType::Never)
                }
                (ty_lhs, ty_rhs) => {
                    let lut_lhs = get_fn_lut(&ty_lhs, state);
                    let lut_rhs = get_fn_lut(&ty_rhs, state);
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
                CMType::Void => {
                    state.add_diagnostic(Raw2CommonDiagnostic {
                        text: format!("cannot operate on void"),
                    });
                    (CMExpression::FailAfter(vec![value]), CMType::Unknown)
                }
                CMType::Never => {
                    // just do the first thing, it will never finish anyway so dont bother wrapping it anyway
                    state.add_diagnostic(Raw2CommonDiagnostic {
                        text: format!("cannot operate on never"),
                    });
                    (value, CMType::Never)
                }
                CMType::Unknown => {
                    // forward the unknown
                    (CMExpression::FailAfter(vec![value]), CMType::Unknown)
                }
                ty => {
                    let lut = get_fn_lut(&ty, state);
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
                        (CMExpression::FailAfter(vec![value]), CMType::Unknown)
                    }
                }
            }
        }
        */
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
                    (CMType::Never | CMType::Unknown, ty_block) => {
                        *ty_eval = ty_block;
                        None
                    }
                    (a, b) => {
                        /* ok if no type mismatch */
                        if *a != &b {
                            Some(b)
                        } else {
                            None
                        }
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
                    CMType::Bool | CMType::Never => condition,
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
                None
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
            else_block,
        } => todo!(),
        RMExpression::LoopWhile {
            condition,
            block,
            else_block,
        } => {
            let (condition, condition_ty) = resolve_expr(
                *condition,
                state,
                current_fn_stack,
                statics_stack,
                locals,
                locals_lookup,
                loop_context_stack,
                ty_return,
            );
            let else_block = match else_block {
                Some(else_block) => {
                    let (else_block, break_ty) = resolve_block(
                        else_block,
                        state,
                        current_fn_stack,
                        statics_stack,
                        locals,
                        locals_lookup,
                        loop_context_stack,
                        ty_return,
                    );
                    loop_context_stack.push(LoopContext {
                        returnable: true,
                        return_value: break_ty,
                    });
                    Some(else_block)
                }
                None => {
                    loop_context_stack.push(LoopContext {
                        returnable: false,
                        return_value: CMType::Never, // <- doesn't really matter
                    });
                    None
                }
            };

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
            let LoopContext {
                return_value,
                returnable,
            } = loop_context_stack
                .pop()
                .expect("loop_context_stack somehow got drained");
            let return_ty = if returnable {
                return_value
            } else {
                CMType::Void
            };
            if condition_ty != CMType::Bool {
                state.add_diagnostic(Raw2CommonDiagnostic {
                    text: format!("while loop condition does not evalate to boolean"),
                });
                return (CMExpression::FailAfter(vec![condition]), return_ty);
            }
            (
                CMExpression::LoopWhile {
                    condition: Box::new(condition),
                    block,
                    else_block: else_block.map(|block| (block, return_ty.clone())),
                },
                return_ty,
            ) // return_value defaults to never because if there are no breaks it won't ever end. it's just that shrimple, such a sofishticated solution, mantastic.
        }
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
                                ty_value => {
                                    if ty_value == CMType::Void {
                                        state.add_diagnostic(Raw2CommonDiagnostic {
                                            text: format!("use of void as value (in break)"),
                                        });
                                    }
                                    match &loop_context.return_value {
                                        CMType::Never | CMType::Unknown => {
                                            // set loop return value
                                            loop_context.return_value = ty_value;
                                            None
                                        }
                                        ty_ret => {
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
                                CMType::Never | CMType::Unknown => {
                                    loop_context.return_value = CMType::Void;
                                    None
                                }
                                CMType::Void => {
                                    // everything checks out
                                    None
                                }
                                v => Some(state.add_diagnostic(Raw2CommonDiagnostic {
                                    text: format!("expected value in return / break statement"),
                                })),
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
                            CMType::Never | CMType::Unknown => {
                                *ty_return = ty_value;
                                None
                            }
                            ty_return => {
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
                            CMType::Never | CMType::Unknown => {
                                *ty_return = CMType::Void;
                                None
                            }
                            ty_return => {
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
        RMExpression::InlineMacroCall { call, ty_ret } => {
            let ty_ret = static_resolve_type(&statics_stack, &ty_ret);

            (
                CMExpression::InlineMacroCall {
                    call: resolve_macro(call),
                    ty_ret: ty_ret.clone(),
                },
                ty_ret,
            )
        }
    }
}
