use std::collections::HashMap;

use crate::common::{common_module::CMAssociatedFunction, IdentInt, IdentStr};

use super::{
    ast::{
        ASTAnonymousFunction, ASTArray, ASTCompoundPostfixContents, ASTExpression,
        ASTFunctionDefinition, ASTLiteral, ASTModule, ASTOptionallyTypedIdent, ASTTraitImpl,
        ASTTypedIdent, ASTVarAccessExpression,
    },
    raw_module::{
        LiteralStructInit, RMBlock, RMExpression, RMFunction, RMFunctionInfo, RMLiteralArray,
        RMLiteralValue, RMStruct, RMTrait, RMTraitImpl, RawModule, ScopedStatics,
    },
    submoduletree::{SubModuleResolution, SubModuleTree},
};

struct StaticsGlobalState<'a> {
    structs: Vec<RMStruct>,
    traits: Vec<RMTrait>,
    functions: Vec<RMFunction>,
    submodule_tree: &'a SubModuleTree,
}

struct StaticsCurrentScope {
    structs: HashMap<IdentStr, IdentInt>,
    traits: HashMap<IdentStr, IdentInt>,
    functions: HashMap<IdentStr, Vec<IdentInt>>,
    imports: HashMap<IdentStr, SubModuleResolution>,
}
impl StaticsCurrentScope {
    fn new() -> Self {
        Self {
            structs: HashMap::new(),
            traits: HashMap::new(),
            functions: HashMap::new(),
            imports: HashMap::new(),
        }
    }
    fn apply(self, state: &mut StaticsGlobalState) -> ScopedStatics {
        fn add_refs(all_scoped: &mut ScopedStatics, scope_to_add: &StaticsCurrentScope) {
            for (ident, overloads) in &scope_to_add.functions {
                let fns = all_scoped
                    .functions
                    .entry(ident.clone())
                    .or_insert_with(|| vec![]);
                for overload in overloads {
                    fns.push(*overload);
                    // Because inner scopes are added first, and functions pushed
                    // first will be considered higher priority for resolution,
                    // this order is correct.
                }
            }
            for (ident, id) in &scope_to_add.structs {
                all_scoped.structs.entry(ident.clone()).or_insert(*id);
                // no `and_modify`, because we want the innermost scope to determine
                // which class is referenced, and inner scopes are applied first.
            }
            for (ident, id) in &scope_to_add.traits {
                all_scoped.traits.entry(ident.clone()).or_insert(*id);
                // no `and_modify`, because we want the innermost scope to determine
                // which class is referenced, and inner scopes are applied first.
            }

            for (ident, id) in &scope_to_add.imports {
                all_scoped
                    .imports
                    .entry(ident.clone())
                    .or_insert(id.clone());
                // no `and_modify`, because we want the innermost scope to determine
                // which class is referenced, and inner scopes are applied first.
            }
        }
        // for each function, add all references
        for (fn_name, overloads) in &self.functions {
            let fns = self.functions.get(fn_name).unwrap();
            for fn_id_i in fns {
                add_refs(&mut state.functions[*fn_id_i].info.all_scoped, &self);
            }
        }
        // for each struct, add all references
        for (fn_name, st_id_i) in &self.structs {
            add_refs(&mut state.structs[*st_id_i].all_scoped, &self);
        }
        // for each trait, add all references
        for (fn_name, tr_id_i) in &self.traits {
            add_refs(&mut state.traits[*tr_id_i].all_scoped, &self);
        }

        ScopedStatics {
            structs: self.structs,
            traits: self.traits,
            functions: self.functions,
            imports: self.imports,
        }
    }
}

pub fn ast_2_raw(ast: ASTModule) -> RawModule {
    let mut state = StaticsGlobalState {
        structs: vec![],
        traits: vec![],
        functions: vec![],
        submodule_tree: &ast.submodule_tree,
    };
    let top_level = ast
        .modules
        .into_iter()
        .map(|ast_submodule| transform_expr_block_inner_scoped(ast_submodule, &mut state))
        .collect::<Vec<_>>();

    RawModule {
        structs: state.structs,
        traits: state.traits,
        functions: state.functions,
        top_level,
        submodule_tree: ast.submodule_tree,
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

fn transform_function(
    ASTFunctionDefinition {
        doc_comment,
        ident,
        is_member: _,
        can_be_disembodied,
        params,
        return_ty,
        block,
        local_template_defs,
    }: ASTFunctionDefinition,
    state: &mut StaticsGlobalState,
    scope: &mut StaticsCurrentScope,
) -> IdentInt {
    let function = RMFunction {
        info: RMFunctionInfo {
            doc_comment,
            local_template_defs,
            params: params
                .into_iter()
                .map(|ASTTypedIdent { ident, ty }| (ident, ty))
                .collect(),
            return_ty,
            can_be_disembodied,
            all_scoped: ScopedStatics::empty(),
        },
        block: block.map(|block| transform_expr_block_inner_scoped(block, state)),
    };

    let id = state.functions.len();
    state.functions.push(function);
    scope
        .functions
        .entry(ident)
        .or_insert_with(|| vec![])
        .push(id);

    return id;
}
fn transform_associated_function(
    def: ASTFunctionDefinition,
    state: &mut StaticsGlobalState,
    scope: &mut StaticsCurrentScope,
) -> CMAssociatedFunction {
    let is_member = def.is_member;
    let id = transform_function(def, state, scope);

    CMAssociatedFunction { id, is_member }
}
fn transform_trait_impl(
    def: ASTTraitImpl,
    state: &mut StaticsGlobalState,
    scope: &mut StaticsCurrentScope,
) -> RMTraitImpl {
    RMTraitImpl {
        trait_id: def.trait_ident,
        functions: def
            .functions
            .into_iter()
            .map(|(ident, function)| (ident, transform_associated_function(function, state, scope)))
            .collect(),
    }
}

fn transform_expr(
    expr: ASTExpression,
    state: &mut StaticsGlobalState,
    scope: &mut StaticsCurrentScope,
) -> RMExpression {
    match expr {
        ASTExpression::Import { include_paths } => {
            for path in include_paths {
                let local_name = path.last().unwrap();
                let mod_resolution = state.submodule_tree.resolve_at_root(&path);
                if let Some(mod_resolution) = mod_resolution {
                    scope
                        .imports
                        .entry(local_name.clone())
                        .and_modify(|_| {
                            todo!("// TODO report imports with the same name in local scopes");
                        })
                        .or_insert(mod_resolution);
                } else {
                    todo!("// TODO report unresolved import in import statement");
                }
            }
            RMExpression::Void
        }

        ////////////////////////////////////////////
        // move function/struct definitions to static list
        //
        ASTExpression::FunctionDefinition(def) => {
            transform_function(def, state, scope);

            RMExpression::Void
        }
        ASTExpression::StructDefinition {
            doc_comment,
            ident,
            properties,
            impl_functions,
            impl_traits,
        } => {
            let st = RMStruct {
                doc_comment,
                all_scoped: ScopedStatics::empty(),
                fields: properties
                    .into_iter()
                    .map(|(ident, doc_comment, ty)| (ident, (ty, doc_comment)))
                    .collect(), // TODO dedupe properties by name
                impl_functions: impl_functions
                    .into_iter()
                    .map(|it| (it.0, transform_associated_function(it.1, state, scope)))
                    .collect(),
                impl_traits: impl_traits
                    .into_iter()
                    .map(|it| (it.0, transform_trait_impl(it.1, state, scope)))
                    .collect(),
            };
            let id = state.structs.len();
            state.structs.push(st);
            scope
                .structs
                .entry(ident)
                .and_modify(|_| todo!("handle structs with name conflicts"))
                .or_insert(id);

            RMExpression::Void
        }
        ASTExpression::TraitDefinition {
            doc_comment,
            ident,
            bounds,
            functions,
        } => {
            todo!("// TODO ast_2_raw trait def")
        }

        // TODO enums in ast_2_raw

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
        ASTExpression::Assign { target, value, op } => {
            let value = transform_expr_box(value, state, scope);
            match target {
                ASTVarAccessExpression::Var { ident } => {
                    RMExpression::AssignVar { ident, value, op }
                }
                ASTVarAccessExpression::Index { object, indices } => RMExpression::AssignIndex {
                    object: transform_expr_box(object, state, scope),
                    indices: transform_expr_vec(indices, state, scope),
                    value,
                    op,
                },
                ASTVarAccessExpression::PropertyAccess {
                    object,
                    property_ident,
                } => RMExpression::AssignProperty {
                    object: transform_expr_box(object, state, scope),
                    property: property_ident,
                    value,
                    op,
                },
            }
        }
        ASTExpression::Ident(ident) => RMExpression::Ident { ident },
        ASTExpression::CompoundPostfix {
            target,
            contents: ASTCompoundPostfixContents::Call(arguments, callback),
        } => RMExpression::Call {
            callable: transform_expr_box(target, state, scope),
            arguments: transform_expr_vec(arguments, state, scope)
                .into_iter()
                .chain(
                    callback
                        .into_iter()
                        .map(|ASTAnonymousFunction { params, block }| {
                            RMExpression::AnonymousFunction {
                                params: params
                                    .expect("// TODO handle omitted anonymous function parameters")
                                    .into_iter()
                                    .map(|ASTOptionallyTypedIdent { ident, ty }| (ident, ty))
                                    .collect(),
                                block: transform_expr_block_inner_scoped(block, state),
                            }
                        }),
                )
                .collect(),
        },
        ASTExpression::CompoundPostfix {
            target,
            contents: ASTCompoundPostfixContents::Index(indices),
        } => RMExpression::ReadIndex {
            expr: transform_expr_box(target, state, scope),
            indices: transform_expr_vec(indices, state, scope),
        },
        ASTExpression::CompoundPostfix {
            target,
            contents: ASTCompoundPostfixContents::PropertyAccess(property_ident),
        } => RMExpression::ReadProperty {
            expr: transform_expr_box(target, state, scope),
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
        ASTExpression::CompoundPostfix {
            target,
            contents: ASTCompoundPostfixContents::BlockStructInit(properties),
        } => RMExpression::LiteralStructInit {
            ident: match *target {
                ASTExpression::Ident(ident) => ident,
                _ => todo!("// TODO struct init things that are not just raw identifiers"),
            },
            properties: LiteralStructInit::Struct(
                properties
                    .properties
                    .into_iter()
                    .map(|(ident, expr)| (ident, transform_expr(expr, state, scope)))
                    .collect(),
            ),
        },
        ASTExpression::CompoundPostfix {
            target,
            contents: ASTCompoundPostfixContents::TupleStructInit(properties),
        } => RMExpression::LiteralStructInit {
            ident: match *target {
                ASTExpression::Ident(ident) => ident,
                _ => todo!("// TODO struct init things that are not just raw identifiers"),
            },
            properties: LiteralStructInit::Tuple(
                properties
                    .into_iter()
                    .map(|expr| transform_expr(expr, state, scope))
                    .collect(),
            ),
        },
        ASTExpression::AnonymousFunction(ASTAnonymousFunction { params, block }) => {
            RMExpression::AnonymousFunction {
                params: params
                    .expect("// TODO handle omitted anonymous function parameters")
                    .into_iter()
                    .map(|ASTOptionallyTypedIdent { ident, ty }| (ident, ty))
                    .collect(),
                block: transform_expr_block_inner_scoped(block, state),
            }
        }
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
            else_block,
        } => RMExpression::LoopFor {
            loop_var,
            iterable: transform_expr_box(iterable, state, scope),
            block: transform_expr_block_inner_scoped(block, state),
            else_block: match else_block {
                Some(else_block) => Some(transform_expr_block_inner_scoped(else_block, state)),
                None => None,
            },
        },
        ASTExpression::LoopWhile {
            condition,
            block,
            else_block,
        } => RMExpression::LoopWhile {
            condition: transform_expr_box(condition, state, scope),
            block: transform_expr_block_inner_scoped(block, state),
            else_block: match else_block {
                Some(else_block) => Some(transform_expr_block_inner_scoped(else_block, state)),
                None => None,
            },
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
