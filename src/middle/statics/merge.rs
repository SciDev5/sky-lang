//! needs to be able to represent the collective contributions of all the target platforms
//!
//! Cross-target merging only happens for public/exported items
//!
//! DATA
//!  - at a given target level datas can either have
//!    - undefined content -> `data abstract`
//!    - defined content from this level -> `data struct`/`data tuple`/...
//!    - inherited content from a more abstract level -> (write nothing)
//!  - trait impls must be declared on the same level as data
//! FUNCTION / CONST:
//!  - at a given target level datas can either have
//!    - undefined content -> `pub const PLATFORM_NAME: string`
//!    - defined content from this level -> `pub const PLATFORM_NAME = "hello world"` (infers type)
//!    - inherited content from a more abstract level -> (write nothing)
//! TYPE ALIAS / TRAIT:
//!  - only allowed to be definet at a single level, overlaps are ignored
//! IMPL:
//!  - not handled at this phase
//!

use std::collections::HashMap;

use crate::{
    back::BackendId,
    front::ast::{
        ASTConst, ASTData, ASTFreeImpl, ASTFunction, ASTScope, ASTStatics, ASTTrait, ASTTypeAlias,
    },
    modularity::Id,
};

use super::{
    module::{ModuleExports, ModuleParts},
    scopes::{ScopeId, Scopes},
};

#[derive(Debug, Default)]
pub struct MergedStatics<'src> {
    pub functions: Vec<Merged<ASTFunction<'src>>>,
    pub datas: Vec<Merged<ASTData<'src>>>,
    pub traits: Vec<Merged<ASTTrait<'src>>>,
    pub consts: Vec<Merged<ASTConst<'src>>>,
    pub typealiases: Vec<Merged<ASTTypeAlias<'src>>>,

    pub free_impls: Vec<ASTFreeImpl<'src>>,
}
impl<'src> MergedStatics<'src> {
    fn new_with_capacity_like(statics_unmerged: &ASTStatics<'src>) -> Self {
        Self {
            functions: Vec::with_capacity(statics_unmerged.functions.len()),
            datas: Vec::with_capacity(statics_unmerged.datas.len()),
            traits: Vec::with_capacity(statics_unmerged.traits.len()),
            consts: Vec::with_capacity(statics_unmerged.consts.len()),
            typealiases: Vec::with_capacity(statics_unmerged.typealiases.len()),

            free_impls: Vec::new(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Merged<T> {
    pub base_target_level: BackendId,
    pub contents: HashMap<BackendId, T>,
}

pub struct MergeIdRemapLookup {
    functions: Vec<usize>,
    datas: Vec<usize>,
    traits: Vec<usize>,
    consts: Vec<usize>,
    typealiases: Vec<usize>,
}
impl MergeIdRemapLookup {
    fn new<'src>(statics_unmerged: &ASTStatics<'src>) -> Self {
        Self {
            functions: vec![usize::MAX; statics_unmerged.functions.len()],
            datas: vec![usize::MAX; statics_unmerged.datas.len()],
            traits: vec![usize::MAX; statics_unmerged.traits.len()],
            consts: vec![usize::MAX; statics_unmerged.consts.len()],
            typealiases: vec![usize::MAX; statics_unmerged.typealiases.len()],
        }
    }
}

pub fn merge_statics<'src>(
    modules_in: Vec<ModuleParts<'src>>,
    statics_unmerged: ASTStatics<'src>,
    scopes: &mut Scopes<ASTScope<'src>>,
) -> (Vec<ModuleExports<'src>>, MergedStatics<'src>) {
    ASTScope::attatch_backend_ids(&modules_in, scopes);

    let mut id_lut = MergeIdRemapLookup::new(&statics_unmerged);
    let mut statics: MergedStatics = MergedStatics::new_with_capacity_like(&statics_unmerged);
    statics.free_impls = statics_unmerged.free_impls;

    macro_rules! optionalize_elements {
        ($x:ident) => {
            statics_unmerged
                .$x
                .into_iter()
                .map(|v| Some(v))
                .collect::<Vec<_>>()
        };
    }
    let mut functions_unmerged = optionalize_elements!(functions);
    let mut datas_unmerged = optionalize_elements!(datas);
    let mut traits_unmerged = optionalize_elements!(traits);
    let mut consts_unmerged = optionalize_elements!(consts);
    let mut typealiases_unmerged = optionalize_elements!(typealiases);

    let mut modules_out = Vec::with_capacity(modules_in.len());
    for module_parts in modules_in {
        let mut module_out = ModuleExports::new_empty(module_parts.parent, module_parts.children);

        for (backend_id, exports) in module_parts.export_parts {
            macro_rules! add_exports_to_merged {
                ($x:ident, $unmerged:expr) => {
                    merge_exports(
                        backend_id,
                        exports.$x,
                        &mut module_out.$x,
                        &mut $unmerged,
                        &mut id_lut.$x,
                        &mut statics.$x,
                    );
                };
            }
            add_exports_to_merged!(functions, functions_unmerged);
            add_exports_to_merged!(datas, datas_unmerged);
            add_exports_to_merged!(traits, traits_unmerged);
            add_exports_to_merged!(consts, consts_unmerged);
            add_exports_to_merged!(typealiases, typealiases_unmerged);
            module_out.sources.push((backend_id, exports.source));
        }

        modules_out.push(module_out);
    }

    macro_rules! merge_remeining {
        ($x:ident, $unmerged:expr) => {
            for (id_from, from) in $unmerged.into_iter().enumerate() {
                merge_remaining(
                    &mut id_lut.$x,
                    &mut statics.$x,
                    scopes,
                    |v| v.containing_scope,
                    id_from,
                    from,
                );
            }
        };
    }
    merge_remeining!(functions, functions_unmerged);
    merge_remeining!(datas, datas_unmerged);
    merge_remeining!(traits, traits_unmerged);
    merge_remeining!(consts, consts_unmerged);
    merge_remeining!(typealiases, typealiases_unmerged);

    scopes.modify(|scope| {
        macro_rules! remap_to_merged_ids {
            ($v:ident) => {
                scope
                    .$v
                    .values_mut()
                    .for_each(|id| *id = id.map_local(|id| id_lut.$v[id]));
            };
        }
        remap_to_merged_ids!(functions);
        remap_to_merged_ids!(datas);
        remap_to_merged_ids!(traits);
        remap_to_merged_ids!(consts);
        remap_to_merged_ids!(typealiases);
    });

    (modules_out, statics)
}

fn merge_exports<'src, T>(
    backend_id: BackendId,
    exports: HashMap<&'src str, Id>,
    exports_out: &mut HashMap<&'src str, Id>,
    unmerged: &mut Vec<Option<T>>,
    lut: &mut Vec<usize>,
    statics: &mut Vec<Merged<T>>,
) {
    for (name, id_from) in exports {
        let id_from = id_from.unwrap_local();
        let id_to = exports_out
            .entry(name)
            .or_insert_with(|| Id::Local {
                id: register_merged_static(statics),
            })
            .unwrap_local();

        let from = unmerged[id_from]
            .take()
            .expect("same item was exported by more than one module, should be impossible");
        let to = &mut statics[id_to];

        to.base_target_level = to.base_target_level.min(backend_id);
        to.contents.insert(backend_id, from);
        lut[id_from] = id_to;
    }
}

fn merge_remaining<'src, T>(
    lut: &mut Vec<usize>,
    statics: &mut Vec<Merged<T>>,
    scopes: &mut Scopes<ASTScope<'src>>,
    get_scope_id: impl Fn(&T) -> ScopeId,

    id_from: usize,
    from: Option<T>,
) -> Option<()> {
    let from = from?;
    let id_to = register_merged_static(statics);
    let to = &mut statics[id_to];

    let backend_id = scopes.get(get_scope_id(&from)).unwrap().backend_id;

    to.base_target_level = to.base_target_level.min(backend_id);
    to.contents.insert(backend_id, from);
    lut[id_from] = id_to;

    Some(())
}

fn register_merged_static<T>(statics: &mut Vec<Merged<T>>) -> usize {
    let id = statics.len();
    statics.push(Merged {
        base_target_level: usize::MAX,
        contents: HashMap::new(),
    });
    id
}
