use std::{collections::HashMap, hash::Hash, rc::Rc};

use crate::{
    common::{
        backend::{BackendId, BackendInfo, BackendsIndex},
        common_module::CommonModule,
        IdentInt, IdentStr,
    },
    parse::tokenization::{IDENT_PARENT_MODULE, IDENT_ROOT_MODULE},
};

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct SubModuleId(usize);
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct PackageId(pub usize);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SubModuleCodeRef {
    None,
    Common {
        module_id: IdentInt,
    },
    Multiplatform {
        common_module_id: IdentInt,
        platform_specific: HashMap<BackendId, IdentInt>,
    },
}
impl SubModuleCodeRef {
    fn is_none(&self) -> bool {
        match self {
            Self::None => true,
            _ => false,
        }
    }
    fn is_some(&self) -> bool {
        !self.is_none()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SubModuleEntryInfo {
    pub ty: SubModuleType,
    pub id: SubModuleId,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SubModuleType {
    Common,
    MultiplatformCommon,
    MultiplatformSpecific(BackendInfo),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SubModuleTreeEntry {
    children: HashMap<IdentStr, SubModuleId>,
    code_ref: SubModuleCodeRef,
    exports: CombinedModuleExports,
}
impl SubModuleTreeEntry {
    fn empty() -> Self {
        Self {
            children: HashMap::new(),
            code_ref: SubModuleCodeRef::None,
            exports: CombinedModuleExports::new_empty(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FullId {
    Local(IdentInt),
    NonLocal { dependency_id: usize, id: IdentInt },
}
impl FullId {
    fn is_local(&self) -> bool {
        match self {
            Self::Local(_) => true,
            _ => false,
        }
    }
    fn standardize_dependency_refs(self, prev_id: FullId) -> FullId {
        match (prev_id, self) {
            (Self::Local(_), _) => self,
            (Self::NonLocal { dependency_id, .. }, Self::Local(id)) => {
                Self::NonLocal { dependency_id, id }
            }
            _ => todo!("// TODO handle module reexports"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModuleExports {
    pub functions: HashMap<IdentStr, FullId>,
    pub structs: HashMap<IdentStr, FullId>,
    pub traits: HashMap<IdentStr, FullId>,
    pub modules: HashMap<IdentStr, FullId>,
}
impl ModuleExports {
    fn new_empty() -> Self {
        Self {
            functions: HashMap::new(),
            structs: HashMap::new(),
            traits: HashMap::new(),
            modules: HashMap::new(),
        }
    }
    fn clear(&mut self) {
        self.functions.clear();
        self.structs.clear();
        self.traits.clear();
        self.modules.clear();
    }
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CombinedModuleExports {
    pub common: ModuleExports,
    pub platform: HashMap<BackendId, ModuleExports>,
}
macro_rules! combined_module_exports_getfn {
    ($name: ident, $which: ident) => {
        fn $name(
            &self,
            id: &IdentStr,
            backends_index: &BackendsIndex,
            platform_id: Option<BackendId>,
        ) -> Option<MultiplatformFullId> {
            Self::get_any(
                &self.common.$which,
                self.platform
                    .iter()
                    .map(|(backend, it)| (*backend, &it.$which)),
                id,
                backends_index,
                platform_id,
            )
        }
    };
}
impl CombinedModuleExports {
    fn new_empty() -> Self {
        Self {
            common: ModuleExports::new_empty(),
            platform: HashMap::new(),
        }
    }
    /// Gets the [`ModuleExports`] for this platform. If it's not there, it creates one.
    fn edit_for_platform(&mut self, platform: Option<BackendId>) -> &mut ModuleExports {
        match platform {
            None => &mut self.common,
            Some(platform_id) => self
                .platform
                .entry(platform_id)
                .or_insert_with(|| ModuleExports::new_empty()),
        }
    }
    fn get_any<'a>(
        exports_common: &HashMap<IdentStr, FullId>,
        exports_by_platform: impl Iterator<Item = (BackendId, &'a HashMap<IdentStr, FullId>)>,
        id: &IdentStr,
        backends_index: &BackendsIndex,
        platform_id: Option<BackendId>,
    ) -> Option<MultiplatformFullId> {
        // platform_id == base_platform_id -> (common[id],platform[id])
        // platform_id subplatform_of base_platform_id -> (platform[platform_id else compat][id] else common[id], platform[*compat_list])
        // else -> id(platform[id], platform[*compat_list])

        if let Some(platform_id) = platform_id {
            let mut platform_variants = exports_by_platform
                .filter(|(exports_platform_id, _)| {
                    backends_index.a_is_subplatform_of_b(*exports_platform_id, platform_id)
                })
                .filter_map(|(exports_platform_id, exports)| {
                    Some((exports_platform_id, *exports.get(id)?))
                })
                .collect::<HashMap<_, _>>();
            let (_, common) = platform_variants.remove_entry(&platform_id)?;
            let platform_variants = if platform_variants.is_empty() {
                None
            } else {
                Some(platform_variants)
            };

            Some(MultiplatformFullId {
                common,
                platform_variants,
            })
        } else {
            let common = *exports_common.get(id)?;
            let platform_variants = exports_by_platform
                .filter_map(|(exports_platform_id, exports)| {
                    Some((exports_platform_id, *exports.get(id)?))
                })
                .collect::<Vec<_>>();
            let platform_variants = if platform_variants.is_empty() {
                None
            } else {
                Some(platform_variants.into_iter().collect())
            };
            Some(MultiplatformFullId {
                common,
                platform_variants,
            })
        }
    }
    combined_module_exports_getfn!(get_fn, functions);
    combined_module_exports_getfn!(get_struct, structs);
    combined_module_exports_getfn!(get_module, modules);
    combined_module_exports_getfn!(get_trait, traits);
}

#[derive(Debug, Clone)]
struct MultiplatformFullId {
    common: FullId,
    platform_variants: Option<HashMap<BackendId, FullId>>,
}
impl MultiplatformFullId {
    pub fn reify_id(&self, platform_id: BackendId) -> FullId {
        self.platform_variants
            .and_then(|v| {
                eprintln!("// TODO backend inheritence and mid-level compatibility");
                v.get(&platform_id).copied()
            })
            .unwrap_or(self.common)
    }
    pub fn standardize_dependency_refs(&self, prev_id: FullId) -> Self {
        Self {
            common: self.common.standardize_dependency_refs(prev_id),
            platform_variants: self.platform_variants.map(|it| {
                it.iter()
                    .map(|(backend_id, full_id)| {
                        (*backend_id, full_id.standardize_dependency_refs(prev_id))
                    })
                    .collect()
            }),
        }
    }
}

#[derive(Debug, Clone)]
pub struct PackageRef {
    pub id: PackageId,
    pub module: Rc<CommonModule>,
}

#[derive(Debug, Clone)]
pub struct ModuleTree {
    base_platform_id: BackendId,
    sub_modules: Vec<SubModuleTreeEntry>,
    dependencies: HashMap<IdentStr, usize>,
    dependency_list: Vec<PackageRef>,
    reexport_dependency_list: Vec<PackageRef>,
}

impl ModuleTree {
    pub fn new(base_platform_id: BackendId) -> Self {
        Self {
            base_platform_id,
            sub_modules: vec![SubModuleTreeEntry::empty()],
            dependencies: HashMap::new(),
            dependency_list: Vec::new(),
            reexport_dependency_list: Vec::new(),
        }
    }
    pub fn get_dependency(&self, id: usize) -> &CommonModule {
        self.dependency_list[id].module.as_ref()
    }
    pub fn add_dependency(&mut self, name: IdentStr, package: PackageRef) -> bool {
        if self.dependencies.get(&name).is_some() {
            return false;
        }

        let i = self.dependency_list.len();
        self.dependency_list.push(package);
        self.dependencies.insert(name, i);

        true
    }
    pub fn entry_at_root<'a>(
        &'a mut self,
        path: &[IdentStr],
    ) -> (SubModuleId, &'a mut SubModuleCodeRef) {
        self.entry(SubModuleId(0), path)
    }
    pub fn entry<'a>(
        &'a mut self,
        base_submodule_id: SubModuleId,
        path: &[IdentStr],
    ) -> (SubModuleId, &'a mut SubModuleCodeRef) {
        let mut id = base_submodule_id;
        if id.0 >= self.sub_modules.len() {
            panic!("[{}:{}] Somehow provided base_submodule_id that is out of bounds for this SubModuleTree.", file!(), line!());
        }

        for path_step in path {
            let n_entries = self.sub_modules.len();
            let mut new_tree_entry = false;
            id = *self.sub_modules[id.0]
                .children
                .entry(path_step.clone())
                .or_insert_with(|| {
                    new_tree_entry = true;
                    SubModuleId(n_entries)
                });
            if new_tree_entry {
                self.sub_modules
                    .push(SubModuleTreeEntry::empty(self.base_platform_id));
            }
        }

        (id, &mut self.sub_modules[id.0].code_ref)
    }
    pub fn edit_submodule_data(
        &mut self,
        submodule_id: usize,
        platform_id: Option<BackendId>,
    ) -> ModuleTreeSubModuleHandle {
        ModuleTreeSubModuleHandle {
            module_tree: self,
            submodule_id,
            platform_id,
            is_multiplatform_common: false,
        }
    }

    pub fn finish_get(
        &self,
        preluid: ModuleTreeLookupPreliminary,
        // platform_id: Option<BackendId>,
    ) -> Result<ModuleTreeLookup, ModuleTreeLookupError> {
        self.get(preluid.lookup, &preluid.unmatched_path, preluid.platform_id)
    }
    pub fn get_fn(
        &self,
        mut current_lookup: ModuleTreeLookup,
        path: &[IdentStr],
        platform_id: Option<BackendId>,
    ) -> Result<FullId, ModuleTreeLookupError> {
        current_lookup = self.get(current_lookup, &path[..path.len() - 1], platform_id)?;

        self.get_exports(current_lookup)
            .and_then(|exports| exports.get_fn(&path[path.len() - 1], platform_id))
            .ok_or(ModuleTreeLookupError {
                n_matched_before_fail: path.len() - 1,
            })
    }
    fn get_exports(&self, current_lookup: ModuleTreeLookup) -> Option<&CombinedModuleExports> {
        Some(match current_lookup {
            ModuleTreeLookup::Function(_) => {
                return None;
            }
            ModuleTreeLookup::Struct(_) | ModuleTreeLookup::Trait(_) => {
                todo!("// TODO handle imports from inside structs/traits");
            }
            ModuleTreeLookup::Module(FullId::Local(submod_id)) => {
                &self.sub_modules[submod_id].exports
            }
            ModuleTreeLookup::Module(FullId::NonLocal {
                dependency_id,
                id: submod_id,
            }) => {
                &self.dependency_list[dependency_id]
                    .module
                    .submodule_tree
                    .sub_modules[submod_id]
                    .exports
            }
        })
    }
    pub fn get(
        &self,
        mut current_lookup: ModuleTreeLookup,
        path: &[IdentStr],
        backends_index: &BackendsIndex,
        platform_id: BackendId,
    ) -> Result<ModuleTreeLookup, ModuleTreeLookupError> {
        for (i, ident) in path.iter().enumerate() {
            if let ModuleTreeLookup::Module(FullId::Local(submod_id)) = &current_lookup {
                if let Some(next_id) = self.sub_modules[*submod_id].children.get(ident) {
                    current_lookup = ModuleTreeLookup::Module(FullId::Local(next_id.0));
                    continue;
                }
            }
            let Some(exports) = self.get_exports(current_lookup) else {
                return Err(ModuleTreeLookupError {
                    n_matched_before_fail: i,
                });
            };
            let prev_id = current_lookup.id();
            current_lookup =
                if let Some(id) = exports.get_module(ident, backends_index, platform_id) {
                    ModuleTreeLookup::Module(id.standardize_dependency_refs(prev_id))
                } else if let Some(id) = exports.get_struct(ident, backends_index, platform_id) {
                    ModuleTreeLookup::Struct(id.standardize_dependency_refs(prev_id))
                } else if let Some(id) = exports.get_trait(ident, backends_index, platform_id) {
                    ModuleTreeLookup::Trait(id.standardize_dependency_refs(prev_id))
                } else if let Some(id) = exports.get_fn(ident, backends_index, platform_id) {
                    ModuleTreeLookup::Function(id.standardize_dependency_refs(prev_id))
                } else {
                    return Err(ModuleTreeLookupError {
                        n_matched_before_fail: i,
                    });
                };
        }
        Ok(current_lookup)
    }
    // pub fn resolve_at_root(&self, path: &[IdentStr]) -> Option<ModuleResolution> {
    //     self.resolve(SubModuleId(0), path)
    // }

    // pub fn resolve(
    //     &self,
    //     base_submodule_id: SubModuleId,
    //     path: &[IdentStr],
    // ) -> Option<ModuleResolution> {
    //     let mut id = base_submodule_id;
    //     if id.0 >= self.sub_modules.len() {
    //         eprintln!("[{}:{}] Somehow provided base_submodule_id that is out of bounds for this SubModuleTree.", file!(), line!());
    //         return None;
    //     }

    //     if path.len() == 1 {
    //         self.sub_modules[id.0]
    //             .children
    //             .get(&path[0])
    //             .map(|id| ModuleResolution {
    //                 id: *id,
    //                 child: None,
    //             })
    //     } else {
    //         for path_step in &path[..path.len() - 1] {
    //             id = *self.sub_modules[id.0].children.get(path_step)?;
    //         }
    //         let child = {
    //             let path_step = path.last().unwrap();
    //             match self.sub_modules[id.0].children.get(path_step) {
    //                 Some(new_id) => {
    //                     id = *new_id;
    //                     None
    //                 }
    //                 None => Some(path_step.clone()),
    //             }
    //         };

    //         Some(ModuleResolution { id, child })
    //     }
    // }
    // pub fn get<'a>(
    //     &self,
    //     base_submodule_id: SubModuleId,
    //     mut path: impl Iterator<Item = &'a IdentStr>,
    // ) -> Option<IdentInt> {
    //     let mut id = base_submodule_id;
    //     if id.0 >= self.sub_modules.len() {
    //         eprintln!("[{}:{}] Somehow provided base_submodule_id that is out of bounds for this SubModuleTree.", file!(), line!());
    //         return None;
    //     }

    //     while let Some(path_step) = path.next() {
    //         id = *self.sub_modules[id.0].children.get(path_step)?;
    //     }

    //     self.sub_modules[id.0].code_mod_id
    // }
}

pub struct ModuleTreeSubModuleHandle<'a> {
    module_tree: &'a mut ModuleTree,
    submodule_id: usize,
    is_multiplatform_common: bool,
    platform_id: Option<BackendId>,
}

impl<'a> ModuleTreeSubModuleHandle<'a> {
    pub fn get_current_submodule_id(&self) -> (SubModuleId, Option<BackendId>, bool) {
        (
            SubModuleId(self.submodule_id),
            self.platform_id,
            self.is_multiplatform_common,
        )
    }
    pub fn switch_submodule(
        &mut self,
        submodule_id: SubModuleId,
        platform_id: Option<BackendId>,
        is_multiplatform_common: bool,
    ) {
        self.submodule_id = submodule_id.0;
        self.platform_id = platform_id;
        self.is_multiplatform_common = is_multiplatform_common;
    }
    pub fn public_exports_mut(&mut self) -> &mut ModuleExports {
        &mut self.module_tree.sub_modules[self.submodule_id]
            .exports
            .edit_for_platform(self.platform_id)
    }
    fn preliminary_get_local(
        &self,
        start_id: SubModuleId,
        i0: usize,
        mut path: Vec<IdentStr>,
    ) -> ModuleTreeLookupPreliminary {
        let mut id = start_id.0;
        for i in i0..path.len() {
            if let Some(new_id) = self.module_tree.sub_modules[id].children.get(&path[i]) {
                id = new_id.0;
            } else {
                path.drain(..i);
                return ModuleTreeLookupPreliminary {
                    platform_id: self.platform_id,
                    lookup: ModuleTreeLookup::Module(FullId::Local(id)),
                    unmatched_path: path,
                };
            }
        }
        ModuleTreeLookupPreliminary {
            platform_id: self.platform_id,
            lookup: ModuleTreeLookup::Module(FullId::Local(id)),
            unmatched_path: vec![],
        }
    }
    pub fn preliminary_get<'b>(
        &self,
        path: Vec<IdentStr>,
    ) -> Result<ModuleTreeLookupPreliminary, ModuleTreeLookupError> {
        assert!(path.len() > 0);
        if path[0] == IDENT_ROOT_MODULE {
            Ok(self.preliminary_get_local(SubModuleId(0), 1, path))
        } else if path[0] == IDENT_PARENT_MODULE {
            todo!("// TODO import super.whatever");
        } else if self.module_tree.sub_modules[self.submodule_id]
            .children
            .get(&path[0])
            .is_some()
        {
            Ok(self.preliminary_get_local(SubModuleId(self.submodule_id), 0, path))
        } else if let Some(dependency_id) = self.module_tree.dependencies.get(&path[0]).copied() {
            Ok(ModuleTreeLookupPreliminary {
                platform_id: self.platform_id,
                lookup: self
                    .module_tree
                    .get(
                        ModuleTreeLookup::Module(FullId::NonLocal {
                            dependency_id,
                            id: 0,
                        }),
                        &path[1..],
                        self.platform_id,
                    )
                    .map_err(|ok_count| ok_count + 1)?,
                unmatched_path: vec![],
            })
        } else {
            Err(ModuleTreeLookupError {
                n_matched_before_fail: 0,
            })
        }
    }
}

#[derive(Debug, Clone)]
pub enum ModuleTreeLookup {
    Function(MultiplatformFullId),
    Struct(MultiplatformFullId),
    Trait(MultiplatformFullId),
    Module(MultiplatformFullId),
}
impl ModuleTreeLookup {
    fn id(self, platform_id: Option<BackendId>) -> FullId {
        match self {
            Self::Module(id) | Self::Function(id) | Self::Struct(id) | Self::Trait(id) => {
                match platform_id {
                    Some(platform_id) => id.reify_id(platform_id),
                    None => id.common,
                }
            }
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct ModuleTreeLookupError {
    pub n_matched_before_fail: usize,
}
impl std::ops::Add<usize> for ModuleTreeLookupError {
    type Output = ModuleTreeLookupError;
    fn add(mut self, rhs: usize) -> Self::Output {
        Self {
            n_matched_before_fail: self.n_matched_before_fail + 1,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ModuleTreeLookupPreliminary {
    platform_id: Option<BackendId>,
    lookup: ModuleTreeLookup,
    unmatched_path: Vec<IdentStr>,
}
