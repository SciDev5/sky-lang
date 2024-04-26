use std::{collections::HashMap, hash::Hash, rc::Rc};

use crate::{
    common::{
        backend::{BackendId, PlatformInfo},
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
    MultiplatformSpecific(PlatformInfo),
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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
pub struct CombinedModuleExports(HashMap<BackendId, ModuleExports>);
impl CombinedModuleExports {
    fn new_empty() -> Self {
        Self(HashMap::new())
    }
    /// Gets the [`ModuleExports`] for this platform. If it's not there, it creates one.
    fn edit_for_platform(&mut self, platform_id: BackendId) -> &mut ModuleExports {
        self.0
            .entry(platform_id)
            .or_insert_with(|| ModuleExports::new_empty())
    }

    fn get_compatible_exports(
        &self,
        origin_platform_info: PlatformInfo,
    ) -> impl std::iter::Iterator<Item = (BackendId, &ModuleExports)> {
        origin_platform_info
            .compat_ids
            .into_iter()
            .filter_map(|id| self.0.get(id).map(|v| (*id, v)))
    }

    fn iter(&self) -> impl std::iter::Iterator<Item = (BackendId, &ModuleExports)> {
        self.0.iter().map(|(id, exports)| (*id, exports))
    }
}

#[derive(Debug, Clone)]
pub struct MultiplatformFullId(HashMap<BackendId, FullId>);
impl MultiplatformFullId {
    pub fn new_simple(platform_id: BackendId, id: FullId) -> Self {
        Self(HashMap::from([(platform_id, id)]))
    }
    pub fn reify_id(&self, platform_info: PlatformInfo) -> FullId {
        eprintln!("// TODO backend inheritence and mid-level compatibility");
        for platform_id in platform_info.compat_ids {
            if let Some(out_id) = self.0.get(&platform_id) {
                return *out_id;
            }
        }
        todo!("// TODO find some way to deal with MultiplatformFullId::reify_id not matching anything.");
    }
    pub fn dbg_new(id: FullId) -> Self {
        eprintln!("// TODO handle traits/structs in the multiplatform system");
        Self::new_simple(0, id)
    }
    pub fn dbg_reify_id(&self) -> FullId {
        eprintln!("// TODO handle traits/structs in the multiplatform system");
        // return *self.0.get(&0).expect("no common impl");
        return *self.0.get(&0).unwrap_or(
            self.0
                .values()
                .next()
                .expect("no implementation in any backend?!"),
        );
    }
}

#[derive(Debug, Clone)]
pub struct PackageRef {
    pub id: PackageId,
    pub module: Rc<CommonModule>,
}

#[derive(Debug, Clone)]
pub struct ModuleTree {
    sub_modules: Vec<SubModuleTreeEntry>,
    dependencies: HashMap<IdentStr, usize>,
    dependency_list: Vec<PackageRef>,
    reexport_dependency_list: Vec<PackageRef>,
}

impl ModuleTree {
    pub fn new() -> Self {
        Self {
            sub_modules: vec![SubModuleTreeEntry::empty()],
            dependencies: HashMap::new(),
            dependency_list: Vec::new(),
            reexport_dependency_list: Vec::new(),
        }
    }
    pub fn get_dependency_list(&self) -> &Vec<PackageRef> {
        &self.dependency_list
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
                self.sub_modules.push(SubModuleTreeEntry::empty());
            }
        }

        (id, &mut self.sub_modules[id.0].code_ref)
    }
    pub fn edit_submodule_data(
        &mut self,
        submodule_id: usize,
        platform_info: PlatformInfo,
    ) -> ModuleTreeSubModuleHandle {
        ModuleTreeSubModuleHandle {
            module_tree: self,
            submodule_id,
            platform_info,
            is_multiplatform_common: false,
        }
    }
    /*

        Lookup stuff:

        Knowns:
        - all modules' exports
        - import path
        - origin module and its target backend

        Constraints:
        - can only import from modules with same or more general target backends (only constrained by origin module, though)

        Want:
        - full id and type of imported resource

    ///////////////////////////////////////////

        Types of lookup:
        - lookup import preliminary (local modules unknown, library modules known; failures/unknowns are left resolved up to failure point; outputs generic partial import or terminal if obvious)
        - lookup import finalize (all modules known; failures reported; outputs generic import or terminal if obvious)
        - lookup access (contextual; all modules known; failures reported; leaves context-specific terminal else failure)
         */

    fn import_step(
        &self,
        origin_platform_info: PlatformInfo,
        mod_id: FullId,
        path_step: &String,
    ) -> Option<ModuleTreeLookup> {
        let exports = match mod_id {
            FullId::NonLocal {
                dependency_id,
                id: mod_id,
            } => {
                // ---- dependency module ---- //
                &self.dependency_list[dependency_id]
                    .module
                    .submodule_tree
                    .sub_modules[mod_id]
                    .exports
            }

            FullId::Local(mod_id) => {
                // ---- local module ---- //
                if let Some(new_mod_id) =
                    self.import_step_local_children(SubModuleId(mod_id), path_step)
                {
                    return Some(ModuleTreeLookup::Module(FullId::Local(mod_id)));
                }
                &self.sub_modules[mod_id].exports
            }
        };

        let mut all_exports_vec = vec![];
        let mut does_export_fn = false;
        let mut does_export_struct = false;
        let mut does_export_trait = false;
        for (platform_id, exports) in exports.iter()
        /*exports.get_compatible_exports(origin_platform_info)*/
        {
            if let Some(new_mod_id) = exports.modules.get(path_step) {
                return Some(ModuleTreeLookup::Module(
                    new_mod_id.standardize_dependency_refs(mod_id),
                ));
            }
            if exports.functions.contains_key(path_step) {
                does_export_fn = true;
            }
            if exports.structs.contains_key(path_step) {
                does_export_struct = true;
            }
            if exports.traits.contains_key(path_step) {
                does_export_trait = true;
            }
            all_exports_vec.push((platform_id, exports));
        }

        let n = [does_export_fn, does_export_struct, does_export_trait]
            .into_iter()
            .filter(|does_export_it| *does_export_it)
            .count();
        if n > 1 {
            panic!("Failed to prevent multiple types of things from being exported under the same name. // TODO add something to prevent this before");
        }
        if n == 0 {
            return None;
        }

        let new_id = MultiplatformFullId(
            all_exports_vec
                .into_iter()
                .filter_map(|(id, exports)| {
                    (if does_export_fn {
                        &exports.functions
                    } else if does_export_struct {
                        &exports.structs
                    } else if does_export_trait {
                        &exports.traits
                    } else {
                        unreachable!();
                    })
                    .get(path_step)
                    .map(|v| (id, v.standardize_dependency_refs(mod_id)))
                })
                .collect(),
        );
        Some(if does_export_fn {
            ModuleTreeLookup::Function(new_id)
        } else if does_export_struct {
            ModuleTreeLookup::Struct(new_id)
        } else if does_export_trait {
            ModuleTreeLookup::Trait(new_id)
        } else {
            unreachable!();
        })
    }
    fn import_step_local_children(
        &self,
        mod_id: SubModuleId,
        path_step: &String,
    ) -> Option<SubModuleId> {
        self.sub_modules[mod_id.0].children.get(path_step).copied()
    }

    pub fn import_preliminary(
        &self,
        origin_platform_info: PlatformInfo,
        mod_id: SubModuleId,
        mut path: Vec<IdentStr>,
    ) -> ModuleTreeLookupPreliminary {
        assert!(path.len() > 0);

        let mut current_mod_id = FullId::Local(mod_id.0);
        let mut i = 0;
        match path[0].as_str() {
            IDENT_ROOT_MODULE => {
                i += 1;
                current_mod_id = FullId::Local(0); // jump to package root
            }
            IDENT_PARENT_MODULE => {
                // should consume all in a chain of parent accesses `super.super.su...`
                // only allowed at the start
                todo!("// TODO imports from parent module");
            }
            _ => {
                if let Some(dependency_id) = self.dependencies.get(&path[0]).copied() {
                    i += 1;
                    current_mod_id = FullId::NonLocal {
                        dependency_id,
                        id: 0,
                    };
                } else {
                    let FullId::Local(mod_id) = current_mod_id else { unreachable!() };
                    let mut mod_id = SubModuleId(mod_id);
                    while let Some(new_mod_id) = self.import_step_local_children(mod_id, &path[i]) {
                        i += 1;
                        mod_id = new_mod_id;

                        if i >= path.len() {
                            break;
                        }
                    }
                    current_mod_id = FullId::Local(mod_id.0);
                }
            }
        }

        path.drain(..i);
        ModuleTreeLookupPreliminary {
            origin_platform_info,
            current: ModuleTreeLookup::Module(current_mod_id),
            n_consumed: i,
            unmatched_path: path,
        }
    }

    pub fn import_finish(
        &self,
        base: ModuleTreeLookupPreliminary,
    ) -> Result<ModuleTreeLookup, ModuleTreeLookupError> {
        self.import_extend(
            &base.current,
            &base.unmatched_path,
            base.n_consumed,
            base.origin_platform_info,
        )
    }

    fn import_extend(
        &self,
        base: &ModuleTreeLookup,
        path: &[IdentStr],
        n_consumed: usize,
        origin_platform_info: PlatformInfo,
    ) -> Result<ModuleTreeLookup, ModuleTreeLookupError> {
        let mut current = base.clone();
        for (j, path_step) in path.into_iter().enumerate() {
            macro_rules! fail {
                () => {
                    return Err(ModuleTreeLookupError {
                        n_matched_before_fail: j + n_consumed,
                    });
                };
            }
            match current {
                ModuleTreeLookup::Module(mod_id) => {
                    if let Some(new) = self.import_step(origin_platform_info, mod_id, &path_step) {
                        current = new;
                    } else {
                        fail!();
                    }
                }
                ModuleTreeLookup::Struct(id) => {
                    todo!("// TODO access struct functions from outside");
                }
                ModuleTreeLookup::Trait(id) => {
                    todo!("// TODO access trait functions from outside");
                }
                _ => {
                    fail!();
                }
            }
        }
        Ok(current)
    }
    pub fn access_fn(
        &self,
        base: &ModuleTreeLookup,
        path: &[IdentStr],
        origin_platform_info: PlatformInfo,
    ) -> Result<MultiplatformFullId, ModuleTreeLookupError> {
        match self.import_extend(base, path, 0, origin_platform_info)? {
            ModuleTreeLookup::Function(id) => Ok(id),
            _ => Err(ModuleTreeLookupError {
                n_matched_before_fail: path.len() - 1,
            }),
        }
    }
    pub fn access_struct(
        &self,
        base: &ModuleTreeLookup,
        path: &[IdentStr],
        origin_platform_info: PlatformInfo,
    ) -> Result<MultiplatformFullId, ModuleTreeLookupError> {
        match self.import_extend(base, path, 0, origin_platform_info)? {
            ModuleTreeLookup::Struct(id) => Ok(id),
            _ => Err(ModuleTreeLookupError {
                n_matched_before_fail: path.len() - 1,
            }),
        }
    }
    pub fn access_trait(
        &self,
        base: &ModuleTreeLookup,
        path: &[IdentStr],
        origin_platform_info: PlatformInfo,
    ) -> Result<MultiplatformFullId, ModuleTreeLookupError> {
        match self.import_extend(base, path, 0, origin_platform_info)? {
            ModuleTreeLookup::Trait(id) => Ok(id),
            _ => Err(ModuleTreeLookupError {
                n_matched_before_fail: path.len() - 1,
            }),
        }
    }
}

pub struct ModuleTreeSubModuleHandle<'a> {
    module_tree: &'a mut ModuleTree,
    submodule_id: usize,
    is_multiplatform_common: bool,
    platform_info: PlatformInfo,
}

impl<'a> ModuleTreeSubModuleHandle<'a> {
    pub fn get_current_submodule_id(&self) -> (SubModuleId, BackendId, bool) {
        (
            SubModuleId(self.submodule_id),
            self.platform_info.id,
            self.is_multiplatform_common,
        )
    }
    pub fn switch_submodule(
        &mut self,
        submodule_id: SubModuleId,
        platform_info: PlatformInfo,
        is_multiplatform_common: bool,
    ) {
        self.submodule_id = submodule_id.0;
        self.platform_info = platform_info;
        self.is_multiplatform_common = is_multiplatform_common;
    }
    pub fn public_exports_mut(&mut self) -> &mut ModuleExports {
        self.module_tree.sub_modules[self.submodule_id]
            .exports
            .edit_for_platform(self.platform_info.id)
    }

    pub fn preliminary_get<'b>(&self, path: Vec<IdentStr>) -> ModuleTreeLookupPreliminary {
        self.module_tree.import_preliminary(
            self.platform_info,
            SubModuleId(self.submodule_id),
            path,
        )
    }
}

#[derive(Debug, Clone)]
pub enum ModuleTreeLookup {
    Function(MultiplatformFullId),
    Struct(MultiplatformFullId),
    Trait(MultiplatformFullId),
    Module(FullId),
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
    origin_platform_info: PlatformInfo,
    current: ModuleTreeLookup,
    n_consumed: usize,
    unmatched_path: Vec<IdentStr>,
}
