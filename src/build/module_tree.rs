use std::{collections::HashMap, rc::Rc};

use crate::{
    common::{common_module::CommonModule, IdentInt, IdentStr},
    parse::tokenization::{IDENT_PARENT_MODULE, IDENT_ROOT_MODULE},
};

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct SubModuleId(usize);
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct PackageId(pub usize);

#[derive(Debug, Clone, PartialEq, Eq)]
struct SubModuleTreeEntry {
    children: HashMap<IdentStr, SubModuleId>,
    code_mod_id: Option<IdentInt>,
    pub public_module_exports: ModuleExports,
}
impl SubModuleTreeEntry {
    fn empty() -> Self {
        Self {
            children: HashMap::new(),
            code_mod_id: None,
            public_module_exports: ModuleExports::new_empty(),
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
    pub fn insert_at_root(
        &mut self,
        path: &[IdentStr],
        module_code_id: IdentInt,
    ) -> Option<SubModuleId> {
        self.insert(SubModuleId(0), path, module_code_id)
    }
    pub fn insert(
        &mut self,
        base_submodule_id: SubModuleId,
        path: &[IdentStr],
        module_code_id: IdentInt,
    ) -> Option<SubModuleId> {
        let mut id = base_submodule_id;
        if id.0 >= self.sub_modules.len() {
            eprintln!("[{}:{}] Somehow provided base_submodule_id that is out of bounds for this SubModuleTree.", file!(), line!());
            return None;
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

        let prev_module_code_id = &mut self.sub_modules[id.0].code_mod_id;
        if prev_module_code_id.is_some() {
            None
        } else {
            *prev_module_code_id = Some(module_code_id);
            Some(id)
        }
    }
    pub fn edit_submodule_data(&mut self, submodule_id: usize) -> ModuleTreeSubModuleHandle {
        ModuleTreeSubModuleHandle {
            module_tree: self,
            submodule_id,
        }
    }

    pub fn finish_get(
        &self,
        preluid: ModuleTreeLookupPreliminary,
    ) -> Result<ModuleTreeLookup, ModuleTreeLookupError> {
        self.get(preluid.lookup, &preluid.unmatched_path)
    }
    pub fn get_fn(
        &self,
        mut current_lookup: ModuleTreeLookup,
        path: &[IdentStr],
    ) -> Result<FullId, ModuleTreeLookupError> {
        current_lookup = self.get(current_lookup, &path[..path.len() - 1])?;

        self.get_exports(current_lookup)
            .and_then(|exports| exports.functions.get(&path[path.len() - 1]))
            .copied()
            .ok_or(ModuleTreeLookupError {
                n_matched_before_fail: path.len() - 1,
            })
    }
    fn get_exports(&self, current_lookup: ModuleTreeLookup) -> Option<&ModuleExports> {
        Some(match current_lookup {
            ModuleTreeLookup::Function(_) => {
                return None;
            }
            ModuleTreeLookup::Struct(_) | ModuleTreeLookup::Trait(_) => {
                todo!("// TODO handle imports from inside structs/traits");
            }
            ModuleTreeLookup::Module(FullId::Local(submod_id)) => {
                &self.sub_modules[submod_id].public_module_exports
            }
            ModuleTreeLookup::Module(FullId::NonLocal {
                dependency_id,
                id: submod_id,
            }) => {
                &self.dependency_list[dependency_id]
                    .module
                    .submodule_tree
                    .sub_modules[submod_id]
                    .public_module_exports
            }
        })
    }
    pub fn get(
        &self,
        mut current_lookup: ModuleTreeLookup,
        path: &[IdentStr],
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
            current_lookup = if let Some(id) = exports.modules.get(ident) {
                ModuleTreeLookup::Module(id.standardize_dependency_refs(prev_id))
            } else if let Some(id) = exports.structs.get(ident) {
                ModuleTreeLookup::Struct(id.standardize_dependency_refs(prev_id))
            } else if let Some(id) = exports.traits.get(ident) {
                ModuleTreeLookup::Trait(id.standardize_dependency_refs(prev_id))
            } else if let Some(id) = exports.functions.get(ident) {
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
}

impl<'a> ModuleTreeSubModuleHandle<'a> {
    pub fn switch_submodule(&mut self, new_submodule_id: usize) {
        self.submodule_id = new_submodule_id
    }
    pub fn public_exports_mut(&mut self) -> &mut ModuleExports {
        &mut self.module_tree.sub_modules[self.submodule_id].public_module_exports
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
                    lookup: ModuleTreeLookup::Module(FullId::Local(id)),
                    unmatched_path: path,
                };
            }
        }
        ModuleTreeLookupPreliminary {
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
                lookup: self
                    .module_tree
                    .get(
                        ModuleTreeLookup::Module(FullId::NonLocal {
                            dependency_id,
                            id: 0,
                        }),
                        &path[1..],
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

#[derive(Debug, Clone, Copy)]
pub enum ModuleTreeLookup {
    Function(FullId),
    Struct(FullId),
    Trait(FullId),
    Module(FullId),
}
impl ModuleTreeLookup {
    fn id(self) -> FullId {
        match self {
            Self::Function(id) | Self::Struct(id) | Self::Trait(id) | Self::Module(id) => id,
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
    lookup: ModuleTreeLookup,
    unmatched_path: Vec<IdentStr>,
}
