use std::collections::HashMap;

use crate::common::{IdentInt, IdentStr};

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct SubModuleId(usize);

#[derive(Debug, Clone, PartialEq, Eq)]
struct SubModuleTreeEntry {
    children: HashMap<IdentStr, SubModuleId>,
    code_mod_id: Option<IdentInt>,
}
impl SubModuleTreeEntry {
    fn empty() -> Self {
        Self {
            children: HashMap::new(),
            code_mod_id: None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct SubModuleResolution {
    pub id: SubModuleId,
    pub child: Option<IdentStr>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SubModuleTree {
    entries: Vec<SubModuleTreeEntry>,
}

impl SubModuleTree {
    pub fn new() -> Self {
        Self {
            entries: vec![SubModuleTreeEntry::empty()],
        }
    }
    pub fn insert_at_root(
        &mut self,
        path: impl Iterator<Item = IdentStr>,
        module_code_id: IdentInt,
    ) -> Option<SubModuleId> {
        self.insert(SubModuleId(0), path, module_code_id)
    }
    pub fn insert(
        &mut self,
        base_submodule_id: SubModuleId,
        mut path: impl Iterator<Item = IdentStr>,
        module_code_id: IdentInt,
    ) -> Option<SubModuleId> {
        let mut id = base_submodule_id;
        if id.0 >= self.entries.len() {
            eprintln!("[{}:{}] Somehow provided base_submodule_id that is out of bounds for this SubModuleTree.", file!(), line!());
            return None;
        }

        while let Some(path_step) = path.next() {
            let n_entries = self.entries.len();
            let mut new_tree_entry = false;
            id = *self.entries[id.0]
                .children
                .entry(path_step)
                .or_insert_with(|| {
                    new_tree_entry = true;
                    SubModuleId(n_entries)
                });
            if new_tree_entry {
                self.entries.push(SubModuleTreeEntry::empty());
            }
        }

        let prev_module_code_id = &mut self.entries[id.0].code_mod_id;
        if prev_module_code_id.is_some() {
            None
        } else {
            *prev_module_code_id = Some(module_code_id);
            Some(id)
        }
    }
    pub fn resolve_at_root(&self, path: &[IdentStr]) -> Option<SubModuleResolution> {
        self.resolve(SubModuleId(0), path)
    }

    pub fn resolve(
        &self,
        base_submodule_id: SubModuleId,
        path: &[IdentStr],
    ) -> Option<SubModuleResolution> {
        let mut id = base_submodule_id;
        if id.0 >= self.entries.len() {
            eprintln!("[{}:{}] Somehow provided base_submodule_id that is out of bounds for this SubModuleTree.", file!(), line!());
            return None;
        }

        if path.len() == 1 {
            self.entries[id.0]
                .children
                .get(&path[0])
                .map(|id| SubModuleResolution {
                    id: *id,
                    child: None,
                })
        } else {
            for path_step in &path[..path.len() - 1] {
                id = *self.entries[id.0].children.get(path_step)?;
            }
            let child = {
                let path_step = path.last().unwrap();
                match self.entries[id.0].children.get(path_step) {
                    Some(new_id) => {
                        id = *new_id;
                        None
                    }
                    None => Some(path_step.clone()),
                }
            };

            Some(SubModuleResolution { id, child })
        }
    }
    pub fn get<'a>(
        &self,
        base_submodule_id: SubModuleId,
        mut path: impl Iterator<Item = &'a IdentStr>,
    ) -> Option<IdentInt> {
        let mut id = base_submodule_id;
        if id.0 >= self.entries.len() {
            eprintln!("[{}:{}] Somehow provided base_submodule_id that is out of bounds for this SubModuleTree.", file!(), line!());
            return None;
        }

        while let Some(path_step) = path.next() {
            id = *self.entries[id.0].children.get(path_step)?;
        }

        self.entries[id.0].code_mod_id
    }
}
