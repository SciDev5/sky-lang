use std::collections::HashMap;

use crate::common::{IdentInt, IdentStr};

use super::ast::ASTBlock;

pub struct ASTModule {
    modules: Vec<ASTBlock>,
    submodule_tree: ASTSubModuleTree,
}
impl ASTModule {
    pub fn new(iter: impl Iterator<Item = (Vec<IdentStr>, ASTBlock)>) -> Self {
        let mut modules = vec![];
        let mut submodule_tree = ASTSubModuleTree::new();
        for (i, (path, block)) in iter.enumerate() {
            modules.push(block);
            submodule_tree.insert(path.into_iter(), i);
        }
        Self {
            modules,
            submodule_tree,
        }
    }
}

struct ASTSubModuleTree {
    self_mod: Option<IdentInt>,
    children: HashMap<IdentStr, ASTSubModuleTree>,
}

impl ASTSubModuleTree {
    fn new() -> Self {
        Self {
            self_mod: None,
            children: HashMap::new(),
        }
    }
    fn insert(&mut self, mut path: impl Iterator<Item = IdentStr>, module: IdentInt) -> bool {
        match (path.next(), &self.self_mod) {
            (None, Some(_)) => {
                eprintln!("// TODO errors about module name collisions");
                false
            }
            (None, None) => {
                self.self_mod = Some(module);
                true
            }
            (Some(path_next), _) => self
                .children
                .entry(path_next)
                .or_insert(Self {
                    self_mod: None,
                    children: HashMap::new(),
                })
                .insert(path, module),
        }
    }
}
