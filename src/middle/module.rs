use std::collections::HashMap;

use crate::{back::BackendId, front::ast::ASTSourceFile, modularity::Id};

#[derive(Debug, Clone, PartialEq)]
pub struct ModuleParts<'src> {
    pub parent: Option<usize>,
    pub children: HashMap<&'src str, usize>,

    pub parts: HashMap<BackendId, ASTSourceFile<'src>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LocalModule<'src> {
    pub parent: Option<usize>,
    pub children: HashMap<&'src str, usize>,

    pub exports: ModuleExports<'src>,

    pub sources: Vec<(BackendId, ASTSourceFile<'src>)>,
}
impl<'src> LocalModule<'src> {
    pub fn new_empty(parent: Option<usize>, children: HashMap<&'src str, usize>) -> Self {
        Self {
            parent,
            children,
            exports: ModuleExports::new(),
            sources: Vec::new(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ModuleExports<'src> {
    pub functions: HashMap<&'src str, Id>,
    pub datas: HashMap<&'src str, Id>,
    pub traits: HashMap<&'src str, Id>,
    pub consts: HashMap<&'src str, Id>,
    pub typealiases: HashMap<&'src str, Id>,
    pub modules: HashMap<&'src str, Id>,
}
impl<'src> ModuleExports<'src> {
    fn new() -> Self {
        Self {
            functions: HashMap::new(),
            datas: HashMap::new(),
            traits: HashMap::new(),
            consts: HashMap::new(),
            typealiases: HashMap::new(),
            modules: HashMap::new(),
        }
    }
}
