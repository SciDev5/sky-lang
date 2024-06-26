use std::collections::HashMap;

use crate::{back::BackendId, front::ast::ASTSourceFile, modularity::Id};

#[derive(Debug, Clone, PartialEq)]
pub struct ModuleExports<'src> {
    pub functions: HashMap<&'src str, Id>,
    pub datas: HashMap<&'src str, Id>,
    pub traits: HashMap<&'src str, Id>,
    pub consts: HashMap<&'src str, Id>,
    pub typealiases: HashMap<&'src str, Id>,

    pub sources: Vec<(BackendId, ASTSourceFile<'src>)>,
}
impl<'src> ModuleExports<'src> {
    pub fn new_empty() -> Self {
        Self {
            functions: HashMap::new(),
            datas: HashMap::new(),
            traits: HashMap::new(),
            consts: HashMap::new(),
            typealiases: HashMap::new(),
            sources: Vec::new(),
        }
    }
}
