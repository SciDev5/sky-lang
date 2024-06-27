use std::collections::HashMap;

use crate::{back::BackendId, front::ast::ASTSourceFile, modularity::Id};

#[derive(Debug, Clone, PartialEq)]
pub struct ModuleExportPart<'src> {
    pub functions: HashMap<&'src str, Id>,
    pub datas: HashMap<&'src str, Id>,
    pub traits: HashMap<&'src str, Id>,
    pub consts: HashMap<&'src str, Id>,
    pub typealiases: HashMap<&'src str, Id>,

    pub source: ASTSourceFile<'src>,
}
pub struct ModuleParts<'src> {
    pub parent: Option<usize>,
    pub children: HashMap<&'src str, usize>,

    pub export_parts: HashMap<BackendId, ModuleExportPart<'src>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ModuleExports<'src> {
    pub parent: Option<usize>,
    pub children: HashMap<&'src str, usize>,

    pub functions: HashMap<&'src str, Id>,
    pub datas: HashMap<&'src str, Id>,
    pub traits: HashMap<&'src str, Id>,
    pub consts: HashMap<&'src str, Id>,
    pub typealiases: HashMap<&'src str, Id>,

    pub sources: Vec<(BackendId, ASTSourceFile<'src>)>,
}
impl<'src> ModuleExports<'src> {
    pub fn new_empty(parent: Option<usize>, children: HashMap<&'src str, usize>) -> Self {
        Self {
            parent,
            children,
            functions: HashMap::new(),
            datas: HashMap::new(),
            traits: HashMap::new(),
            consts: HashMap::new(),
            typealiases: HashMap::new(),
            sources: Vec::new(),
        }
    }
}
