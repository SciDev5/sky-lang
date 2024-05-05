//! Modules are the individual components that packages are assembled out of,
//! beginning with a root module and referencing child modules by name in a
//! tree structure.
//!
//! Additionally, modules contain one or more source files targeting different
//! compilation backends ([`crate::back`]), allowing compilation to cater to
//! the needs of each target platform.
//!
//! Modules may also import and export references to static entities
//! ([`crate::middle::entity`]), such as datas, traits, and constants. Exports are
//! coordinated between these separate sources in order to allow more general
//! code to be reused between platforms.

use super::Id;
use crate::{
    back::{BackendId, BackendInfo},
    TODO_entities,
};
use std::{collections::HashMap, ops::Index};

/// Generic form for data that exists for each module in the package.
pub struct ModuleList<T> {
    module_list: Vec<T>,
}
impl<T> Index<usize> for ModuleList<T> {
    type Output = T;
    fn index(&self, index: usize) -> &Self::Output {
        &self.module_list[index]
    }
}

/// Map of module children by name for each module in the package.
pub type ModuleTree<'src> = ModuleList<HashMap<&'src str, usize>>;
/// Map of module exports for each module in the package.
pub type ModuleExports<'src> = ModuleList<Exports>;
/// Map of module entities for each module in the package.
pub type ModuleEntities<'src> = ModuleList<TODO_entities>;

/// This is the lookup action of an import statement. This function traces through
/// the module and entity exports based on the names `path`.
///
/// Ex. `import a.b.c` &rarr; `from = Export { id: <id for mod "a"> , ty: Mod },
/// path = ["b","c"]`
pub fn import_lookup<'src, 'a, F: Fn(usize, ExportType) -> Option<&'a Exports>>(
    from_backend: &'static BackendInfo,
    from: Export,
    path: &[&'src str],
    exports_local: &ModuleExports<'src>,
    entity_exports_local: F,
    nonlocal: &[(&ModuleExports<'src>, &ModuleEntities<'src>)],
) -> Result<Export, ImportLookupError> {
    let mut current = from;
    for (i, path_step) in path.iter().copied().enumerate() {
        let next = match (current.id, current.ty) {
            (id, ExportType::Mod) => match id {
                Id::Local { id } => exports_local[id].lut.get(path_step),
                Id::Dependency { package_id, id } => nonlocal[package_id].0[id].lut.get(path_step),
            },
            (Id::Local { id }, export_type) => {
                entity_exports_local(id, export_type).and_then(|exports| exports.lut.get(path_step))
            }
            (Id::Dependency { package_id, id }, export_type) => {
                todo!("resolve exports of entities in compiled modules");
            }
        };
        if let Some(next) = next {
            if let Some(next) = from_backend
                .compat_ids
                .iter()
                .find_map(|compat| next.get(compat))
            {
                current = *next;
            } else {
                return Err(ImportLookupError { matched_count: i });
            }
        } else {
            return Err(ImportLookupError { matched_count: i });
        }
    }
    Ok(current)
}
pub struct ImportLookupError {
    /// The number of steps successfully matched from [`import_lookup`] path
    /// before a failed match.
    matched_count: usize,
}

/// A map to references representing what a certain module or entity can export
/// to other scopes.
pub struct Exports {
    /// A map from strings to name the exported data to a map from backend id
    /// (representing the most general backend that exports this entity
    /// specifically) to entity export.
    lut: HashMap<String, HashMap<BackendId, Export>>,
}

/// An id and associated type. This is a general format used to represent what
/// is exported from some module or entity.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Export {
    id: Id,
    ty: ExportType,
}

/// Type of exportable entities/modules. Used by [`Export`].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExportType {
    Mod,
    Const,
    Data,
    Trait,
    Function,
}
