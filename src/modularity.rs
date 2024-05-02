use std::collections::HashMap;

use self::semver::SemVerBound;

mod module;
mod package;
mod semver;

/// Ids that can point to any entity in this package or any dependency.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Id {
    Local {
        id: usize,
    },
    Dependency {
        /// numeric id that references into the dependency reference LUT.
        drl_id: usize,
    },
}

/// Exact id of some entity from another package, only valid for the
/// duration of this build.
struct NonlocalId {
    package: usize,
    id: usize,
}
/// Exact id of some entity from another package, only valid for the
/// duration of this build.
struct NonlocalExport {
    package: usize,
    export: Export,
}
/// A series of text ids that signify the path traced through package
/// dependencies and module children to uniquely access a certain entity
/// in a way which allows for quicker hot-reloads.
struct NonlocalPath {
    self_dep_id: usize,
    /// Names of dependencies of dependencies, ending with the package
    /// that contains the entity of interest.
    package_steps: Vec<String>,
    /// Names of children of modules, ending with the module that contains
    /// the entity of interest.
    module_steps: Vec<String>,
    /// Names of direct exports of these, ending in the entity of interest.
    /// These direct exports do not include re-exports, only direct
    /// children, such as functions associated with datas
    entity_steps: Vec<String>,
}

pub struct Exports {
    lut: HashMap<String, Export>,
}
pub struct Export {
    id: usize,
    ty: ExportType,
}
pub enum ExportType {
    Mod,
    Const,
    Data,
    Trait,
    Function,
}

struct PackageSpec {
    name: String,
    /// Map of dependency ids to the relevant information to look them up
    deps: Vec<DependencySpec>,
}

/// Information that can be used to look up a package to include as a
/// dependency.
struct DependencySpec {
    name: String,
    version: SemVerBound,
}

///
struct DependencyReferenceLUT {
    /// Maps from text-based import step strings to indices into [dependency_references]
    names: Vec<HashMap<String, usize>>,
    references: Vec<NonlocalExport>,
}
