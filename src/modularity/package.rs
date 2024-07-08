//! Packages are the largest modular components directly supported by
//! the language, containing many modules ([`crate::middle::module`]),
//! and referencing other packages as dependencies.
//!
//! Packages may have zero or more dependencies, which are other
//! packages that the code in this package may reference, serving
//! as libraries or similar.
//!

use crate::middle::{module::BuiltModuleExports, statics::BuiltStatics};

use super::semver::SemVerBound;

pub struct PackageSpec {
    pub name: String,
    /// Map of dependency ids to the relevant information to look them up
    pub deps: Vec<DependencySpec>,
}

/// Information that can be used to look up a package to include as a
/// dependency.
pub struct DependencySpec {
    pub name: String,
    pub version: SemVerBound,
}

/// Mappings from dependency ids to package ids, valid for the duration of
/// this build.
pub struct BuildDependencies {
    /// Uses the same index as [`PackageSpec::deps`].
    ///
    /// Stores the current true package id that these dependencies
    /// map to.
    pub dep_packages: Vec<usize>,
}

pub struct BuiltPackage {
    pub name: String,

    pub statics: BuiltStatics,
    pub exports: Vec<BuiltModuleExports>,

    pub deps: Vec<DependencySpec>,
    pub reexpoted: Vec<usize>,
}
