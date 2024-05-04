mod module;
mod package;
mod semver;

/// Ids that can point to any entity in this package or any dependency.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Id {
    Local { id: usize },
    Dependency { package_id: usize, id: usize },
}
