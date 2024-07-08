pub mod package;
pub mod semver;

/// Ids that can point to any entity in this package or any dependency.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Id {
    Local { id: usize },
    Dependency { package_id: usize, id: usize },
}
impl Id {
    pub fn map_local<F: FnMut(usize) -> usize>(self, mut map: F) -> Self {
        match self {
            Id::Local { id } => Id::Local { id: map(id) },
            id => id,
        }
    }
    pub fn unwrap_local(self) -> usize {
        match self {
            Id::Local { id } => id,
            _ => panic!("expected id to be `Id::Local`"),
        }
    }
}
