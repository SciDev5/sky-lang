/// Struct that contains a valid semver version.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SemVer {
    major: u16,
    minor: u16,
    patch: u16,
    release_meta: Option<String>,
    build_meta: Option<String>,
}
/// Struct that contains part of a valid semver  version for use in
/// locating dependencies in a list with many versions.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SemVerBound {
    Major { major: u16 },
    MajorMinor { major: u16, minor: u16 },
    Full(SemVer),
}
