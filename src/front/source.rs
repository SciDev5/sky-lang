pub type SourceFileId = usize;

/// A range of char indices in the source code.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Loc {
    pub start: usize,
    pub length: usize,
}
impl Loc {
    pub fn merge(self, to: Self) -> Self {
        Self {
            start: self.start,
            length: (to.start - self.start) + to.length,
        }
    }
}
/// A range of char indices in the source code in a specific file.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LocInFile {
    pub file_id: SourceFileId,
    pub loc: Loc,
}
