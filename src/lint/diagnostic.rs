use std::fmt::Debug;

use crate::{
    front::{parse::ParseDiagnostic, source::Loc},
    middle::{
        import::ImportDiagnostic, resolution_diagnostics::ResolutionDiagnostic,
        statics::verify_merge::StaticVerifyDiagnostic, types::TypeDiagnostic,
    },
};

pub type Fallible<T> = Result<T, DiagnosticId>;
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct DiagnosticId(usize);
pub struct Diagnostics {
    list: Vec<Diagnostic>,
}
impl Diagnostics {
    pub fn init() -> Self {
        Self { list: Vec::new() }
    }
    pub fn raise<T: ToDiagnostic>(&mut self, d: T, loc: Loc) -> DiagnosticId {
        let id = self.list.len();
        self.list.push(Diagnostic {
            loc,
            content: d.to_content(),
        });
        return DiagnosticId(id);
    }
    /// Sets the number of diagnostics stored to the given length or less if it was already less.
    pub fn resize_shrink(&mut self, new_len: usize) {
        if self.list.len() > new_len {
            self.list.resize_with(new_len, || unreachable!());
        }
    }
    /// Gets the number of diagnostics raised so far.
    pub fn len(&mut self) -> usize {
        self.list.len()
    }

    #[cfg(test)]
    pub fn dbg_as_slice(&self) -> &[Diagnostic] {
        &self.list
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Diagnostic {
    pub loc: Loc,
    pub content: DiagnosticContent,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DiagnosticContent {
    Parse(ParseDiagnostic),
    Resolution(ResolutionDiagnostic),
    Import(ImportDiagnostic),
    Type(TypeDiagnostic),
    StaticVerify(StaticVerifyDiagnostic),
}
pub trait ToDiagnostic: Clone + Debug + Copy {
    fn to_content(self) -> DiagnosticContent;
}
