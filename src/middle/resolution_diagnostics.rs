use crate::{
    front::source::Loc,
    lint::diagnostic::{DiagnosticContent, ToDiagnostic},
};

#[derive(Debug, Clone)]
pub enum ResolutionDiagnostic {
    DuplicateName,
}

impl ToDiagnostic for ResolutionDiagnostic {
    fn to_content(self) -> DiagnosticContent {
        DiagnosticContent::Resolution(self)
    }
}

pub struct ResolutionDiagnostics {
    pub diagnostics: Vec<(ResolutionDiagnostic, Loc)>,
}
impl ResolutionDiagnostics {
    pub fn new() -> Self {
        Self {
            diagnostics: Vec::new(),
        }
    }
    pub fn raise(&mut self, d: ResolutionDiagnostic, loc: Loc) {
        self.diagnostics.push((d, loc))
    }
}
