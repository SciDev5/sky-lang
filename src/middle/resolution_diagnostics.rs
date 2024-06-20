use crate::lint::diagnostic::{DiagnosticContent, ToDiagnostic};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ResolutionDiagnostic {
    DuplicateName,
}

impl ToDiagnostic for ResolutionDiagnostic {
    fn to_content(self) -> DiagnosticContent {
        DiagnosticContent::Resolution(self)
    }
}
