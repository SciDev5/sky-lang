use crate::lint::diagnostic::{DiagnosticContent, ToDiagnostic};

#[derive(Debug, Clone)]
pub enum ParseDiagnostic {}
impl ToDiagnostic for ParseDiagnostic {
    fn to_content(self) -> DiagnosticContent {
        DiagnosticContent::Parse(self)
    }
}
