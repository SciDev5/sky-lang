use std::fmt::Debug;

use crate::front::{
    parse::ParseDiagnostic,
    source::{Loc, LocInFile},
};

pub struct Diagnostics {
    list: Vec<Diagnostic>,
}
impl Diagnostics {
    pub fn push<T: ToDiagnostic>(&mut self, d: T, loc: Loc, file_id: usize) {
        self.list.push(Diagnostic {
            loc: LocInFile { file_id, loc },
            content: d.to_content(),
        });
    }
}

pub struct Diagnostic {
    loc: LocInFile,
    content: DiagnosticContent,
}

#[derive(Debug, Clone)]
pub enum DiagnosticContent {
    Parse(ParseDiagnostic),
}
pub trait ToDiagnostic: Clone + Debug {
    fn to_content(self) -> DiagnosticContent;
}
