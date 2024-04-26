use std::fmt::Debug;

use crate::common::IdentStr;

use super::tokenization::{BracketType, SeparatorType};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MacroCall<XExpr: Debug + Clone> {
    pub name: IdentStr,
    pub object: MacroObject<XExpr>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MacroObject<XExpr: Debug + Clone> {
    Listlike {
        ty: BracketType,
        children: Vec<(MacroObject<XExpr>, Option<SeparatorType>)>,
    },
    Typelike {
        lookup: Vec<IdentStr>,
    },
    Ident(IdentStr),
    LiteralIdent(IdentStr),
    String(String, Option<Vec<(XExpr, String)>>),
    Expr(Vec<XExpr>),
}

impl<XExpr: Debug + Clone> MacroCall<XExpr> {
    pub fn lazy_map<YExpr: Debug + Clone, F: FnMut(Vec<XExpr>) -> Vec<YExpr>>(
        self,
        mut f: F,
    ) -> MacroCall<YExpr> {
        MacroCall {
            name: self.name,
            object: self.object.lazy_map(&mut f),
        }
    }
}
impl<XExpr: Debug + Clone> MacroObject<XExpr> {
    pub fn lazy_map<YExpr: Debug + Clone>(
        self,
        f: &mut dyn FnMut(Vec<XExpr>) -> Vec<YExpr>,
    ) -> MacroObject<YExpr> {
        match self {
            MacroObject::Listlike { ty, children } => MacroObject::Listlike {
                ty,
                children: children
                    .into_iter()
                    .map(|(child, ty)| (child.lazy_map(&mut |expr| f(expr)), ty))
                    .collect(),
            },
            MacroObject::String(str, children) => MacroObject::String(
                str,
                children.map(|children| {
                    children
                        .into_iter()
                        .map(|(expr, str)| (f(Vec::from([expr])).into_iter().next().unwrap(), str))
                        .collect()
                }),
            ),
            MacroObject::Typelike { lookup } => MacroObject::Typelike { lookup },
            MacroObject::Ident(ident) => MacroObject::Ident(ident),
            MacroObject::LiteralIdent(ident) => MacroObject::LiteralIdent(ident),
            MacroObject::Expr(expr) => MacroObject::Expr(f(expr)),
        }
    }
}
