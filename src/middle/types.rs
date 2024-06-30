//! Types are the hopefully not unfamilliar to you. In this context they
//! come in two categories:
//! - Traitlike types
//!     > behave like constraints and consist of a reference to one or more
//!     > traits that must be satisfied for this type to be satisfied. They
//!     > can be bounded by generic types.
//! - Datalike types
//!     > behave like constraints and consist of a reference to one struct
//!     > type. They too can be bounded by generic types, and this is how
//!     > most complexity is created.
//!
//!     > A few weird things exist within the system, such as accesses to
//!     > associated types, but this requires impls to be solved.

use std::collections::HashMap;

use crate::{
    front::{
        ast::{ASTIdent, ASTIdentValue, ASTName, ASTType},
        source::{HasLoc, Loc},
    },
    lint::diagnostic::{DiagnosticContent, Diagnostics, Fallible, ToDiagnostic},
    modularity::Id,
};

use super::{
    import::{resolve_data, ExportsLookup, ImportedScope},
    module::LocalModule,
    statics::scopes::{ScopeId, Scopes},
};
mod solve;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TypeDiagnostic {
    IllegalNamedTemplateArgsOnMatcherType,
    FailedToConvertType,
    InnerTypeCannotTakeTemplateArgs,
    IllegalUseOfUndefinedSelfTy,
}
impl ToDiagnostic for TypeDiagnostic {
    fn to_content(self) -> DiagnosticContent {
        DiagnosticContent::Type(self)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeDatalike {
    Data(TypeData),
    Template(TypeTemplate),
    Associated(TypeDataAssociated),
}
impl HasLoc for TypeDatalike {
    fn loc(&self) -> Loc {
        match self {
            Self::Data(v) => v.loc,
            Self::Template(v) => v.loc,
            Self::Associated(v) => v.loc,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeData {
    loc: Loc,
    id: Id,
    templates: Vec<Fallible<TypeDatalike>>,
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeTraitlike {
    loc: Loc,
    id: Id,
    templates: Vec<TypeDatalike>,
    constrained_associated: HashMap<usize, TypeDatalike>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeTemplate {
    loc: Loc,
    id: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeDataAssociated {
    loc: Loc,
    inner: TypeData,
    associated_id: usize,
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeDataTraitAssociated {
    loc: Loc,
    inner: TypeData,
    from_trait: TypeTraitlike,
    associated_id: usize,
}

struct ConvertTypeDatalike<'a, 'src> {
    scopes: &'a Scopes<ImportedScope<'src>>,
    scope: ScopeId,
    exports: &'a ExportsLookup<'src>,
    local_modules: &'a Vec<LocalModule<'src>>,
    mod_id: usize,
    templates: Vec<&'src str>,
    self_ty: Option<&'a TypeDatalike>,
    diagnostics: &'a mut Diagnostics,
}

fn try_type_as_import_path<'src>(ty: &ASTType<'src>) -> Option<Vec<ASTIdent<'src>>> {
    Some(match ty {
        ASTType::Ident(ident) => vec![ident.clone()],
        ASTType::Access { ident, inner } => {
            let mut path = vec![ident.clone()];
            let mut current = inner.as_ref();
            loop {
                match current {
                    ASTType::Ident(ident) => {
                        path.push(ident.clone());
                        path.reverse();
                        break;
                    }
                    ASTType::Access { ident, inner } => {
                        path.push(ident.clone());
                        current = inner.as_ref();
                    }
                    _ => return None,
                }
            }
            path
        }
        _ => return None,
    })
}

pub fn convert_type_datalike<'src>(
    ty: ASTType<'src>,
    scopes: &Scopes<ImportedScope<'src>>,
    scope: ScopeId,
    exports: &ExportsLookup<'src>,
    local_modules: &Vec<LocalModule<'src>>,
    mod_id: usize,
    templates: Vec<&'src str>,
    self_ty: Option<&TypeDatalike>,
    diagnostics: &mut Diagnostics,
) -> Fallible<TypeDatalike> {
    ConvertTypeDatalike {
        scopes,
        scope,
        exports,
        local_modules,
        mod_id,
        templates,
        self_ty,
        diagnostics,
    }
    .convert(&ty)
}

impl<'a, 'src> ConvertTypeDatalike<'a, 'src> {
    fn convert(&mut self, ty: &ASTType<'src>) -> Fallible<TypeDatalike> {
        if let Some(ty) = self.try_convert_singular(ty) {
            ty
        } else if let Some(ty) = self.try_convert_with_templateargs(ty) {
            ty
        } else {
            Err(self
                .diagnostics
                .raise(TypeDiagnostic::FailedToConvertType, ty.loc()))
        }
    }

    fn try_convert_with_templateargs(
        &mut self,
        ty: &ASTType<'src>,
    ) -> Option<Fallible<TypeDatalike>> {
        let ASTType::TypeParam {
            loc,
            inner,
            params,
            named_params,
        } = ty
        else {
            return None;
        };

        let inner = match self.convert(inner) {
            Ok(v) => v,
            Err(v) => return Some(Err(v)),
        };

        let templates = params
            .into_iter()
            .map(|ty| ty.as_ref().map_err(|e| *e).and_then(|ty| self.convert(ty)))
            .collect::<Vec<_>>();

        for named in named_params {
            self.diagnostics.raise(
                TypeDiagnostic::IllegalNamedTemplateArgsOnMatcherType,
                named
                    .0
                    .loc
                    .merge_some(named.1.as_ref().ok().map(|it| it.loc())),
            );
        }

        let (id, loc) = match inner {
            TypeDatalike::Data(TypeData { id, loc, templates }) if templates.is_empty() => {
                (id, loc)
            }
            v => {
                self.diagnostics
                    .raise(TypeDiagnostic::InnerTypeCannotTakeTemplateArgs, v.loc());
                return Some(Ok(v));
            }
        };

        Some(Ok(TypeDatalike::Data(TypeData { loc, id, templates })))
    }

    fn access_inner<T>(
        &mut self,
        ty: TypeDatalike,
        path: &[T],
        f: impl Fn(&T) -> Option<(&'src str, Loc)>,
    ) -> Fallible<TypeDatalike> {
        if path.len() > 0 {
            todo!("accesses into traitful type -----------------------------------------------------------------");
        }
        Ok(ty)
    }

    fn try_convert_singular(&mut self, ty: &ASTType<'src>) -> Option<Fallible<TypeDatalike>> {
        let path = try_type_as_import_path(ty)?;

        if let ASTIdent {
            loc,
            value: ASTIdentValue::Name(name),
        } = path[0]
        {
            if let Some(template_id) =
                self.templates
                    .iter()
                    .enumerate()
                    .find_map(|(i, v)| if *v == name { Some(i) } else { None })
            {
                if path.len() > 1 {}

                return Some(self.access_inner(
                    TypeDatalike::Template(TypeTemplate {
                        loc,
                        id: template_id,
                    }),
                    &path[1..],
                    |k| match k.value {
                        ASTIdentValue::Name(v) => Some((v, loc)),
                        _ => None,
                    },
                ));
            }
        }
        if let ASTIdent {
            loc,
            value: ASTIdentValue::SelfTy,
        } = path[0]
        {
            if let Some(self_ty) = self.self_ty {
                return Some(
                    self.access_inner(self_ty.clone(), &path[1..], |k| match k.value {
                        ASTIdentValue::Name(v) => Some((v, loc)),
                        _ => None,
                    }),
                );
            } else {
                return Some(Err(self
                    .diagnostics
                    .raise(TypeDiagnostic::IllegalUseOfUndefinedSelfTy, loc)));
            }
        }

        Some(
            resolve_data(
                path,
                self.scopes,
                self.scope,
                self.local_modules,
                self.mod_id,
                self.exports,
                self.diagnostics,
            )
            .and_then(|((id, loc), trailing)| {
                self.access_inner(
                    TypeDatalike::Data(TypeData {
                        loc,
                        id,
                        templates: Vec::new(),
                    }),
                    &trailing,
                    |v| Some(*v),
                )
            }),
        )
    }
}
