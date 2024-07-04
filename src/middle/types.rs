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
    statics::{
        scopes::{ScopeId, Scopes},
        verify_merge::{TemplateNamesRef, TraitAssociatedTyIdMap},
        TypeAlias,
    },
};
mod solve;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TypeDiagnostic {
    IllegalNamedTemplateArgsOnMatcherType,
    FailedToConvertType,
    InnerTypeCannotTakeTemplateArgs,
    IllegalUseOfUndefinedSelfTy,
    IllegalAccessInnerNonName,
    IllegalAccessInnerNoValidOptions,
    IllegalAccessInnerTooManyValidOptions,
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
    Associated(TypeDataTraitAssociated),
    Aliased(TypeDtatlikeAliased),
}
impl HasLoc for TypeDatalike {
    fn loc(&self) -> Loc {
        match self {
            Self::Data(v) => v.loc,
            Self::Template(v) => v.loc,
            Self::Associated(v) => v.loc,
            Self::Aliased(v) => v.loc,
        }
    }
}
impl TypeDatalike {
    fn edit_loc(&mut self) -> &mut Loc {
        match self {
            Self::Data(v) => &mut v.loc,
            Self::Template(v) => &mut v.loc,
            Self::Associated(v) => &mut v.loc,
            Self::Aliased(v) => &mut v.loc,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeDtatlikeAliased {
    pub loc: Loc,
    pub id: Id,
    pub templates: Vec<Fallible<TypeDatalike>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeData {
    pub loc: Loc,
    pub id: Id,
    pub templates: Vec<Fallible<TypeDatalike>>,
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeTraitlike {
    pub loc: Loc,
    pub id: Id,
    pub templates: Vec<TypeDatalike>,
    pub constrained_associated: HashMap<usize, TypeDatalike>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeTemplate {
    pub loc: Loc,
    pub id: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeDataTraitAssociated {
    pub loc: Loc,
    pub inner: Box<TypeDatalike>,
    pub from_trait: TypeTraitlike,
    pub associated_id: usize,
}

struct ConvertTypeDatalike<'a, 'src> {
    scopes: &'a Scopes<ImportedScope<'src>>,
    scope: ScopeId,
    exports: &'a ExportsLookup<'src>,
    local_modules: &'a Vec<LocalModule<'src>>,
    mod_id: usize,
    templates: &'a TemplateNamesRef<'src>,
    self_ty: Option<&'a TypeDatalike>,
    diagnostics: &'a mut Diagnostics,
    trait_associated_ty_id_map: &'a TraitAssociatedTyIdMap<'src>,
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

pub struct StaticsInfo<'a, 'src> {
    pub scopes: &'a Scopes<ImportedScope<'src>>,
    pub exports: &'a ExportsLookup<'src>,
    pub local_modules: &'a Vec<LocalModule<'src>>,
    pub diagnostics: &'a mut Diagnostics,
}

pub fn convert_type_datalike<'src>(
    ty: ASTType<'src>,
    scope: ScopeId,
    templates: &TemplateNamesRef,
    self_ty: Option<&TypeDatalike>,
    trait_associated_ty_id_map: &TraitAssociatedTyIdMap<'src>,
    StaticsInfo {
        diagnostics,
        exports,
        local_modules,
        scopes,
    }: &mut StaticsInfo<'_, 'src>,
) -> Fallible<TypeDatalike> {
    let mod_id = {
        scopes
            .find_map(scope, |scope| Some(scope.mod_id))
            .expect("should exist, if it doesn't, there's a static object somewhere with no static declarations, which shouldnt be possible")
    };
    ConvertTypeDatalike {
        scopes,
        scope,
        exports,
        local_modules,
        mod_id,
        templates,
        self_ty,
        diagnostics,
        trait_associated_ty_id_map,
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
            // todo! type aliases
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
        if path.len() == 1 {
            let bounds = match &ty {
                TypeDatalike::Template(t) => self.templates[t.id].2.as_slice(),
                _ => todo!("get bounds from more complex types"),
            };
            let (path_0, loc_name) = if let Some(v) = f(&path[0]) {
                v
            } else {
                self.diagnostics.raise(
                    TypeDiagnostic::IllegalAccessInnerNonName,
                    ty.loc().new_from_end(),
                );
                return Ok(ty); // fail but fallback
            };

            let options = bounds
                .iter()
                .filter_map(|bound| match bound.id {
                    Id::Local { id } => {
                        Some((*self.trait_associated_ty_id_map[id].get(path_0)?, bound))
                    }
                    Id::Dependency { .. } => todo!("get associated types of dependency traits"),
                })
                .collect::<Vec<_>>();

            if options.len() == 1 {
                let (associated_id, from_trait) = options[0];
                return Ok(TypeDatalike::Associated(TypeDataTraitAssociated {
                    loc: ty.loc().merge(loc_name),
                    inner: Box::new(ty),
                    from_trait: from_trait.clone(),
                    associated_id,
                }));
            } else {
                self.diagnostics.raise(
                    if options.len() == 0 {
                        TypeDiagnostic::IllegalAccessInnerNoValidOptions
                    } else {
                        TypeDiagnostic::IllegalAccessInnerTooManyValidOptions
                    },
                    ty.loc().new_from_end(),
                );
                return Ok(ty); // fail but fallback
            }
        }
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
            if let Some(template_id) = self
                .templates
                .iter()
                .enumerate()
                .find_map(|(i, (v, _, _))| if *v == name { Some(i) } else { None })
            {
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
                let mut self_ty = self_ty.clone();
                *self_ty.edit_loc() = loc;
                return Some(self.access_inner(self_ty, &path[1..], |k| match k.value {
                    ASTIdentValue::Name(v) => Some((v, loc)),
                    _ => None,
                }));
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

pub fn convert_type_traitlike<'src>(
    ty: ASTType<'src>,
    scope: ScopeId,
    templates: &TemplateNamesRef,
    self_ty: Option<&TypeDatalike>,
    trait_associated_ty_id_map: &TraitAssociatedTyIdMap<'src>,
    StaticsInfo {
        diagnostics,
        exports,
        local_modules,
        scopes,
    }: &mut StaticsInfo<'_, 'src>,
) -> Fallible<TypeTraitlike> {
    todo!("ast type into traitlike")
}

pub fn eq_type_datalike(a: &TypeDatalike, b: &TypeDatalike, typealiases: &Vec<TypeAlias>) -> bool {
    match (a, b) {
        (TypeDatalike::Data(a), TypeDatalike::Data(b)) => {
            a.id == b.id && eq_templatevalues(&a.templates, &b.templates, typealiases)
        }
        (TypeDatalike::Template(a), TypeDatalike::Template(b)) => a.id == b.id,
        (TypeDatalike::Associated(a), TypeDatalike::Associated(b)) => {
            a.associated_id == b.associated_id
                && a.from_trait == b.from_trait
                && eq_type_datalike(a.inner.as_ref(), b.inner.as_ref(), typealiases)
        }
        _ => todo!("associated types and typealiases"),
    }
}
pub fn eq_type_traitlike(
    a: &TypeTraitlike,
    b: &TypeTraitlike,
    typealiases: &Vec<TypeAlias>,
) -> bool {
    todo!("traitlike type equality");
}
fn eq_templatevalues(
    a: &Vec<Fallible<TypeDatalike>>,
    b: &Vec<Fallible<TypeDatalike>>,
    typealiases: &Vec<TypeAlias>,
) -> bool {
    a.len() == b.len()
        && a.iter().zip(b.iter()).all(|(a, b)| match (a, b) {
            (Ok(a), Ok(b)) => eq_type_datalike(a, b, typealiases),
            _ => false,
        })
}
