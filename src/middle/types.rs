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
    lint::diagnostic::{DiagnosticContent, Diagnostics, ToDiagnostic},
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
    IllegalAccessIntoMatchableType,
    IllegalNamedTemplateArgsOnMatcherType,
    FailedToConvertType,
    InnerTypeCannotTakeTemplateArgs,
}
impl ToDiagnostic for TypeDiagnostic {
    fn to_content(self) -> DiagnosticContent {
        DiagnosticContent::Type(self)
    }
}

pub enum TypeDatalikeMatchable {
    Data(TypeMatchable),
    Template(TypeTemplate),
}
impl HasLoc for TypeDatalikeMatchable {
    fn loc(&self) -> Loc {
        match self {
            Self::Data(v) => v.loc,
            Self::Template(v) => v.loc,
        }
    }
}
pub enum TypeDatalike {
    Data(TypeData),
    Template(TypeTemplate),
    Associated(TypeDataAssociated),
    TraitAssociated(TypeDataAssociated),
}

pub struct TypeData {
    loc: Loc,
    id: Id,
    templates: Vec<TypeDatalike>,
}
pub struct TypeMatchable {
    loc: Loc,
    id: Id,
    templates: Vec<Option<TypeDatalikeMatchable>>,
}
pub struct TypeTrait {
    loc: Loc,
    id: Id,
    templates: Vec<TypeDatalike>,
    constrained_associated: HashMap<usize, TypeDatalike>,
}

pub struct TypeTemplate {
    loc: Loc,
    id: usize,
}

pub struct TypeDataAssociated {
    loc: Loc,
    inner: TypeData,
    associated_id: usize,
}
pub struct TypeDataTraitAssociated {
    loc: Loc,
    inner: TypeData,
    from_trait: TypeTrait,
    associated_id: usize,
}

struct ConvertTypeMatchable<'a, 'src> {
    scopes: &'a Scopes<ImportedScope<'src>>,
    scope: ScopeId,
    exports: &'a ExportsLookup<'src>,
    local_modules: &'a Vec<LocalModule<'src>>,
    mod_id: usize,
    templates: Vec<&'src str>,
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

pub fn convert_type_matchable<'src>(
    ty: ASTType<'src>,
    scopes: &Scopes<ImportedScope<'src>>,
    scope: ScopeId,
    exports: &ExportsLookup<'src>,
    local_modules: &Vec<LocalModule<'src>>,
    mod_id: usize,
    templates: Vec<&'src str>,
    diagnostics: &mut Diagnostics,
) -> Option<TypeDatalikeMatchable> {
    ConvertTypeMatchable {
        scopes,
        scope,
        exports,
        local_modules,
        mod_id,
        templates,
        diagnostics,
    }
    .convert(&ty)
}

impl<'a, 'src> ConvertTypeMatchable<'a, 'src> {
    fn convert(&mut self, ty: &ASTType<'src>) -> Option<TypeDatalikeMatchable> {
        if let Some(ty) = self.try_convert_pathlike(ty) {
            ty
        } else if let Some(ty) = self.try_convert_templateargs(ty) {
            ty
        } else {
            self.diagnostics
                .raise(TypeDiagnostic::FailedToConvertType, ty.loc());
            None
        }
    }

    fn try_convert_templateargs(
        &mut self,
        ty: &ASTType<'src>,
    ) -> Option<Option<TypeDatalikeMatchable>> {
        let ASTType::TypeParam {
            loc,
            inner,
            params,
            named_params,
        } = ty
        else {
            return None;
        };

        let Some(inner) = self.convert(inner) else {
            return Some(None);
        };

        let templates = params
            .into_iter()
            .map(|ty| ty.as_ref().ok().and_then(|ty| self.convert(ty)))
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
            TypeDatalikeMatchable::Data(TypeMatchable { id, loc, templates })
                if templates.is_empty() =>
            {
                (id, loc)
            }
            v => {
                self.diagnostics
                    .raise(TypeDiagnostic::InnerTypeCannotTakeTemplateArgs, v.loc());
                return Some(Some(v));
            }
        };

        Some(Some(TypeDatalikeMatchable::Data(TypeMatchable {
            loc,
            id,
            templates,
        })))
    }

    fn try_convert_pathlike(
        &mut self,
        ty: &ASTType<'src>,
    ) -> Option<Option<TypeDatalikeMatchable>> {
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
                if path.len() > 1 {
                    self.diagnostics.raise(
                        TypeDiagnostic::IllegalAccessIntoMatchableType,
                        path[0].loc().merge_some(path.last().map(|it| it.loc())),
                    );
                    return Some(None);
                }

                return Some(Some(TypeDatalikeMatchable::Template(TypeTemplate {
                    loc,
                    id: template_id,
                })));
            }
        }

        Some(
            if let Some(((id, loc), trailing)) = resolve_data(
                path,
                self.scopes,
                self.scope,
                self.local_modules,
                self.mod_id,
                self.exports,
                self.diagnostics,
            ) {
                if trailing.len() > 0 {
                    self.diagnostics.raise(
                        TypeDiagnostic::IllegalAccessIntoMatchableType,
                        trailing[0]
                            .loc()
                            .merge_some(trailing.last().map(|(_, loc)| *loc)),
                    );
                    None
                } else {
                    Some(TypeDatalikeMatchable::Data(TypeMatchable {
                        loc,
                        id,
                        templates: Vec::new(),
                    }))
                }
            } else {
                None
            },
        )
    }
}
