use std::collections::HashMap;

use crate::{
    back::{BackendId, BackendInfo},
    front::{
        ast::{
            ASTAnnot, ASTConst, ASTData, ASTDestructure, ASTFreeImpl, ASTFunction, ASTName,
            ASTStatics, ASTTemplateBound, ASTTemplates, ASTTrait, ASTType, ASTTypeAlias,
        },
        source::Loc,
    },
    lint::diagnostic::{DiagnosticContent, Diagnostics, Fallible, ToDiagnostic},
    middle::{
        import::{ExportsLookup, ImportedScope},
        module::LocalModule,
        statics::{Templates, TraitGeneric},
        types::{
            convert_type_datalike, convert_type_traitlike, StaticsInfo, TypeDatalike,
            TypeDtatlikeAliased, TypeTemplate, TypeTraitlike,
        },
    },
    modularity::Id,
};

use super::{
    merge::{Merged, MergedStatics},
    scopes::{ScopeId, Scopes},
    Annot, ConstUnsolved, Data, Destructure, FunctionUnresolved, TraitConst, TraitFunction,
    TraitTypeAlias, TraitUnresolved, TypeAlias, UnresolvedStatics,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StaticVerifyDiagnostic {
    TraitConstInitializersNotSupported,
    ConstInitializerMissing,
    TraitFunctionArgTyMissing,
    TraitFunctionReturnTypeMissing,
}
impl ToDiagnostic for StaticVerifyDiagnostic {
    fn to_content(self) -> DiagnosticContent {
        DiagnosticContent::StaticVerify(self)
    }
}

fn convert_annot<'src>(ASTAnnot { is_public, doc }: ASTAnnot) -> Annot {
    Annot {
        is_public,
        doc: doc
            .into_iter()
            .map(|it| it.processed_text)
            .reduce(|mut a, b| {
                a.push('\n');
                a.push_str(&b);
                a
            }),
    }
}

struct FailedNameGenerator {
    n: usize,
}
impl FailedNameGenerator {
    fn new() -> Self {
        Self { n: 0 }
    }
    fn gen(&mut self) -> String {
        let str = format!("<name_failed_{}>", self.n);
        self.n += 1;
        str
    }
    fn convert_name<'src>(&mut self, name: Fallible<ASTName<'src>>) -> String {
        name.map(|name| name.value.to_string())
            .unwrap_or_else(|_| self.gen())
    }
}

type TemplateNamesRef<'src> = [(&'src str, Vec<TypeTraitlike>)];
type TemplateNames<'src> = Vec<(&'src str, Vec<TypeTraitlike>)>;

fn verify_merge_type_aliases<'src>(
    typealiases: Vec<Merged<ASTTypeAlias<'src>>>,
    backend_infos: &HashMap<BackendId, BackendInfo>,
    failed_name_gen: &mut FailedNameGenerator,
    trait_associated_ty_id_map: &TraitAssociatedTyIdMap<'src>,
    statics_info: &mut StaticsInfo<'_, 'src>,
) -> Vec<TypeAlias> {
    typealiases
        .into_iter()
        .enumerate()
        .map(|(self_id, merged)| {
            convert_typealias(
                self_id,
                merged,
                backend_infos,
                failed_name_gen,
                trait_associated_ty_id_map,
                statics_info,
            )
        })
        .collect()
}
fn gen_template_names<'src>(templates: &ASTTemplates<'src>) -> TemplateNames<'src> {
    templates
        .as_ref()
        .map(|it| &it.0 as &[_])
        .unwrap_or(&[])
        .iter()
        .map(|template: &ASTTemplateBound| (template.name.value, Vec::new()))
        .collect::<Vec<_>>()
}
fn convert_templates<'src>(
    templates: ASTTemplates<'src>,
    containing_scope: ScopeId,
    template_names: &mut TemplateNames,
    self_ty: Option<&TypeDatalike>,
    trait_associated_ty_id_map: &TraitAssociatedTyIdMap<'src>,
    statics_info: &mut StaticsInfo<'_, 'src>,
) -> Templates {
    let templates = Templates {
        def: templates
            .map(|(templates, _)| {
                templates
                    .into_iter()
                    .map(|template| {
                        (
                            template.name.value.to_string(),
                            template
                                .bounds
                                .into_iter()
                                .flat_map(|bound| {
                                    convert_type_traitlike(
                                        bound,
                                        containing_scope,
                                        &template_names,
                                        self_ty,
                                        trait_associated_ty_id_map,
                                        statics_info,
                                    )
                                    .ok()
                                })
                                .collect(),
                        )
                    })
                    .collect::<Vec<_>>()
            })
            .unwrap_or(Vec::new()),
    };
    for ((_, bounds_to), (_, bounds_from)) in template_names.iter_mut().zip(templates.def.iter()) {
        *bounds_to = bounds_from.clone();
    }
    templates
}

fn convert_typealias<'src>(
    self_id: usize,
    merged: Merged<ASTTypeAlias<'src>>,

    backend_infos: &HashMap<BackendId, BackendInfo>,
    failed_name_gen: &mut FailedNameGenerator,
    trait_associated_ty_id_map: &TraitAssociatedTyIdMap<'src>,
    statics_info: &mut StaticsInfo<'_, 'src>,
) -> TypeAlias {
    if merged.contents.len() == 1 {
        // as it should
        let (
            backend_id,
            ASTTypeAlias {
                loc,
                containing_scope,
                annot,
                templates,
                name,
                value,
            },
        ) = merged.contents.into_iter().next().unwrap();

        let mut template_names = gen_template_names(&templates);
        let self_ty = TypeDatalike::Aliased(TypeDtatlikeAliased {
            loc,
            id: Id::Local { id: self_id },
            templates: (0..templates.as_ref().map_or(0, |it| it.0.len()))
                .map(|id| Ok(TypeDatalike::Template(TypeTemplate { id, loc })))
                .collect(),
        });
        let templates = convert_templates(
            templates,
            containing_scope,
            &mut template_names,
            Some(&self_ty),
            trait_associated_ty_id_map,
            statics_info,
        );
        let ty = value.and_then(|ty| {
            convert_type_datalike(
                ty,
                containing_scope,
                &template_names,
                Some(&self_ty),
                trait_associated_ty_id_map,
                statics_info,
            )
        });

        TypeAlias {
            annot: convert_annot(annot),
            name: failed_name_gen.convert_name(name),
            templates,
            ty,
            loc,
            target: backend_id,
        }
    } else {
        // user wrote bad code trying to multiplatform-ify type aliases
        // evil code even
        todo!("handle when type alias when the no-multiplatform rule is broken")
    }
}

pub type TraitAssociatedTyIdMap<'src> = Vec<HashMap<&'src str, usize>>;
/// Generates for each trait a mapping from the names of the associated types inside
/// to the numeric ids they are about to be assigned.
fn gen_trait_associated_type_map<'src>(
    traits: &Vec<Merged<ASTTrait<'src>>>,
) -> TraitAssociatedTyIdMap<'src> {
    traits
        .iter()
        .map(|t| {
            if t.contents.len() == 1 {
                t.contents
                    .get(&t.base_target_level)
                    .unwrap()
                    .types
                    .iter()
                    .enumerate()
                    .flat_map(|(i, ty)| Some((ty.name.ok()?.value, i)))
                    .collect::<HashMap<_, _>>()
            } else {
                // default to no associated types, errors will be raised about this elsewhere in the trait verifier.
                HashMap::new()
            }
        })
        .collect()
}
fn convert_bounds<'src>(
    bounds: Vec<ASTType<'src>>,

    containing_scope: ScopeId,
    template_names: &mut TemplateNames,
    trait_associated_ty_id_map: &TraitAssociatedTyIdMap<'src>,
    statics_info: &mut StaticsInfo<'_, 'src>,
) -> Vec<TypeTraitlike> {
    bounds
        .into_iter()
        .flat_map(|bound| {
            convert_type_traitlike(
                bound,
                containing_scope,
                &template_names,
                None,
                trait_associated_ty_id_map,
                statics_info,
            )
            .ok()
        })
        .collect()
}

fn convert_destructure<'src>(destructure: ASTDestructure<'src>) -> Fallible<Destructure> {
    Ok(match destructure {
        ASTDestructure::Name(name) => Destructure::Name {
            loc: name.loc,
            name: name.value.to_string(),
        },
        ASTDestructure::Paren { inner, .. } => convert_destructure(*inner?)?,
        ASTDestructure::Tuple { .. } => todo!("complex destructures"),
    })
}

fn convert_trait<'src>(
    merged: Merged<ASTTrait<'src>>,

    backend_infos: &HashMap<BackendId, BackendInfo>,
    failed_name_gen: &mut FailedNameGenerator,
    trait_associated_ty_id_map: &TraitAssociatedTyIdMap<'src>,
    statics_info: &mut StaticsInfo<'_, 'src>,
) -> TraitUnresolved<'src> {
    if merged.contents.len() == 1 {
        // as it should
        let (
            backend_id,
            ASTTrait {
                loc,
                containing_scope,
                annot,
                name,
                templates,
                bounds,
                functions,
                consts,
                types,
            },
        ) = merged.contents.into_iter().next().unwrap();

        let mut template_names = gen_template_names(&templates);
        let templates = convert_templates(
            templates,
            containing_scope,
            &mut template_names,
            None,
            trait_associated_ty_id_map,
            statics_info,
        );

        let consts = consts
            .into_iter()
            .map(|ast| TraitConst {
                annot: convert_annot(ast.annot),
                loc: ast.loc,
                name: failed_name_gen.convert_name(ast.name),
                ty: ast.ty.and_then(|ty| {
                    convert_type_datalike(
                        ty,
                        containing_scope,
                        &template_names,
                        None,
                        trait_associated_ty_id_map,
                        statics_info,
                    )
                }),
            })
            .collect::<Vec<_>>();
        let functions = functions
            .into_iter()
            .map(|ast| {
                let mut template_names = template_names.clone();
                template_names.extend(gen_template_names(&ast.templates).into_iter());
                let templates = convert_templates(
                    ast.templates,
                    containing_scope,
                    &mut template_names,
                    None,
                    trait_associated_ty_id_map,
                    statics_info,
                );

                TraitFunction {
                    annot: convert_annot(ast.annot),
                    loc: ast.loc,
                    name: failed_name_gen.convert_name(ast.name),
                    templates,
                    body: ast.block,
                    args: ast
                        .args
                        .into_iter()
                        .map(|arg| {
                            arg.and_then(|arg| {
                                Ok((
                                    convert_destructure(arg.destructure)?,
                                    arg.ty
                                        .ok_or_else(|| {
                                            statics_info.diagnostics.raise(
                                                StaticVerifyDiagnostic::TraitFunctionArgTyMissing,
                                                ast.loc,
                                            )
                                        })
                                        .and_then(|v| v)
                                        .and_then(|ty| {
                                            convert_type_datalike(
                                                ty,
                                                containing_scope,
                                                &template_names,
                                                None,
                                                trait_associated_ty_id_map,
                                                statics_info,
                                            )
                                        }),
                                ))
                            })
                        })
                        .collect(),
                    return_ty: ast
                        .ty_return
                        .ok_or_else(|| {
                            statics_info.diagnostics.raise(
                                StaticVerifyDiagnostic::TraitFunctionReturnTypeMissing,
                                ast.loc,
                            )
                        })
                        .and_then(|v| v)
                        .and_then(|ty| {
                            convert_type_datalike(
                                ty,
                                containing_scope,
                                &template_names,
                                None,
                                trait_associated_ty_id_map,
                                statics_info,
                            )
                        }),
                }
            })
            .collect::<Vec<_>>();
        let types = types
            .into_iter()
            .map(|ast| TraitTypeAlias {
                annot: convert_annot(ast.annot),
                loc: ast.loc,
                name: failed_name_gen.convert_name(ast.name),
                bounds: convert_bounds(
                    ast.bounds,
                    containing_scope,
                    &mut template_names,
                    trait_associated_ty_id_map,
                    statics_info,
                ),
            })
            .collect::<Vec<_>>();

        let bounds = convert_bounds(
            bounds,
            containing_scope,
            &mut template_names,
            trait_associated_ty_id_map,
            statics_info,
        );

        TraitGeneric {
            loc,
            annot: convert_annot(annot),
            name: failed_name_gen.convert_name(name),
            templates,
            bounds,
            consts,
            functions,
            types,
            base_target: backend_id,
        }
    } else {
        // user wrote bad code trying to multiplatform-ify type aliases
        // evil code even
        todo!("handle when trait when the no-multiplatform rule is broken")
    }
}

fn convert_data<'src>(
    merged: Merged<ASTData<'src>>,

    backend_infos: &HashMap<BackendId, BackendInfo>,
    failed_name_gen: &mut FailedNameGenerator,
    trait_associated_ty_id_map: &TraitAssociatedTyIdMap<'src>,
    statics_info: &mut StaticsInfo<'_, 'src>,
) -> Data {
    todo!()
}
fn convert_const<'src>(
    merged: Merged<ASTConst<'src>>,

    backend_infos: &HashMap<BackendId, BackendInfo>,
    failed_name_gen: &mut FailedNameGenerator,
    trait_associated_ty_id_map: &TraitAssociatedTyIdMap<'src>,
    statics_info: &mut StaticsInfo<'_, 'src>,
) -> ConstUnsolved<'src> {
    todo!()
}
fn convert_function<'src>(
    merged: Merged<ASTFunction<'src>>,

    backend_infos: &HashMap<BackendId, BackendInfo>,
    failed_name_gen: &mut FailedNameGenerator,
    trait_associated_ty_id_map: &TraitAssociatedTyIdMap<'src>,
    statics_info: &mut StaticsInfo<'_, 'src>,
) -> FunctionUnresolved<'src> {
    todo!()
}

fn verify_merge<'src>(
    statics: MergedStatics<'src>,
    backend_infos: &HashMap<BackendId, BackendInfo>,
    statics_info: &mut StaticsInfo<'_, 'src>,
) -> (UnresolvedStatics<'src>, ASTFreeImpl<'src>) {
    let mut failed_name_gen = FailedNameGenerator::new();

    let trait_associated_ty_id_map = gen_trait_associated_type_map(&statics.traits);

    let typealiases = verify_merge_type_aliases(
        statics.typealiases,
        backend_infos,
        &mut failed_name_gen,
        &trait_associated_ty_id_map,
        statics_info,
    );

    let traits = statics
        .traits
        .into_iter()
        .map(|merged| {
            convert_trait(
                merged,
                backend_infos,
                &mut failed_name_gen,
                &trait_associated_ty_id_map,
                statics_info,
            )
        })
        .collect::<Vec<_>>();
    let datas = statics
        .datas
        .into_iter()
        .map(|merged| {
            convert_data(
                merged,
                backend_infos,
                &mut failed_name_gen,
                &trait_associated_ty_id_map,
                statics_info,
            )
        })
        .collect::<Vec<_>>();
    let consts = statics
        .consts
        .into_iter()
        .map(|merged| {
            convert_const(
                merged,
                backend_infos,
                &mut failed_name_gen,
                &trait_associated_ty_id_map,
                statics_info,
            )
        })
        .collect::<Vec<_>>();
    let functions = statics
        .functions
        .into_iter()
        .map(|merged| {
            convert_function(
                merged,
                backend_infos,
                &mut failed_name_gen,
                &trait_associated_ty_id_map,
                statics_info,
            )
        })
        .collect::<Vec<_>>();

    todo!()
}
