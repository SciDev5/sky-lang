use std::collections::HashMap;

use crate::{
    back::{BackendId, BackendInfo},
    front::{
        ast::{
            ASTAnnot, ASTConst, ASTData, ASTDataContents, ASTDataProperty, ASTDestructure,
            ASTEnumVariantType, ASTFreeImpl, ASTFunction, ASTImpl, ASTImplContents, ASTName,
            ASTTemplateBound, ASTTemplates, ASTTrait, ASTType, ASTTypeAlias, ASTTypedDestructure,
        },
        source::{HasLoc, Loc},
    },
    lint::diagnostic::{DiagnosticContent, Diagnostics, Fallible, ToDiagnostic},
    middle::{
        statics::{
            ConstGeneric, DataEnumVariant, DataEnumVariantContent, FunctionGeneric, Templates,
            Trait,
        },
        types::{
            convert_type_datalike, convert_type_traitlike, eq_type_datalike, eq_type_traitlike,
            StaticsInfo, TypeData, TypeDatalike, TypeDtatlikeAliased, TypeTemplate, TypeTraitlike,
        },
    },
    modularity::Id,
};

use super::{
    merge::{Merged, MergedStatics},
    scopes::ScopeId,
    Annot, ConstUnresolved, Data, DataProperty, DataVariant, DataVariantContent, Destructure,
    FunctionUnresolved, FunctionVariant, ImplData, ImplTrait, Name, TraitConst, TraitFunction,
    TraitTypeAlias, TypeAlias, UnresolvedStatics,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StaticVerifyDiagnostic {
    ConstInitializerMissing,
    TraitFunctionArgTyMissing,
    TraitFunctionReturnTypeMissing,
    SiblingNotInBackendTree,
    MissingConcreteImpl,
    PropertyTyMissing,
    MismatchedCrossPlatTemplates,
    MismatchedCrossPlatTemplateBounds,
    ChildVariantHasDoc,
    ChildVariantAlsoImplemented,
    RequiredCompatNotImplemented(BackendId),
    MissingReturnTyOnCrossplat,
    ChildVariantFunctionExtraArgs,
    ChildVariantFunctionMissingArgs,
    ChildVariantFunctionMismatchArgs,
    ImplTraitFnNotInTrait,
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
    fn convert_name<'src>(&mut self, name: Fallible<ASTName<'src>>) -> Name {
        name.map(|name| name.into()).unwrap_or_else(|_| Name {
            value: self.gen(),
            loc: Loc::INVALID,
        })
    }
}

pub type TemplateNamesRef<'src> = [(&'src str, Loc, Vec<TypeTraitlike>)];
pub type TemplateNames<'src> = Vec<(&'src str, Loc, Vec<TypeTraitlike>)>;

fn verify_merge_type_aliases<'src>(
    typealiases: Vec<Merged<ASTTypeAlias<'src>>>,
    compat_spec: &CompatSpec,
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
                compat_spec,
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
        .map(|template: &ASTTemplateBound| (template.name.value, template.loc, Vec::new()))
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
                            template.name.into(),
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
    for ((_, _, bounds_to), (_, bounds_from)) in template_names.iter_mut().zip(templates.def.iter())
    {
        *bounds_to = bounds_from.clone();
    }
    templates
}

fn convert_typealias<'src>(
    self_id: usize,
    merged: Merged<ASTTypeAlias<'src>>,

    compat_spec: &CompatSpec,
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
                    .values()
                    .next()
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
            name: name.into(),
        },
        ASTDestructure::Paren { inner, .. } => convert_destructure(*inner?)?,
        ASTDestructure::Tuple { .. } => todo!("complex destructures"),
    })
}

pub struct CompatSpec {
    /// A list of all backends that the program must be able to compile (and so have no abstract stuff at that level).
    pub compat: Vec<BackendId>,
    pub backend_infos: HashMap<BackendId, BackendInfo>,
}
impl CompatSpec {
    /// Generates a HashMap containing the ids and infos of all backends in self.compat who are children base_backend_id
    fn gen_unmatched(&self, base_backend_id: BackendId) -> HashMap<BackendId, &BackendInfo> {
        self.compat
            .iter()
            .flat_map(|backend_id| {
                if let Some(info) = self.backend_infos.get(backend_id) {
                    if info.compat_ids.binary_search(&base_backend_id).is_ok() {
                        return Some((*backend_id, info));
                    }
                }
                None
            })
            .collect::<HashMap<_, _>>()
    }
}
struct BackendHierarchy {
    backend_id: usize,
    loc: Loc,
    children: Vec<BackendHierarchy>,
}
fn gen_backend_hierarchy(
    backends: impl Iterator<Item = (BackendId, Loc)>,
    compat_spec: &CompatSpec,
    diagnostics: &mut Diagnostics,
) -> BackendHierarchy {
    let mut backends = backends.collect::<Vec<_>>();
    backends.sort_unstable_by_key(|(id, _)| *id); // ascending order of ids, eg. abstractmost first

    let mut tree = BackendHierarchy {
        backend_id: backends[0].0,
        loc: backends[0].1,
        children: Vec::new(),
    };
    for (backend_id, loc) in backends.into_iter().skip(1) {
        let backend_info = compat_spec
            .backend_infos
            .get(&backend_id)
            .expect("BackendInfo not loaded for all utilized backends");

        let i0 = backend_info
            .compat_ids
            .iter()
            .enumerate()
            .find(|(_, compat_id)| tree.backend_id == **compat_id)
            .map(|(i, _)| i);
        if let Some(i0) = i0 {
            let mut current = &mut tree;

            for i in i0 + 1..backend_info.compat_ids.len() {
                if let Some(next_i) = current
                    .children
                    .iter()
                    .enumerate()
                    .find(|(_, it)| it.backend_id == backend_info.compat_ids[i])
                    .map(|(i, _)| i)
                {
                    current = &mut current.children[next_i];
                }
            }
            current.children.push(BackendHierarchy {
                backend_id,
                loc,
                children: Vec::new(),
            });
        } else {
            diagnostics.raise(StaticVerifyDiagnostic::SiblingNotInBackendTree, loc);
        }
    }
    tree
}

fn verify_annot(annot: ASTAnnot, loc: Loc, diagnostics: &mut Diagnostics) {
    if !annot.doc.is_empty() {
        diagnostics.raise(StaticVerifyDiagnostic::ChildVariantHasDoc, loc);
    }
}
fn verify_crossplatform_templates<'src>(
    templates_variant: ASTTemplates<'src>,
    templates_base: &Templates,

    containing_scope: ScopeId,
    self_ty: Option<&TypeDatalike>,

    trait_associated_ty_id_map: &TraitAssociatedTyIdMap<'src>,
    typealiases: &Vec<TypeAlias>,
    statics_info: &mut StaticsInfo<'_, 'src>,
) -> Option<TemplateNames<'src>> {
    templates_variant.and_then(|templates_variant| {
        if templates_variant.0.len() != templates_base.def.len() {
            statics_info.diagnostics.raise(
                StaticVerifyDiagnostic::MismatchedCrossPlatTemplates,
                templates_variant.loc(),
            );
            return None;
        }

        let templates_variant = Some(templates_variant);
        let mut template_names = gen_template_names(&templates_variant);
        let templates_variant = convert_templates(
            templates_variant,
            containing_scope,
            &mut template_names,
            self_ty,
            trait_associated_ty_id_map,
            statics_info,
        );

        for ((name_variant, bounds_variant), (_, bounds_base)) in
            templates_variant.def.iter().zip(templates_base.def.iter())
        {
            let all_match = bounds_base.len() == bounds_variant.len()
                && bounds_variant.iter().zip(bounds_base.iter()).all(
                    |(bound_variant, bound_base)| {
                        eq_type_traitlike(bound_base, bound_variant, typealiases)
                    },
                );
            if !all_match {
                statics_info.diagnostics.raise(
                    StaticVerifyDiagnostic::MismatchedCrossPlatTemplateBounds,
                    name_variant.loc,
                );
            }
        }

        Some(template_names)
    })
}

fn verify_variants<T: HasLoc, R>(
    mut variants_in: HashMap<BackendId, T>,
    hierarchy: BackendHierarchy,
    loc_base: Loc,
    compat_spec: &CompatSpec,
    is_concrete: impl Fn(T) -> Option<R>,
    diagnostics: &mut Diagnostics,
) -> HashMap<BackendId, R> {
    let mut variants = HashMap::new();

    let mut unmatched_compat = compat_spec.gen_unmatched(hierarchy.backend_id);
    let mut variants_errant_double_implemented = Vec::new();
    let mut hierarchies_to_process = vec![hierarchy];
    while let Some(hierarchy) = hierarchies_to_process.pop() {
        let variant_in = variants_in.remove(&hierarchy.backend_id).unwrap();

        if let Some(variant) = is_concrete(variant_in) {
            // is concrete
            variants.insert(hierarchy.backend_id, variant);

            // mark backends that we have implemented over as ok by removing them from "unmatched"
            unmatched_compat
                .retain(|_, v| v.compat_ids.binary_search(&hierarchy.backend_id).is_err());
            // these will necesarrily be over-implementing this type and must be marked as errant and ignored
            variants_errant_double_implemented.extend(hierarchy.children.into_iter());
        } else {
            // is abstract

            // process children
            hierarchies_to_process.extend(hierarchy.children.into_iter());
        }
    }
    while let Some(variant_errant) = variants_errant_double_implemented.pop() {
        diagnostics.raise(
            StaticVerifyDiagnostic::ChildVariantAlsoImplemented,
            variants_in.get(&variant_errant.backend_id).unwrap().loc(),
        );
        variants_errant_double_implemented.extend(variant_errant.children.into_iter())
    }
    for (backend_id_unmatched, _) in unmatched_compat {
        diagnostics.raise(
            StaticVerifyDiagnostic::RequiredCompatNotImplemented(backend_id_unmatched),
            loc_base,
        );
    }

    variants
}

fn convert_trait<'src>(
    merged: Merged<ASTTrait<'src>>,
    self_id: usize,

    register_function: &mut impl FnMut(FunctionUnresolved<'src>) -> usize,

    failed_name_gen: &mut FailedNameGenerator,
    trait_associated_ty_id_map: &TraitAssociatedTyIdMap<'src>,
    statics_info: &mut StaticsInfo<'_, 'src>,
) -> Trait {
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
        let self_bound_ty = TypeTraitlike {
            constrained_associated: HashMap::new(),
            loc,
            id: Id::Local { id: self_id },
            templates: template_names
                .iter()
                .enumerate()
                .map(|(id, (_, loc, _))| TypeDatalike::Template(TypeTemplate { loc: *loc, id }))
                .collect(),
        };
        let functions = functions
            .into_iter()
            .map(|ast| {
                let mut template_names = template_names.clone();
                let self_template_id = template_names.len();
                let fn_self_ty = TypeDatalike::Template(TypeTemplate {
                    loc: Loc::INVALID,
                    id: self_template_id,
                });
                template_names.push(("", loc, vec![self_bound_ty.clone()]));
                template_names.extend(gen_template_names(&ast.templates).into_iter());
                let templates = convert_templates(
                    ast.templates,
                    containing_scope,
                    &mut template_names,
                    Some(&fn_self_ty),
                    trait_associated_ty_id_map,
                    statics_info,
                );

                let (args, ty_args) = convert_args(
                    ast.args,
                    containing_scope,
                    &template_names,
                    Some(&fn_self_ty),
                    trait_associated_ty_id_map,
                    statics_info,
                );
                let loc = ast.loc;
                let name = failed_name_gen.convert_name(ast.name);
                let annot = convert_annot(ast.annot);
                let ty_return = ast
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
                            Some(&fn_self_ty),
                            trait_associated_ty_id_map,
                            statics_info,
                        )
                    });

                let dedault_impl = ast.block.map(|block| {
                    register_function(FunctionGeneric {
                        annot: annot.clone(),
                        base_target: backend_id,
                        name: name.clone(),
                        templates: templates.clone(),
                        ty_args: ty_args.clone(),
                        ty_return: ty_return.clone().map(|v| Some(v)),
                        ty_self: Some(fn_self_ty),
                        variants: [(
                            backend_id,
                            FunctionVariant {
                                loc,
                                args: args.clone(),
                                body: (block, Some(template_names)),
                            },
                        )]
                        .into_iter()
                        .collect::<HashMap<_, _>>(),
                    })
                });

                TraitFunction {
                    dedault_impl,
                    annot,
                    loc,
                    name,
                    templates,
                    args,
                    ty_args,
                    ty_return,
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

        Trait {
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
    self_id: usize,

    freed_impls: &mut Vec<FreedDataImpl<'src>>,

    compat_spec: &CompatSpec,
    failed_name_gen: &mut FailedNameGenerator,
    trait_associated_ty_id_map: &TraitAssociatedTyIdMap<'src>,
    typealiases: &Vec<TypeAlias>,
    statics_info: &mut StaticsInfo<'_, 'src>,
) -> Data {
    let hierarchy = gen_backend_hierarchy(
        merged
            .contents
            .iter()
            .map(|(backend_id, data_)| (*backend_id, data_.loc)),
        compat_spec,
        statics_info.diagnostics,
    );
    let base = merged.contents.get(&hierarchy.backend_id).unwrap();
    let annot = convert_annot(base.annot.clone());
    let name = failed_name_gen.convert_name(base.name);
    let base_target = hierarchy.backend_id;
    let loc_base = base.loc;

    let mut template_names = gen_template_names(&base.templates);
    let self_ty = TypeDatalike::Data(TypeData {
        loc: Loc::INVALID,
        id: Id::Local { id: self_id },
        templates: (0..template_names.len())
            .map(|id| {
                Ok(TypeDatalike::Template(TypeTemplate {
                    id,
                    loc: Loc::INVALID,
                }))
            })
            .collect(),
    });
    let templates = convert_templates(
        base.templates.clone(),
        base.containing_scope,
        &mut template_names,
        Some(&self_ty),
        trait_associated_ty_id_map,
        statics_info,
    );

    let variants = {
        let variants_in = merged
            .contents
            .into_iter()
            .map(|(backend_id, variant)| {
                let template_names_variant = if backend_id != base_target {
                    verify_annot(variant.annot, variant.loc, statics_info.diagnostics);
                    verify_crossplatform_templates(
                        variant.templates,
                        &templates,
                        variant.containing_scope,
                        Some(&self_ty),
                        trait_associated_ty_id_map,
                        typealiases,
                        statics_info,
                    )
                } else {
                    // already processed, dont need to do it again
                    None
                };

                for attatched_impl in variant.attatched_impls {
                    let freed_impl = FreedDataImpl {
                        templates: Templates {
                            def: match &template_names_variant {
                                Some(tnv) => templates
                                    .def
                                    .iter()
                                    .cloned()
                                    .zip(tnv.iter())
                                    .map(|((_, bounds), (new_name, new_loc, _))| {
                                        (
                                            Name {
                                                loc: *new_loc,
                                                value: new_name.to_string(),
                                            },
                                            bounds,
                                        )
                                    })
                                    .collect(),
                                None => templates.def.clone(),
                            },
                        },
                        attatched_impl,
                        containing_scope: variant.containing_scope,
                        target: self_ty.clone(),
                    };
                    freed_impls.push(freed_impl);
                }

                let contents = convert_datavariant(
                    variant.contents,
                    variant.containing_scope,
                    variant.loc,
                    Some(&self_ty),
                    template_names_variant.as_ref().unwrap_or(&template_names),
                    trait_associated_ty_id_map,
                    statics_info,
                );

                (backend_id, contents)
            })
            .collect::<HashMap<_, _>>();

        verify_variants(
            variants_in,
            hierarchy,
            loc_base,
            compat_spec,
            |t| match &t.content {
                DataVariantContent::Abstract => None,
                _ => Some(t),
            },
            statics_info.diagnostics,
        )
    };

    Data {
        annot,
        name,
        templates,
        base_target,
        variants,
    }
}

fn convert_dataproperty<'src>(
    ASTDataProperty {
        loc,
        annot,
        name,
        ty,
    }: ASTDataProperty<'src>,

    containing_scope: ScopeId,
    self_ty: Option<&TypeDatalike>,
    template_names: &TemplateNames,

    trait_associated_ty_id_map: &TraitAssociatedTyIdMap<'src>,
    statics_info: &mut StaticsInfo<'_, 'src>,
) -> DataProperty {
    DataProperty {
        loc,
        annot: convert_annot(annot),
        name: name.into(),
        ty: ty
            .ok_or_else(|| {
                statics_info
                    .diagnostics
                    .raise(StaticVerifyDiagnostic::PropertyTyMissing, loc)
            })
            .and_then(|v| v)
            .and_then(|ty| {
                convert_type_datalike(
                    ty,
                    containing_scope,
                    &template_names,
                    self_ty,
                    trait_associated_ty_id_map,
                    statics_info,
                )
            }),
    }
}
fn convert_datavariant<'src>(
    contents: ASTDataContents<'src>,

    containing_scope: ScopeId,
    base_loc: Loc,
    self_ty: Option<&TypeDatalike>,
    template_names: &TemplateNames,

    trait_associated_ty_id_map: &TraitAssociatedTyIdMap<'src>,
    statics_info: &mut StaticsInfo<'_, 'src>,
) -> DataVariant {
    DataVariant {
        loc: base_loc,
        content: match contents {
            ASTDataContents::Abstract => {
                statics_info
                    .diagnostics
                    .raise(StaticVerifyDiagnostic::MissingConcreteImpl, base_loc);
                DataVariantContent::Abstract
            }
            ASTDataContents::Unit => DataVariantContent::Unit,
            ASTDataContents::Struct { properties } => DataVariantContent::Struct {
                properties: properties
                    .into_iter()
                    .map(|property| {
                        convert_dataproperty(
                            property,
                            containing_scope,
                            self_ty,
                            &template_names,
                            trait_associated_ty_id_map,
                            statics_info,
                        )
                    })
                    .collect(),
            },
            ASTDataContents::Tuple { properties } => DataVariantContent::Tuple {
                properties: properties
                    .into_iter()
                    .map(|p| {
                        p.and_then(|ty| {
                            convert_type_datalike(
                                ty,
                                containing_scope,
                                &template_names,
                                self_ty,
                                trait_associated_ty_id_map,
                                statics_info,
                            )
                        })
                    })
                    .collect(),
            },
            ASTDataContents::Enum { variants } => DataVariantContent::Enum {
                variants: variants
                    .into_iter()
                    .map(|v| DataEnumVariant {
                        loc: v.loc,
                        annot: convert_annot(v.annot),
                        name: v.name.into(),
                        contents: match v.contents {
                            ASTEnumVariantType::Unit => DataEnumVariantContent::Unit,
                            ASTEnumVariantType::Struct { properties } => {
                                DataEnumVariantContent::Struct {
                                    properties: properties
                                        .into_iter()
                                        .map(|property| {
                                            convert_dataproperty(
                                                property,
                                                containing_scope,
                                                self_ty,
                                                &template_names,
                                                trait_associated_ty_id_map,
                                                statics_info,
                                            )
                                        })
                                        .collect(),
                                }
                            }
                            ASTEnumVariantType::Tuple { properties } => {
                                DataEnumVariantContent::Tuple {
                                    properties: properties
                                        .into_iter()
                                        .map(|p| {
                                            p.and_then(|ty| {
                                                convert_type_datalike(
                                                    ty,
                                                    containing_scope,
                                                    &template_names,
                                                    self_ty,
                                                    trait_associated_ty_id_map,
                                                    statics_info,
                                                )
                                            })
                                        })
                                        .collect(),
                                }
                            }
                        },
                    })
                    .collect(),
            },
        },
    }
}

fn convert_const<'src>(
    merged: Merged<ASTConst<'src>>,

    compat_spec: &CompatSpec,
    failed_name_gen: &mut FailedNameGenerator,
    trait_associated_ty_id_map: &TraitAssociatedTyIdMap<'src>,
    statics_info: &mut StaticsInfo<'_, 'src>,
) -> ConstUnresolved<'src> {
    let hierarchy = gen_backend_hierarchy(
        merged
            .contents
            .iter()
            .map(|(backend_id, const_)| (*backend_id, const_.loc)),
        compat_spec,
        statics_info.diagnostics,
    );
    let base = merged.contents.get(&hierarchy.backend_id).unwrap();

    let ty = base.ty.clone().map(|it| {
        it.and_then(|ty| {
            convert_type_datalike(
                ty,
                base.containing_scope,
                &[],
                None,
                trait_associated_ty_id_map,
                statics_info,
            )
        })
    });

    if hierarchy.children.is_empty() {
        return ConstGeneric {
            annot: convert_annot(base.annot.clone()),
            name: failed_name_gen.convert_name(base.name),
            base_target: hierarchy.backend_id,
            ty: match ty {
                Some(Ok(v)) => Ok(Some(v)),
                Some(Err(err)) => Err(err),
                None => Ok(None),
            },
            initializer: [(
                hierarchy.backend_id,
                base.value
                    .clone()
                    .ok_or_else(|| {
                        statics_info
                            .diagnostics
                            .raise(StaticVerifyDiagnostic::ConstInitializerMissing, base.loc)
                    })
                    .and_then(|it| it)
                    .map(|expr| (expr, base.containing_scope, base.loc)),
            )]
            .into_iter()
            .collect::<HashMap<_, _>>(),
        };
    } else {
        todo!("multiplatform consts")
    }
}
fn convert_function<'src>(
    merged: Merged<ASTFunction<'src>>,

    self_ty: Option<&TypeDatalike>,

    compat_spec: &CompatSpec,
    failed_name_gen: &mut FailedNameGenerator,
    trait_associated_ty_id_map: &TraitAssociatedTyIdMap<'src>,
    typealiases: &Vec<TypeAlias>,
    statics_info: &mut StaticsInfo<'_, 'src>,
) -> FunctionUnresolved<'src> {
    let hierarchy = gen_backend_hierarchy(
        merged
            .contents
            .iter()
            .map(|(backend_id, function_)| (*backend_id, function_.loc)),
        compat_spec,
        statics_info.diagnostics,
    );
    let base = merged.contents.get(&hierarchy.backend_id).unwrap();
    let annot = convert_annot(base.annot.clone());
    let name = failed_name_gen.convert_name(base.name);
    let base_target = hierarchy.backend_id;
    let loc_base = base.loc;

    let mut template_names = gen_template_names(&base.templates);
    let templates = convert_templates(
        base.templates.clone(),
        base.containing_scope,
        &mut template_names,
        self_ty,
        trait_associated_ty_id_map,
        statics_info,
    );

    let (destruct_args, ty_args_base): (Vec<_>, Vec<_>) = convert_args(
        base.args.clone(),
        base.containing_scope,
        &template_names,
        self_ty,
        trait_associated_ty_id_map,
        statics_info,
    );
    let mut destruct_args_base = Some(destruct_args);

    let ty_return = match base.ty_return.clone() {
        Some(Ok(ty_return)) => convert_type_datalike(
            ty_return,
            base.containing_scope,
            &template_names,
            self_ty,
            trait_associated_ty_id_map,
            statics_info,
        )
        .map(|ty| Some(ty)),
        None => {
            if hierarchy.children.is_empty() {
                Ok(None)
            } else {
                Err(statics_info
                    .diagnostics
                    .raise(StaticVerifyDiagnostic::MissingReturnTyOnCrossplat, loc_base))
            }
        }
        Some(Err(err)) => Err(err),
    };

    let variants = {
        let variants_in = merged
            .contents
            .into_iter()
            .map(|(backend_id, variant)| {
                if backend_id == base_target {
                    let args = destruct_args_base.take().unwrap();

                    (backend_id, ((args, variant.block, None), variant.loc))
                } else {
                    verify_annot(variant.annot, variant.loc, statics_info.diagnostics);
                    let template_names_variant = verify_crossplatform_templates(
                        variant.templates,
                        &templates,
                        variant.containing_scope,
                        self_ty,
                        trait_associated_ty_id_map,
                        typealiases,
                        statics_info,
                    );

                    let (mut args, mut ty_args_variant): (Vec<_>, Vec<_>) = convert_args(
                        variant.args,
                        variant.containing_scope,
                        template_names_variant.as_ref().unwrap_or(&template_names),
                        self_ty,
                        trait_associated_ty_id_map,
                        statics_info,
                    );

                    // handle too many args
                    if ty_args_variant.len() > ty_args_base.len() {
                        statics_info.diagnostics.raise(
                            StaticVerifyDiagnostic::ChildVariantFunctionExtraArgs,
                            variant.loc,
                        );
                        args.drain(ty_args_base.len()..);
                        ty_args_variant.drain(ty_args_base.len()..);
                    }
                    // handle too few args
                    if ty_args_variant.len() < ty_args_base.len() {
                        let diagnostic = statics_info.diagnostics.raise(
                            StaticVerifyDiagnostic::ChildVariantFunctionMissingArgs,
                            variant.loc,
                        );
                        let n_missing = ty_args_base.len() - ty_args_variant.len();
                        args.extend(
                            std::iter::repeat(())
                                .take(n_missing)
                                .map(|_| Err(diagnostic)),
                        );
                        ty_args_variant.extend(
                            std::iter::repeat(())
                                .take(n_missing)
                                .map(|_| Err(diagnostic)),
                        );
                    }

                    for (ty_arg_base, ty_arg_variant) in
                        ty_args_base.iter().zip(ty_args_variant.iter_mut())
                    {
                        match (ty_arg_base, ty_arg_variant.as_mut()) {
                            (Ok(ty_arg_base), Ok(ty_arg_variant_)) => {
                                if !eq_type_datalike(ty_arg_base, ty_arg_variant_, typealiases) {
                                    let loc_arg_variant = ty_arg_variant_.loc();

                                    *ty_arg_variant = Err(statics_info.diagnostics.raise(
                                        StaticVerifyDiagnostic::ChildVariantFunctionMismatchArgs,
                                        loc_arg_variant,
                                    ));
                                }
                            }
                            _ => {
                                // can't compare, errors for them have already been raised, assume they're fine
                            }
                        }
                    }

                    (
                        backend_id,
                        ((args, variant.block, template_names_variant), variant.loc),
                    )
                }
            })
            .collect::<HashMap<_, _>>();

        verify_variants(
            variants_in,
            hierarchy,
            loc_base,
            compat_spec,
            |((args, body, template_names), loc)| {
                Some(FunctionVariant {
                    loc,
                    args,
                    body: (body?, template_names),
                })
            },
            statics_info.diagnostics,
        )
    };

    FunctionGeneric {
        annot,
        name,
        base_target,
        templates,

        ty_self: self_ty.cloned(),

        ty_args: ty_args_base,
        ty_return,
        variants,
    }
}

fn convert_args<'src>(
    args: Vec<Fallible<ASTTypedDestructure<'src>>>,

    containing_scope: BackendId,
    template_names: &TemplateNames,
    self_ty: Option<&TypeDatalike>,
    trait_associated_ty_id_map: &TraitAssociatedTyIdMap<'src>,
    statics_info: &mut StaticsInfo<'_, 'src>,
) -> (Vec<Fallible<Destructure>>, Vec<Fallible<TypeDatalike>>) {
    args.into_iter()
        .map(|arg| match arg {
            Ok(arg) => {
                let loc_arg = arg.destructure.loc();
                (
                    convert_destructure(arg.destructure),
                    arg.ty
                        .ok_or_else(|| {
                            statics_info
                                .diagnostics
                                .raise(StaticVerifyDiagnostic::TraitFunctionArgTyMissing, loc_arg)
                        })
                        .and_then(|v| v)
                        .and_then(|ty| {
                            convert_type_datalike(
                                ty,
                                containing_scope,
                                &template_names,
                                self_ty,
                                trait_associated_ty_id_map,
                                statics_info,
                            )
                        }),
                )
            }
            Err(err) => (Err(err), Err(err)),
        })
        .unzip()
}

fn convert_impls<'src>(
    free_impls: Vec<ASTFreeImpl<'src>>,
    freed_databound_impls: Vec<FreedDataImpl<'src>>,

    register_function: &mut impl FnMut(FunctionUnresolved<'src>) -> usize,
    register_const: &mut impl FnMut(ConstUnresolved<'src>) -> usize,
    register_typealias: &mut impl FnMut(TypeAlias) -> usize,
    traits: &Vec<Trait>,

    compat_spec: &CompatSpec,
    failed_name_gen: &mut FailedNameGenerator,
    trait_associated_ty_id_map: &TraitAssociatedTyIdMap<'src>,
    typealiases: &Vec<TypeAlias>,
    statics_info: &mut StaticsInfo<'_, 'src>,
) -> (Vec<ImplData>, Vec<ImplTrait>) {
    let impls_in = unify_impls(
        free_impls,
        freed_databound_impls,
        trait_associated_ty_id_map,
        statics_info,
    );

    let mut impls_data = Vec::new();
    let mut impls_trait = Vec::new();
    for UnifiedImpl {
        loc,
        containing_scope,
        templates,
        target,
        target_trait,
        contents:
            ASTImplContents {
                loc: _,
                functions,
                consts,
                types,
            },
    } in impls_in
    {
        let template_names = templates
            .def
            .iter()
            .map(|(name, bounds)| (name.value.as_str(), name.loc, bounds.clone()))
            .collect::<Vec<_>>();
        let target_trait = target_trait.map(|ty| {
            convert_type_traitlike(
                ty,
                containing_scope,
                &template_names,
                target.as_ref().ok(),
                trait_associated_ty_id_map,
                statics_info,
            )
        });

        if consts.len() > 0 || types.len() > 0 {
            todo!("actually convert/verify impls");
        }

        let functions_converted = functions
            .into_iter()
            .map(|fn_| {
                let backend_id = statics_info
                    .scopes
                    .find_map(containing_scope, |f| Some(f.backend_id))
                    .expect("todo! handle empty scope");
                // todo! allow argument type omission in trait impls
                // todo! merge outer impl templates into function templates
                let fn_ = convert_function(
                    Merged {
                        contents: [(backend_id, fn_)].into_iter().collect(),
                    },
                    target.as_ref().ok(),
                    compat_spec,
                    failed_name_gen,
                    trait_associated_ty_id_map,
                    typealiases,
                    statics_info,
                );
                (
                    fn_.name.clone(),
                    fn_.ty_args.clone(),
                    fn_.ty_return.clone(),
                    register_function(fn_),
                )
            })
            .collect::<Vec<_>>();

        let Ok(target) = target else {
            continue;
        };
        if let Some(target_trait) = target_trait {
            let Ok(target_trait) = target_trait else {
                continue;
            };
            let target_trait_ref = match target_trait.id {
                Id::Local { id } => &traits[id],
                Id::Dependency { package_id, id } => todo!("lookup traits from other packages"),
            };
            let mut functions = Vec::from_iter(
                std::iter::repeat_with(|| None).take(target_trait_ref.functions.len()),
            );
            for (name, ty_args, ty_return, id) in functions_converted {
                if let Some((trait_fn_i, trait_fn)) = target_trait_ref
                    .functions
                    .iter()
                    .enumerate()
                    .find(|(_, trait_fn)| trait_fn.name.value == name.value)
                {
                    dbg!("todo! check trait types");

                    functions[trait_fn_i] = Some(id);
                } else {
                    statics_info
                        .diagnostics
                        .raise(StaticVerifyDiagnostic::ImplTraitFnNotInTrait, name.loc);
                }
            }
            dbg!("todo! check all required trait functions implemented");
            impls_trait.push(ImplTrait {
                templates,
                target_data: target,
                target_trait,
                functions,
                consts: Vec::new(),
                typealiases: Vec::new(),
            })
        } else {
            impls_data.push(ImplData {
                templates,
                target,
                functions: functions_converted
                    .into_iter()
                    .map(|(name, ty_args, ty_return, id)| todo!("impl data"))
                    .collect(),
                consts: HashMap::new(),
                typealiases: HashMap::new(),
            })
        }
    }

    (impls_data, impls_trait)
}

struct FreedDataImpl<'src> {
    templates: Templates,
    containing_scope: ScopeId,
    target: TypeDatalike,
    attatched_impl: ASTImpl<'src>,
}
struct UnifiedImpl<'src> {
    loc: Loc,
    containing_scope: ScopeId,
    templates: Templates,
    target: Fallible<TypeDatalike>,
    target_trait: Option<ASTType<'src>>,
    contents: ASTImplContents<'src>,
}
fn unify_impls<'src>(
    free_impls: Vec<ASTFreeImpl<'src>>,
    freed_databound_impls: Vec<FreedDataImpl<'src>>,

    trait_associated_ty_id_map: &TraitAssociatedTyIdMap<'src>,
    statics_info: &mut StaticsInfo<'_, 'src>,
) -> Vec<UnifiedImpl<'src>> {
    let mut unified_impls = Vec::with_capacity(free_impls.len() + freed_databound_impls.len());
    for impl_ in free_impls {
        let mut template_names = gen_template_names(&impl_.attatched_impl.templates);
        let target = impl_.target.and_then(|ty| {
            convert_type_datalike(
                ty,
                impl_.containing_scope,
                &template_names,
                None,
                trait_associated_ty_id_map,
                statics_info,
            )
        });
        let templates = convert_templates(
            impl_.attatched_impl.templates,
            impl_.containing_scope,
            &mut template_names,
            // Some(&target),
            target.as_ref().ok(), // todo! this could lead to unexpected issues. what else could I even do tho?
            trait_associated_ty_id_map,
            statics_info,
        );

        unified_impls.push(UnifiedImpl {
            loc: impl_.attatched_impl.loc,
            containing_scope: impl_.containing_scope,
            templates,
            target,
            target_trait: impl_.attatched_impl.target_trait,
            contents: impl_.attatched_impl.contents,
        });
    }
    for impl_ in freed_databound_impls {
        // convert templates to from `data<A,B,C... > impl<D,E,F... >` to `impl<A,B,C... D,E,F... >`
        // eg. concat the two template lists together.
        let mut template_names = impl_
            .templates
            .def
            .iter()
            .map(|(name, bounds)| (name.value.as_str(), name.loc, bounds.clone()))
            .chain(gen_template_names(&impl_.attatched_impl.templates).into_iter())
            .collect::<Vec<_>>();
        let templates = convert_templates(
            impl_.attatched_impl.templates,
            impl_.containing_scope,
            &mut template_names,
            Some(&impl_.target),
            trait_associated_ty_id_map,
            statics_info,
        );
        unified_impls.push(UnifiedImpl {
            loc: impl_.attatched_impl.loc,
            containing_scope: impl_.containing_scope,
            templates,
            target: Ok(impl_.target),
            target_trait: impl_.attatched_impl.target_trait,
            contents: impl_.attatched_impl.contents,
        });
    }

    unified_impls
}

pub fn verify_merge<'src>(
    statics: MergedStatics<'src>,
    compat_spec: &CompatSpec,
    mut statics_info: StaticsInfo<'_, 'src>,
) -> UnresolvedStatics<'src> {
    let mut failed_name_gen = FailedNameGenerator::new();

    let trait_associated_ty_id_map = gen_trait_associated_type_map(&statics.traits);

    let mut typealiases = verify_merge_type_aliases(
        statics.typealiases,
        compat_spec,
        &mut failed_name_gen,
        &trait_associated_ty_id_map,
        &mut statics_info,
    );

    let mut freed_databound_impls = Vec::new();

    fn make_extras_registrar<'a, T>(extras: &'a mut Vec<T>) -> impl FnMut(T) -> usize + 'a {
        let initial_n = extras.len();
        move |new| {
            extras.push(new);
            initial_n + extras.len() - 1
        }
    }

    let mut extra_functions = Vec::new();
    let mut extra_consts = Vec::new();
    let mut extra_typealiases = Vec::new();
    let mut register_function = make_extras_registrar(&mut extra_functions);
    let mut register_const = make_extras_registrar(&mut extra_consts);
    let mut register_typealias = make_extras_registrar(&mut extra_typealiases);

    let traits = statics
        .traits
        .into_iter()
        .enumerate()
        .map(|(self_id, merged)| {
            convert_trait(
                merged,
                self_id,
                &mut register_function,
                &mut failed_name_gen,
                &trait_associated_ty_id_map,
                &mut statics_info,
            )
        })
        .collect::<Vec<_>>();
    let datas = statics
        .datas
        .into_iter()
        .enumerate()
        .map(|(self_id, merged)| {
            convert_data(
                merged,
                self_id,
                &mut freed_databound_impls,
                compat_spec,
                &mut failed_name_gen,
                &trait_associated_ty_id_map,
                &typealiases,
                &mut statics_info,
            )
        })
        .collect::<Vec<_>>();
    let mut consts = statics
        .consts
        .into_iter()
        .map(|merged| {
            convert_const(
                merged,
                compat_spec,
                &mut failed_name_gen,
                &trait_associated_ty_id_map,
                &mut statics_info,
            )
        })
        .collect::<Vec<_>>();
    let mut functions = statics
        .functions
        .into_iter()
        .map(|merged| {
            convert_function(
                merged,
                None,
                compat_spec,
                &mut failed_name_gen,
                &trait_associated_ty_id_map,
                &typealiases,
                &mut statics_info,
            )
        })
        .collect::<Vec<_>>();

    let (impls_data, impls_trait) = convert_impls(
        statics.free_impls,
        freed_databound_impls,
        &mut register_function,
        &mut register_const,
        &mut register_typealias,
        &traits,
        compat_spec,
        &mut failed_name_gen,
        &trait_associated_ty_id_map,
        &typealiases,
        &mut statics_info,
    );

    drop((register_function, register_const, register_typealias));
    functions.extend(extra_functions.into_iter());
    consts.extend(extra_consts.into_iter());
    typealiases.extend(extra_typealiases.into_iter());

    UnresolvedStatics {
        consts,
        datas,
        functions,
        traits,
        typealiases,
        impls_data,
        impls_trait,
    }
}
