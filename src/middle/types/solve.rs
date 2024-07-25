use std::collections::HashMap;

use crate::{
    front::ast::{ASTBlock, ASTType},
    lint::diagnostic::Fallible,
    middle::{
        core::Core,
        statics::{
            scopes::ScopeId,
            verify_merge::{TemplateNames, TemplateNamesRef, TraitAssociatedTyIdMap},
            FunctionGeneric, FunctionVariant, UnresolvedStatics,
        },
        types::convert_type_datalike,
    },
};

use super::{solve_data::SolvingBlock, StaticsInfo, TypeDatalike};

struct TypeSolvingFunctionBody<'a> {
    template_names_variant: Option<TemplateNames<'a>>,
    body: SolvingBlock<'a>,
}

struct PrepareContext<'a, 'src> {
    scope: ScopeId,
    templates: &'a TemplateNamesRef<'src>,
    self_ty: &'a Option<TypeDatalike>,
    trait_associated_ty_id_map: &'a TraitAssociatedTyIdMap<'src>,
    statics_info: StaticsInfo<'a, 'src>,

    ty_locals: Vec<Option<TypeDatalike>>,
    locals_lookup: Vec<HashMap<&'a str, usize>>,

    core: &'a Core,
}
impl<'a, 'src> PrepareContext<'a, 'src> {
    fn convert_ty(&mut self, ty: ASTType<'src>) -> Fallible<TypeDatalike> {
        convert_type_datalike(
            ty,
            self.scope,
            self.templates,
            self.self_ty.as_ref(),
            self.trait_associated_ty_id_map,
            &mut self.statics_info,
        )
    }
    fn prepare_function_variant(
        &mut self,
        variant: FunctionVariant<(ASTBlock<'a>, Option<TemplateNames<'a>>)>,
    ) -> FunctionVariant<TypeSolvingFunctionBody<'a>> {
        todo!()
    }

    fn prepare_for_solver(
        &mut self,
        statics: &mut UnresolvedStatics<'a>,
    ) -> Vec<FunctionGeneric<Option<TypeDatalike>, TypeSolvingFunctionBody<'a>>> {
        todo!()
    }
}
