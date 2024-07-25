use std::collections::HashMap;

use crate::{
    back::{
        common::{BackCommonMacroInfo, Op},
        BackendId,
    },
    front::{
        ast::{ASTMacroInvocation, ASTMacroInvocationBody},
        parse::ParseDiagnostic,
        source::HasLoc,
    },
    lint::diagnostic::{DiagnosticId, Diagnostics},
    macros::MacroSpecEnt,
};

use super::{
    core::Core,
    statics::{verify_merge::CompatSpec, UnresolvedStatics},
};

pub enum AttrMacroProcState<'src> {
    Unhandled(ASTMacroInvocation<'src>),
    OkConsumed,
    OkKeep {
        for_backend: BackendId,
        attr: ASTMacroInvocation<'src>,
    },
    Err(DiagnosticId),
}

#[derive(Debug, Clone, Copy)]
pub enum MacroMidCallLocation {
    Trait { id: usize },
    // todo! other spots where attrs can be
}

pub trait BackendProcessMacros {
    fn process_attrs_mid<'a>(
        &self,
        call: MacroMidCallLocation,
        statics: &mut UnresolvedStatics<'a>,
        attr: &mut AttrMacroProcState<'a>,
    );
}

pub fn process_attrs_mid<'a, 'src>(
    core_gen: Option<(&mut Core, &BackCommonMacroInfo)>,
    compat: &'a CompatSpec,

    statics: &'a mut UnresolvedStatics<'src>,
    attrs: Vec<ASTMacroInvocation<'src>>,
    diagnostics: &'a mut Diagnostics,
) -> Vec<Option<(BackendId, ASTMacroInvocation<'src>)>> {
    let mut attrs = attrs
        .into_iter()
        .map(|v| AttrMacroProcState::Unhandled(v))
        .collect::<Vec<_>>();

    ProcessAttrsMid::process(core_gen, compat, statics, &mut attrs, diagnostics);

    attrs
        .into_iter()
        .map(|attr| match attr {
            AttrMacroProcState::OkConsumed => None,
            AttrMacroProcState::OkKeep { for_backend, attr } => Some((for_backend, attr)),
            AttrMacroProcState::Unhandled(attr) => {
                diagnostics.raise(ParseDiagnostic::NonexistentMacro, attr.loc());
                None
            }
            AttrMacroProcState::Err(_err) => None,
        })
        .collect()
}
struct ProcessAttrsMid<'a, 'src> {
    compat: &'a CompatSpec,
    statics: &'a mut UnresolvedStatics<'src>,
    diagnostics: &'a mut Diagnostics,
}
impl<'a, 'src> ProcessAttrsMid<'a, 'src> {
    fn process(
        core_gen: Option<(&mut Core, &BackCommonMacroInfo)>,
        compat: &'a CompatSpec,
        statics: &'a mut UnresolvedStatics<'src>,
        attrs: &mut Vec<AttrMacroProcState<'src>>,
        diagnostics: &'a mut Diagnostics,
    ) {
        let statics_in = statics.clone();
        let mut state = Self {
            compat,
            diagnostics,
            statics,
        };

        if let Some((core, backcommon_macroinfo)) = core_gen {
            state.process_gen_core(&statics_in, attrs, core, backcommon_macroinfo);
        }

        state.process_traits(&statics_in, attrs);
        // todo! process other statics
    }

    fn process_gen_core(
        &mut self,
        statics_in: &UnresolvedStatics<'src>,
        attrs: &mut Vec<AttrMacroProcState<'src>>,
        core: &mut Core,
        backcommon_macroinfo: &BackCommonMacroInfo,
    ) {
        let backcommon = self
            .compat
            .backend_infos
            .get(&0)
            .expect("common platform should always exist");

        for (id, trait_) in statics_in.traits.iter().enumerate() {
            for (_backend_id, attr_ids) in &trait_.annot.attrs {
                for attr_id in attr_ids {
                    let attr_state = &mut attrs[*attr_id];

                    let attr = match attr_state {
                        AttrMacroProcState::Unhandled(attr) => attr,
                        _ => continue,
                    };

                    let macro_bind_op_trait =
                        &backcommon.macro_specs[backcommon_macroinfo.bind_op_trait.0];
                    if attr.name.value == macro_bind_op_trait.name.as_str() {
                        if let Some((variant, spec)) = macro_bind_op_trait
                            .variants
                            .iter()
                            .enumerate()
                            .find(|(_, spec)| match_macro_ent(&attr.body, spec))
                        {
                            let attr_name = match &attr.body {
                                ASTMacroInvocationBody::Name { name } => name.value,
                                _ => unreachable!(),
                            };
                            let enum_val = spec
                                .unwrap_enum()
                                .iter()
                                .enumerate()
                                .find(|(_, enum_val)| enum_val.as_str() == attr_name)
                                .unwrap()
                                .0;
                            match backcommon_macroinfo.bind_op_trait.1(variant, enum_val) {
                                Op::In(op) => {
                                    core.op_traits
                                        .infix
                                        .entry(op)
                                        .and_modify(|_| {
                                            self.diagnostics.raise(
                                                ParseDiagnostic::IllegalCoreOverride,
                                                attr.loc,
                                            );
                                        })
                                        .or_insert(id);
                                }
                                Op::Pre(op) => {
                                    core.op_traits
                                        .prefix
                                        .entry(op)
                                        .and_modify(|_| {
                                            self.diagnostics.raise(
                                                ParseDiagnostic::IllegalCoreOverride,
                                                attr.loc,
                                            );
                                        })
                                        .or_insert(id);
                                }
                                Op::Post(op) => {
                                    core.op_traits
                                        .postfix
                                        .entry(op)
                                        .and_modify(|_| {
                                            self.diagnostics.raise(
                                                ParseDiagnostic::IllegalCoreOverride,
                                                attr.loc,
                                            );
                                        })
                                        .or_insert(id);
                                }
                            }
                        } else {
                            *attr_state = AttrMacroProcState::Err(
                                self.diagnostics
                                    .raise(ParseDiagnostic::MacroPatternFailure, attr.loc),
                            );
                        }
                    }
                }
            }
        }
    }
    fn process_attr_core(&mut self) {}

    fn process_traits(
        &mut self,
        statics_in: &UnresolvedStatics<'src>,
        attrs: &mut Vec<AttrMacroProcState<'src>>,
    ) {
        for (id, trait_) in statics_in.traits.iter().enumerate() {
            self.process_annot_attrs(
                MacroMidCallLocation::Trait { id },
                &trait_.annot.attrs,
                attrs,
            );
        }
    }

    fn process_annot_attrs(
        &mut self,
        call: MacroMidCallLocation,
        attr_ids: &HashMap<BackendId, Vec<usize>>,
        attrs: &mut Vec<AttrMacroProcState<'src>>,
    ) {
        for (backend_id, attr_ids) in attr_ids {
            let compat = self.compat.get_compatible_backend_infos(*backend_id);
            for attr_id in attr_ids {
                let attr = &mut attrs[*attr_id];
                for compat in &compat {
                    if !matches!(attr, AttrMacroProcState::Unhandled(_)) {
                        break;
                    }
                    compat.process_attrs_mid(call, self.statics, attr);
                }
            }
        }
    }
}

pub fn match_macro_ent<'src>(ast: &ASTMacroInvocationBody<'src>, spec: &MacroSpecEnt) -> bool {
    match (ast, spec) {
        (ASTMacroInvocationBody::Name { name }, MacroSpecEnt::Enum { one_of }) => {
            one_of.iter().any(|opt| opt.as_str() == name.value)
        }
        _ => todo!("match macro ent"),
    }
}
