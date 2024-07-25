use crate::{
    front::tokenize::{TInfixOperatorType, TPostfixOperatorType, TPrefixOperatorType},
    macros::{MacroSpec, MacroSpecEnt},
    middle::{
        macro_processing::{AttrMacroProcState, BackendProcessMacros, MacroMidCallLocation},
        statics::UnresolvedStatics,
    },
};

use super::{Backend, BackendId, BackendInfo};

pub struct BackCommon;
impl Backend for BackCommon {
    const ID: BackendId = 0;
    type Output = ();
    type Config<'a> = ();
    type MacroInfo = BackCommonMacroInfo;
    fn info() -> (BackendInfo, Self::MacroInfo) {
        let mut macro_specs = Vec::new();
        let mut bind = |(v, info)| {
            macro_specs.push(v);
            (macro_specs.len() - 1, info)
        };
        let macro_info = BackCommonMacroInfo {
            bind_op_trait: bind(genmacro_bind_op_trait()),
        };
        (
            BackendInfo {
                id: Self::ID,
                compat_ids: &[Self::ID],
                name: "common",
                macro_specs,
            },
            macro_info,
        )
    }
    fn compile<'a>(_: &crate::TODO_common_source, _: &Self::Config<'a>) -> Self::Output {
        panic!("cannot compile to common")
    }
}
#[derive(Debug, Clone, Copy)]
pub struct BackCommonMacroInfo {
    pub bind_op_trait: (usize, fn(usize, usize) -> Op),
}
impl BackendProcessMacros for BackCommonMacroInfo {
    fn process_attrs_mid<'a>(
        &self,
        call: MacroMidCallLocation,
        statics: &mut UnresolvedStatics<'a>,
        attr: &mut AttrMacroProcState<'a>,
    ) {
        // does nothing generic, generation of Core is handled explicitly
    }
}

pub enum Op {
    In(TInfixOperatorType),
    Pre(TPrefixOperatorType),
    Post(TPostfixOperatorType),
}
fn genmacro_bind_op_trait() -> (MacroSpec, fn(usize, usize) -> Op) {
    (
        MacroSpec {
            name: "bind_op_trait".to_string(),
            variants: [
                MacroSpecEnt::Enum {
                    one_of: TInfixOperatorType::ALL
                        .into_iter()
                        .map(|op| op.name().to_string())
                        .collect(),
                },
                MacroSpecEnt::Enum {
                    one_of: TPrefixOperatorType::ALL
                        .into_iter()
                        .map(|op| op.name().to_string())
                        .collect(),
                },
                MacroSpecEnt::Enum {
                    one_of: TPostfixOperatorType::ALL
                        .into_iter()
                        .map(|op| op.name().to_string())
                        .collect(),
                },
            ]
            .into_iter()
            .collect(),
        },
        |varient_id, enum_id| match varient_id {
            0 => Op::In(TInfixOperatorType::ALL[enum_id]),
            1 => Op::Pre(TPrefixOperatorType::ALL[enum_id]),
            2 => Op::Post(TPostfixOperatorType::ALL[enum_id]),
            _ => unreachable!(),
        },
    )
}
