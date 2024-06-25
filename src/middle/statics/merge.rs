//! needs to be able to represent the collective contributions of all the target platforms
//!
//! Cross-target merging only happens for public/exported items
//!
//! DATA
//!  - at a given target level datas can either have
//!    - undefined content -> `data abstract`
//!    - defined content from this level -> `data struct`/`data tuple`/...
//!    - inherited content from a more abstract level -> (write nothing)
//!  - trait impls must be declared on the same level as data
//! IMPL/FUNCTION:
//!  - temporary impl limitations:
//!    - free impls must be defined in the top level of the same module as either the trait or data it uses.
//!    - impls must be for a specific data, `impl<T: SomeLocalTrait> X for T` is not yet allowed.
//!  - ref to impl is stored with either the data or trait, prefering to stick to data if both are near.
//!  - all declared functions must be implemented by the packages given abstractmost compilable levels.
//! CONST:
//!  - at a given target level datas can either have
//!    - undefined content -> `pub const PLATFORM_NAME: string`
//!    - defined content from this level -> `pub const PLATFORM_NAME = "hello world"` (infers type)
//!    - inherited content from a more abstract level -> (write nothing)
//! TYPE ALIAS / TRAIT:
//!  - only allowed to be definet at a single level, overlaps are ignored
//!

use std::collections::{BTreeMap, HashMap};

use crate::{
    back::BackendId,
    front::{
        ast::{
            ASTConst, ASTData, ASTFallible, ASTFunction, ASTName, ASTSourceFile, ASTTemplates,
            ASTTrait, ASTType, ASTTypeAlias,
        },
        source::{HasLoc, Loc},
    },
    impl_hasloc_simple,
};

//////////////////// MERGED STATICS STRUCTURES //////////////////

#[derive(Debug)]
pub struct MergedTypeStatics<'src> {
    pub modules: Vec<Merged<'src, ASTSourceFile<'src>>>,
    pub datas: Vec<Merged<'src, ASTData<'src>>>,
    pub traits: Vec<Merged<'src, ASTTrait<'src>>>,
}
#[derive(Debug, Clone, PartialEq)]
pub struct MergedModule<'src> {
    pub functions: HashMap<&'src str, usize>,
    pub datas: HashMap<&'src str, usize>,
    pub traits: HashMap<&'src str, usize>,
    pub consts: HashMap<&'src str, usize>,
    pub typealiases: HashMap<&'src str, usize>,

    pub sources: BTreeMap<BackendId, ASTSourceFile<'src>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Merged<'src, T> {
    pub name: ASTFallible<ASTName<'src>>,
    pub base_target_level: BackendId,
    pub contents: HashMap<BackendId, (usize, T)>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MergedTypeAlias<'src> {
    pub target_level: BackendId,
    pub inner: ASTTypeAlias<'src>,
}
impl<'src> HasLoc for MergedTypeAlias<'src> {
    fn loc(&self) -> Loc {
        self.inner.loc()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct MergedUnboundImpl<'src> {
    pub target_data: ASTType<'src>,
    pub inner: MergedImpl<'src>,
}
impl<'src> HasLoc for MergedUnboundImpl<'src> {
    fn loc(&self) -> Loc {
        self.inner.loc
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct MergedImpl<'src> {
    pub loc: Loc,

    pub templates: ASTTemplates<'src>,
    pub target_trait: Option<ASTType<'src>>,

    pub functions: Vec<Merged<'src, ASTFunction<'src>>>,
    pub consts: Vec<Merged<'src, ASTConst<'src>>>,
    pub types: Vec<ASTTypeAlias<'src>>,
}
impl_hasloc_simple!(MergedImpl<'src>);

//////////////////// MERGED STATICS IMPLEMENTATION //////////////////

// pub fn merge_statics<'src>(
//     src_files: Vec<HashMap<BackendId, ASTSourceFile<'src>>>,
//     statics: ASTStatics<'src>,
// ) -> (Vec<MergedModule<'src>>, MergedStatics<'src>) {
//     macro_rules! hashmapify {
//         ($v:expr) => {
//             $v.into_iter().enumerate().collect::<HashMap<_, _>>()
//         };
//     }
//     let mut ast_functions = hashmapify!(statics.functions);
//     let mut ast_datas = hashmapify!(statics.datas);
//     let mut ast_traits = hashmapify!(statics.traits);
//     let mut ast_consts = hashmapify!(statics.consts);
//     let mut ast_typealiases = hashmapify!(statics.typealiases);
//     let mut ast_freeimpls = hashmapify!(statics.freeimpls);
//     let mut merged_statics = MergedStatics {
//         functions: Vec::new(),
//         datas: Vec::new(),
//         traits: Vec::new(),
//         consts: Vec::new(),
//         typealiases: Vec::new(),
//         unbound_impls: Vec::new(),
//     };

//     let modules = src_files
//         .into_iter()
//         .map(|src| {
//             if src.len() == 1 {
//                 // this is most modules, so may as well make a simplified special case
//                 // statics.functions.push(MergedFunction {
//                 //     loc:
//                 // })
//             }
//         })
//         .collect::<Vec<_>>();

//     todo!()
// }
