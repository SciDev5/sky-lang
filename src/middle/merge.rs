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

use std::collections::HashMap;

use crate::{
    back::BackendId,
    front::{
        ast::{
            ASTAnnot, ASTBlock, ASTDataContents, ASTExpr, ASTFallible, ASTName, ASTTemplates,
            ASTTrait, ASTType, ASTTypeAlias, ASTTypedDestructure,
        },
        source::{HasLoc, Loc},
    },
    impl_hasloc_simple,
};

#[derive(Debug)]
pub struct MergedStatics<'src> {
    pub functions: Vec<MergedFunction<'src>>,
    pub datas: Vec<MergedData<'src>>,
    pub traits: Vec<MergedTrait<'src>>,
    pub consts: Vec<MergedConst<'src>>,
    pub typealiases: Vec<MergedTypeAlias<'src>>,
    pub unbound_impls: Vec<MergedUnboundImpl<'src>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MergedFunction<'src> {
    pub loc: Loc,

    pub annot: ASTAnnot,
    pub name: ASTFallible<ASTName<'src>>,
    pub templates: ASTTemplates<'src>,
    pub args: Vec<ASTFallible<ASTTypedDestructure<'src>>>,
    pub ty_return: Option<ASTFallible<ASTType<'src>>>,

    pub base_target_level: BackendId,
    pub contents: HashMap<BackendId, ASTBlock<'src>>,
}
impl_hasloc_simple!(MergedFunction<'src>);

#[derive(Debug, Clone, PartialEq)]
pub struct MergedData<'src> {
    loc: Loc,

    pub annot: ASTAnnot,
    pub name: ASTFallible<ASTName<'src>>,
    pub templates: ASTTemplates<'src>,

    pub base_target_level: BackendId,
    pub contents: HashMap<BackendId, ASTDataContents<'src>>,

    pub attatced_impls: Vec<MergedImpl<'src>>,
}
impl_hasloc_simple!(MergedData<'src>);

#[derive(Debug, Clone, PartialEq)]
pub struct MergedTrait<'src> {
    pub target_level: BackendId,
    pub inner: ASTTrait<'src>,
}
impl<'src> HasLoc for MergedTrait<'src> {
    fn loc(&self) -> Loc {
        self.inner.loc()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct MergedConst<'src> {
    pub loc: Loc,
    pub name: ASTName<'src>,
    pub ty: ASTType<'src>,
    pub base_target_level: BackendId,
    pub contents: HashMap<BackendId, ASTExpr<'src>>,
}
impl_hasloc_simple!(MergedConst<'src>);

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

    pub functions: Vec<MergedFunction<'src>>,
    pub consts: Vec<MergedConst<'src>>,
    pub types: Vec<ASTTypeAlias<'src>>,
}
impl_hasloc_simple!(MergedImpl<'src>);
