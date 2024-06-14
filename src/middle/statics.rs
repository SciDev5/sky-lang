use crate::{
    front::{
        ast::{ASTConst, ASTData, ASTFreeImpl, ASTFunction, ASTTrait, ASTTypeAlias},
        source::{HasLoc, Loc},
    },
    impl_hasloc_simple,
};

#[derive(Debug)]
pub struct ASTStatics<'src> {
    pub functions: Vec<ASTFunction<'src>>,
    pub datas: Vec<ASTData<'src>>,
    pub traits: Vec<ASTTrait<'src>>,
    pub consts: Vec<ASTConst<'src>>,
    pub typealiases: Vec<ASTTypeAlias<'src>>,
    pub freeimpls: Vec<ASTFreeImpl<'src>>,
}
impl<'src> ASTStatics<'src> {
    pub fn new() -> Self {
        Self {
            functions: Vec::new(),
            datas: Vec::new(),
            traits: Vec::new(),
            consts: Vec::new(),
            typealiases: Vec::new(),
            freeimpls: Vec::new(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct SFunction {
    loc: Loc,
    // ...
}
impl_hasloc_simple!(SFunction);

#[derive(Debug, Clone, PartialEq)]
pub struct SData {
    loc: Loc,
    // ...
}
impl_hasloc_simple!(SData);

#[derive(Debug, Clone, PartialEq)]
pub struct STrait {
    loc: Loc,
    // ...
}
impl_hasloc_simple!(STrait);

#[derive(Debug, Clone, PartialEq)]
pub struct SConst {
    loc: Loc,
    // ...
}
impl_hasloc_simple!(SConst);

#[derive(Debug, Clone, PartialEq)]
pub struct STypeAlias {
    loc: Loc,
    // ...
}
impl_hasloc_simple!(STypeAlias);

#[derive(Debug, Clone, PartialEq)]
pub struct SFreeImpl {
    loc: Loc,
    // ...
}
impl_hasloc_simple!(SFreeImpl);
