use std::collections::HashMap;

use scopes::ScopeId;

use crate::{
    back::BackendId,
    front::{
        ast::{ASTBlock, ASTExpr, ASTImpl, ASTName},
        source::{HasLoc, Loc},
    },
    impl_hasloc_simple,
    lint::diagnostic::Fallible,
};

use super::types::{TypeDatalike, TypeTraitlike};

pub mod extract;
pub mod merge;
pub mod scopes;
pub mod verify_merge;

#[derive(Debug, Clone)]
pub struct UnresolvedStatics<'src> {
    pub functions: Vec<FunctionUnresolved<'src>>,
    pub datas: Vec<Data>,
    pub traits: Vec<Trait>,
    pub consts: Vec<ConstUnsolved<'src>>,
    pub typealiases: Vec<TypeAlias>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Data {
    pub annot: Annot,
    pub name: Name,
    pub templates: Templates,

    pub base_target: BackendId,
    pub variants: HashMap<BackendId, DataVariant>,
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DataVariant {
    pub loc: Loc,
    pub content: DataVariantContent,
}
impl_hasloc_simple!(DataVariant);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DataVariantContent {
    Unit,
    Abstract,
    Struct {
        properties: Vec<DataProperty>,
    },
    Tuple {
        properties: Vec<Fallible<TypeDatalike>>,
    },
    Enum {
        variants: Vec<DataEnumVariant>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DataProperty {
    pub loc: Loc,
    pub annot: Annot,
    pub name: Name,
    pub ty: Fallible<TypeDatalike>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DataEnumVariant {
    pub loc: Loc,
    pub annot: Annot,
    pub name: Name,
    pub contents: DataEnumVariantContent,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DataEnumVariantContent {
    Unit,
    Struct {
        properties: Vec<DataProperty>,
    },
    Tuple {
        properties: Vec<Fallible<TypeDatalike>>,
    },
}

pub struct FreedDataImpl<'src> {
    pub templates: Templates,
    pub containing_scope: ScopeId,
    pub target: TypeDatalike,
    pub attatched_impl: ASTImpl<'src>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Trait {
    pub loc: Loc,
    pub annot: Annot,
    pub name: Name,
    pub templates: Templates,
    pub bounds: Vec<TypeTraitlike>,

    pub base_target: BackendId,
    pub consts: Vec<TraitConst>,
    pub functions: Vec<TraitFunction>,
    pub types: Vec<TraitTypeAlias>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TraitConst {
    pub loc: Loc,
    pub annot: Annot,
    pub name: Name,
    pub ty: Fallible<TypeDatalike>,
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TraitTypeAlias {
    pub loc: Loc,
    pub annot: Annot,
    pub name: Name,
    pub bounds: Vec<TypeTraitlike>,
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TraitFunction {
    pub loc: Loc,
    pub annot: Annot,
    pub name: Name,
    pub templates: Templates,
    pub ty_args: Vec<Fallible<TypeDatalike>>,
    pub ty_return: Fallible<TypeDatalike>,
    pub args: Vec<Fallible<Destructure>>,
    pub dedault_impl: Option<usize>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Destructure {
    Name { loc: Loc, name: Name },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ConstGeneric<Type, Initilizer> {
    pub annot: Annot,
    pub name: Name,
    pub ty: Fallible<Type>,

    pub base_target: BackendId,
    pub initializer: HashMap<BackendId, Fallible<Initilizer>>,
}
pub type ConstUnsolved<'src> = ConstGeneric<Option<TypeDatalike>, (ASTExpr<'src>, ScopeId, Loc)>;
// pub type Const = ConstGeneric<TypeDatalike, ___todo!____>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionGeneric<ReturnTy, BodyBlock> {
    pub annot: Annot,
    pub name: Name,
    pub templates: Templates,
    pub ty_args: Vec<Fallible<TypeDatalike>>,
    pub ty_return: Fallible<ReturnTy>,

    pub base_target: BackendId,
    pub variants: HashMap<BackendId, FunctionVariant<BodyBlock>>,
}
pub type FunctionUnresolved<'src> =
    FunctionGeneric<Option<TypeDatalike>, (ASTBlock<'src>, Option<Vec<Name>>)>;
// pub type Function = FunctionGeneric<TypeDatalike, ___todo!____>;
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionVariant<BodyBlock> {
    pub loc: Loc,

    pub args: Vec<Fallible<Destructure>>,
    pub body: BodyBlock,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeAlias {
    pub annot: Annot,
    pub name: Name,
    pub templates: Templates,
    pub ty: Fallible<TypeDatalike>,

    pub loc: Loc,
    pub target: BackendId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Name {
    pub loc: Loc,
    pub value: String,
}
impl Name {
    pub fn from_astname(name: ASTName) -> Self {
        Self {
            loc: name.loc,
            value: name.value.to_string(),
        }
    }
}
impl<'src> From<ASTName<'src>> for Name {
    fn from(value: ASTName<'src>) -> Self {
        Self::from_astname(value)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Templates {
    pub def: Vec<(Name, Vec<TypeTraitlike>)>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Annot {
    pub doc: Option<String>,
    pub is_public: Option<Loc>,
}
