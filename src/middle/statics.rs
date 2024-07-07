use std::collections::HashMap;

use scopes::ScopeId;
use verify_merge::TemplateNames;

use crate::{
    back::BackendId,
    front::{
        ast::{ASTBlock, ASTExpr, ASTName},
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
    pub consts: Vec<ConstUnresolved<'src>>,
    pub typealiases: Vec<TypeAlias>,
    pub impls_data: Vec<ImplData>,
    pub impls_trait: Vec<ImplTrait>,
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ImplData {
    pub templates: Templates,
    pub target: TypeDatalike,

    pub functions: HashMap<String, usize>,
    pub consts: HashMap<String, usize>,
    pub typealiases: HashMap<String, usize>,
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ImplTrait {
    pub templates: Templates,
    pub target_data: TypeDatalike,
    pub target_trait: TypeTraitlike,

    pub functions: Vec<Option<usize>>,
    pub consts: Vec<Option<usize>>,
    pub typealiases: Vec<Option<usize>>,
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
pub type ConstUnresolved<'src> = ConstGeneric<Option<TypeDatalike>, (ASTExpr<'src>, ScopeId, Loc)>;
// pub type Const = ConstGeneric<TypeDatalike, ___todo!____>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionGeneric<ReturnTy, BodyBlock> {
    pub annot: Annot,
    pub name: Name,
    pub templates: Templates,
    pub ty_args: Vec<Fallible<TypeDatalike>>,
    pub ty_return: Fallible<ReturnTy>,

    pub ty_self: Option<TypeDatalike>,

    pub base_target: BackendId,
    pub variants: HashMap<BackendId, FunctionVariant<BodyBlock>>,
}
pub type FunctionUnresolved<'src> =
    FunctionGeneric<Option<TypeDatalike>, (ASTBlock<'src>, Option<TemplateNames<'src>>)>;
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
