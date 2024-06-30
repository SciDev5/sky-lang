use std::collections::HashMap;

use scopes::ScopeId;

use crate::{
    back::BackendId,
    front::{
        ast::{ASTBlock, ASTExpr},
        source::Loc,
    },
    lint::diagnostic::Fallible,
};

use super::types::{TypeDatalike, TypeTraitlike};

pub mod extract;
pub mod merge;
pub mod scopes;
pub mod verify_merge;

pub struct Data {
    pub annot: Annot,
    pub name: String,
    pub templates: Templates,

    pub base_target: BackendId,
    pub variants: HashMap<BackendId, DataVariant>,
}
pub struct DataVariant {
    pub loc: Loc,
    pub content: DataVariantContent,
}
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

pub struct DataProperty {
    pub loc: Loc,
    pub annot: Annot,
    pub name: String,
    pub ty: Fallible<TypeDatalike>,
}

pub struct DataEnumVariant {
    pub loc: Loc,
    pub annot: Annot,
    pub name: String,
    pub contents: DataEnumVariantContent,
}

pub enum DataEnumVariantContent {
    Unit,
    Struct {
        properties: Vec<DataProperty>,
    },
    Tuple {
        properties: Vec<Fallible<TypeDatalike>>,
    },
}

pub struct TraitGeneric<BodyBlock> {
    pub loc: Loc,
    pub annot: Annot,
    pub name: String,
    pub templates: Templates,
    pub bounds: Vec<TypeTraitlike>,

    pub base_target: BackendId,
    pub consts: Vec<TraitConst>,
    pub functions: Vec<TraitFunction<BodyBlock>>,
    pub typealias: Vec<TraitTypeAlias>,
}
pub type TraitUnresolved<'src> = TraitGeneric<ASTBlock<'src>>;
// pub type Trait = TraitGeneric<___todo!____>;
pub struct TraitConst {
    pub loc: Loc,
    pub annot: Annot,
    pub name: String,
    pub ty: Fallible<TypeDatalike>,
}
pub struct TraitTypeAlias {
    pub loc: Loc,
    pub annot: Annot,
    pub name: String,
    pub bounds: Vec<TypeTraitlike>,
}
pub struct TraitFunction<BodyBlock> {
    pub loc: Loc,
    pub annot: Annot,
    pub name: String,
    pub templates: Templates,
    pub params: Vec<Fallible<TypeDatalike>>,
    pub return_ty: Fallible<TypeDatalike>,
    pub body: Option<BodyBlock>,
}

pub struct ConstGeneric<Type, Initilizer> {
    pub annot: Annot,
    pub name: String,
    pub templates: Templates,
    pub ty: Type,
    pub initializer: Fallible<Initilizer>,

    pub base_target: BackendId,
}
pub type ConstUnsolved<'src> = ConstGeneric<Option<TypeDatalike>, (ASTExpr<'src>, ScopeId, Loc)>;
// pub type Const = ConstGeneric<TypeDatalike, ___todo!____>;

pub struct FunctionGeneric<ReturnTy, BodyBlock> {
    pub annot: Annot,
    pub name: String,
    pub templates: Templates,
    pub params: Vec<Fallible<TypeDatalike>>,
    pub return_ty: Fallible<ReturnTy>,

    pub base_target: BackendId,
    pub variants: HashMap<BackendId, FunctionVariant<BodyBlock>>,
}
pub type FunctionUnresolved<'src> = FunctionGeneric<Option<TypeDatalike>, ASTBlock<'src>>;
// pub type Function = FunctionGeneric<TypeDatalike, ___todo!____>;
pub struct FunctionVariant<BodyBlock> {
    pub loc: Loc,

    pub body: BodyBlock,
}

pub struct TypeAlias {
    pub annot: Annot,
    pub name: String,
    pub templates: Templates,
    pub ty: Fallible<TypeDatalike>,

    pub loc: Loc,
    pub target: BackendId,
}

pub struct Templates {
    pub def: Vec<(String, Vec<TypeTraitlike>)>,
}

pub struct Annot {
    pub doc: Option<String>,
    pub is_public: bool,
}
