use std::{collections::HashMap, fmt::Debug};

use num::complex::Complex64;

use crate::{
    common::{
        common_module::{CMAssociatedFunction, DocComment},
        IdentInt, IdentStr,
    },
    math::tensor::Tensor,
};

use super::ops::SLOperator;

#[derive(Debug, Clone)]
pub struct ScopedStatics {
    pub functions: HashMap<IdentStr, Vec<IdentInt>>,
    pub structs: HashMap<IdentStr, IdentInt>,
    pub traits: HashMap<IdentStr, IdentInt>,
}
impl ScopedStatics {
    pub fn empty() -> Self {
        Self {
            functions: HashMap::new(),
            structs: HashMap::new(),
            traits: HashMap::new(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct RMTemplateDef {
    pub ident: IdentStr,
    // TODO trait bounds
}

#[derive(Debug, Clone)]
pub struct RMFunctionInfo {
    pub doc_comment: DocComment,
    pub params: Vec<(IdentStr, RMType)>,
    pub local_template_defs: Vec<RMTemplateDef>,
    pub return_ty: Option<RMType>,
    pub can_be_disembodied: bool,
    /// Contains references to all static references this function can see, including itself.
    pub all_scoped: ScopedStatics,
}
#[derive(Debug, Clone)]
pub struct RMFunction {
    pub info: RMFunctionInfo,
    pub block: Option<RMBlock>,
}
// #[derive(Debug, Clone, Copy)]
// pub struct RMAssociatedFunction {
//     pub id: IdentInt,
//     pub is_member: bool,
// }
#[derive(Debug)]
pub struct RMStruct {
    pub doc_comment: DocComment,
    pub fields: HashMap<IdentStr, (RMType, DocComment)>,
    pub impl_functions: HashMap<IdentStr, CMAssociatedFunction>,
    pub impl_traits: HashMap<IdentStr, RMTraitImpl>,
    /// Contains references to all static references this struct can see, including itself.
    pub all_scoped: ScopedStatics,
}
#[derive(Debug, Clone)]
pub struct RMTraitImpl {
    pub trait_id: IdentStr,
    pub functions: HashMap<String, CMAssociatedFunction>,
}
#[derive(Debug)]
pub struct RMTrait {
    pub doc_comment: DocComment,
    pub functions: HashMap<IdentStr, CMAssociatedFunction>,
    /// Contains references to all static references this struct can see, including itself.
    pub all_scoped: ScopedStatics,
}

#[derive(Debug, Clone, PartialEq)]
pub enum RMType {
    Never,
    Void,
    Int,
    Float,
    Complex,
    Bool,
    String,
    FunctionRef {
        params: Vec<RMType>,
        return_ty: Box<RMType>,
    },
    Identified(IdentStr),
    Tuple(Vec<RMType>),
    // List(Box<RMValueType>),
    // TODO Template types
    // TODO Units / dimensional analysis
}

#[derive(Debug, Clone)]
pub enum RMLiteralValue {
    Int(i128),
    Float(f64),
    Complex(Complex64),
    Bool(bool),
    String(String),
}
#[derive(Debug, Clone)]
pub enum RMLiteralArray {
    List(Vec<RMExpression>),
    Matrix(Tensor<RMExpression>),
    Tensor(Tensor<RMExpression>),
}

#[derive(Debug, Clone)]
pub enum LiteralStructInit<Expr: Debug + Clone> {
    Struct(Vec<(IdentStr, Expr)>),
    Tuple(Vec<Expr>),
}
impl<Expr: Debug + Clone> LiteralStructInit<Expr> {
    pub fn transform<ExprOut: Debug + Clone, F: FnMut(Expr) -> ExprOut>(
        self,
        mut tf: F,
    ) -> LiteralStructInit<ExprOut> {
        match self {
            LiteralStructInit::Tuple(data) => {
                LiteralStructInit::Tuple(data.into_iter().map(tf).collect())
            }
            LiteralStructInit::Struct(data) => LiteralStructInit::Struct(
                data.into_iter()
                    .map(|(ident, data)| (ident, tf(data)))
                    .collect(),
            ),
        }
    }
}

#[derive(Debug, Clone)]
pub enum RMExpression {
    Void,

    DeclareVar {
        doc_comment: DocComment,
        ident: IdentStr,
        writable: bool,
        initial_value: Option<Box<RMExpression>>,
        ty: Option<RMType>,
    },

    AssignIndex {
        object: Box<RMExpression>,
        indices: Vec<RMExpression>,
        value: Box<RMExpression>,
        op: Option<SLOperator>,
    },
    AssignProperty {
        object: Box<RMExpression>,
        property: IdentStr,
        value: Box<RMExpression>,
        op: Option<SLOperator>,
    },
    AssignVar {
        ident: IdentStr,
        value: Box<RMExpression>,
        op: Option<SLOperator>,
    },
    ReadIndex {
        expr: Box<RMExpression>,
        indices: Vec<RMExpression>,
    },
    ReadProperty {
        expr: Box<RMExpression>,
        property_ident: IdentStr,
    },
    Ident {
        ident: IdentStr,
    },

    Call {
        callable: Box<RMExpression>,
        arguments: Vec<RMExpression>,
    },

    LiteralValue(RMLiteralValue),
    LiteralRange {
        start: Option<Box<RMExpression>>,
        step: Option<Box<RMExpression>>,
        end: Option<Box<RMExpression>>,
    },
    LiteralStructInit {
        ident: IdentStr,
        properties: LiteralStructInit<RMExpression>,
    },
    LiteralArray(RMLiteralArray),

    AnonymousFunction {
        params: Vec<(IdentStr, Option<RMType>)>,
        block: RMBlock,
    },

    OpBinary {
        op: SLOperator,
        lhs: Box<RMExpression>,
        rhs: Box<RMExpression>,
    },
    OpUnary {
        op: SLOperator,
        value: Box<RMExpression>,
    },

    Conditional {
        condition: Box<RMExpression>,
        block: RMBlock,
        elifs: Vec<(RMExpression, RMBlock)>,
        else_block: Option<RMBlock>,
    },
    Loop {
        block: RMBlock,
    },
    LoopFor {
        loop_var: (IdentStr, Option<RMType>),
        iterable: Box<RMExpression>,
        block: RMBlock,
        else_block: Option<RMBlock>,
    },
    LoopWhile {
        condition: Box<RMExpression>,
        block: RMBlock,
        else_block: Option<RMBlock>,
    },
    LoopBreak(Option<Box<RMExpression>>),
    LoopContinue,

    Return(Option<Box<RMExpression>>),
}

#[derive(Debug, Clone)]
pub struct RMBlock {
    pub block: Vec<RMExpression>,
    /// Contains a list of references to the functions contianed in this scope,
    /// excluding outer scopes.
    pub inner_scoped: ScopedStatics,
}
#[derive(Debug)]
pub struct RawModule {
    pub functions: Vec<RMFunction>,
    pub structs: Vec<RMStruct>,
    pub traits: Vec<RMTrait>,

    pub top_level: RMBlock,
}
