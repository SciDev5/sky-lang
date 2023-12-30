use std::collections::HashMap;

use num::complex::Complex64;

use crate::{
    common::{IdentInt, IdentStr},
    math::tensor::Tensor,
};

use super::ops::SLOperator;

#[derive(Debug, Clone)]
pub struct ScopedStatics {
    pub functions: HashMap<IdentStr, Vec<IdentInt>>,
    pub classes: HashMap<IdentStr, IdentInt>,
}
impl ScopedStatics {
    pub fn empty() -> Self {
        Self {
            functions: HashMap::new(),
            classes: HashMap::new(),
        }
    }
}

pub struct RMFunction {
    pub doc_comment: Option<String>,
    pub params: Vec<(IdentStr, RMValueType)>,
    pub return_ty: Option<RMType>,
    pub block: RMBlock,
    /// Contains references to all static references this function can see, including itself.
    pub all_scoped: ScopedStatics,
}
pub struct RMClass {
    pub doc_comment: Option<String>,
    pub fields: HashMap<IdentStr, RMValueType>,
    pub functions: HashMap<IdentStr, Vec<IdentInt>>,
}

#[derive(Debug, Clone)]
pub enum RMValueType {
    Int,
    Float,
    Complex,
    Bool,
    String,
    FunctionRef {
        params: Vec<RMValueType>,
        return_ty: Box<RMType>,
    },
    Identified(IdentStr),
    Tuple(Vec<RMValueType>),
    // List(Box<RMValueType>),
    // TODO Template types
    // TODO Units / dimensional analysis
}
#[derive(Debug, Clone)]
pub enum RMType {
    Void,
    Never,
    Value(RMValueType),
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
pub enum RMExpression {
    Void,

    DeclareVar {
        doc_comment: Option<String>,
        ident: IdentStr,
        writable: bool,
        initial_value: Option<Box<RMExpression>>,
        ty: Option<RMValueType>,
    },

    AssignIndex {
        object: Box<RMExpression>,
        indices: Vec<RMExpression>,
        value: Box<RMExpression>,
    },
    AssignProperty {
        object: Box<RMExpression>,
        property: IdentStr,
        value: Box<RMExpression>,
    },
    AssignVar {
        ident: IdentStr,
        value: Box<RMExpression>,
    },
    ReadIndex {
        expr: Box<RMExpression>,
        indices: Vec<RMExpression>,
    },
    ReadProperty {
        expr: Box<RMExpression>,
        property_ident: IdentStr,
    },
    Read {
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
    LiteralArray(RMLiteralArray),

    AnonymousFunction {
        params: Vec<(IdentStr, Option<RMValueType>)>,
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
        loop_var: (IdentStr, Option<RMValueType>),
        iterable: Box<RMExpression>,
        block: RMBlock,
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

pub struct RawModule {
    pub functions: Vec<RMFunction>,
    pub classes: Vec<RMClass>,

    pub top_level: RMBlock,
}
