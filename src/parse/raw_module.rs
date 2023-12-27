use std::collections::HashMap;

use num::complex::Complex64;

use crate::{
    common::{IdentInt, IdentStr},
    math::tensor::Tensor,
};

use super::ops::SLOperator;

pub struct RMFunction {
    pub doc_comment: Option<String>,
    pub params: Vec<(IdentStr, RMValueType)>,
    pub return_ty: Option<RMType>,
    pub block: Vec<RMExpression>,
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
    FunctionRef { params: Vec<RMValueType>, return_ty: Box<RMType>, },
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
pub enum RMLiteralNumber {
    Int(i128),
    Float(f64),
    Complex(Complex64),
}
#[derive(Debug, Clone)]
pub enum RMLiteralArray {
    List(Vec<RMExpression>),
    Matrix(Tensor<RMExpression>),
    Tensor(Tensor<RMExpression>),
}

#[derive(Debug, Clone)]
pub enum RMExpression {
    DeclareVar {
        doc_comment: Option<String>,
        ident: IdentStr,
        writable: bool,
        initial_value: Option<Box<RMExpression>>,
        ty: Option<RMValueType>,
    },
    DeclareFunction {
        id: IdentInt,
        ident: IdentStr,
    },
    DeclareClass {
        id: IdentInt,
        ident: IdentStr,
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

    LiteralNumber(RMLiteralNumber),
    LiteralBool(bool),
    LiteralString(String),
    LiteralRange {
        start: Option<Box<RMExpression>>,
        step: Option<Box<RMExpression>>,
        end: Option<Box<RMExpression>>,
    },
    LiteralArray(RMLiteralArray),

    AnonymousFunction {
        params: Vec<(IdentStr, Option<RMValueType>)>,
        block: Vec<RMExpression>,
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
        block: Vec<RMExpression>,
        elifs: Vec<(RMExpression, Vec<RMExpression>)>,
        else_block: Option<Vec<RMExpression>>,
    },
    Loop {
        block: Vec<RMExpression>,
    },
    LoopFor {
        loop_var: (IdentStr, Option<RMValueType>),
        iterable: Box<RMExpression>,
        block: Vec<RMExpression>,
    },
    LoopBreak(Option<Box<RMExpression>>),
    LoopContinue,

    Return(Option<Box<RMExpression>>),
}

pub struct RawModule {
    pub functions: Vec<RMFunction>,
    pub classes: Vec<RMClass>,

    pub top_level: Vec<RMExpression>,
}
