use std::collections::HashMap;

use num::complex::Complex64;

use crate::{
    common::{IdentInt, IdentStr},
    math::tensor::Tensor,
};

pub struct CMFunction {
    doc_comment: Option<String>,
    params: Vec<CMValueType>,
    locals: Vec<CMValueType>,
    return_ty: CMType,
    block: Vec<CMExpression>,
}
pub struct CMClosureFunction {
    params: Vec<CMValueType>,
    captures: Vec<IdentInt>,
    locals: Vec<CMValueType>,
    return_ty: CMType,
    block: Vec<CMExpression>,
}
#[derive(Debug, Clone)]
pub struct CMInlineLambda {
    params: Vec<CMValueType>,
    captures: Vec<IdentInt>,
    locals: Vec<CMValueType>,
    block: Vec<CMExpression>,
}
pub struct CMClass {
    doc_comment: Option<String>,
    fields: HashMap<IdentStr, CMValueType>,
    functions: HashMap<IdentStr, Vec<IdentInt>>,
}

#[derive(Debug, Clone, Copy)]
pub enum CMValueType {
    // TODO RawModule | Type
}
#[derive(Debug, Clone, Copy)]
pub enum CMType {
    Void,
    Never,
    Value(CMValueType),
}

#[derive(Debug, Clone)]
pub enum CMLiteralNumber {
    Int(i128),
    Float(f64),
    Complex(Complex64),
}
#[derive(Debug, Clone)]
pub enum CMLiteralArray {
    List(Vec<CMExpression>),
    Matrix(Tensor<CMExpression>),
    Tensor(Tensor<CMExpression>),
}

#[derive(Debug, Clone)]
pub enum CMExpression {
    AssignProperty {
        object: Box<CMExpression>,
        property: IdentInt,
        value: Box<CMExpression>,
    },
    AssignVar {
        ident: IdentInt,
        value: Box<CMExpression>,
    },
    ReadProperty {
        expr: Box<CMExpression>,
        property_ident: IdentInt,
    },
    Read {
        ident: IdentInt,
    },

    Call {
        function_id: IdentInt,
        arguments: Vec<CMExpression>,
        always_inline: bool,
        inlined_lambdas: Option<Vec<CMInlineLambda>>,
    },

    LiteralNumber(CMLiteralNumber),
    LiteralBool(bool),
    LiteralString(String),
    LiteralArray(CMLiteralArray),
    LiteralFunctionRef {
        function_id: IdentInt,
    },

    Closure {
        closure_function_id: IdentInt,
    },

    Conditional {
        condition: Box<CMExpression>,
        block: Vec<CMExpression>,
        elifs: Vec<(CMExpression, Vec<CMExpression>)>,
        else_block: Option<Vec<CMExpression>>,
    },
    Loop {
        block: Vec<CMExpression>,
    },
    LoopFor {
        loop_var: (IdentStr, CMValueType),
        iterable: Box<CMExpression>,
        block: Vec<CMExpression>,
    },
    LoopBreak(Option<Box<CMExpression>>),
    LoopContinue,

    Return(Option<Box<CMExpression>>),
}

pub struct CommonModule {
    functions: Vec<CMFunction>,
    closure_functions: Vec<CMClosureFunction>,
    classes: Vec<CMClass>,

    top_level: Vec<CMExpression>,
}
