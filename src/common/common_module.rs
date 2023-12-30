use std::{collections::HashMap, fmt::Debug};

use num::complex::Complex64;

use crate::{
    common::{IdentInt, IdentStr},
    math::tensor::Tensor,
};

#[derive(Debug, Clone)]
pub struct CMLocalVarInfo {
    pub doc_comment: Option<String>,
    pub ty: CMType,
    pub writable: bool,
}

pub struct CMFunction<Cfg: CMCfg> {
    pub doc_comment: Option<String>,
    pub params: Vec<CMValueType>,
    /// local variables, including auto-generated parameter locals
    pub locals: Vec<CMLocalVarInfo>,
    pub ty_return: CMType,
    pub block: Vec<CMExpression<Cfg>>,
}
pub struct CMClosureFunction<Cfg: CMCfg> {
    params: Vec<CMValueType>,
    captures: Vec<IdentInt>,
    /// local variables, including auto-generated parameter locals
    locals: Vec<CMLocalVarInfo>,
    return_ty: CMType,
    block: Vec<CMExpression<Cfg>>,
}
#[derive(Debug, Clone)]
pub struct CMInlineLambda<Cfg: CMCfg> {
    params: Vec<CMValueType>,
    captures: Vec<IdentInt>,
    /// local variables, including auto-generated parameter locals
    locals: Vec<CMLocalVarInfo>,
    block: Vec<CMExpression<Cfg>>,
}
pub struct CMClass {
    pub doc_comment: Option<String>,
    pub fields: HashMap<IdentStr, CMValueType>,
    pub functions: HashMap<IdentStr, Vec<IdentInt>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum CMValueType {
    Int,
    Float,
    Complex,
    Bool,
    String,
    FunctionRef {
        params: Vec<CMValueType>,
        return_ty: Box<CMType>,
    },
    ClassRef(IdentInt),
    ClassInstance(IdentInt),
    Tuple(Vec<CMValueType>),
    // List(Box<RMValueType>),
    // TODO Template types
    // TODO Units / dimensional analysis
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum CMType {
    Void,
    Never,
    Value(CMValueType),
}
impl CMType {
    pub fn is_never(&self) -> bool {
        matches!(self, Self::Never)
    }
}

#[derive(Debug, Clone)]
pub enum CMLiteralValue {
    Int(i128),
    Float(f64),
    Complex(Complex64),
    Bool(bool),
    String(String),
}
#[derive(Debug, Clone)]
pub enum CMLiteralArray<Cfg: CMCfg> {
    List(Vec<CMExpression<Cfg>>),
    Matrix(Tensor<CMExpression<Cfg>>),
    Tensor(Tensor<CMExpression<Cfg>>),
}

pub trait CMCfg : Debug + Clone + Copy {
    type IntrinsicFnRef: Debug + Clone;
}

#[derive(Debug, Clone)]
pub enum CMExpression<Cfg: CMCfg> {
    Void,
    Fail, // TODO put references to the causing errors in CMExpression::Fail

    AssignProperty {
        object: Box<CMExpression<Cfg>>,
        property: IdentInt,
        value: Box<CMExpression<Cfg>>,
    },
    AssignVar {
        ident: IdentInt,
        value: Box<CMExpression<Cfg>>,
    },
    ReadProperty {
        expr: Box<CMExpression<Cfg>>,
        property_ident: IdentInt,
    },
    ReadVar {
        ident: IdentInt,
    },

    Call {
        function_id: IdentInt,
        arguments: Vec<CMExpression<Cfg>>,
        always_inline: bool,
        inlined_lambdas: Option<Vec<CMInlineLambda<Cfg>>>,
    },
    CallIntrinsic {
        function_ref: Cfg::IntrinsicFnRef,
        arguments: Vec<CMExpression<Cfg>>,
    },

    LiteralValue(CMLiteralValue),
    LiteralArray(CMLiteralArray<Cfg>),
    LiteralFunctionRef {
        function_id: IdentInt,
    },

    Closure {
        closure_function_id: IdentInt,
    },

    Conditional {
        condition: Box<CMExpression<Cfg>>,
        block: Vec<CMExpression<Cfg>>,
        elifs: Vec<(CMExpression<Cfg>, Vec<CMExpression<Cfg>>)>,
        else_block: Option<Vec<CMExpression<Cfg>>>,
    },
    Loop {
        block: Vec<CMExpression<Cfg>>,
    },
    LoopFor {
        loop_var: (IdentStr, CMValueType),
        iterable: Box<CMExpression<Cfg>>,
        block: Vec<CMExpression<Cfg>>,
    },
    LoopBreak(Option<Box<CMExpression<Cfg>>>),
    LoopContinue,

    Return(Option<Box<CMExpression<Cfg>>>),
}

pub struct CommonModule<Cfg: CMCfg> {
    pub functions: Vec<CMFunction<Cfg>>,
    pub closure_functions: Vec<CMClosureFunction<Cfg>>,
    pub classes: Vec<CMClass>,

    pub top_level: (Vec<CMExpression<Cfg>>, Vec<CMLocalVarInfo>, CMType),
}
