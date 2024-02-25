use std::{collections::HashMap, fmt::Debug};

use num::complex::Complex64;

use crate::{
    common::{IdentInt, IdentStr},
    math::tensor::Tensor,
    parse::fn_lookup::FnRef,
};

pub type DocComment = Option<String>;

#[derive(Debug, Clone)]
pub struct CMLocalVarInfo {
    pub doc_comment: DocComment,
    pub ty: CMType,
    pub writable: bool,
}
#[derive(Debug, Clone)]
pub struct CMFunctionInfo {
    pub doc_comment: DocComment,
    pub params: Vec<CMType>,
    pub ty_return: CMType,
}

#[derive(Debug, Clone)]
pub struct CMFunction {
    pub info: CMFunctionInfo,
    /// local variables, including auto-generated parameter locals
    pub locals: Vec<CMLocalVarInfo>,
    pub block: Vec<CMExpression>,
}
#[derive(Debug, Clone)]
pub struct CMAssociatedFunction {
    pub id: IdentInt,
    pub is_member: bool,
}

#[derive(Debug)]
pub struct CMClosureFunction {
    params: Vec<CMType>,
    captures: Vec<IdentInt>,
    /// local variables, including auto-generated parameter locals
    locals: Vec<CMLocalVarInfo>,
    return_ty: CMType,
    block: Vec<CMExpression>,
}
#[derive(Debug, Clone)]
pub struct CMInlineLambda {
    params: Vec<CMType>,
    captures: Vec<IdentInt>,
    /// local variables, including auto-generated parameter locals
    locals: Vec<CMLocalVarInfo>,
    block: Vec<CMExpression>,
}
#[derive(Debug, Clone)]
pub struct CMStruct {
    pub doc_comment: DocComment,
    pub fields: Vec<CMType>,
    pub fields_info: HashMap<IdentStr, (IdentInt, DocComment)>,
    // pub impl_functions: Vec<CMAssociatedFunction>,
    // pub impl_traits: HashMap<IdentInt, CMTraitImpl>,
}
#[derive(Debug, Clone)]
struct CMTraitImpl {
    pub trait_id: IdentInt,
    pub functions: Vec<Option<CMAssociatedFunction>>,
}
#[derive(Debug, Clone)]
struct CMTrait {
    pub doc_comment: DocComment,
    pub function_lut: HashMap<IdentStr, IdentInt>,
    pub functions: Vec<CMAssociatedFunction>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum CMType {
    Unknown,
    Never,
    Void,
    Int,
    Float,
    Complex,
    Bool,
    String,
    FunctionRef {
        params: Vec<CMType>,
        return_ty: Box<CMType>,
    },
    StructData(IdentInt),
    StructInstance(IdentInt),
    Tuple(Vec<CMType>),
    // List(Box<RMValueType>),
    // TODO Template types
    // TODO Units / dimensional analysis
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
pub enum CMLiteralArray {
    List(Vec<CMExpression>),
    Matrix(Tensor<CMExpression>),
    Tensor(Tensor<CMExpression>),
}

#[derive(Debug, Clone)]
pub enum CMExpression {
    Void,
    Fail, // TODO put references to the causing errors in CMExpression::Fail
    FailAfter(Vec<CMExpression>), // TODO put references to the causing errors in CMExpression::Fail

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
    ReadVar {
        ident: IdentInt,
    },

    Call {
        function_id: FnRef,
        arguments: Vec<CMExpression>,
        always_inline: bool,
        inlined_lambdas: Option<Vec<CMInlineLambda>>,
    },

    CallDyn {
        trait_id: IdentInt,
        function_id: IdentInt,
        arguments: Vec<CMExpression>,
        always_inline: bool,
        inlined_lambdas: Option<Vec<CMInlineLambda>>,
    },

    LiteralValue(CMLiteralValue),
    LiteralArray(CMLiteralArray),
    LiteralFunctionRef {
        function_id: IdentInt,
    },
    LiteralStructInit {
        ident: IdentInt,
        data: Vec<CMExpression>,
        assign_to: Vec<IdentInt>,
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
        is_infinite: bool,
    },
    LoopFor {
        loop_var: (IdentStr, CMType),
        iterable: Box<CMExpression>,
        block: Vec<CMExpression>,
        else_block: Option<(Vec<CMExpression>, CMType)>,
    },
    LoopWhile {
        condition: Box<CMExpression>,
        block: Vec<CMExpression>,
        else_block: Option<(Vec<CMExpression>, CMType)>,
    },
    LoopBreak(Option<Box<CMExpression>>),
    LoopContinue,

    Return(Option<Box<CMExpression>>),
}

#[derive(Debug)]
pub struct CommonModule {
    pub functions: Vec<CMFunction>,
    pub closure_functions: Vec<CMClosureFunction>,
    pub structs: Vec<CMStruct>,
    // pub traits: Vec<CMTrait>,
    pub top_level: (Vec<CMExpression>, Vec<CMLocalVarInfo>, CMType),
}
