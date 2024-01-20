use std::fmt::Debug;

use num::complex::Complex64;

use crate::{
    common::{common_module::DocComment, IdentStr},
    math::tensor::Tensor,
};

use super::{
    ops::SLOperator,
    raw_module::RMType,
};

#[derive(Debug, Clone, PartialEq)]
pub enum ASTVarAccessExpression {
    Var {
        ident: IdentStr,
    },
    Index {
        object: Box<ASTExpression>,
        indices: Vec<ASTExpression>,
    },
    PropertyAccess {
        object: Box<ASTExpression>,
        property_ident: IdentStr,
    },
}

/*

"a + b" ok

"a(b)" ambiguous, either tuple struct init or function call, but can be handled later in raw_2_common

ambiguous, either struct init or function call and needs to be parsed differently in each case:
    "a { b }"

non-ambiguous if ranges not allowed on top level:
    "a { b: c }"

non-ambiguous:
    "a { -> b: c }"
    "a { 3 }"
    "a() { b: c }"
    "a { b, c }"

*/

#[derive(Debug, Clone, PartialEq)]
pub struct ASTOptionallyTypedIdent {
    pub ident: IdentStr,
    pub ty: Option<RMType>,
}
#[derive(Debug, Clone, PartialEq)]
pub struct ASTTypedIdent {
    pub ident: IdentStr,
    pub ty: RMType,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTAnonymousFunction {
    pub params: Option<Vec<ASTOptionallyTypedIdent>>,
    pub block: ASTBlock,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTBlockStructInit {
    pub properties: Vec<(IdentStr, ASTExpression)>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ASTCompoundPostfixContents {
    /// Function calls
    /// > `target (arg_a, arg_b, arc_c, ...)`
    /// > `target (arg_a, arg_b, arc_c, ...) { param_a, param_b, ... -> code_block }`
    /// > `target { param_a, param_b, ... -> code_block }`
    Call(Vec<ASTExpression>, Option<ASTAnonymousFunction>),
    /// Tuple struct initializations.
    /// > `target.(arg_a, arg_b, arc_c, ...)`
    TupleStructInit(Vec<ASTExpression>),
    /// Struct initialization
    /// > `struct_ident.{ property_a: expr_a, property_b, ... }`
    BlockStructInit(ASTBlockStructInit),
    /// Indexing call
    /// > `target [arg_a, arg_b, ...]`
    Index(Vec<ASTExpression>),
    /// Property access
    /// > `target. property``
    PropertyAccess(IdentStr),
}

#[derive(Debug, Clone, PartialEq)]
pub enum ASTExpression {
    VarDeclare {
        doc_comment: DocComment,
        ident: IdentStr,
        writable: bool,
        initial_value: Option<Box<ASTExpression>>,
        ty: Option<RMType>,
    },
    Assign {
        target: ASTVarAccessExpression,
        op: Option<SLOperator>,
        value: Box<ASTExpression>,
    },

    Ident(IdentStr),

    Literal(ASTLiteral),
    Range {
        start: Option<Box<ASTExpression>>,
        step: Option<Box<ASTExpression>>,
        end: Option<Box<ASTExpression>>,
    },
    Array(ASTArray),
    AnonymousFunction(ASTAnonymousFunction),

    BinaryOp {
        op: SLOperator,
        lhs: Box<ASTExpression>,
        rhs: Box<ASTExpression>,
    },
    UnaryOp {
        op: SLOperator,
        value: Box<ASTExpression>,
    },
    CompoundPostfix {
        target: Box<ASTExpression>,
        contents: ASTCompoundPostfixContents,
    },

    Conditional {
        condition: Box<ASTExpression>,
        block: ASTBlock,
        elifs: Vec<(ASTExpression, ASTBlock)>,
        else_block: Option<ASTBlock>,
    },
    Loop {
        block: ASTBlock,
    },
    LoopWhile {
        condition: Box<ASTExpression>,
        block: ASTBlock,
    },
    For {
        loop_var: (IdentStr, Option<RMType>),
        iterable: Box<ASTExpression>,
        block: ASTBlock,
    },
    Break(Option<Box<ASTExpression>>),
    Continue,

    Return(Option<Box<ASTExpression>>),
    FunctionDefinition {
        doc_comment: DocComment,
        ident: IdentStr,
        params: Vec<ASTTypedIdent>,
        return_ty: Option<RMType>,
        block: ASTBlock,
    },

    StructDefinition {
        doc_comment: DocComment,
        ident: IdentStr,
        properties: Vec<(IdentStr, DocComment, RMType)>,
        // TODO associated functionality
    },
}

pub type ASTBlock = Vec<ASTExpression>;

#[derive(Debug, Clone, PartialEq)]
pub enum ASTLiteral {
    Int(i128),
    Float(f64),
    Complex(Complex64),
    Bool(bool),
    String(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum ASTArray {
    List(Vec<ASTExpression>),
    Matrix(Tensor<ASTExpression>),
    Tensor(Tensor<ASTExpression>),
}
