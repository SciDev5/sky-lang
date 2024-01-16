use std::fmt::Debug;

use num::complex::Complex64;

use crate::{
    common::{common_module::DocComment, IdentStr},
    math::tensor::Tensor,
};

use super::{
    ops::SLOperator,
    raw_module::{RMType, RMValueType},
};

#[derive(Debug, Clone, PartialEq)]
pub enum AST2VarAccessExpression {
    Var {
        ident: IdentStr,
    },
    Index {
        object: Box<AST2Expression>,
        indices: Vec<AST2Expression>,
    },
    PropertyAccess {
        object: Box<AST2Expression>,
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
pub struct AST2OptionallyTypedIdent {
    pub ident: IdentStr,
    pub ty: Option<RMValueType>,
}
#[derive(Debug, Clone, PartialEq)]
pub struct AST2TypedIdent {
    pub ident: IdentStr,
    pub ty: RMValueType,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AST2AnonymousFunction {
    pub params: Option<Vec<AST2OptionallyTypedIdent>>,
    pub block: AST2Block,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AST2BlockStructInit {
    pub properties: Vec<(IdentStr, DocComment, AST2Expression)>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AST2CompoundPostfixContents {
    /// Function calls
    /// > `target (arg_a, arg_b, arc_c, ...)`
    Call(Vec<AST2Expression>),
    /// Tuple struct initializations.
    /// > `target.(arg_a, arg_b, arc_c, ...)`
    TupleStructInit(Vec<AST2Expression>),
    /// Function callback block
    /// > `target_expr { param_a, param_b, ... -> code_block }`
    BlockCallback(AST2AnonymousFunction),
    /// Struct initialization
    /// > `struct_ident.{ property_a: expr_a, property_b, ... }`
    BlockStructInit(AST2BlockStructInit),
    /// Indexing call
    /// > `target [arg_a, arg_b, ...]`
    Index(Vec<AST2Expression>),
    /// Property access
    /// > `target. property``
    PropertyAccess(IdentStr),
}

#[derive(Debug, Clone, PartialEq)]
pub enum AST2Expression {
    VarDeclare {
        doc_comment: DocComment,
        ident: IdentStr,
        writable: bool,
        initial_value: Option<Box<AST2Expression>>,
        ty: Option<RMValueType>,
    },
    Assign {
        target: AST2VarAccessExpression,
        op: Option<SLOperator>,
        value: Box<AST2Expression>,
    },

    Ident(IdentStr),

    Literal(AST2Literal),
    Range {
        start: Option<Box<AST2Expression>>,
        step: Option<Box<AST2Expression>>,
        end: Option<Box<AST2Expression>>,
    },
    Array(AST2Array),
    AnonymousFunction(AST2AnonymousFunction),

    BinaryOp {
        op: SLOperator,
        lhs: Box<AST2Expression>,
        rhs: Box<AST2Expression>,
    },
    UnaryOp {
        op: SLOperator,
        value: Box<AST2Expression>,
    },
    CompoundPostfix {
        target: Box<AST2Expression>,
        contents: AST2CompoundPostfixContents,
    },

    Conditional {
        condition: Box<AST2Expression>,
        block: AST2Block,
        elifs: Vec<(AST2Expression, AST2Block)>,
        else_block: Option<AST2Block>,
    },
    Loop {
        block: AST2Block,
    },
    LoopWhile {
        condition: Box<AST2Expression>,
        block: AST2Block,
    },
    For {
        loop_var: (IdentStr, Option<RMValueType>),
        iterable: Box<AST2Expression>,
        block: AST2Block,
    },
    Break(Option<Box<AST2Expression>>),
    Continue,

    Return(Option<Box<AST2Expression>>),
    FunctionDefinition {
        doc_comment: DocComment,
        ident: IdentStr,
        params: Vec<AST2TypedIdent>,
        return_ty: Option<RMType>,
        block: AST2Block,
    },

    StructDefinition {
        doc_comment: DocComment,
        ident: IdentStr,
        properties: Vec<(IdentStr, DocComment, RMValueType)>,
        // TODO associated functionality
    },
}

pub type AST2Block = Vec<AST2Expression>;

#[derive(Debug, Clone, PartialEq)]
pub enum AST2Literal {
    Int(i128),
    Float(f64),
    Complex(Complex64),
    Bool(bool),
    String(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum AST2Array {
    List(Vec<AST2Expression>),
    Matrix(Tensor<AST2Expression>),
    Tensor(Tensor<AST2Expression>),
}
