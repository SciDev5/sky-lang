use std::fmt::Debug;

use num::complex::Complex64;

use crate::{
    common::IdentStr,
    math::tensor::Tensor,
};

use super::{raw_module::{RMType, RMValueType}, ops::SLOperator};

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub enum ASTExpression {
    VarDeclare {
        doc_comment: Option<String>,
        ident: IdentStr,
        writable: bool,
        initial_value: Option<Box<ASTExpression>>,
        ty: Option<RMValueType>,
    },
    Assign(ASTVarAccessExpression, Box<ASTExpression>),
    Read(IdentStr),
    Call {
        callable: Box<ASTExpression>,
        arguments: Vec<ASTExpression>,
    },
    Index {
        expr: Box<ASTExpression>,
        indices: Vec<ASTExpression>,
    },
    PropertyAccess {
        expr: Box<ASTExpression>,
        property_ident: IdentStr,
    },

    Literal(ASTLiteral),
    Range {
        start: Option<Box<ASTExpression>>,
        step: Option<Box<ASTExpression>>,
        end: Option<Box<ASTExpression>>,
    },
    Array(ASTArray),
    AnonymousFunction {
        params: Vec<(IdentStr, Option<RMValueType>)>,
        block: ASTBlock,
    },

    BinaryOp {
        op: SLOperator,
        lhs: Box<ASTExpression>,
        rhs: Box<ASTExpression>,
    },
    UnaryOp {
        op: SLOperator,
        value: Box<ASTExpression>,
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
    For {
        loop_var: (IdentStr, Option<RMValueType>),
        iterable: Box<ASTExpression>,
        block: ASTBlock,
    },
    Break {
        value: Option<Box<ASTExpression>>,
    },
    Continue,

    Return {
        value: Option<Box<ASTExpression>>,
    },
    FunctionDefinition {
        doc_comment: Option<String>,
        ident: IdentStr,
        params: Vec<(IdentStr, RMValueType)>,
        return_ty: Option<RMType>,
        block: ASTBlock,
    },
}

impl Default for ASTExpression {
    fn default() -> Self {
        ASTExpression::Literal(ASTLiteral::Int(0))
    }
}

pub type ASTBlock = Vec<ASTExpression>;

#[derive(Debug, Clone)]
pub enum ASTLiteral {
    Int(i128),
    Float(f64),
    Complex(Complex64),
    Bool(bool),
    String(String),
}

#[derive(Debug, Clone)]
pub enum ASTArray {
    List(Vec<ASTExpression>),
    Matrix(Tensor<ASTExpression>),
    Tensor(Tensor<ASTExpression>),
}
