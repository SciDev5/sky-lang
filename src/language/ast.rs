use num::complex::Complex64;

use crate::math::tensor::Tensor;

use super::ops::SLOperator;

pub type ASTIdent = Box<str>;

#[derive(Debug, Clone)]
pub enum ASTVarAccessExpression {
    Read(ASTIdent),
    Index {
        expr: Box<ASTVarAccessExpression>,
        indices: Vec<ASTExpression>,
    },
    PropertyAccess {
        expr: Box<ASTVarAccessExpression>,
        property_ident: ASTIdent,
    },
}

#[derive(Debug, Clone)]
pub enum ASTExpression {
    Read(ASTIdent),
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
        property_ident: ASTIdent,
    },

    Literal(ASTLiteral),
    Range {
        start: Option<Box<ASTExpression>>,
        step: Option<Box<ASTExpression>>,
        end: Option<Box<ASTExpression>>,
    },
    Array(SLIRArray),
    AnonymousFunction {
        params: Vec<ASTIdent>,
        block: ASTBlock,
    },

    BinaryOp(SLOperator, Box<ASTExpression>, Box<ASTExpression>),
    UnaryOp(SLOperator, Box<ASTExpression>),

    Conditional {
        condition: Box<ASTExpression>,
        block: ASTBlock,
        elifs: Vec<(ASTExpression, ASTBlock)>,
        else_block: Option<ASTBlock>,
    },
    Loop(ASTBlock),
    For {
        loop_var: ASTIdent,
        iterable: Box<ASTExpression>,
        block: ASTBlock,
    },
    Break(Option<Box<ASTExpression>>),
    Continue,
    Return(Box<ASTExpression>),
}

impl Default for ASTExpression {
    fn default() -> Self {
        ASTExpression::Literal(ASTLiteral::Int(0))
    }
}

#[derive(Debug, Clone)]
pub struct ASTBlock(pub Vec<ASTStatement>);

#[derive(Debug, Clone)]
pub enum ASTStatement {
    FunctionDefinition {
        doc_comment: Option<String>,
        ident: ASTIdent,
        params: Vec<ASTIdent>,
        block: ASTBlock,
    },

    VarDeclare {
        doc_comment: Option<String>,
        ident: ASTIdent,
        writable: bool,
        initial_assignment: Option<Box<ASTExpression>>,
    },
    Assign(ASTVarAccessExpression, Box<ASTExpression>),

    Expr(Box<ASTExpression>),
}

#[derive(Debug, Clone)]
pub enum ASTLiteral {
    Int(i128),
    Float(f64),
    Complex(Complex64),
    Bool(bool),
    String(String),
}

#[derive(Debug, Clone)]
pub enum SLIRArray {
    List(Vec<ASTExpression>),
    Matrix(Tensor<ASTExpression>),
    Tensor(Tensor<ASTExpression>),
}
