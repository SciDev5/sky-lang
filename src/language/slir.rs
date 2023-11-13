use crate::math::tensor::Tensor;

use super::ops::SLOperator;

pub type SLIRIdent = Box<str>;

#[derive(Debug, Clone)]
pub enum SLIRVarAccessExpression {
    Ident(SLIRIdent),
}

#[derive(Debug, Clone)]
pub enum SLIRExpression {
    VarRead(SLIRVarAccessExpression),
    BinaryOp(SLOperator, Box<SLIRExpression>, Box<SLIRExpression>),
    UnaryOp(SLOperator, Box<SLIRExpression>),
    Literal(SLIRLiteral),
    Array(SLIRArray),
}

impl Default for SLIRExpression {
    fn default() -> Self {
        SLIRExpression::Literal(SLIRLiteral::Int { re: 0, im: 0 })
    }
}

#[derive(Debug, Clone)]
pub enum SLIRStatement {
    VarDeclare(SLIRIdent),
    VarAssign(SLIRVarAccessExpression, Box<SLIRExpression>),
    Expr(Box<SLIRExpression>),
}

#[derive(Debug, Clone)]
pub enum SLIRLiteral {
    Int { re: i128, im: i128 },
    Float { re: f64, im: f64 },
}

#[derive(Debug, Clone)]
pub enum SLIRArray {
    List(Vec<SLIRExpression>),
    Matrix(Tensor<SLIRExpression>),
    Tensor(Tensor<SLIRExpression>),
}
