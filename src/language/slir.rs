use crate::math::tensor::Tensor;

use super::ops::SLOperator;

pub type SLIRIdent = Box<str>;

#[derive(Debug, Clone)]
pub enum SLIRVarAccessExpression {
    Read(SLIRIdent),
    Index {
        expr: Box<SLIRVarAccessExpression>,
        indices: Vec<SLIRExpression>,
    },
    PropertyAccess {
        expr: Box<SLIRVarAccessExpression>,
        property_ident: SLIRIdent,
    },
}

#[derive(Debug, Clone)]
pub enum SLIRExpression {
    Read(SLIRIdent),
    Call {
        callable: Box<SLIRExpression>,
        arguments: Vec<SLIRExpression>,
    },
    Index {
        expr: Box<SLIRExpression>,
        indices: Vec<SLIRExpression>,
    },
    PropertyAccess {
        expr: Box<SLIRExpression>,
        property_ident: SLIRIdent,
    },

    Literal(SLIRLiteral),
    Range {
        start: Option<Box<SLIRExpression>>,
        step: Option<Box<SLIRExpression>>,
        end: Option<Box<SLIRExpression>>,
    },
    Array(SLIRArray),
    AnonymousFunction {
        params: Vec<SLIRIdent>,
        block: SLIRBlock,
    },

    BinaryOp(SLOperator, Box<SLIRExpression>, Box<SLIRExpression>),
    UnaryOp(SLOperator, Box<SLIRExpression>),

    Conditional {
        condition: Box<SLIRExpression>,
        block: SLIRBlock,
        elifs: Vec<(SLIRExpression, SLIRBlock)>,
        else_block: Option<SLIRBlock>,
    },
    Loop(SLIRBlock),
    For {
        loop_var: SLIRIdent,
        iterable: Box<SLIRExpression>,
        block: SLIRBlock,
    },
    Break(Option<Box<SLIRExpression>>),
    Continue,
    Return(Box<SLIRExpression>),
}

impl Default for SLIRExpression {
    fn default() -> Self {
        SLIRExpression::Literal(SLIRLiteral::Int { re: 0, im: 0 })
    }
}

#[derive(Debug, Clone)]
pub struct SLIRBlock(pub Vec<SLIRStatement>);

#[derive(Debug, Clone)]
pub enum SLIRStatement {
    FunctionDefinition {
        doc_comment: Option<String>,
        ident: SLIRIdent,
        params: Vec<SLIRIdent>,
        block: SLIRBlock,
    },

    VarDeclare {
        doc_comment: Option<String>,
        ident: SLIRIdent,
        writable: bool,
        initial_assignment: Option<Box<SLIRExpression>>,
    },
    Assign(SLIRVarAccessExpression, Box<SLIRExpression>),

    Expr(Box<SLIRExpression>),
}

#[derive(Debug, Clone)]
pub enum SLIRLiteral {
    Int { re: i128, im: i128 },
    Float { re: f64, im: f64 },
    Bool(bool),
    String(String),
}

#[derive(Debug, Clone)]
pub enum SLIRArray {
    List(Vec<SLIRExpression>),
    Matrix(Tensor<SLIRExpression>),
    Tensor(Tensor<SLIRExpression>),
}
