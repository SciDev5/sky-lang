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
