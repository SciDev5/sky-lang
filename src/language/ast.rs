use std::{fmt::Debug, collections::HashMap};

use num::complex::Complex64;

use crate::{interpreter::{data::{Type, Function, VoidableType}, module::ModuleComponentId}, math::tensor::Tensor};

use super::ops::SLOperator;

pub type ASTIdent = Box<str>;

#[derive(Debug, Clone)]
pub enum ASTVarAccessExpression {
    Var(ASTIdent),
    Index {
        expr: Box<ASTExpression>,
        indices: Vec<ASTExpression>,
    },
    PropertyAccess {
        expr: Box<ASTExpression>,
        property_ident: ASTIdent,
    },
}

pub trait ASTTypes {
    type ValueType: Debug + Clone;
    type VoidableType: Debug + Clone;
    type PropertyId: Debug + Clone + Copy;
    fn ty_void() -> Self::VoidableType;
    fn value_as_voidable(ty: Self::ValueType) -> Self::VoidableType;
    fn wrap(ty: Type) -> Self::ValueType;
    fn wrap_voidable(ty: Type) -> Self::VoidableType {
        Self::value_as_voidable(Self::wrap(ty))
    }
}

#[derive(Debug, Clone, Copy)]
pub struct ASTTypesFull;
#[derive(Debug, Clone, Copy)]
pub struct ASTTypesIncomplete;
impl ASTTypes for ASTTypesFull {
    type ValueType = Type;
    type VoidableType = Option<Type>;
    type PropertyId = ModuleComponentId;
    fn ty_void() -> Self::VoidableType {
        None
    }
    fn value_as_voidable(ty: Self::ValueType) -> Self::VoidableType {
        Some(ty)
    }
    fn wrap(ty: Type) -> Self::ValueType {
        ty
    }
}
impl ASTTypes for ASTTypesIncomplete {
    type ValueType = Option<Type>;
    type VoidableType = Option<Option<Type>>;
    type PropertyId = Option<ModuleComponentId>;
    fn ty_void() -> Self::VoidableType {
        Some(None)
    }
    fn value_as_voidable(ty: Self::ValueType) -> Self::VoidableType {
        ty.map(|it| Some(it))
    }
    fn wrap(ty: Type) -> Self::ValueType {
        Some(ty)
    }
}

#[derive(Debug, Clone)]
pub enum ASTExpression {
    VarDeclare {
        doc_comment: Option<String>,
        ident: ASTIdent,
        writable: bool,
        initial_assignment: Option<Box<ASTExpression>>,
        ty: Option<Type>,
    },
    Assign(ASTVarAccessExpression, Box<ASTExpression>),
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
        params: Vec<(ASTIdent, Option<Type>)>,
        block: ASTBlock,
    },

    BinaryOp(
        SLOperator,
        Box<ASTExpression>,
        Box<ASTExpression>,
    ),
    UnaryOp(SLOperator, Box<ASTExpression>),

    Conditional {
        condition: Box<ASTExpression>,
        block: ASTBlock,
        elifs: Vec<(ASTExpression, ASTBlock)>,
        else_block: Option<ASTBlock>,
    },
    Loop(ASTBlock),
    For {
        loop_var: (ASTIdent, Option<Type>),
        iterable: Box<ASTExpression>,
        block: ASTBlock,
    },
    Break(Option<Box<ASTExpression>>),
    Continue,

    Return(Option<Box<ASTExpression>>),
    FunctionDefinition {
        doc_comment: Option<String>,
        ident: ASTIdent,
        params: Vec<(ASTIdent, Type)>,
        return_ty: Option<VoidableType>,
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
pub enum SLIRArray {
    List(Vec<ASTExpression>),
    Matrix(Tensor<ASTExpression>),
    Tensor(Tensor<ASTExpression>),
}
