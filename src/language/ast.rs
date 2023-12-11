use std::fmt::Debug;

use num::complex::Complex64;

use crate::{interpreter::data::Type, math::tensor::Tensor};

use super::ops::SLOperator;

pub type ASTIdent = Box<str>;

#[derive(Debug, Clone)]
pub enum ASTVarAccessExpression<Ty: ASTTypes> {
    Var(ASTIdent, Ty::ValueType),
    Index {
        expr: Box<ASTExpression<Ty>>,
        indices: Vec<ASTExpression<Ty>>,
        associated_func_id: Option<Ty::PropertyId>,
        ty: Ty::ValueType,
    },
    PropertyAccess {
        expr: Box<ASTExpression<Ty>>,
        property_ident: ASTIdent,
        property_id: Ty::PropertyId,
        ty: Ty::ValueType,
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
    type PropertyId = u16;
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
    type PropertyId = Option<u16>;
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
pub enum ASTExpression<Ty: ASTTypes> {
    VarDeclare {
        doc_comment: Option<String>,
        ident: ASTIdent,
        writable: bool,
        initial_assignment: Option<Box<ASTExpression<Ty>>>,
        ty: Ty::ValueType,
    },
    Assign(ASTVarAccessExpression<Ty>, Box<ASTExpression<Ty>>),
    Read(ASTIdent, Ty::ValueType),
    Call {
        callable: Box<ASTExpression<Ty>>,
        arguments: Vec<ASTExpression<Ty>>,
        output_ty: Ty::VoidableType,
    },
    Index {
        expr: Box<ASTExpression<Ty>>,
        indices: Vec<ASTExpression<Ty>>,
        output_ty: Ty::ValueType,
    },
    PropertyAccess {
        expr: Box<ASTExpression<Ty>>,
        property_ident: ASTIdent,
        property_id: Ty::PropertyId,
        output_ty: Ty::ValueType,
    },
    CallAssociated {
        expr: Box<ASTExpression<Ty>>,
        property_ident: ASTIdent,
        property_id: Ty::PropertyId,
        arguments: Vec<ASTExpression<Ty>>,
        output_ty: Ty::ValueType,
    },

    Literal(ASTLiteral),
    Range {
        start: Option<Box<ASTExpression<Ty>>>,
        step: Option<Box<ASTExpression<Ty>>>,
        end: Option<Box<ASTExpression<Ty>>>,
    },
    Array(SLIRArray<Ty>),
    AnonymousFunction {
        params: Vec<(ASTIdent, Ty::ValueType)>,
        block: ASTBlock<Ty>,
    },

    BinaryOp(
        SLOperator,
        Box<ASTExpression<Ty>>,
        Box<ASTExpression<Ty>>,
        Ty::ValueType,
    ),
    UnaryOp(SLOperator, Box<ASTExpression<Ty>>, Ty::ValueType),

    Conditional {
        condition: Box<ASTExpression<Ty>>,
        block: ASTBlock<Ty>,
        elifs: Vec<(ASTExpression<Ty>, ASTBlock<Ty>)>,
        else_block: Option<ASTBlock<Ty>>,
        output_ty: Ty::VoidableType,
    },
    Loop(ASTBlock<Ty>, Ty::VoidableType),
    For {
        loop_var: (ASTIdent, Ty::ValueType),
        iterable: Box<ASTExpression<Ty>>,
        block: ASTBlock<Ty>,
    },
    Break(Option<Box<ASTExpression<Ty>>>),
    Continue,

    Return(Box<ASTExpression<Ty>>),
    FunctionDefinition {
        doc_comment: Option<String>,
        ident: ASTIdent,
        params: Vec<(ASTIdent, Type)>,
        return_ty: Ty::VoidableType,
        block: ASTBlock<Ty>,
    },
}

impl ASTExpression<ASTTypesFull> {
    pub fn eval_ty(&self) -> Option<Type> {
        const VOID: Option<Type> = None;

        match self {
            ASTExpression::VarDeclare { ty, .. } => Some(ty.clone()),
            ASTExpression::Assign(_, v) => v.eval_ty(),
            ASTExpression::Read(_, ty) => Some(ty.clone()),
            ASTExpression::Call { output_ty, .. } => output_ty.clone(),
            ASTExpression::Index { output_ty, .. } => Some(output_ty.clone()),
            ASTExpression::PropertyAccess { output_ty, .. } => {
                Some(output_ty.clone())
            }
            ASTExpression::CallAssociated { output_ty, .. } => {
                Some(output_ty.clone())
            }
            ASTExpression::Literal(literal) => Some(match literal {
                ASTLiteral::Int(_) => Type::Int(None),
                ASTLiteral::Float(_) => Type::Float(None),
                ASTLiteral::Complex(_) => Type::Complex(None),
                ASTLiteral::Bool(_) => Type::Bool,
                ASTLiteral::String(_) => Type::String,
            }),
            ASTExpression::Range { .. } => todo!(),
            ASTExpression::Array(_) => todo!(),
            ASTExpression::AnonymousFunction { params, block } => todo!(),
            ASTExpression::BinaryOp(_, _, _, ty) => Some(ty.clone()),
            ASTExpression::UnaryOp(_, _, ty) => Some(ty.clone()),
            ASTExpression::Conditional { output_ty, .. } => output_ty.clone(),
            ASTExpression::Loop(_, ty) => ty.clone(),
            ASTExpression::For {
                loop_var,
                iterable,
                block,
            } => VOID,
            ASTExpression::Break(_) => Some(Type::Never),
            ASTExpression::Continue => Some(Type::Never),
            ASTExpression::Return(_) => Some(Type::Never),
            ASTExpression::FunctionDefinition { params, return_ty, .. } => {
                Some(Type::Function(
                    params
                        .iter()
                        .map(|(_, it)| {
                        it.clone()
                        })
                        .collect::<Vec<_>>(),
                    {
                        return_ty.clone().map(Box::new)
                    },
                ))
            }
        }
    }
}

impl<Ty: ASTTypes> Default for ASTExpression<Ty> {
    fn default() -> Self {
        ASTExpression::Literal(ASTLiteral::Int(0))
    }
}

#[derive(Debug, Clone)]
pub struct ASTBlock<Ty: ASTTypes>(pub Vec<ASTExpression<Ty>>, pub Ty::VoidableType);

#[derive(Debug, Clone)]
pub enum ASTLiteral {
    Int(i128),
    Float(f64),
    Complex(Complex64),
    Bool(bool),
    String(String),
}

#[derive(Debug, Clone)]
pub enum SLIRArray<Ty: ASTTypes> {
    List(Vec<ASTExpression<Ty>>),
    Matrix(Tensor<ASTExpression<Ty>>),
    Tensor(Tensor<ASTExpression<Ty>>),
}
