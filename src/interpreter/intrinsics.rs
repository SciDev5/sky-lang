use std::{collections::HashMap, ops::Not};

use crate::{
    common::common_module::{CMType, CMValueType},
    interpreter::value::{ValueBool, ValueComplex, ValueFloat, ValueInt},
    parse::{
        fn_lookup::{AssociatedFnLut, FnRef, IntrinsicAssociatedFnLuts},
        ops::SLOperator,
    },
};

use super::value::Value;

type Args = Vec<Value>;
macro_rules! unpack_args {
    ($args: expr; $($id: ident : $ty: ty),* $(,)?) => {
        let [ $( ref $id ),* ] = $args [ .. ] else { panic!("intrinsic function received incorrect number of arguments") };
        $(
            let $id = unsafe { value_contents_into::<$ty>(& $id) };
        )*
    };
}

mod rhs_val {
    use crate::{
        common::common_module::CMValueType,
        interpreter::value::{ValueBool, ValueComplex, ValueFloat, ValueInt, ValueString},
    };

    pub trait RhsTy {
        const TY_VAL: CMValueType;
        type Ty;
    }
    // int
    pub struct RhsInt;
    impl RhsTy for RhsInt {
        const TY_VAL: CMValueType = CMValueType::Int;
        type Ty = ValueInt;
    }
    // float
    pub struct RhsFloat;
    impl RhsTy for RhsFloat {
        const TY_VAL: CMValueType = CMValueType::Float;
        type Ty = ValueFloat;
    }
    // complex
    pub struct RhsComplex;
    impl RhsTy for RhsComplex {
        const TY_VAL: CMValueType = CMValueType::Complex;
        type Ty = ValueComplex;
    }
    // bool
    pub struct RhsBool;
    impl RhsTy for RhsBool {
        const TY_VAL: CMValueType = CMValueType::Bool;
        type Ty = ValueBool;
    }
    // string
    pub struct RhsString;
    impl RhsTy for RhsString {
        const TY_VAL: CMValueType = CMValueType::String;
        type Ty = ValueString;
    }
}

macro_rules! gen_ops_binary {
    (
        lhs ( $ty_lhs: ty );
        [
            $(( $rhs: ty) [
                $(
                    ($op: expr) { $v_lhs: ident, $v_rhs: ident -> $ret: expr } $block: block
                ),* $(,)?
            ]),* $(,)?
        ]
    ) => {
        {
            HashMap::from([
                $(
                    $(
                        {
                            use rhs_val::*;
                            (
                                ({
                                    use SLOperator::*;
                                    $op
                                }, <$rhs> :: TY_VAL),
                                FnRef::Intrinsic(InterpreterIntrinsicFn(|args| {
                                    type TyRhs = <$rhs as RhsTy> :: Ty;

                                    unpack_args!(args; $v_lhs : $ty_lhs, $v_rhs : TyRhs);
                                    $block
                                }), {
                                    use CMValueType::*;
                                    CMType::Value($ret)
                                }),
                            )
                        }
                    ),*,
                ),*
            ])
        }
    };
}
macro_rules! gen_ops_unary {
    (
        lhs ( $ty_lhs: ty );
        [
            $(
                ($op: expr) { $v: ident -> $ret: expr } $block: block
            ),* $(,)?
        ]
    ) => {
        {
            HashMap::from([
                $(
                    {
                        (
                            {
                                use SLOperator::*;
                                $op
                            },
                            FnRef::Intrinsic(InterpreterIntrinsicFn(|args| {
                                unpack_args!(args; $v : $ty_lhs);
                                $block
                            }), {
                                use CMValueType::*;
                                CMType::Value($ret)
                            }),
                        )
                    }
                ),*
            ])
        }
    };
}

#[inline(always)]
unsafe fn value_contents_into<T>(v: &Value) -> T {
    const VALUE_DISCRIMINANT_SIZE: usize = std::mem::size_of::<std::mem::Discriminant<Value>>();
    std::mem::transmute_copy(
        (v as *const Value)
            .add(VALUE_DISCRIMINANT_SIZE)
            .as_ref()
            .unwrap_unchecked(),
    )
}

#[derive(Debug, Clone, Copy)]
pub struct InterpreterIntrinsicFn(fn(Args) -> Value);

pub fn interpreter_gen_intrinsics() -> IntrinsicAssociatedFnLuts<InterpreterIntrinsicFn> {
    IntrinsicAssociatedFnLuts {
        int: AssociatedFnLut {
            op_binary: gen_ops_binary!(lhs (ValueInt); [
                (RhsInt) [
                    (Plus) { l, r -> Int } { Value::Int(l + r) },
                    (Minus) { l, r -> Int } { Value::Int(l - r) },
                    (Equal) { l, r -> Bool } { Value::Bool(l == r) },
                    (NotEqual) { l, r -> Bool } { Value::Bool(l != r) },
                ],
            ]),
            op_binary_rhs: HashMap::from([]),
            op_unary: gen_ops_unary!(lhs (ValueInt); [
                (Plus) { v -> Int } { Value::Int(v) },
                (Minus) { v -> Int } { Value::Int(-v) },
                (BitXor) { v -> Int } { Value::Int(v.not()) },
                (HermitianConjugate) { v -> Int } { Value::Int(v) },
                (Transpose) { v -> Int } { Value::Int(v) },
            ]),

            named: HashMap::from([]),
            op_call: vec![],
            op_index: vec![],
        },
        float: AssociatedFnLut {
            op_binary: gen_ops_binary!(lhs (ValueFloat); [
                (RhsFloat) [
                    (Plus) { l, r -> Float } { Value::Float(l + r) },
                    (Minus) { l, r -> Float } { Value::Float(l - r) },
                    (Equal) { l, r -> Bool } { Value::Bool(l == r) },
                    (NotEqual) { l, r -> Bool } { Value::Bool(l != r) },
                ],
            ]),
            op_binary_rhs: HashMap::from([]),
            op_unary: gen_ops_unary!(lhs (ValueFloat); [
                (Plus) { v -> Float } { Value::Float(v) },
                (Minus) { v -> Float } { Value::Float(-v) },
                (HermitianConjugate) { v -> Float } { Value::Float(v) },
                (Transpose) { v -> Float } { Value::Float(v) },
            ]),

            named: HashMap::from([]),
            op_call: vec![],
            op_index: vec![],
        },
        complex: AssociatedFnLut {
            op_binary: gen_ops_binary!(lhs (ValueComplex); [
                (RhsComplex) [
                    (Plus) { l, r -> Complex } { Value::Complex(l + r) },
                    (Minus) { l, r -> Complex } { Value::Complex(l - r) },
                    (Equal) { l, r -> Bool } { Value::Bool(l == r) },
                    (NotEqual) { l, r -> Bool } { Value::Bool(l != r) },
                ],
            ]),
            op_binary_rhs: HashMap::from([]),
            op_unary: gen_ops_unary!(lhs (ValueComplex); [
                (Plus) { v -> Complex } { Value::Complex(v) },
                (Minus) { v -> Complex } { Value::Complex(-v) },
                (HermitianConjugate) { v -> Complex } { Value::Complex(v.conj()) },
                (Transpose) { v -> Complex } { Value::Complex(v) },
            ]),

            named: HashMap::from([]),
            op_call: vec![],
            op_index: vec![],
        },
        bool: AssociatedFnLut {
            op_binary: gen_ops_binary!(lhs (ValueBool); [
                (RhsBool) [
                    (And) { l, r -> Bool } { Value::Bool(l && r) },
                    (Or) { l, r -> Bool } { Value::Bool(l || r) },
                    (Xor) { l, r -> Bool } { Value::Bool(l ^ r) },
                    (Equal) { l, r -> Bool } { Value::Bool(l == r) },
                    (NotEqual) { l, r -> Bool } { Value::Bool(l != r) },
                ],
            ]),
            op_binary_rhs: HashMap::from([]),
            op_unary: gen_ops_unary!(lhs (ValueBool); [
                (Not) { v -> Bool } { Value::Bool(v) },
            ]),

            named: HashMap::from([]),
            op_call: vec![],
            op_index: vec![],
        },
        string: AssociatedFnLut::empty(), // TODO string functions
        empty_lut: AssociatedFnLut::empty(),
    }
}
