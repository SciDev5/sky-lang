use num::traits::Pow;

use crate::{
    interpreter::interpreter::PanicGen,
    parse::fn_lookup::{Intrinsic1FnId, Intrinsic2FnId, IntrinsicFnId},
};

use super::value::Value;

pub fn apply_intrinsic1(id: Intrinsic1FnId, value: Value) -> Value {
    use Value::*;
    match id {
        Intrinsic1FnId::IntMinus => Int(-value.into_int()),
        Intrinsic1FnId::IntBitNot => Int(!value.into_int()),
        Intrinsic1FnId::FloatMinus => Float(-value.into_float()),
        Intrinsic1FnId::ComplexMinus => Complex(-value.into_complex()),
        Intrinsic1FnId::ComplexConj => Complex(value.into_complex().conj()),
        Intrinsic1FnId::BoolNot => Bool(!value.into_bool()),
    }
}
pub fn apply_intrinsic2(id: Intrinsic2FnId, lhs: Value, rhs: Value) -> Result<Value, PanicGen> {
    use Value::*;
    Ok(match id {
        Intrinsic2FnId::PrimitiveEquals => Bool(lhs == rhs),
        Intrinsic2FnId::PrimitiveNotEquals => Bool(lhs != rhs),
        Intrinsic2FnId::IntLessEqual => Bool(lhs.into_int() <= rhs.into_int()),
        Intrinsic2FnId::IntLessThan => Bool(lhs.into_int() < rhs.into_int()),
        Intrinsic2FnId::IntGreaterEqual => Bool(lhs.into_int() >= rhs.into_int()),
        Intrinsic2FnId::IntGreaterThan => Bool(lhs.into_int() > rhs.into_int()),

        Intrinsic2FnId::FloatLessEqual => Bool(lhs.into_float() <= rhs.into_float()),
        Intrinsic2FnId::FloatLessThan => Bool(lhs.into_float() < rhs.into_float()),
        Intrinsic2FnId::FloatGreaterEqual => Bool(lhs.into_float() >= rhs.into_float()),
        Intrinsic2FnId::FloatGreaterThan => Bool(lhs.into_float() > rhs.into_float()),

        Intrinsic2FnId::IntPlus => Int(lhs.into_int() + rhs.into_int()),
        Intrinsic2FnId::IntMinus => Int(lhs.into_int() - rhs.into_int()),
        Intrinsic2FnId::IntTimes => Int(lhs.into_int() * rhs.into_int()),
        Intrinsic2FnId::IntDiv => Int(lhs.into_int() / rhs.into_int()),
        Intrinsic2FnId::IntPow => Int({
            let lhs = lhs.into_int();
            let rhs: u32 = rhs.into_int().try_into().map_err(|_| PanicGen)?;

            if rhs == 0 && lhs == 0 {
                // 0 ^ 0
                return Err(PanicGen);
            } else {
                lhs.pow(rhs)
            }
        }),

        Intrinsic2FnId::IntBitXor => Int(lhs.into_int() ^ rhs.into_int()),
        Intrinsic2FnId::IntBitAnd => Int(lhs.into_int() & rhs.into_int()),
        Intrinsic2FnId::IntBitOr => Int(lhs.into_int() | rhs.into_int()),

        Intrinsic2FnId::FloatPlus => Float(lhs.into_float() + rhs.into_float()),
        Intrinsic2FnId::FloatMinus => Float(lhs.into_float() - rhs.into_float()),
        Intrinsic2FnId::FloatTimes => Float(lhs.into_float() * rhs.into_float()),
        Intrinsic2FnId::FloatDiv => Float(lhs.into_float() / rhs.into_float()),
        Intrinsic2FnId::FloatPow => Float({
            let lhs = lhs.into_float();
            let rhs = rhs.into_float();
            if lhs < 0.0 && rhs.fract() != 0.0 || lhs == 0.0 && rhs == 0.0 {
                // 0^0 or root of negative
                return Err(PanicGen);
            } else {
                lhs.pow(rhs)
            }
        }),

        Intrinsic2FnId::ComplexPlus => Complex(lhs.into_complex() + rhs.into_complex()),
        Intrinsic2FnId::ComplexMinus => Complex(lhs.into_complex() - rhs.into_complex()),
        Intrinsic2FnId::ComplexTimes => Complex(lhs.into_complex() * rhs.into_complex()),
        Intrinsic2FnId::ComplexDiv => Complex(lhs.into_complex() / rhs.into_complex()),
        Intrinsic2FnId::ComplexPow => todo!(),

        Intrinsic2FnId::BoolAnd => Bool(lhs.into_bool() && rhs.into_bool()),
        Intrinsic2FnId::BoolXor => Bool(lhs.into_bool() ^ rhs.into_bool()),
        Intrinsic2FnId::BoolOr => Bool(lhs.into_bool() || rhs.into_bool()),
    })
}
pub fn apply_intrinsic(id: IntrinsicFnId, _values: Vec<Value>) -> Result<Value, PanicGen> {
    match id {}
}
