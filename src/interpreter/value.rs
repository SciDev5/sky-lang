use std::fmt::Debug;

use num::complex::Complex64;

pub type ValueInt = i128;
pub type ValueFloat = f64;
pub type ValueComplex = Complex64;
pub type ValueBool = bool;
pub type ValueString = String;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Void,
    Int(ValueInt),
    Float(ValueFloat),
    Complex(ValueComplex),
    Bool(ValueBool),
    String(ValueString),
    Object(Vec<Value>),
}

macro_rules! impl_cast {
    ($v: ident, $pat: pat => $vout: expr) => {{
        use Value::*;
        match $v {
            $pat => $vout,
            v => {
                dbg!(v);
                panic!("type conversion failed")
            },
        }
    }};
}
impl Value {
    #[inline(always)]
    pub fn into_int(self) -> ValueInt {
        impl_cast!(self, Int(v) => v)
    }
    #[inline(always)]
    pub fn into_float(self) -> ValueFloat {
        impl_cast!(self, Float(v) => v)
    }
    #[inline(always)]
    pub fn into_complex(self) -> ValueComplex {
        impl_cast!(self, Complex(v) => v)
    }
    #[inline(always)]
    pub fn into_bool(self) -> ValueBool {
        impl_cast!(self, Bool(v) => v)
    }
}
