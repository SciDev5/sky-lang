use num::complex::Complex64;


pub type ValueInt = i128;
pub type ValueFloat = f64;
pub type ValueComplex = Complex64;
pub type ValueBool = bool;
pub type ValueString = String;

#[derive(Debug, Clone)]
pub enum Value {
    Void,
    Int(ValueInt),
    Float(ValueFloat),
    Complex(ValueComplex),
    Bool(ValueBool),
    String(ValueString),
    Object(Vec<Value>),
}
