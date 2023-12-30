use num::complex::Complex64;


#[derive(Debug, Clone)]
pub enum Value {
    Void,
    Int(i128),
    Float(f64),
    Complex(Complex64),
    Bool(bool),
    String(String),
    Object(Vec<Value>),
}
