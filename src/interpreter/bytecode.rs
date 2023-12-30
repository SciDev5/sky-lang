use num::complex::Complex64;

use crate::common::{common_module::{CMValueType, CMType}, IdentInt};

pub enum Literal {
    Int(i128),
    Float(f64),
    Complex(Complex64),
    Bool(bool),
    String(String),
}

pub enum Instr {
    Call { function_id: IdentInt },
    LiteralFunctionRef { function_id: IdentInt },
    CallDyn,
    /// `[..., obj] -> invalidated`
    Return,

    /// `[...] -> [...], []`
    PushScope,
    /// `[...<keep>], [...<discard>, value] -> [...<keep>, value]`
    PopScope,

    /// `[..., obj] -> [..., value]`
    ReadProp(IdentInt),
    /// `[..., obj, value] -> [..., <void>]`
    WriteProp(IdentInt),
    
    /// `[...] -> [..., value]`
    ReadLocal(IdentInt),
    /// `[..., value] -> [..., <void>]`
    WriteLocal(IdentInt),
    
    /// `[...] -> [...]`
    Jump(isize),
    /// `[..., condition] -> [...]`
    JumpIfFalse(isize),
    
    /// `[...] -> [..., <void>]`
    PushVoid,
    /// `[..., value] -> [...]`
    Discard,
    /// `[...] -> guaranteed fail, iv stack invalidated`
    Fail,
    
    /// `[...] -> [..., value]`
    Literal(Literal),
}

pub struct BFunction {
    pub params: Vec<CMValueType>,
    pub locals: Vec<CMType>,

    pub code: Vec<Instr>,
}
pub struct BClass {
    pub fields: Vec<CMValueType>,
    pub functions: Vec<IdentInt>,
}

pub struct BytecodeModule {
    pub functions: Vec<BFunction>,
    pub classes: Vec<BClass>,

    pub top_level: (Vec<Instr>, Vec<CMType>),
}