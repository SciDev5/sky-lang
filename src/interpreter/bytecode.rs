use num::complex::Complex64;

use crate::{
    common::{common_module::CMType, IdentInt},
    parse::fn_lookup::{Intrinsic1FnId, Intrinsic2FnId, IntrinsicNFnId},
};

#[derive(Debug)]
pub enum Literal {
    Int(i128),
    Float(f64),
    Complex(Complex64),
    Bool(bool),
    String(String),
}

#[derive(Debug)]
pub enum Instr {
    Call {
        function_id: IdentInt,
    },
    CallIntrinsic1 {
        function_id: Intrinsic1FnId,
    },
    CallIntrinsic2 {
        function_id: Intrinsic2FnId,
    },
    CallIntrinsicN {
        function_id: IntrinsicNFnId,
    },
    LiteralFunctionRef {
        function_id: IdentInt,
    },
    CallDyn,
    /// `[..., obj] -> invalidated`
    Return,

    /// `[...] -> [...], []`
    PushScope,
    /// `[...<keep>], [...<discard>, value] -> [...<keep>, value]`
    PopScope,
    /// `[...<keep>], [...<discard>] -> [...<keep>]`
    DiscardScope,
    /// `[...<keep>], [...<discard>] -> [...<keep>]`
    ResetScope,

    /// `[..., obj] -> [..., value]`
    ReadProp(IdentInt),
    /// `[..., obj, value] -> [...]`
    WriteProp(IdentInt),

    /// `[...] -> [..., value]`
    ReadLocal(IdentInt),
    /// `[..., value] -> [...]`
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
    /// `[..., ...props] -> [..., value]`
    LiteralInitStruct(Vec<IdentInt>),
}

pub struct BFunction {
    pub params: Vec<CMType>,
    pub locals: Vec<CMType>,

    pub code: Vec<Instr>,
}
pub struct BStruct {
    pub fields: Vec<CMType>,
}

pub struct BytecodeModule {
    pub functions: Vec<BFunction>,
    pub structs: Vec<BStruct>,

    pub top_level: (Vec<Instr>, Vec<CMType>),
}

#[macro_export]
macro_rules! dbg_bytecode_module_code {
    ($bytecode: expr) => {{
        println!("BytecodeModule::debug_code [{}:{}]", file!(), line!());
        for (i, func) in $bytecode.functions.iter().enumerate() {
            println!("fns[{}]", i);
            for (i, instr) in func.code.iter().enumerate() {
                println!("{} | {:?}", i, instr);
            }
        }
        println!("top_level");
        for (i, instr) in $bytecode.top_level.0.iter().enumerate() {
            println!("{} | {:?}", i, instr);
        }
        println!()
    }};
}
