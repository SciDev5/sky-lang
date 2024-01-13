use std::collections::HashMap;

use lazy_static::lazy_static;

use crate::common::{
    common_module::{CMType, CMValueType},
    IdentInt, IdentStr,
};

use super::ops::SLOperator;

#[derive(Debug, Clone, Copy)]
pub enum FnRef {
    ModuleFunction(IdentInt),
    // a 1 parameter function that does nothing
    Identity,
    // intrinsic function with 1 parameter
    Intrinsic1(Intrinsic1FnId),
    // intrinsic function with 2 parameters
    Intrinsic2(Intrinsic2FnId),
    // intrinsic function with some other number of parameters
    Intrinsic(IntrinsicFnId),
}

#[derive(Debug, Clone, Copy)]
pub enum Intrinsic1FnId {
    IntMinus,
    IntBitNot,

    FloatMinus,

    ComplexMinus,
    ComplexConj,

    BoolNot,
}
#[derive(Debug, Clone, Copy)]
pub enum Intrinsic2FnId {
    PrimitiveEquals,
    PrimitiveNotEquals,

    IntPlus,
    IntMinus,
    IntTimes,
    IntDiv,
    IntPow,
    IntBitXor,
    IntBitAnd,
    IntBitOr,
    IntLessEqual,
    IntLessThan,
    IntGreaterEqual,
    IntGreaterThan,

    FloatPlus,
    FloatMinus,
    FloatTimes,
    FloatDiv,
    FloatPow,
    FloatLessEqual,
    FloatLessThan,
    FloatGreaterEqual,
    FloatGreaterThan,

    ComplexPlus,
    ComplexMinus,
    ComplexTimes,
    ComplexDiv,
    ComplexPow,

    BoolAnd,
    BoolXor,
    BoolOr,
}
#[derive(Debug, Clone, Copy)]
pub enum IntrinsicFnId {}

impl Intrinsic1FnId {
    pub fn ty_ret(self) -> CMType {
        use CMType::*;
        use CMValueType::*;
        match self {
            Intrinsic1FnId::IntMinus => Value(Int),
            Intrinsic1FnId::IntBitNot => Value(Int),
            Intrinsic1FnId::FloatMinus => Value(Int),
            Intrinsic1FnId::ComplexMinus => Value(Complex),
            Intrinsic1FnId::ComplexConj => Value(Complex),
            Intrinsic1FnId::BoolNot => Value(Bool),
        }
    }
}
impl Intrinsic2FnId {
    pub fn ty_ret(self) -> CMType {
        use CMType::*;
        use CMValueType::*;
        match self {
            Intrinsic2FnId::PrimitiveEquals => Value(Bool),
            Intrinsic2FnId::PrimitiveNotEquals => Value(Bool),
            Intrinsic2FnId::IntPlus => Value(Int),
            Intrinsic2FnId::IntMinus => Value(Int),
            Intrinsic2FnId::IntTimes => Value(Int),
            Intrinsic2FnId::IntDiv => Value(Int),
            Intrinsic2FnId::IntPow => Value(Int),
            Intrinsic2FnId::IntBitXor => Value(Int),
            Intrinsic2FnId::IntBitAnd => Value(Int),
            Intrinsic2FnId::IntBitOr => Value(Int),
            Intrinsic2FnId::IntLessEqual => Value(Bool),
            Intrinsic2FnId::IntLessThan => Value(Bool),
            Intrinsic2FnId::IntGreaterEqual => Value(Bool),
            Intrinsic2FnId::IntGreaterThan => Value(Bool),
            Intrinsic2FnId::FloatPlus => Value(Float),
            Intrinsic2FnId::FloatMinus => Value(Float),
            Intrinsic2FnId::FloatTimes => Value(Float),
            Intrinsic2FnId::FloatDiv => Value(Float),
            Intrinsic2FnId::FloatPow => Value(Float),
            Intrinsic2FnId::FloatLessEqual => Value(Bool),
            Intrinsic2FnId::FloatLessThan => Value(Bool),
            Intrinsic2FnId::FloatGreaterEqual => Value(Bool),
            Intrinsic2FnId::FloatGreaterThan => Value(Bool),
            Intrinsic2FnId::ComplexPlus => Value(Complex),
            Intrinsic2FnId::ComplexMinus => Value(Complex),
            Intrinsic2FnId::ComplexTimes => Value(Complex),
            Intrinsic2FnId::ComplexDiv => Value(Complex),
            Intrinsic2FnId::ComplexPow => Value(Complex),
            Intrinsic2FnId::BoolAnd => Value(Bool),
            Intrinsic2FnId::BoolXor => Value(Bool),
            Intrinsic2FnId::BoolOr => Value(Bool),
        }
    }
}
impl IntrinsicFnId {
    pub fn ty_ret(self) -> CMType {
        match self {}
    }
    pub fn n_args(self) -> usize {
        match self {}
    }
}

#[derive(Debug, Clone)]
pub struct Overloads(HashMap<Vec<CMValueType>, FnRef>);

impl Overloads {
    fn new() -> Self {
        Self(HashMap::new())
    }
    pub fn lookup(&self, params: &Vec<CMValueType>) -> Option<FnRef> {
        self.0.get(params).copied()
    }
    pub fn lookup_fallback(&self, params: &Vec<Option<CMValueType>>) -> Option<FnRef> {
        lookup_fallback(params, self.0.iter().map(|(args, f)| (args, *f)))
    }
}

pub fn lookup_fallback<'a, I: Iterator<Item = (&'a Vec<CMValueType>, FnRef)>>(
    params: &Vec<Option<CMValueType>>,
    overloads: I,
) -> Option<FnRef> {
    let mut best_match = vec![];
    for (p, f) in overloads {
        if p.len() == params.len() {
            best_match.push(f)
        }
    }
    <[_; 1]>::try_from(best_match).ok().map(|[f]| f)
}

pub struct AssociatedFnLut {
    /// this <op> other
    pub op_binary: HashMap<(SLOperator, CMValueType), FnRef>,
    /// other <op> this
    pub op_binary_rhs: HashMap<(SLOperator, CMValueType), FnRef>,
    pub op_unary: HashMap<SLOperator, FnRef>,
    pub op_call: Overloads,
    pub op_index: Overloads,
    pub named: HashMap<IdentStr, Overloads>,
}
impl AssociatedFnLut {
    pub fn empty() -> Self {
        Self {
            op_binary: HashMap::new(),
            op_binary_rhs: HashMap::new(),
            op_unary: HashMap::new(),
            op_call: Overloads::new(),
            op_index: Overloads::new(),
            named: HashMap::new(),
        }
    }
    pub fn lookup_unary(&self, op: SLOperator) -> Option<FnRef> {
        self.op_unary.get(&op).copied()
    }
    pub fn lookup_binary(
        lhs: (&Self, CMValueType),
        rhs: (&Self, CMValueType),
        op: SLOperator,
    ) -> Option<FnRef> {
        if let Some(func) = lhs.0.op_binary.get(&(op, rhs.1)).copied() {
            Some(func)
        } else if let Some(func) = rhs.0.op_binary_rhs.get(&(op, lhs.1)).copied() {
            Some(func)
        } else {
            None
        }
    }
}

struct IntrinsicAssociatedFnLuts {
    pub int: AssociatedFnLut,
    pub float: AssociatedFnLut,
    pub complex: AssociatedFnLut,
    pub bool: AssociatedFnLut,
    pub string: AssociatedFnLut,

    pub empty_lut: AssociatedFnLut,
}

lazy_static! {
    static ref INTRINSICS: IntrinsicAssociatedFnLuts = {
        use CMValueType::*;
        use SLOperator::*;
        use FnRef::Identity;
        macro_rules! fn2 {
            ($expr: expr) => {{
                use Intrinsic2FnId::*;
                FnRef::Intrinsic2($expr)
            }};
        }
        macro_rules! fn1 {
            ($expr: expr) => {{
                use Intrinsic1FnId::*;
                FnRef::Intrinsic1($expr)
            }};
        }
        IntrinsicAssociatedFnLuts {
            int: AssociatedFnLut {
                op_binary: HashMap::from([
                    ((Plus, Int), fn2!(IntPlus)),
                    ((Minus, Int), fn2!(IntMinus)),
                    ((Times, Int), fn2!(IntTimes)),
                    ((Div, Int), fn2!(IntDiv)),
                    ((Exp, Int), fn2!(IntPow)),
                    ((Xor, Int), fn2!(IntBitXor)),
                    ((And, Int), fn2!(IntBitAnd)),
                    ((Or, Int), fn2!(IntBitOr)),

                    ((Equal, Int), fn2!(PrimitiveEquals)),
                    ((NotEqual, Int), fn2!(PrimitiveNotEquals)),

                    ((LessThan, Int), fn2!(IntLessThan)),
                    ((LessEqual, Int), fn2!(IntLessEqual)),
                    ((GreaterThan, Int), fn2!(IntGreaterThan)),
                    ((GreaterEqual, Int), fn2!(IntGreaterEqual)),
                ]),
                op_binary_rhs: HashMap::from([]),
                op_unary: HashMap::from([
                    (Plus, Identity),
                    (Minus, fn1!(IntMinus)),
                    (Xor, fn1!(IntBitNot)),

                    (HermitianConjugate, Identity),
                    (Transpose, Identity),
                ]),
                op_call: Overloads::new(),
                op_index: Overloads::new(),
                named: HashMap::from([]),
            },
            float: AssociatedFnLut::empty(), // TODO
            complex: AssociatedFnLut::empty(), // TODO
            bool: AssociatedFnLut::empty(), // TODO
            string: AssociatedFnLut::empty(), // TODO
            empty_lut: AssociatedFnLut::empty(), // TODO
            // float: todo!(),
            // complex: todo!(),
            // bool: todo!(),
            // string: todo!(),
            // empty_lut: todo!(),
        }
    };
}

pub fn get_fn_lut<'a, 'b>(ty: &'b CMValueType) -> &'a AssociatedFnLut {
    match ty {
        CMValueType::Int => &INTRINSICS.int,
        CMValueType::Float => &INTRINSICS.float,
        CMValueType::Complex => &INTRINSICS.complex,
        CMValueType::Bool => &INTRINSICS.bool,
        CMValueType::String => &INTRINSICS.string,
        CMValueType::Tuple(_) => &INTRINSICS.empty_lut,
        CMValueType::FunctionRef { params, return_ty } => todo!(),
        CMValueType::StructData(_) => todo!(),
        CMValueType::StructInstance(_) => todo!(),
    }
}
