use std::collections::HashMap;

use lazy_static::lazy_static;

use crate::common::{
    common_module::{CMAssociatedFunction, CMType},
    IdentInt, IdentStr,
};

use super::{ops::SLOperator, raw_2_common::ResolverGlobalState};

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
    FloatPowAsComplex,
    FloatPowInt,
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
        match self {
            Intrinsic1FnId::IntMinus => Int,
            Intrinsic1FnId::IntBitNot => Int,
            Intrinsic1FnId::FloatMinus => Int,
            Intrinsic1FnId::ComplexMinus => Complex,
            Intrinsic1FnId::ComplexConj => Complex,
            Intrinsic1FnId::BoolNot => Bool,
        }
    }
}
impl Intrinsic2FnId {
    pub fn ty_ret(self) -> CMType {
        use CMType::*;
        match self {
            Intrinsic2FnId::PrimitiveEquals => Bool,
            Intrinsic2FnId::PrimitiveNotEquals => Bool,
            Intrinsic2FnId::IntPlus => Int,
            Intrinsic2FnId::IntMinus => Int,
            Intrinsic2FnId::IntTimes => Int,
            Intrinsic2FnId::IntDiv => Int,
            Intrinsic2FnId::IntPow => Int,
            Intrinsic2FnId::IntBitXor => Int,
            Intrinsic2FnId::IntBitAnd => Int,
            Intrinsic2FnId::IntBitOr => Int,
            Intrinsic2FnId::IntLessEqual => Bool,
            Intrinsic2FnId::IntLessThan => Bool,
            Intrinsic2FnId::IntGreaterEqual => Bool,
            Intrinsic2FnId::IntGreaterThan => Bool,
            Intrinsic2FnId::FloatPlus => Float,
            Intrinsic2FnId::FloatMinus => Float,
            Intrinsic2FnId::FloatTimes => Float,
            Intrinsic2FnId::FloatDiv => Float,
            Intrinsic2FnId::FloatPowInt => Float,
            Intrinsic2FnId::FloatPowAsComplex => Complex,
            Intrinsic2FnId::FloatLessEqual => Bool,
            Intrinsic2FnId::FloatLessThan => Bool,
            Intrinsic2FnId::FloatGreaterEqual => Bool,
            Intrinsic2FnId::FloatGreaterThan => Bool,
            Intrinsic2FnId::ComplexPlus => Complex,
            Intrinsic2FnId::ComplexMinus => Complex,
            Intrinsic2FnId::ComplexTimes => Complex,
            Intrinsic2FnId::ComplexDiv => Complex,
            Intrinsic2FnId::ComplexPow => Complex,
            Intrinsic2FnId::BoolAnd => Bool,
            Intrinsic2FnId::BoolXor => Bool,
            Intrinsic2FnId::BoolOr => Bool,
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
pub struct Overloads(HashMap<Vec<CMType>, FnRef>);

impl Overloads {
    fn new() -> Self {
        Self(HashMap::new())
    }
    pub fn lookup(&self, params: &Vec<CMType>) -> Option<FnRef> {
        self.0.get(params).copied()
    }
    pub fn lookup_fallback(&self, params: &Vec<Option<CMType>>) -> Option<FnRef> {
        lookup_fallback(params, self.0.iter().map(|(args, f)| (args, *f)))
    }
}

pub fn lookup_fallback<'a, I: Iterator<Item = (&'a Vec<CMType>, FnRef)>>(
    params: &Vec<Option<CMType>>,
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

#[derive(Debug, Clone)]
pub struct AssociatedFnLut {
    /// this <op> other
    pub op_binary: HashMap<(SLOperator, CMType), FnRef>,
    /// other <op> this
    pub op_binary_rhs: HashMap<(SLOperator, CMType), FnRef>,
    pub op_unary: HashMap<SLOperator, FnRef>,
    pub op_call: Overloads,
    pub op_index: Overloads,
    pub named: HashMap<IdentStr, usize>,
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
        lhs: (&Self, CMType),
        rhs: (&Self, CMType),
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
        use CMType::*;
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
            // TODO add more cool intrinsic functions
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
            float: AssociatedFnLut {
                op_binary: HashMap::from([
                    ((Plus, Float), fn2!(FloatPlus)),
                    ((Minus, Float), fn2!(FloatMinus)),
                    ((Times, Float), fn2!(FloatTimes)),
                    ((Div, Float), fn2!(FloatDiv)),
                    ((Exp, Int), fn2!(FloatPowInt)),
                    ((Exp, Float), fn2!(FloatPowAsComplex)),

                    ((Equal, Float), fn2!(PrimitiveEquals)),
                    ((NotEqual, Float), fn2!(PrimitiveNotEquals)),

                    ((LessThan, Float), fn2!(FloatLessThan)),
                    ((LessEqual, Float), fn2!(FloatLessEqual)),
                    ((GreaterThan, Float), fn2!(FloatGreaterThan)),
                    ((GreaterEqual, Float), fn2!(FloatGreaterEqual)),
                ]),
                op_binary_rhs: HashMap::from([]),
                op_unary: HashMap::from([
                    (Plus, Identity),
                    (Minus, fn1!(IntMinus)),

                    (HermitianConjugate, Identity),
                    (Transpose, Identity),
                ]),
                op_call: Overloads::new(),
                op_index: Overloads::new(),
                named: HashMap::from([]),
            },
            complex: AssociatedFnLut {
                op_binary: HashMap::from([
                    ((Plus, Complex), fn2!(ComplexPlus)),
                    ((Minus, Complex), fn2!(ComplexMinus)),
                    ((Times, Complex), fn2!(ComplexTimes)),
                    ((Div, Complex), fn2!(ComplexDiv)),
                    ((Exp, Complex), fn2!(ComplexPow)),

                    ((Equal, Complex), fn2!(PrimitiveEquals)),
                    ((NotEqual, Complex), fn2!(PrimitiveNotEquals)),
                ]),
                op_binary_rhs: HashMap::from([]),
                op_unary: HashMap::from([
                    (Plus, Identity),
                    (Minus, fn1!(IntMinus)),

                    (HermitianConjugate, fn1!(ComplexConj)),
                    (Transpose, Identity),
                ]),
                op_call: Overloads::new(),
                op_index: Overloads::new(),
                named: HashMap::from([]),
            },
            bool: AssociatedFnLut {
                op_binary: HashMap::from([
                    ((And, Bool), fn2!(BoolAnd)),
                    ((Or, Bool), fn2!(BoolOr)),
                    ((Xor, Bool), fn2!(BoolXor)),

                    ((Equal, Complex), fn2!(PrimitiveEquals)),
                    ((NotEqual, Complex), fn2!(PrimitiveNotEquals)),
                ]),
                op_binary_rhs: HashMap::from([]),
                op_unary: HashMap::from([
                    (Not, fn1!(BoolNot)),

                    (HermitianConjugate, Identity),
                    (Transpose, Identity),
                ]),
                op_call: Overloads::new(),
                op_index: Overloads::new(),
                named: HashMap::from([]),
            },
            string: AssociatedFnLut::empty(), // TODO implement string intrinsic AssociatedFnLut
            empty_lut: AssociatedFnLut::empty(),
        }
    };
}

pub fn gen_struct_fn_lut(
    impl_functions: &HashMap<String, CMAssociatedFunction>,
    is_instance_lut: bool,
) -> AssociatedFnLut {
    let mut lut = AssociatedFnLut::empty();
    for (name, CMAssociatedFunction { id, is_member }) in impl_functions {
        if *is_member || !is_instance_lut {
            lut.named.insert(name.clone(), *id);
        }
    }
    lut
}
pub fn get_fn_lut<'a>(ty: &CMType, state: &'a ResolverGlobalState) -> &'a AssociatedFnLut {
    match ty {
        CMType::Void => &INTRINSICS.empty_lut,
        CMType::Unknown => panic!("should not have allowed associated function lookup on unknown"),
        CMType::Never => panic!("should not have allowed associated function lookup on never"),

        CMType::Int => &INTRINSICS.int,
        CMType::Float => &INTRINSICS.float,
        CMType::Complex => &INTRINSICS.complex,
        CMType::Bool => &INTRINSICS.bool,
        CMType::String => &INTRINSICS.string,
        CMType::Tuple(_) => &INTRINSICS.empty_lut,
        CMType::FunctionRef { params, return_ty } => todo!(),
        CMType::StructInstance(id) => &state.structs[*id].fn_lut_inst,
        CMType::StructData(id) => &state.structs[*id].fn_lut_clss,
    }
}
