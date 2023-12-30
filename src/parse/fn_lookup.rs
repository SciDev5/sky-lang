use std::collections::HashMap;

use crate::common::{common_module::{CMValueType, CMType}, IdentInt, IdentStr};

use super::ops::SLOperator;

pub enum FnRef<IntrinsicFn> {
    ModuleFunction(IdentInt),
    Intrinsic(IntrinsicFn, CMType),
}

pub struct Overload<IntrinsicFn> {
    params: Vec<CMValueType>,
    func: FnRef<IntrinsicFn>,
}

pub struct AssociatedFnLut<IntrinsicFn> {
    /// this <op> other
    pub op_binary: HashMap<(SLOperator, CMValueType), FnRef<IntrinsicFn>>,
    /// other <op> this
    pub op_binary_rhs: HashMap<(SLOperator, CMValueType), FnRef<IntrinsicFn>>,
    pub op_unary: HashMap<SLOperator, FnRef<IntrinsicFn>>,
    pub op_call: Vec<Overload<IntrinsicFn>>,
    pub op_index: Vec<Overload<IntrinsicFn>>,
    pub named: HashMap<IdentStr, Vec<Overload<IntrinsicFn>>>,
}
impl<T> AssociatedFnLut<T> {
    pub fn empty() -> Self {
        Self {
            op_binary: HashMap::new(),
            op_binary_rhs: HashMap::new(),
            op_unary: HashMap::new(),
            op_call: Vec::new(),
            op_index: Vec::new(),
            named: HashMap::new(),
        }
    }
}

pub struct IntrinsicAssociatedFnLuts<IntrinsicFn> {
    pub int: AssociatedFnLut<IntrinsicFn>,
    pub float: AssociatedFnLut<IntrinsicFn>,
    pub complex: AssociatedFnLut<IntrinsicFn>,
    pub bool: AssociatedFnLut<IntrinsicFn>,
    pub string: AssociatedFnLut<IntrinsicFn>,

    pub empty_lut: AssociatedFnLut<IntrinsicFn>,
}

pub fn get_fn_lut<IntrinsicFn>(
    ty: CMValueType,
    intrinsic_associated_fn_luts: &IntrinsicAssociatedFnLuts<IntrinsicFn>,
) -> &AssociatedFnLut<IntrinsicFn> {
    match ty {
        CMValueType::Int => &intrinsic_associated_fn_luts.int,
        CMValueType::Float => &intrinsic_associated_fn_luts.float,
        CMValueType::Complex => &intrinsic_associated_fn_luts.complex,
        CMValueType::Bool => &intrinsic_associated_fn_luts.bool,
        CMValueType::String => &intrinsic_associated_fn_luts.string,
        CMValueType::Tuple(_) => &intrinsic_associated_fn_luts.empty_lut,
        CMValueType::FunctionRef { params, return_ty } => todo!(),
        CMValueType::ClassRef(_) => todo!(),
        CMValueType::ClassInstance(_) => todo!(),
    }
}
