use std::{
    collections::{hash_map::DefaultHasher, HashMap, HashSet},
    hash::Hasher,
};

use num::{complex::Complex64, Rational32, Zero};

use crate::language::ops::SLOperator;

use super::{
    gc::{GCObjectId, GarbageCollector},
    interpreter::{CallStackFrame, Identifier, Instruction, Scope, ScopeStackFrame, Voidable},
    irrecoverable_error::IrrecoverableError, module::ModuleComponentId,
};

#[derive(Debug, Clone, Copy)]
pub enum Value {
    Int(i128),
    Float(f64),
    Complex(Complex64),
    Bool(bool),
    Ref(GCObjectId),
}

macro_rules! impl_unary_op {
    ($name: ident; $($pat: pat => $expr: expr),* $(,)?) => {
        fn $name (self) -> Result<Value, IrrecoverableError> {
            match self {
                $($pat => Ok($expr),)*
                _ => Err(IrrecoverableError::IllegalOperator),
            }
        }
    };
    ($name: ident; TODO) => {
        fn $name (self) -> Result<Value, IrrecoverableError> {
            todo!()
        }
    };
}
macro_rules! impl_binary_op {
    ($name: ident $( [ $coercer: ident ] )? ; $($pat: pat => $expr: expr),* $(,)?) => {
        fn $name (self, rhs: Self) -> Result<Value, IrrecoverableError> {
            match $($coercer)? (self, rhs) {
                $($pat => Ok($expr),)*
                _ => Err(IrrecoverableError::IllegalOperator),
            }
        }
    };
    ($name: ident; TODO) => {
        fn $name (self, rhs: Self) -> Result<Value, IrrecoverableError> {
            todo!()
        }
    };
}

fn numeric_coercer(lhs: Value, rhs: Value) -> (Value, Value) {
    match (lhs, rhs) {
        (Value::Int(lhs), Value::Int(rhs)) => (Value::Int(lhs), Value::Int(rhs)),
        (Value::Int(lhs), Value::Float(rhs)) => (Value::Float(lhs as f64), Value::Float(rhs)),
        (Value::Int(lhs), Value::Complex(rhs)) => {
            (Value::Complex((lhs as f64).into()), Value::Complex(rhs))
        }
        (Value::Float(lhs), Value::Int(rhs)) => (Value::Float(lhs), Value::Float(rhs as f64)),
        (Value::Float(lhs), Value::Float(rhs)) => (Value::Float(lhs), Value::Float(rhs)),
        (Value::Float(lhs), Value::Complex(rhs)) => {
            (Value::Complex(lhs.into()), Value::Complex(rhs))
        }
        (Value::Complex(lhs), Value::Int(rhs)) => {
            (Value::Complex(lhs), Value::Complex((rhs as f64).into()))
        }
        (Value::Complex(lhs), Value::Float(rhs)) => {
            (Value::Complex(lhs), Value::Complex(rhs.into()))
        }
        (Value::Complex(lhs), Value::Complex(rhs)) => (Value::Complex(lhs), Value::Complex(rhs)),
        v => v,
    }
}
impl Value {
    impl_unary_op!(not;
        Self::Bool(it) => Self::Bool(!it)
    );
    impl_unary_op!(unary_plus;
        Self::Int(n) => Self::Int(n),
        Self::Float(v) => Self::Float(v),
        Self::Complex(v) => Self::Complex(v),
    );
    impl_unary_op!(unary_minus;
        Self::Int(n) => Self::Int(-n),
        Self::Float(v) => Self::Float(-v),
        Self::Complex(v) => Self::Complex(-v),
    );
    impl_unary_op!(hermitian_conjugate;
        Self::Int(n) => Self::Int(n),
        Self::Float(n) => Self::Float(n),
        Self::Complex(n) => Self::Complex(n.conj()),
        // TODO
    );
    impl_unary_op!(transpose; TODO);
    impl_unary_op!(inverse; TODO);

    pub fn apply_op_unary(self, op: SLOperator) -> Result<Value, IrrecoverableError> {
        match op {
            SLOperator::Not => self.not(),
            SLOperator::Plus => self.unary_plus(),
            SLOperator::Minus => self.unary_minus(),
            SLOperator::HermitianConjugate => self.hermitian_conjugate(),
            SLOperator::Transpose => self.transpose(),
            SLOperator::Inverse => self.inverse(),
            _ => Err(IrrecoverableError::IllegalOperator),
        }
    }

    impl_binary_op!(plus [numeric_coercer];
        (Self::Int(n0), Self::Int(n1)) => Self::Int(n0+n1),
        (Self::Float(v0), Self::Float(v1)) => Self::Float(v0+v1),
        (Self::Complex(v0), Self::Complex(v1)) => Self::Complex(v0+v1),
    );
    impl_binary_op!(scalar_times [numeric_coercer];
        (Self::Int(n0), Self::Int(n1)) => Self::Int(n0*n1),
        (Self::Float(v0), Self::Float(v1)) => Self::Float(v0*v1),
        (Self::Complex(v0), Self::Complex(v1)) => Self::Complex(v0*v1),
    );
    impl_binary_op!(gt [numeric_coercer];
        (Self::Int(n0), Self::Int(n1)) => Self::Bool(n0 > n1),
        (Self::Float(n0), Self::Float(n1)) => Self::Bool(n0 > n1),
    );
    impl_binary_op!(ge [numeric_coercer];
        (Self::Int(n0), Self::Int(n1)) => Self::Bool(n0 >= n1),
        (Self::Float(n0), Self::Float(n1)) => Self::Bool(n0 >= n1),
    );
    impl_binary_op!(lt [numeric_coercer];
        (Self::Int(n0), Self::Int(n1)) => Self::Bool(n0 < n1),
        (Self::Float(n0), Self::Float(n1)) => Self::Bool(n0 < n1),
    );
    impl_binary_op!(le [numeric_coercer];
        (Self::Int(n0), Self::Int(n1)) => Self::Bool(n0 <= n1),
        (Self::Float(n0), Self::Float(n1)) => Self::Bool(n0 <= n1),
    );
    impl_binary_op!(eq [numeric_coercer];
        (Self::Int(n0), Self::Int(n1)) => Self::Bool(n0 == n1),
        (Self::Float(n0), Self::Float(n1)) => Self::Bool(n0 == n1),
    );
    impl_binary_op!(ne [numeric_coercer];
        (Self::Int(n0), Self::Int(n1)) => Self::Bool(n0 != n1),
        (Self::Float(n0), Self::Float(n1)) => Self::Bool(n0 != n1),
    );

    pub fn apply_op_binary(self, rhs: Self, op: SLOperator) -> Result<Value, IrrecoverableError> {
        match op {
            SLOperator::Plus => self.plus(rhs),
            SLOperator::ScalarTimes => self.scalar_times(rhs),
            SLOperator::GreaterThan => self.gt(rhs),
            SLOperator::GreaterEqual => self.ge(rhs),
            SLOperator::LessThan => self.lt(rhs),
            SLOperator::LessEqual => self.le(rhs),
            SLOperator::Equal => self.eq(rhs),
            SLOperator::NotEqual => self.ne(rhs),
            _ => Err(IrrecoverableError::IllegalOperator),
        }
    }

    pub fn as_object<'gc>(
        &self,
        gc: &'gc GarbageCollector,
    ) -> Result<&'gc Object, IrrecoverableError> {
        match self {
            Self::Ref(id) => Ok(gc.borrow(*id)),
            _ => Err(IrrecoverableError::NotAnObject),
        }
    }
}

#[derive(Debug, Clone)]
pub struct FunctionOverload {
    args_ty: Vec<Type>,
    return_ty: Type,
    id: ModuleComponentId,
}

#[derive(Debug)]
pub enum Function {
    Bytecode {
        params: Vec<Identifier>,
        closure: Scope,
        code: Vec<Instruction>,
    },
    Native(fn(&[Value]) -> Voidable, usize),
}
impl Function {
    pub fn n_args(&self) -> usize {
        match self {
            Self::Bytecode { params, .. } => params.len(),
            Self::Native(_, n_args) => *n_args,
        }
    }
    pub fn call(
        &self,
        arg_values: Vec<Value>,
        call_stack: &mut Vec<CallStackFrame>,
        working_value: &mut Voidable,
    ) {
        match self {
            Self::Bytecode {
                params,
                closure,
                code,
            } => {
                let mut new_stack_frame = ScopeStackFrame::base();
                for (i, value) in arg_values.into_iter().enumerate() {
                    new_stack_frame.vars.insert(
                        params[i].clone(),
                        Var {
                            writable: false,
                            value: Some(value),
                        },
                    );
                }

                call_stack.push(CallStackFrame {
                    scope: Scope {
                        stack: closure
                            .stack
                            .iter()
                            .map(|it| it.clone())
                            .chain(std::iter::once(new_stack_frame))
                            .collect(),
                    },
                    code: code.clone(),
                    exec_index: 0,
                });
            }
            Self::Native(native_fn, _) => {
                *working_value = native_fn(&arg_values);
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct UnitsSI {
    label: Option<&'static str>,
    order: [Rational32; 7],
    scale: Rational32,
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct UnitsCountable {
    label: Identifier,
    order: Rational32,
}
#[derive(Debug, Clone, Eq)]
pub struct Units {
    si: Vec<UnitsSI>,
    countable: Vec<UnitsCountable>,
}
impl Units {
    fn reduce_fully(&self) -> (UnitsSI, HashMap<&Identifier, Rational32>) {
        let mut si_scale = Rational32::new(1, 1);
        let mut si_order = [Rational32::zero(); 7];
        for UnitsSI { order, scale, .. } in &self.si {
            for i in 0..si_order.len() {
                si_order[i] += order[i];
            }
            si_scale *= scale;
        }
        let mut labeled_unit_orders = HashMap::new();
        for UnitsCountable { label, order } in &self.countable {
            labeled_unit_orders
                .entry(label)
                .and_modify(|ord| *ord *= *order)
                .or_insert(*order);
        }
        labeled_unit_orders.retain(|_, order| !order.is_zero());

        (
            UnitsSI {
                label: None,
                order: si_order,
                scale: si_scale,
            },
            labeled_unit_orders,
        )
    }
    fn or(self, other: Self) -> Self {
        if self == other {
            self
        } else {
            todo!("// TODO type merge failure")
        }
    }
}
impl PartialEq for Units {
    fn eq(&self, other: &Self) -> bool {
        let self_reduced = self.reduce_fully();
        let other_reduced = other.reduce_fully();

        self_reduced.0 == other_reduced.0 && self_reduced.1 == other_reduced.1
    }
}

#[derive(Debug, Clone)]
pub enum Type {
    Int(Option<Units>),
    Float(Option<Units>),
    Complex(Option<Units>),
    Bool,
    String,
    Function(Vec<Type>, Option<Box<Type>>),
    ClassInstance(GCObjectId),
    EnumInstance(GCObjectId),
    Never,
}
impl Type {
    pub fn or(self, other: Self) -> Type {
        match (self, other) {
            (Self::Int(Some(self_units)), Self::Int(Some(other_units))) => {
                Self::Int(Some(self_units.or(other_units)))
            }
            (Self::Int(None), Self::Int(None)) => Self::Int(None),
            (Self::Float(Some(self_units)), Self::Float(Some(other_units))) => {
                Self::Float(Some(self_units.or(other_units)))
            }
            (Self::Float(None), Self::Float(None)) => Self::Float(None),
            (Self::Complex(Some(self_units)), Self::Complex(Some(other_units))) => {
                Self::Complex(Some(self_units.or(other_units)))
            }
            (Self::Complex(None), Self::Complex(None)) => Self::Complex(None),
            (Self::Bool, Self::Bool) => Self::Bool,
            (Self::String, Self::String) => Self::String,
            (Self::Function(self_args, self_ret), Self::Function(other_args, other_ret)) => {
                todo!("// TODO function type merge")
            }
            (Self::ClassInstance(_), Self::ClassInstance(_)) => todo!("// TODO polymorphism"),
            (Self::EnumInstance(_), Self::EnumInstance(_)) => todo!("// TODO polymorphism"),
            (Self::Never, Self::Never) => Self::Never,
            (Self::Never, t) => t,
            (t, Self::Never) => t,
            _ => todo!("// TODO handle type merge failure"),
        }
    }
    pub fn voidable_or(
        self_: Option<Self>,
        other: Option<Self>,
        default_to_void: bool,
    ) -> Option<Type> {
        match (self_, other) {
            (Some(self_), Some(other)) => Some(self_.or(other)),
            (None, None) => None,
            _ if default_to_void => None,
            _ => todo!("// TODO handle type merge failure"),
        }
    }
}

#[derive(Debug)]
pub struct AssociatedFunctionLUTEntry {
    pub id: u16,
    pub arg_types: Vec<Type>,
    pub return_type: Type,
}

#[derive(Debug, Hash, PartialEq, Eq, Clone, Copy)]
pub enum FnLookupType {
    /// fn(self, ...)
    Instance,
    /// fn(...)
    Static,
    /// +self
    OpUnary,
    /// T + self
    OpRHS,
    /// self + T
    OpLHS,
    /// self + self
    OpSelf,
}

#[derive(Debug)]
pub struct AssociatedFunctions {
    /// Lookup table that maps functions by identifier and type to a
    /// list of functions overloads with that name.
    lut: HashMap<(Identifier, FnLookupType), Vec<FunctionOverload>>,
    /// Lookup table that maps operator overloads by operation and type
    /// to a list of functions overloads with that name.
    op_lut: HashMap<(SLOperator, FnLookupType), Vec<FunctionOverload>>,
    /// Lookup table that maps indexing overloads to functions.
    idx_lut: Vec<FunctionOverload>,
    /// List of function implementations by numeric id.
    function_array: Vec<Function>,
}
impl AssociatedFunctions {
    pub fn lookup_fn(&self, ident: Identifier, ty: FnLookupType) -> &[FunctionOverload] {
        self.lut.get(&(ident, ty)).map_or(&[], |it| &it[..])
    }
    pub fn lookup_op_fn(&self, op: SLOperator, ty: FnLookupType) -> &[FunctionOverload] {
        self.op_lut.get(&(op, ty)).map_or(&[], |it| &it[..])
    }
    pub fn lookup_idx_fn(&self) -> &[FunctionOverload] {
        &self.idx_lut
    }
    pub fn access_fn(&self, id: ModuleComponentId) -> &Function {
        &self.function_array[id]
    }
}

#[derive(Debug)]
pub struct Class {
    // function array
    // function LUTs
    functions: AssociatedFunctions,
    // property LUT
    property_lut: HashMap<Identifier, (u16, Type)>,
}
impl Class {
    fn associated_functions(&self) -> &AssociatedFunctions {
        &self.functions
    }
    fn access_fn(&self, id: ModuleComponentId) -> &Function {
        self.functions.access_fn(id)
    }
}
#[derive(Debug)]
pub struct ClassInstance {
    /// Reference to class.
    class_id: GCObjectId,
    /// property array
    property_array: Vec<Value>,
}
impl ClassInstance {
    fn get_class<'gc>(&self, gc: &'gc GarbageCollector) -> &'gc Class {
        gc.borrow_class(self.class_id)
    }
    fn access_property(&self, id: ModuleComponentId) -> &Value {
        &self.property_array[id as usize]
    }
}
#[derive(Debug)]
pub struct Enum {
    // function array
    // function LUTs
    functions: AssociatedFunctions,

    // variant id LUT
    variant_id_lut: HashMap<Identifier, u16>,
    // property LUT array
    property_lut_array: Vec<HashMap<Identifier, (ModuleComponentId, Type)>>,
}
impl Enum {
    fn associated_functions(&self) -> &AssociatedFunctions {
        &self.functions
    }
    fn access_fn(&self, id: ModuleComponentId) -> &Function {
        self.functions.access_fn(id)
    }
}
#[derive(Debug)]
pub struct EnumInstance {
    /// Reference to class.
    class_id: GCObjectId,
    // variant id
    variant_id: u16,
    // property array
    property_array: Vec<Value>,
}
impl EnumInstance {
    fn get_class<'gc>(&self, gc: &'gc GarbageCollector) -> &'gc Enum {
        gc.borrow_enum(self.class_id)
    }
    fn access_property(&self, id: ModuleComponentId) -> &Value {
        &self.property_array[id]
    }
}

#[derive(Debug)]
pub enum Object {
    Function(Box<Function>),
    List(Vec<Value>),
    String(String),
    ClassInstance(ClassInstance),
    Class(Class),
    EnumInstance(EnumInstance),
    Enum(Enum),
}
impl Object {
    pub fn associated_functions<'a: 'gc, 'gc>(
        &'a self,
        gc: &'gc GarbageCollector,
    ) -> &'gc AssociatedFunctions {
        match self {
            Self::Class(v) => v.associated_functions(),
            Self::ClassInstance(v) => v.get_class(gc).associated_functions(),
            Self::Enum(v) => v.associated_functions(),
            Self::EnumInstance(v) => v.get_class(gc).associated_functions(),
            _ => todo!(),
        }
    }
    pub fn access_property<'a: 'gc, 'gc>(&'a self, id: ModuleComponentId) -> Result<Value, IrrecoverableError> {
        match self {
            Self::Class(v) => Err(IrrecoverableError::PropertyNotFound),
            Self::Enum(v) => Err(IrrecoverableError::PropertyNotFound),
            Self::ClassInstance(v) => Ok(*v.access_property(id)),
            Self::EnumInstance(v) => Ok(*v.access_property(id)),
            _ => todo!(),
        }
    }
    fn index(&self, indices: &Vec<Value>) -> Option<Value> {
        match self {
            Object::Function(_) => None,
            Object::List(values) => {
                if indices.len() == 1 {
                    todo!()
                } else {
                    None
                }
            }
            _ => todo!(),
        }
    }
    fn index_mut(&mut self, indices: &Vec<Value>) -> Option<&mut Value> {
        match self {
            Object::Function(_) => None,
            Object::List(values) => {
                if indices.len() == 1 {
                    todo!()
                } else {
                    None
                }
            }
            _ => todo!(),
        }
    }
}

#[derive(Debug)]
pub enum DataRef {
    Index(GCObjectId, Vec<Value>),
    Identifier(Identifier),
    None,
}
impl DataRef {
    pub fn read(self, scope: &Scope, gc: &GarbageCollector) -> Option<Value> {
        match self {
            Self::None => None,
            Self::Index(obj_id, indices) => gc.borrow(obj_id).index(&indices),
            Self::Identifier(ident) => scope.read(&ident).ok(),
        }
    }
    pub fn write<'a>(
        self,
        scope: &'a mut Scope,
        gc: &'a mut GarbageCollector,
        value: Value,
    ) -> Option<()> {
        match self {
            Self::None => None,
            Self::Identifier(ident) => {
                scope.get_var_mut(&ident).ok()?.write(value).ok()?;
                Some(())
            }
            Self::Index(obj_id, indices) => {
                let value_ref = gc.borrow_mut(obj_id).index_mut(&indices)?;
                *value_ref = value;
                Some(())
            }
        }
    }
    pub fn index(self, scope: &Scope, gc: &GarbageCollector, index: Vec<Value>) -> Option<DataRef> {
        match self.read(scope, gc) {
            Some(Value::Ref(obj_id)) => Some(DataRef::Index(obj_id, index)),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Var {
    pub writable: bool,
    pub value: Option<Value>,
}
impl Var {
    pub fn read(&self) -> Result<Value, IrrecoverableError> {
        self.value
            .ok_or(IrrecoverableError::VarReadBeforeInitialized)
    }
    pub fn write(&mut self, value: Value) -> Result<(), IrrecoverableError> {
        if !self.writable && self.value.is_some() {
            Err(IrrecoverableError::VarNotWritable)
        } else {
            self.value = Some(value);
            Ok(())
        }
    }
}

/*

a: A, b: B, op

primitive (into) -> primitive ;; predefined
any (into) -> ref ;; ref.overloadFrom
ref (into) -> any ;; ref.overloadInto
<> primitive -> _ ;; predefined
<> ref -> _ ;; ref.overloadUnary.<>
primitive <> primitive -> _ ;; predefined
primitive <> ref -> _ ;; ref.overloadRHS.<>
ref <> primitive -> _ ;; ref.overloadLHS.<>
ref0 <> ref0 -> _ ;; ref.overloadSelf.<>
ref0 <> ref1 -> _ ;; ref0.overloadLHS.<>, ref1.overloadRHS.<> (error if both are defined)

a: int + b: float -> _

int(+):
    (self,int) -> int
float(+):
    (self,float) -> float

int(into):
    () -> float
    () -> complex
    () -> rational



mat:
    op-unary('):
        (self) -> self,
    op-lhs(*):
        (self, int) -> self,
        (self, float) -> self,
        (self, complex) -> self,
    op-rhs(*):
        (int, self) -> self,
        (float, self) -> self,
        (complex, self) -> self,
    op-self(**):
        (self, self) -> self,



*/
