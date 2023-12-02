use num::complex::Complex64;

use crate::language::ops::SLOperator;

use super::{
    gc::{GCObjectId, GarbageCollector},
    interpreter::{CallStackFrame, Identifier, Instruction, Scope, ScopeStackFrame},
    irrecoverable_error::IrrecoverableError,
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
        (Value::Int(lhs), Value::Complex(rhs)) => (Value::Complex((lhs as f64).into()), Value::Complex(rhs)),
        (Value::Float(lhs), Value::Int(rhs)) => (Value::Float(lhs), Value::Float(rhs as f64)),
        (Value::Float(lhs), Value::Float(rhs)) => (Value::Float(lhs), Value::Float(rhs)),
        (Value::Float(lhs), Value::Complex(rhs)) => (Value::Complex(lhs.into()), Value::Complex(rhs)),
        (Value::Complex(lhs), Value::Int(rhs)) => (Value::Complex(lhs), Value::Complex((rhs as f64).into())),
        (Value::Complex(lhs), Value::Float(rhs)) => (Value::Complex(lhs), Value::Complex(rhs.into())),
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
}

#[derive(Debug)]
pub struct Function {
    pub params: Vec<Identifier>,
    pub closure: Scope,
    pub code: Vec<Instruction>,
}
impl Function {
    pub fn produce_call_stack_frame(&self, arg_values: Vec<Value>) -> CallStackFrame {
        let mut new_stack_frame = ScopeStackFrame::base();
        for (i, value) in arg_values.into_iter().enumerate() {
            new_stack_frame.vars.insert(
                self.params[i].clone(),
                Var {
                    writable: false,
                    value: Some(value),
                },
            );
        }

        CallStackFrame {
            scope: Scope {
                stack: self
                    .closure
                    .stack
                    .iter()
                    .map(|it| it.clone())
                    .chain(std::iter::once(new_stack_frame))
                    .collect(),
            },
            code: self.code.clone(),
            exec_index: 0,
        }
    }
}

#[derive(Debug)]
pub enum Object {
    Function(Box<Function>),
    List(Vec<Value>),
    String(String),
}
impl Object {
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
            Object::String(_) => todo!(),
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
            Object::String(_) => todo!(),
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
