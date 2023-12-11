#[derive(Debug, Clone, Copy)]
pub enum IrrecoverableError {
    IllegalBreak,
    IllegalBreakValue,
    IllegalContinue,
    IllegalReturn,
    IllegalOperator,
    NonBooleanCondition,
    VoidAsValue,
    NotAnObject,
    VarNotFound,
    VarReadBeforeInitialized,
    VarNotWritable,
    VarRedeclaration,
    PropertyNotFound,
    NotCallable,
    NotIndexable,
    InternalError(&'static str),
}
