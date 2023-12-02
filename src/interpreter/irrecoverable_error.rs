#[derive(Debug, Clone, Copy)]
pub enum IrrecoverableError {
    IllegalBreak,
    IllegalBreakValue,
    IllegalContinue,
    IllegalReturn,
    IllegalOperator,
    NonBooleanCondition,
    VoidAsValue,
    VarNotFound,
    VarReadBeforeInitialized,
    VarNotWritable,
    VarRedeclaration,
    NotCallable,
    NotIndexable,
    InternalError(&'static str),
}
