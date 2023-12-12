use std::collections::HashMap;

use super::{data::{Function, Class, Enum, FunctionOverload}, interpreter::Identifier};

pub type ModuleComponentId = usize;

struct Module {
    function_lut: HashMap<(Identifier, u16), Box<[FunctionOverload]>>,
    class_lut: HashMap<(Identifier, u16), Box<[ModuleComponentId]>>,
    enum_lut: HashMap<(Identifier, u16), Box<[ModuleComponentId]>>,
    function_array: Box<[Function]>,
    class_array: Box<[Class]>,
    enum_array: Box<[Enum]>,
}