use std::collections::HashMap;

use crate::front::tokenize::{TInfixOperatorType, TPostfixOperatorType, TPrefixOperatorType};

#[derive(Debug)]
pub struct Core {
    pub op_traits: CoreOpTraits,
}
#[derive(Debug)]
pub struct CoreOpTraits {
    pub infix: HashMap<TInfixOperatorType, usize>,
    pub prefix: HashMap<TPrefixOperatorType, usize>,
    pub postfix: HashMap<TPostfixOperatorType, usize>,
}
impl Core {
    pub fn init_empty() -> Self {
        Self {
            op_traits: CoreOpTraits {
                infix: HashMap::new(),
                prefix: HashMap::new(),
                postfix: HashMap::new(),
            },
        }
    }
}
