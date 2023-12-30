use std::collections::HashMap;

use super::value::Value;

pub type GCAllocId = u64;

pub struct GarbageCollector {
    allocated: HashMap<GCAllocId, Value>,
    next_alloc_id: GCAllocId,
}
impl GarbageCollector {
    pub fn new() -> Self {
        Self {
            allocated: HashMap::new(),
            next_alloc_id: 0,
        }
    }
    fn next_safe_alloc_id(&mut self) -> GCAllocId {
        while self.allocated.contains_key(&self.next_alloc_id) {
            self.next_alloc_id += 1;
        }
        self.next_alloc_id
    }
    pub fn alloc(&mut self, value: Value) -> GCAllocId {
        let id = self.next_safe_alloc_id();
        self.allocated.insert(id, value);
        id
    }
    pub fn borrow(&self, id: GCAllocId) -> Option<&Value> {
        self.allocated.get(&id)
    }
    pub fn borrow_mut(&mut self, id: GCAllocId) -> Option<&mut Value> {
        self.allocated.get_mut(&id)
    }
}
