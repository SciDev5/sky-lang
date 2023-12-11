use std::collections::HashMap;

use super::{data::{Object, Enum, Class}, interpreter::ScopeStackFrame};

pub type GCObjectId = u64;
#[derive(Debug)]
pub struct GarbageCollector {
    objects: HashMap<GCObjectId, Object>,
    current_id: GCObjectId,

    classes: HashMap<GCObjectId, Class>,
    enums: HashMap<GCObjectId, Enum>,
    current_class_id: GCObjectId,

    root_scopes: Vec<ScopeStackFrame>,
}
impl<'data> GarbageCollector {
    pub fn new() -> Self {
        Self {
            objects: HashMap::new(),
            classes: HashMap::new(),
            enums: HashMap::new(),
            current_id: 0,
            current_class_id: 0,
            root_scopes: vec![],
        }
    }
    fn next_free_id(&mut self) -> GCObjectId {
        while self.objects.contains_key(&self.current_id) {
            self.current_id += 1;
        }
        let selected_id = self.current_id;
        self.current_id += 1;
        selected_id
    }
    pub fn alloc(&mut self, object: Object) -> GCObjectId {
        let id = self.next_free_id();
        self.objects.insert(id, object);
        id
    }
    pub fn borrow(&self, id: GCObjectId) -> &Object {
        self.objects
            .get(&id)
            .expect("reference to object managed to outlive object")
    }
    pub fn borrow_mut(&mut self, id: GCObjectId) -> &mut Object {
        self.objects
            .get_mut(&id)
            .expect("reference to object managed to outlive object")
    }
    
    fn next_free_class_id(&mut self) -> GCObjectId {
        while self.classes.contains_key(&self.current_id) || self.enums.contains_key(&self.current_id) {
            self.current_class_id += 1;
        }
        let selected_id = self.current_class_id;
        self.current_class_id += 1;
        selected_id
    }
    pub fn alloc_class(&mut self, object: Class) -> GCObjectId {
        let id = self.next_free_class_id();
        self.classes.insert(id, object);
        id
    }
    pub fn alloc_enum(&mut self, object: Enum) -> GCObjectId {
        let id = self.next_free_class_id();
        self.enums.insert(id, object);
        id
    }
    pub fn borrow_class(&self, id: GCObjectId) -> &Class {
        self.classes
            .get(&id)
            .expect("reference to object managed to outlive object")
    }
    pub fn borrow_enum(&self, id: GCObjectId) -> &Enum {
        self.enums
            .get(&id)
            .expect("reference to object managed to outlive object")
    }
    // fn mark_and_sweep(&mut self) {
    //     let encounter: HashMap<u64, bool, RandomState> = HashMap::from_iter(self.objects.keys().map(|it| (*it, false)));
    //     self.root_scopes.

    // }
}
