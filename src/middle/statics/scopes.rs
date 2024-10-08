use std::collections::HashMap;

pub type ScopeId = usize;
#[derive(Debug, Clone, PartialEq)]
pub struct Scopes<T> {
    has_registered_children: bool,
    scopes: Vec<Scope<T>>,
}
impl<T> Scopes<T> {
    pub fn init() -> Self {
        Self {
            has_registered_children: false,
            scopes: Vec::new(),
        }
    }
    pub fn register(&mut self, parent: Option<ScopeId>) -> ScopeId {
        assert!(!self.has_registered_children);
        let id = self.scopes.len();
        self.scopes.push(Scope {
            parent,
            children: Vec::new(),
            inner: None,
        });
        id
    }
    pub fn insert(&mut self, id: ScopeId, scope: T) {
        self.scopes[id].inner = Some(Box::new(scope));
    }
    pub fn get_or_insert(&mut self, id: ScopeId, default: T) -> &mut T {
        let v = self.scopes[id]
            .inner
            .get_or_insert_with(|| Box::new(default));
        v.as_mut()
    }
    pub fn get(&mut self, id: ScopeId) -> Option<&T> {
        self.scopes[id].inner.as_ref().map(|b: &Box<T>| b.as_ref())
    }
    pub fn calculate_children(&mut self) {
        assert!(!self.has_registered_children);
        for i_child in 0..self.scopes.len() {
            if let Some(i_parent) = self.scopes[i_child].parent {
                self.scopes[i_parent].children.push(i_child);
            }
        }
        self.has_registered_children = true;
    }

    pub fn modify<F: Fn(&mut T)>(&mut self, mapping: F) {
        for scope in &mut self.scopes {
            if let Some(inner) = &mut scope.inner {
                mapping(inner.as_mut())
            }
        }
    }

    pub fn map<R, F: FnMut(T) -> R>(self, mut mapping: F) -> Scopes<R> {
        Scopes {
            scopes: self
                .scopes
                .into_iter()
                .map(
                    |Scope {
                         parent,
                         children,
                         inner,
                     }| Scope {
                        parent,
                        children,
                        inner: inner.map(|v| Box::new(mapping(*v))),
                    },
                )
                .collect(),
            has_registered_children: self.has_registered_children,
        }
    }

    pub fn zip<V, R, F: Fn(T, V) -> R>(self, other: Scopes<V>, mapping: F) -> Scopes<R> {
        assert!(self.has_registered_children);
        assert!(other.has_registered_children);
        assert_eq!(self.scopes.len(), other.scopes.len());

        Scopes {
            has_registered_children: true,
            scopes: self
                .scopes
                .into_iter()
                .zip(other.scopes.into_iter())
                .map(|(this, other)| {
                    assert_eq!(this.parent, other.parent);
                    assert_eq!(&this.children, &other.children);
                    Scope {
                        parent: this.parent,
                        children: this.children,
                        inner: match (this.inner, other.inner) {
                            (Some(this), Some(other)) => Some(Box::new(mapping(*this, *other))),
                            (None, None) => None,
                            _ => panic!("Scopes mismatched in zip"),
                        },
                    }
                })
                .collect(),
        }
    }

    /// Modify the scopes within the context of all their parent scopes.
    pub fn modify_contextual<B, F: FnMut(&mut T, &B, &[Box<T>])>(
        &mut self,
        mut mapping: F,
        base_scopes: &HashMap<ScopeId, B>,
    ) {
        assert_eq!(self.has_registered_children, true);
        assert!(self
            .scopes
            .iter()
            .enumerate()
            .all(|(id, scope)| scope.parent.is_none() == base_scopes.get(&id).is_some()));

        let mut scopes_out = Vec::with_capacity(self.scopes.len());
        scopes_out.extend(std::iter::repeat_with(|| None).take(self.scopes.len()));
        let mut scopes_in = self
            .scopes
            .drain(..)
            .map(|scope| Some(scope))
            .collect::<Vec<_>>();

        let mut parent_scopes_inner = Vec::with_capacity(6);
        let mut parent_scopes = Vec::with_capacity(6);

        for (i, base_scope) in base_scopes {
            let Scope {
                parent,
                children,
                inner,
            } = scopes_in[*i].take().unwrap();
            let has_inner = inner.is_some();
            if let Some(mut inner) = inner {
                mapping(inner.as_mut(), base_scope, &mut parent_scopes_inner);
                parent_scopes_inner.push(inner);
            }
            parent_scopes.push((parent, (children, 0), has_inner));
            while let Some((_, (children, j), _)) = parent_scopes.last_mut() {
                // if scope has already mapped all its children, we are done with it and can output it
                if *j >= children.len() {
                    let (parent, (children, _), has_inner) = parent_scopes.pop().unwrap();
                    let inner = if has_inner {
                        parent_scopes_inner.pop()
                    } else {
                        None
                    };
                    let self_i = parent_scopes
                        .last()
                        // `... - 1` here because j is incremented before children are processed.
                        .map(|(_, (parent_children, parent_j), _)| parent_children[*parent_j - 1])
                        .unwrap_or(*i);
                    scopes_out[self_i] = Some(Scope {
                        parent,
                        children,
                        inner,
                    });
                    continue;
                }

                // map next child scope
                let Scope {
                    parent,
                    children,
                    inner,
                } = scopes_in[children[*j]].take().unwrap();
                *j += 1;

                let has_inner = inner.is_some();
                if let Some(mut inner) = inner {
                    mapping(inner.as_mut(), base_scope, &mut parent_scopes_inner);
                    parent_scopes_inner.push(inner);
                }
                parent_scopes.push((parent, (children, 0), has_inner));
            }
        }

        self.scopes
            .extend(scopes_out.into_iter().map(Option::unwrap))
    }

    /// Map the scopes within the context of all their parent scopes.
    pub fn map_contextual<R, B, F: FnMut(T, &B, &mut [R]) -> R>(
        self,
        mut mapping: F,
        base_scopes: &HashMap<ScopeId, B>,
    ) -> Scopes<R> {
        assert_eq!(self.has_registered_children, true);
        assert!(self
            .scopes
            .iter()
            .enumerate()
            .all(|(id, scope)| scope.parent.is_none() == base_scopes.get(&id).is_some()));

        let mut scopes_out = Vec::with_capacity(self.scopes.len());
        scopes_out.extend(std::iter::repeat_with(|| None).take(self.scopes.len()));
        let mut scopes_in = self
            .scopes
            .into_iter()
            .map(|scope| Some(scope))
            .collect::<Vec<_>>();

        let mut parent_scopes_inner = Vec::with_capacity(6);
        let mut parent_scopes = Vec::with_capacity(6);

        for (i, base_scope) in base_scopes {
            let Scope {
                parent,
                children,
                inner,
            } = scopes_in[*i].take().unwrap();
            let has_inner = inner.is_some();
            if let Some(inner) = inner {
                let mapped = mapping(*inner, base_scope, &mut parent_scopes_inner);
                parent_scopes_inner.push(mapped);
            }
            parent_scopes.push((parent, (children, 0), has_inner));
            while let Some((_, (children, j), _)) = parent_scopes.last_mut() {
                // if scope has already mapped all its children, we are done with it and can output it
                if *j >= children.len() {
                    let (parent, (children, _), has_inner) = parent_scopes.pop().unwrap();
                    let inner = if has_inner {
                        parent_scopes_inner.pop()
                    } else {
                        None
                    };
                    let self_i = parent_scopes
                        .last()
                        // `... - 1` here because j is incremented before children are processed.
                        .map(|(_, (parent_children, parent_j), _)| parent_children[*parent_j - 1])
                        .unwrap_or(*i);
                    scopes_out[self_i] = Some(Scope {
                        parent,
                        children,
                        inner: inner.map(Box::new),
                    });
                    continue;
                }

                // map next child scope
                let Scope {
                    parent,
                    children,
                    inner,
                } = scopes_in[children[*j]].take().unwrap();
                *j += 1;

                let has_inner = inner.is_some();
                if let Some(inner) = inner {
                    let mapped = mapping(*inner, base_scope, &mut parent_scopes_inner);
                    parent_scopes_inner.push(mapped);
                }
                parent_scopes.push((parent, (children, 0), has_inner));
            }
        }

        Scopes {
            has_registered_children: true,
            scopes: scopes_out.into_iter().map(Option::unwrap).collect(),
        }
    }

    pub fn find_map<R, F: Fn(&T) -> Option<R>>(
        &self,
        origin_scope: ScopeId,
        predicate: F,
    ) -> Option<R> {
        let mut i = origin_scope;
        loop {
            let scope = &self.scopes[i];
            if let Some(result) = scope
                .inner
                .as_ref()
                .and_then(|inner| predicate(inner.as_ref()))
            {
                return Some(result);
            }
            i = scope.parent?;
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Scope<T> {
    parent: Option<ScopeId>,
    children: Vec<ScopeId>,
    inner: Option<Box<T>>,
}
