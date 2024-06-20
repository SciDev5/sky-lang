pub type SourceFileId = usize;

/// A range of char indices in the source code.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Loc {
    pub src_id: SourceFileId,
    pub start: usize,
    pub length: usize,
}
impl Loc {
    pub fn merge(self, to: Self) -> Self {
        assert_eq!(self.src_id, to.src_id);
        Self {
            src_id: self.src_id,
            start: self.start,
            length: (to.start - self.start) + to.length,
        }
    }
    pub fn merge_some(self, to: Option<Self>) -> Self {
        match to {
            Some(to) => self.merge(to),
            None => self,
        }
    }
    pub fn new_from_end(self) -> Self {
        Self {
            src_id: self.src_id,
            start: self.start + self.length,
            length: 0,
        }
    }
    pub fn new_from_start(self) -> Self {
        Self {
            src_id: self.src_id,
            start: self.start,
            length: 0,
        }
    }
}

pub trait HasLoc {
    fn loc(&self) -> Loc;
}
#[macro_export]
macro_rules! impl_hasloc_simple {
    ($struct:ident $(< $lt:lifetime >)?) => {
        impl $(< $lt >)? HasLoc for $struct $(< $lt >)? {
            fn loc(&self) -> Loc {
                self.loc
            }
        }
    };
}
impl HasLoc for Loc {
    fn loc(&self) -> Loc {
        *self
    }
}
impl<T> HasLoc for (T, Loc) {
    fn loc(&self) -> Loc {
        self.1
    }
}
