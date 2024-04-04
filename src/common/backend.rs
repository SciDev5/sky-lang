use std::collections::HashMap;

use super::common_module::CommonModule;

pub type BackendId = u64;
pub type BackendCompatIds = &'static [BackendId];

pub trait BackendCompiler {
    /// A unique ID for this backend.
    ///
    /// To guarantee uniqueness, select ids like so:
    /// `((unix_seconds & 0xffff_ffff_ffff) << 16) | (random & 0xffff)`
    const ID: BackendId;
    const NAME: &'static str;
    /// Contains all the ids of the backends that are direct parents of this.
    const PARENT_IDS: &'static [BackendId];

    /// Contains the ids of all the backends that for code written in it, this backend can use it as-is (eg. all parents
    /// and parents' parents, does not include self).
    const COMPAT_IDS: BackendCompatIds;

    // const MACRO_SIGNATURES = todo!("// TODO compiler backend macro signatures (and macro signatures in general) ");

    // TODO more complex tasks like partial compilation and hot patching definition

    type Output;
    fn compile(&self, source: &Vec<CommonModule>) -> Self::Output;

    fn info() -> BackendInfo {
        BackendInfo {
            name: Self::NAME,
            id: Self::ID,
            parent_ids: Self::PARENT_IDS,
            compat_ids: Self::COMPAT_IDS,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct BackendInfo {
    pub name: &'static str,
    pub id: BackendId,
    pub parent_ids: &'static [BackendId],
    pub compat_ids: BackendCompatIds,
}

pub struct BackendsIndex {
    loaded: HashMap<BackendId, Option<BackendInfo>>,
    by_name: HashMap<&'static str, BackendInfo>,
}

impl BackendsIndex {
    pub fn new(preinit: impl Iterator<Item = BackendId>) -> Self {
        let mut this = Self {
            loaded: HashMap::new(),
            by_name: HashMap::new(),
        };
        for id in preinit {
            this.lookup_or_load(id);
        }
        this
    }
    fn load_backend_by_id(&self, id: BackendId) -> Option<BackendInfo> {
        Some(match id {
            CommonBackend::ID => CommonBackend::info(),
            _ => return None,
        })
    }
    pub fn lookup_or_load(&mut self, id: BackendId) -> Option<BackendInfo> {
        Some(match self.loaded.get(&id) {
            Some(info) => *info.as_ref()?,
            None => {
                let info = self.load_backend_by_id(id);
                self.loaded.insert(id, info);
                info?
            }
        })
    }
    pub fn lookup(&self, id: BackendId) -> Option<BackendInfo> {
        self.loaded.get(&id).copied().flatten()
    }
    pub fn a_is_subplatform_of_b(&self, trial_child: BackendId, trial_parent: BackendId) -> bool {
        self.lookup(trial_child)
            .unwrap_or_else(|| {
                panic!(
                    "Backend {} not found in lookup, should have been loaded.",
                    trial_child
                )
            })
            .compat_ids
            .contains(&trial_parent)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct CommonBackend;
impl BackendCompiler for CommonBackend {
    const NAME: &'static str = "common";
    const ID: BackendId = 0;
    const PARENT_IDS: &'static [BackendId] = &[];

    const COMPAT_IDS: BackendCompatIds = &[Self::ID];

    type Output = ();
    fn compile(&self, source: &Vec<CommonModule>) -> Self::Output {
        panic!("CommonBackend is not a compiler, but rather an empty common ground that all actual compilers are a superset of.");
    }
}
