use std::collections::HashMap;

use super::common_module::CommonModule;

pub type BackendId = u64;
pub type BackendIdArr = &'static [BackendId];

pub trait BackendCompiler {
    /// A unique ID for this backend.
    ///
    /// To guarantee uniqueness, select ids like so:
    /// `((unix_seconds & 0xffff_ffff_ffff) << 16) | (random & 0xffff)`
    const ID: BackendId;
    const PLATFORM_INFO: PlatformInfo;

    // const MACRO_SIGNATURES = todo!("// TODO compiler backend macro signatures (and macro signatures in general) ");

    // TODO more complex tasks like partial compilation and hot patching definition

    type Output;
    fn compile(&self, source: &Vec<CommonModule>) -> Self::Output;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct PlatformInfo {
    /// A unique ID for this backend.
    ///
    /// To guarantee uniqueness, select ids like so:
    /// `((unix_seconds & 0xffff_ffff_ffff) << 16) | (random & 0xffff)`
    ///
    /// Must match [`BackendCompiler::ID`]
    pub id: BackendId,
    pub name: &'static str,
    // /// The id of the backend that is the direct parent of this.
    // pub parent_id: BackendId,
    /// Contains the ids of all the backends that for code written in it, this backend can use it as-is (eg. all parents
    /// and parents' parents. Includes self. Ordereded `self, self.parent, self.parent.parent, ... , common`).
    pub compat_ids: BackendIdArr,
}

pub struct BackendsIndex {
    loaded: HashMap<BackendId, Option<PlatformInfo>>,
    by_name: HashMap<&'static str, PlatformInfo>,
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
    fn load_backend_by_id(&self, id: BackendId) -> Option<PlatformInfo> {
        Some(match id {
            CommonBackend::ID => CommonBackend::PLATFORM_INFO,
            _ => return None,
        })
    }
    pub fn lookup_or_load(&mut self, id: BackendId) -> Option<PlatformInfo> {
        Some(match self.loaded.get(&id) {
            Some(info) => *info.as_ref()?,
            None => {
                let info = self.load_backend_by_id(id);
                self.loaded.insert(id, info);
                info?
            }
        })
    }
    pub fn lookup(&self, id: BackendId) -> Option<PlatformInfo> {
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
    const ID: BackendId = 0;
    const PLATFORM_INFO: PlatformInfo = PlatformInfo {
        id: Self::ID,
        name: "common",
        compat_ids: &[Self::ID],
    };

    type Output = ();
    fn compile(&self, source: &Vec<CommonModule>) -> Self::Output {
        panic!("CommonBackend is not a compiler, but rather an empty common ground that all actual compilers are a superset of.");
    }
}
