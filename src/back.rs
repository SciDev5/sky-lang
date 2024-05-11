//! Backends represent the requirements and procudures for compiling
//! to any given target platform, including builtin macro definitions,
//! specifications for sharing code with other backends, and a name
//! for lookup.
//!

use crate::{macros::MacroSpec, TODO_common_source};

mod common;
mod common_garbage_collected;

/// Type alias for a number representing a [`Backend`]'s id.
pub type BackendId = usize;

pub trait Backend {
    /// The unique identifier for this backend
    const ID: BackendId;
    /// Extra information for this backend, including, name and
    /// code compatibility info.
    const INFO: &'static BackendInfo;

    type Config<'a>;
    type Output;

    fn compile<'a>(src: &TODO_common_source, config: &Self::Config<'a>) -> Self::Output;
}

pub struct BackendInfo {
    /// Reference to [`Backend::ID`].
    pub id: BackendId,
    /// The name of this backend, used for looking the backend up.
    pub name: &'static str,

    /// The ids of all backends that this is compatible with and
    /// capable of using code from (this also includes the current
    /// backend's id).
    pub compat_ids: &'static [BackendId],

    /// Macro definitions specific to this backend, and handled by
    /// this backend.
    ///
    /// Note that this does not include macros from compatible
    /// more general backends, which may have their own macros.
    /// Macros from more general backends remain accessible in code
    /// when using this backend, but more specialized backends take
    /// priority in the event of name collisions.
    pub macro_specs: &'static [MacroSpec<'static>],
}