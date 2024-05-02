//! Modules are the individual components that packages are assembled out of,
//! beginning with a root module and referencing child modules by name in a
//! tree structure.
//!
//! Additionally, modules contain one or more source files targeting different
//! compilation backends ([`crate::back`]), allowing compilation to cater to
//! the needs of each target platform.
//!
//! Modules may also import and export references to static entities
//! ([`crate::middle::entity`]), such as datas, traits, and constants. Exports are
//! coordinated between these separate sources in order to allow more general
//! code to be reused between platforms.
//!

use std::collections::HashMap;

struct ModuleList<T> {
    module_list: Vec<T>,
}

type ModuleTree<'src> = ModuleList<HashMap<&'src str, usize>>;
type ModuleExports<'src> = ModuleList<HashMap<&'src str, usize>>;

impl<'src> ModuleTree<'src> {
    fn exports_step(&self, path_step: &str) -> Option<usize> {}
    fn lookup_exportsof<'a, 'b>(
        &self,
        path: &'a [&'b str],
    ) -> Result<usize, ModuleTreeLookupError<'a, 'b>> {
    }
}

pub struct ModuleTreeLookupError<'a, 'b> {
    matched: &'a [&'b str],
    failed: &'b str,
}
