//! The middle layers of the compiler, responsible for converting the literal representation
//! of the code as an abstract syntax tree into a common module format ready for a backend
//! to use. This is the meat of the compiler, handing import/export, name resolution, type
//! resolution, memory safety checks, you name it.
//!
//! General order of events within this package.
//!
//! - create scopes / extract statics:
//!
//! Moves static declarations (eg. function definitions, data definitions, etc.) into their
//! own list (one for the entire module), recording the index it was inserted at as its id.
//! This id is then recorded into the scope that the declaration was made in into a map by
//! the name it was created under, (such as "hello" in the case of `fn hello()`).
//!   
//! - extract statics from AST
//! - statics phase
//!     - typelike static (module refs/datas/traits/type alias/reexports):
//!         - merge
//!             [unchecked; grouping to generate final IDs only]
//!         - resolve refs and imports
//!             [convert ASTType in all statics to ResolvedType]
//!     - codelike static (functions/impls/consts)
//!         - merge
//!         - resolve refs
//!
//! - resolve local scope variables
//! - resolve types
//! - escape analysis / borrow checking
//!
//! - assemble package

pub mod memory;
pub mod optimize;
pub mod resolution_diagnostics;
pub mod statics;
pub mod types;
