//! impl is of two types, function and trait.
//! at package level, exists:
//! - `HashMap<str, ImplTable<FunctionImpl>>`
//! - `HashMap<str, ImplTable<Const>>`
//! - `HashMap<str, ImplTable<TypeAlias>>`
//! - `ImplTable<TraitImpl>`
//!
//! `ImplTable<K>` needs to map generic types like <T>`(T,Array<T,4>)` to `K` valid for it
//! impltable needs to resolve at least this ^
//!
//!
//!
//! when getting an associated function reference (given its not a trait)
//! x.abc()
//! need to pick impl that is compatible with type of x and accessible in the compilation target
//!
//!
//! resolve function first, then select by platform
//!
//!
//!
//!
//!
//!
//! The middle layers of the compiler, responsible for converting the literal representation
//! of the code as an abstract syntax tree into a common module format ready for a backend
//! to use. This is the meat of the compiler, handing import/export, name resolution, type
//! resolution, memory safety checks, you name it.
//!
//! General order of events within this package.
//!  
//! - extract statics from AST
//!         Moves static declarations (eg. function definitions, data definitions, etc.) into their
//!         own list (one for the entire module), recording the index it was inserted at as its id.
//!         This id is then recorded into the scope that the declaration was made in into a map by
//!         the name it was created under, (such as "hello" in the case of `fn hello()`).
//! - statics phase
//!     - merge statics (except impls)
//!     - resolve imports
//!     - free and merge impls (resolve target types)
//!     - resolve types in statics
//!     - verify merge (and compact)
//!     - verify impls (and compact)
//!
//! - resolve local scope variables
//! - resolve types
//! - escape analysis / borrow checking
//!
//! - assemble package

pub mod impls;
pub mod import;
pub mod memory;
pub mod module;
pub mod optimize;
pub mod resolution_diagnostics;
pub mod statics;
pub mod types;
