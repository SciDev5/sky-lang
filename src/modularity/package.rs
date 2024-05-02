//! Packages are the largest modular components directly supported by
//! the language, containing many modules ([`crate::middle::module`]),
//! and referencing other packages as dependencies.
//!
//! Packages may have zero or more dependencies, which are other
//! packages that the code in this package may reference, serving
//! as libraries or similar.
//!
