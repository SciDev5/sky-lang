//! Metaprogramming is acheived through macros, used for self-writing code,
//! for binding into the backend, and as controls for the optimizer layers,
//! among other uses.
//!
//! Macros are structured in a way such that they have a few useful properties:
//! - Macro invocations have one or a few known valid syntax options at
//! parse-time, allowing for linting tools to autofill.
//! - Inline macro invocations are type-safe, and have known eval type before
//! the type-solving step.

use crate::front::tokenize::{TBracketType, TSeparatorType};

/// Defines the layout for a macro.
///
/// This includes meta-variables, which are implicitly generated in order
/// of definition here for
/// - [`MacroSpecEnt::InterpolatedString`]'s text (values treated like
/// `interject` in `Repeat`)
/// - [`MacroSpecEnt::Literal`]
/// - [`MacroSpecEnt::ExprValue`]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MacroSpec {
    pub name: String,
    /// Multiple different possible patterns that this macro could legally
    /// match. In the case of conflict, first by this list's index takes
    /// priority.
    pub variants: Vec<MacroSpecEnt>,
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MacroSpecEnt {
    /// A set of patterns bounded by parentheses/brackets.
    /// Does not imply repetition, and so does not create a repitition
    /// nesting level in [`MacroInvocationVars`].
    Group {
        bracket_ty: TBracketType,
        children: Vec<MacroSpecEnt>,
    },
    /// A set of repeated patterns.
    /// Does not need to be bounded by brackets.
    Repeat {
        // The set of patterns to repeat.
        per_repeat: Vec<MacroSpecEnt>,
        // An optional pattern that goes only between the repeated patterns.
        interject: Option<Box<MacroSpecEnt>>,
        /// Minumim number of matches this pattern requires to be valid.
        count_min: usize,
        /// Number of matches above `count_min` that this pattern allows.
        count_max_over: Option<usize>,
    },
    /// A string template/interpolated string.
    ///
    /// Matches patterns like ```"hello `[x,y,z]` world"```.
    InterpolatedString {
        // The pattern to repeat.
        per_repeat: Box<MacroSpecEnt>,
        /// Minumim number of matches this pattern requires to be valid.
        count_min: usize,
        /// Number of matches above `count_min` that this pattern allows.
        count_max_over: Option<usize>,
    },
    Separator(TSeparatorType),
    /// Require matching a literal, like `3`, or `"helloworld"`
    Literal {
        ty: MacroSpecLiteralTy,
    },
    /// Require an expression that simply evaluates to a value, executed
    /// before the macro-specific code.
    ExprValue,
    /// Require a literal name
    Enum {
        one_of: Vec<String>,
    },
}
impl MacroSpecEnt {
    pub fn unwrap_enum(&self) -> &[String] {
        match self {
            Self::Enum { one_of } => &one_of,
            _ => panic!("unwrap_enum invalid"),
        }
    }
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MacroSpecLiteralTy {
    String,
    Bool,
    U128,
    U64,
    U32,
    U16,
    U8,
    I128,
    I64,
    I32,
    I16,
    I8,
    F32,
    F64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ConstMacroSpec<'a> {
    name: &'a str,
    variants: &'a [ConstMacroSpecEnt<'a>],
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ConstMacroSpecEnt<'a> {
    Group {
        bracket_ty: TBracketType,
        children: &'a [ConstMacroSpecEnt<'a>],
    },
    Repeat {
        per_repeat: &'a [ConstMacroSpecEnt<'a>],
        interject: Option<&'a ConstMacroSpecEnt<'a>>,
        count_min: usize,
        count_max_over: Option<usize>,
    },
    InterpolatedString {
        per_repeat: &'a ConstMacroSpecEnt<'a>,
        count_min: usize,
        count_max_over: Option<usize>,
    },
    Separator(TSeparatorType),
    Literal {
        ty: MacroSpecLiteralTy,
    },
    ExprValue,
    Enum {
        one_of: &'a [&'a str],
    },
}
impl<'a> ConstMacroSpec<'a> {
    pub fn to_allocated(&self) -> MacroSpec {
        MacroSpec {
            name: self.name.to_string(),
            variants: self
                .variants
                .iter()
                .map(ConstMacroSpecEnt::to_allocated)
                .collect(),
        }
    }
}
impl<'a> ConstMacroSpecEnt<'a> {
    fn to_allocated(&self) -> MacroSpecEnt {
        match self {
            ConstMacroSpecEnt::Group {
                bracket_ty,
                children,
            } => MacroSpecEnt::Group {
                bracket_ty: *bracket_ty,
                children: children.iter().map(Self::to_allocated).collect(),
            },
            ConstMacroSpecEnt::Repeat {
                per_repeat,
                interject,
                count_min,
                count_max_over,
            } => MacroSpecEnt::Repeat {
                per_repeat: per_repeat.iter().map(Self::to_allocated).collect(),
                interject: interject.map(Self::to_allocated).map(Box::new),
                count_min: *count_min,
                count_max_over: *count_max_over,
            },
            ConstMacroSpecEnt::InterpolatedString {
                per_repeat,
                count_min,
                count_max_over,
            } => MacroSpecEnt::InterpolatedString {
                per_repeat: Box::new(per_repeat.to_allocated()),
                count_min: *count_min,
                count_max_over: *count_max_over,
            },
            ConstMacroSpecEnt::Separator(sep) => MacroSpecEnt::Separator(*sep),
            ConstMacroSpecEnt::Literal { ty } => MacroSpecEnt::Literal { ty: *ty },
            ConstMacroSpecEnt::ExprValue => MacroSpecEnt::ExprValue,
            ConstMacroSpecEnt::Enum { one_of } => MacroSpecEnt::Enum {
                one_of: one_of.iter().map(|s| s.to_string()).collect(),
            },
        }
    }
}
