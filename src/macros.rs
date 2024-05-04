//! Metaprogramming is acheived through macros, used for self-writing code,
//! for binding into the backend, and as controls for the optimizer layers,
//! among other uses.
//!
//! Macros are structured in a way such that they have a few useful properties:
//! - Macro invocations have one or a few known valid syntax options at
//! parse-time, allowing for linting tools to autofill.
//! - Inline macro invocations are type-safe, and have known eval type before
//! the type-solving step.

use crate::front::{TBracketType, TSeparatorType};

/// Defines the layout for a macro.
///
/// This includes meta-variables, which are implicitly generated in order
/// of definition here for
/// - [`MacroSpecEnt::InterpolatedString`]'s text (values treated like
/// `interject` in `Repeat`)
/// - [`MacroSpecEnt::Literal`]
/// - [`MacroSpecEnt::ExprValue`]
pub struct MacroSpec<'a> {
    name: &'a str,
    /// Multiple different possible patterns that this macro could legally
    /// match. In the case of conflict, first by this list's index takes
    /// priority.
    variants: Vec<MacroSpecEnt>,
}
pub enum MacroSpecEnt {
    /// A set of patterns bounded by parentheses/brackets.
    /// Does not imply repetition, and so does not create a repitition
    /// nesting level in [`MacroInvocationVars`].
    Group {
        bracket_ty: TBracketType,
        children: Box<[MacroSpecEnt]>,
    },
    /// A set of repeated patterns.
    /// Does not need to be bounded by brackets.
    Repeat {
        // The set of patterns to repeat.
        per_repeat: Box<[MacroSpecEnt]>,
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
    ExprValue {
        var_id: usize,
    },
}
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

pub struct MacroInvocation<'src, Expr> {
    /// Lexical content of this macro invocation, first entry is the top level.
    entries: Vec<MacroInvocationEnt<'src>>,
    strings: Vec<&'src str>,
    exprs: Vec<Expr>,

    /// Matches this to a [`MacroSpec`].
    spec_id: usize,
    /// Index into [`MacroSpec::variants`].
    variant: usize,
    /// Variables, data useful to processing the macro after parsing.
    vars: MacroInvocationVars,
}
/// Variables, data useful to processing the macro after parsing.
/// Includes all variables at a given repetition depth.
pub struct MacroInvocationVars {
    /// Represents meta-variables passed to this macro at this level of
    /// repetition.
    ///
    /// For `I` outer repeats, `J` self repeats, and `K` in-scope variables,
    /// this vec is `I*J*K` in length, formatted `[[[i;K];J];I]` where the
    /// `i`s are indices into [`MacroInvocation::entries`].
    ///
    /// Variables in [`MacroSpecEnt::Repeat::interject`] are placed directly
    /// in between variables in [`MacroSpecEnt::Repeat::per_repeat`], just
    /// as in their order of writing.
    ///
    /// Note that on the top level, `I=J=1`
    ///
    /// Order is determined by order in [`MacroSpec`].
    var_entries: Vec<usize>,
    /// Repeating expressions repeated inside this one. Represented as
    /// `(n_self_repeats, vars)`, where `n_self_repeats` is the number of
    /// times the given pattern repeated in code.
    ///
    /// Order is determined by order in [`MacroSpec`].
    nested_repeats: Vec<(usize, MacroInvocationVars)>,
}
pub enum MacroInvocationEnt<'src> {
    /// String literal interspersed with expressions
    InterpolatedString {
        /// Index of the first child entry attributed to this in [`MacroInvocation::entries`].
        entries_i: usize,
        /// Index of the first text entry attributed to this in [`MacroInvocation::strings`].
        strings_i: usize,
        /// Number of expressions interjected into this interpolated string.
        /// The number of string segments is equal to `n+1`.
        n: usize,
    },
    /// Literal value, such as `3`, `true`, or `"helloworld"`.
    Literal(MacroLiteral<'src>),
    /// An expression of code incorperated into the macro call, such as `a.do_thing() + 3`
    Expr {
        /// Index in [`MacroInvocation::exprs`].
        i: usize,
    },
    /// A parentheses/brackets-bounded group of
    Group {
        bracket_ty: TBracketType,
        /// Index in [`MacroInvocation::entries`].
        i: usize,
        /// Number of child entries in this group.
        n: usize,
    },
    Separator(TSeparatorType),
}
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum MacroLiteral<'src> {
    String(&'src str),
    Bool(bool),
    U128(u128),
    U64(u64),
    U32(u32),
    U16(u16),
    U8(u8),
    I128(i128),
    I64(i64),
    I32(i32),
    I16(i16),
    I8(i8),
    F32(f32),
    F64(f64),
}
