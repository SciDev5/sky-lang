mod tokenize_iter;

use std::{fmt::Debug, usize};

use self::tokenize_iter::TokenizeIter;

pub enum TBracketType {
    Square,
    Curly,
    Paren,
    Angle,
}
pub enum TSeparatorType {
    Comma,
    Semicolon,
    Colon,
    ThinArrow,
    WideArrow,
}
pub enum TPrefixOperatorType {
    // ---- Arithmetic ---- //
    Inverse,

    // ---- Boolean and Bitwise ---- //
    Not,

    // ---- Pointers ---- //   (everybody's favorite /s)
    Ref,
    Deref,

    // ---- Range ---- //
    RangeTo,
}
pub enum TPostfixOperatorType {
    // ---- Compare ---- //
    CompareGreater,
    CompareLess,
    CompareGreaterEqual,
    CompareLessEqual,
    CompareEqual,
    CompareNotEqual,

    // ---- Arithmetic ---- //
    Conjugate,

    // ---- Range ---- //
    RangeFrom,
}
pub enum TInfixOperatorType {
    // ---- Compare ---- //
    CompareGreater,
    CompareLess,
    CompareGreaterEqual,
    CompareLessEqual,
    CompareEqual,
    CompareNotEqual,

    // ---- Arithmetic ---- //
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,    // sign from denominator
    Remainder, // sign from numerator (stupid and awful)
    Exponentiate,

    // ---- Boolean and Bitwise ---- //
    Or,
    And,
    Xor,
    Shl,
    Shr,

    // ---- Range ---- //
    RangeFromTo,
}

macro_rules! gen_TSymbol {
    (
        $(
            $(#[doc = $doc: expr])?
            $name:ident
            (
                $text:expr
                $(; separator [$separator_ty:expr] )?
                $(; bracket_open [$bracket_open_ty:expr] )?
                $(; bracket_close [$bracket_close_ty:expr] )?
                $(; op [$infix_op_ty:expr] )?
                $(; prefix_op [$prefix_op_ty:expr] )?
                $(; postfix_op [$postfix_op_ty:expr] )?
                $(; assign_op [$assign_op_ty:expr] )?
            )
        )*
    ) => {
        #[derive(Debug, Clone, Copy)]
        pub enum TSymbol {
            $(
                $(#[doc = $doc])?
                $name
            ),*
        }
        impl CorrespondingTokenStr for TSymbol {
            fn str() -> &'static [(&'static str, Self)] {
                &[
                    $(
                        ( $text, Self:: $name )
                    ),*
                ]
            }
        }
        impl TSymbol {
            pub fn as_separator(&self) -> Option<TSeparatorType> {
                use TSeparatorType::*;
                match self {
                    $($(
                        Self:: $name => Some($separator_ty),
                    )?)*
                    _ => None
                }
            }
            pub fn as_bracket_open(&self) -> Option<TBracketType> {
                use TBracketType::*;
                match self {
                    $($(
                        Self:: $name => Some($bracket_open_ty),
                    )?)*
                    _ => None
                }
            }
            pub fn as_bracket_close(&self) -> Option<TBracketType> {
                use TBracketType::*;
                match self {
                    $($(
                        Self:: $name => Some($bracket_close_ty),
                    )?)*
                    _ => None
                }
            }
            pub fn as_prefix_op(&self) -> Option<TPrefixOperatorType> {
                use TPrefixOperatorType::*;
                match self {
                    $($(
                        Self:: $name => Some($prefix_op_ty),
                    )?)*
                    _ => None
                }
            }
            pub fn as_postfix_op(&self) -> Option<TPostfixOperatorType> {
                use TPostfixOperatorType::*;
                match self {
                    $($(
                        Self:: $name => Some($postfix_op_ty),
                    )?)*
                    _ => None
                }
            }
            pub fn as_infix_op(&self) -> Option<TInfixOperatorType> {
                use TInfixOperatorType::*;
                match self {
                    $($(
                        Self:: $name => Some($infix_op_ty),
                    )?)*
                    _ => None
                }
            }
            pub fn as_assign_op(&self) -> Option<Option<TInfixOperatorType>> {
                use TInfixOperatorType::*;
                match self {
                    $($(
                        Self:: $name => Some($assign_op_ty),
                    )?)*
                    _ => None
                }
            }
        }
    };
}
gen_TSymbol! {
    ParenOpen("("; bracket_open[Paren])
    ParenClose(")"; bracket_close[Paren])
    SquareBracketOpen("["; bracket_open[Square])
    SquareBracketClose("]"; bracket_close[Square])
    CurlyBracketOpen("["; bracket_open[Curly])
    CurlyBracketClose("]"; bracket_close[Curly])

    AddAssign("+="; assign_op[Some(Add)])
    Add("+"; op[Add])
    SubtractAssign("-="; assign_op[Some(Subtract)])
    Subtract("-"; op[Subtract])
    MultiplyAssign("*="; assign_op[Some(Multiply)])
    // asterisk is special and comes later
    RemainderAssign("/%="; assign_op[Some(Remainder)])
    Remainder("/%"; op[Remainder])
    DivideAssign("/="; assign_op[Some(Divide)])
    Divide("/"; op[Divide]; prefix_op[Inverse])
    ModuloAssign("%="; assign_op[Some(Modulo)])
    Modulo("%"; op[Modulo])
    ExponentiateAssign("^="; assign_op[Some(Exponentiate)])
    Exponentiate("^"; op[Exponentiate])

    AndAssign("&="; assign_op[Some(And)])
    And("&"; op[And])
    OrAssign("|="; assign_op[Some(Or)])
    Or("|"; op[Or])
    XorAssign("~="; assign_op[Some(Xor)])
    Xor("~"; op[Xor])
    ShlAssign("<<="; assign_op[Some(Shl)])
    Shl("<<"; op[Shl])
    ShrAssign(">>="; assign_op[Some(Shr)])
    Shr(">>"; op[Shr])

    LessThan("<"; bracket_close[Angle]; op[CompareLess])
    GreaterThan(">"; bracket_open[Angle]; op[CompareGreater])
    LessThanEqual("<="; op[CompareLessEqual])
    GreaterThanEqual(">="; op[CompareGreaterEqual])
    Equal("=="; op[CompareEqual])
    NotEqual("!="; op[CompareNotEqual])

    Assign("="; assign_op[None])

    Comma(","; separator[Comma])
    Semicolon(";"; separator[Semicolon])
    ThinArrow("->"; separator[ThinArrow])
    WideArrow("=>"; separator[WideArrow])

    Range(":"; separator[Colon]; op[RangeFromTo]; prefix_op[RangeTo]; postfix_op[RangeFrom])

    Ref("&"; prefix_op[Ref])
    Asterisk("*"; op[Multiply]; prefix_op[Deref]; postfix_op[Conjugate])
    MacroInline("$")
    MacroAttr("@")
}

macro_rules! gen_TKeyword {
    (
        $(
            $(#[doc = $doc: expr])?
            $name: ident
            ( $text: expr )
        )*
    ) => {
        #[derive(Debug, Clone, Copy)]
        pub enum TKeyword {
            $(
                $(#[doc = $doc])?
                $name
            ),*
        }
        impl CorrespondingTokenStr for TKeyword {
            fn str() -> &'static [(&'static str, Self)] {
                &[
                    $(
                        ( $text, Self:: $name )
                    ),*
                ]
            }
        }
    };
}
gen_TKeyword! {
    Let ("let")
    Del ("del")
    Return ("return")
    For ("for")
    In ("in")
    While ("while")
    Loop ("loop")
    Break ("break")
    Continue ("continue")
    If ("if")
    Else ("else")
    Fn ("fn")
    Struct ("struct")
    Tuple ("tuple")
    Enum ("enum")
    Trait ("trait")
    Impl ("impl")
    Import ("import")
    Export ("export")
}

/// A range of char indices in the source code.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Loc {
    start: usize,
    length: usize,
}

#[derive(Debug, Clone, Copy)]
pub struct Token<'src> {
    loc: Loc,
    content: TokenContent<'src>,
}

#[derive(Debug, Clone, Copy)]
pub enum TokenContent<'src> {
    Space {
        has_linebreak: bool,
    },
    Keyword(TKeyword),
    Symbol(TSymbol),
    Identifier(&'src str),
    Bool(bool),
    /// Integer literal. Contents are None if the number is too big.
    Int(Option<u128>),
    /// Float literal. Contents are None if the number is too big / small.
    Float(Option<f64>),
    Str {
        unescaped: &'src str,
        /// If the string is initiated or followed by ``` ` ``` instead of
        /// `"`, then we are exiting or entering a string interpolation.
        interpolation: (bool, bool),
    },
    Comment {
        content: &'src str,
        /// Whether this is a documenting comment (like this comment here lol)
        /// or not.
        documenting: bool,
    },
    Unknown,
}

/// Returns true and consumes part of the state char iterator if the
/// next sequence in the source is precisely equal to `str`. Otherwise
/// rewinds and returns false.
fn tokenize_matches_str_exact(state: &mut TokenizeIter, str: &'static str) -> bool {
    state.push();
    for chr_expected in str.chars() {
        if let Some((_, (_, chr_found))) = state.char_iter.next() {
            if chr_found == chr_expected {
                continue;
            }
        }
        // else //
        state.pop_rewind();
        return false;
    }
    let _ = state.pop_continue();
    return true;
}

trait CorrespondingTokenStr: Sized + Copy + 'static {
    fn str() -> &'static [(&'static str, Self)];
}
/// Checks all strings outputted by [`CorrespondingTokenStr`] to see if any
/// exactly match the next sequence in the char iter. If a match is found,
/// the corresponding value returned by [`CorrespondingTokenStr::str`] is
/// returned, otherwise None is returned.
fn tokenize_matches_any<T: CorrespondingTokenStr>(state: &mut TokenizeIter) -> Option<T> {
    for (str, v) in T::str() {
        if tokenize_matches_str_exact(state, *str) {
            return Some(*v);
        }
    }
    return None;
}

/// Match the next token in the source file.
fn tokenize_next<'src>(state: &mut TokenizeIter) -> Option<TokenContent<'src>> {
    macro_rules! first_match {
        (
            [$state:expr]
            $(
                $try_tokenize:expr => | $arg:tt | $to_token_content:expr
            ),* $(,)?
        ) => {
            $(
                if let Some($arg) = $try_tokenize ( $state ) {
                    Some($to_token_content)
                } else
            )* {
                None
            }
        };
    }
    first_match! {
        [state]
        tokenize_matches_any::<TKeyword> => |keyword| TokenContent::Keyword(keyword),
        tokenize_matches_any::<TSymbol> => |symbol| TokenContent::Symbol(symbol),
        // TODO
    }
}

pub fn tokenize<'src>(src: &'src str) -> Vec<Token<'src>> {
    TokenizeIter::new(src, tokenize_next).collect()
}

#[test]
fn t() {
    dbg!(tokenize("for+=while/%let_/ jhdkdhk *"));
}
