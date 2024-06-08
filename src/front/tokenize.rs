mod tokenize_iter;

use std::fmt::Debug;

use self::tokenize_iter::{CharIndex, TokenizeIter};

use super::source::Loc;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TBracketType {
    Square,
    Curly,
    Paren,
    Angle,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TSeparatorType {
    Comma,
    Period,
    Semicolon,
    Colon,
    ThinArrow,
    WideArrow,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TPrefixOperatorType {
    // ---- Arithmetic ---- //
    Neg,
    Inverse,

    // ---- Boolean and Bitwise ---- //
    Not,

    // ---- Pointers ---- //   (everybody's favorite /s)
    Ref,
    Deref,

    // ---- Range ---- //
    RangeTo,
}
impl TPrefixOperatorType {
    /// higher precedence -> evaluated first
    pub fn precedence(self) -> u8 {
        match self {
            Self::Ref => 100,
            Self::Deref => 100,
            Self::Inverse => 71,
            Self::Neg => 61,
            Self::Not => 36,
            Self::RangeTo => 10,
        }
    }
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TPostfixOperatorType {
    // ---- Arithmetic ---- //
    Conjugate,

    // ---- Range ---- //
    RangeFrom,
}
impl TPostfixOperatorType {
    /// higher precedence -> evaluated first
    pub fn precedence(self) -> u8 {
        match self {
            Self::Conjugate => 80,
            Self::RangeFrom => 10,
        }
    }
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
impl TInfixOperatorType {
    /// higher precedence -> evaluated first
    pub fn precedence(self) -> u8 {
        match self {
            Self::Exponentiate => 70,

            Self::Multiply => 60,
            Self::Divide => 60,
            Self::Modulo => 60,
            Self::Remainder => 60,

            Self::Add => 50,
            Self::Subtract => 50,

            Self::Shl => 40,
            Self::Shr => 40,

            Self::CompareGreater => 35,
            Self::CompareLess => 35,
            Self::CompareGreaterEqual => 35,
            Self::CompareLessEqual => 35,

            Self::CompareEqual => 30,
            Self::CompareNotEqual => 30,

            Self::Or => 20,
            Self::And => 20,
            Self::Xor => 20,

            Self::RangeFromTo => 10,
        }
    }
    /// whether for any given operator `;`, `a ; b ; c` is read as
    /// `(a ; b) ; c` (left associative), or `a ; (b ; c)` (right
    /// associative).
    pub fn right_associative(self) -> bool {
        matches!(self, Self::Exponentiate)
    }
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
        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
    CurlyBracketOpen("{"; bracket_open[Curly])
    CurlyBracketClose("}"; bracket_close[Curly])

    AddAssign("+="; assign_op[Some(Add)])
    Add("+"; op[Add])
    SubtractAssign("-="; assign_op[Some(Subtract)])
    Subtract("-"; op[Subtract]; prefix_op[Neg])
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
    // ampersand is special and comes later
    OrAssign("|="; assign_op[Some(Or)])
    Or("|"; op[Or])
    XorAssign("~="; assign_op[Some(Xor)])
    Xor("~"; op[Xor])
    ShlAssign("<<="; assign_op[Some(Shl)])
    Shl("<<"; op[Shl])
    ShrAssign(">>="; assign_op[Some(Shr)])
    Shr(">>"; op[Shr])

    LessThan("<"; bracket_open[Angle]; op[CompareLess])
    GreaterThan(">"; bracket_close[Angle]; op[CompareGreater])
    LessThanEqual("<="; op[CompareLessEqual])
    GreaterThanEqual(">="; op[CompareGreaterEqual])
    Equal("=="; op[CompareEqual])
    NotEqual("!="; op[CompareNotEqual])

    Assign("="; assign_op[None])

    Comma(","; separator[Comma])
    Period("."; separator[Period])
    Semicolon(";"; separator[Semicolon])
    ThinArrow("->"; separator[ThinArrow])
    WideArrow("=>"; separator[WideArrow])

    Colon(":"; separator[Colon]; op[RangeFromTo]; prefix_op[RangeTo]; postfix_op[RangeFrom])

    Ampersand("&"; op[And]; prefix_op[Ref])
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
        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
    Const ("const")
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
    Type ("type")
    Data ("data")
    Struct ("struct")
    Tuple ("tuple")
    Enum ("enum")
    Unit ("unit")
    Trait ("trait")
    Impl ("impl")
    Import ("import")
    Export ("export")
    SelfVar ("self")
    SelfTy ("Self")
    Root ("root")
    Super ("super")
}

impl CorrespondingTokenStr for bool {
    fn str() -> &'static [(&'static str, Self)] {
        &[("true", true), ("false", false)]
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Token<'src> {
    pub loc: Loc,
    pub content: TokenContent<'src>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenContent<'src> {
    Space {
        has_linebreak: bool,
    },
    Keyword(TKeyword),
    Symbol(TSymbol),
    Identifier(&'src str),
    Bool(bool),
    Number {
        decimal: bool,
        exp: bool,
        radix: u8,
        unparsed: &'src str,
    },
    Str {
        unescaped: &'src str,
        /// If the string is initiated or followed by ``` ` ``` instead of
        /// `"`, then we are exiting or entering a string interpolation.
        interpolation: (bool, bool),
        /// True if the closing quote of this string was missing, should be
        /// reported as an error by the parser.
        missing_closing: bool,
    },
    Comment {
        content: &'src str,
        /// Whether this is a documenting comment (like this comment here lol)
        /// or not.
        documenting: bool,
        /// True if the closing sequence (like `*/`) of this comment was
        /// missing, should be reported as an error by the parser.
        missing_closing: bool,
    },
    Unknown,
}

/// Returns true and consumes part of the state char iterator if the
/// next sequence in the source is precisely equal to `str`. Otherwise
/// rewinds and returns false.
fn tokenize_matches_str_exact(state: &mut TokenizeIter, str: &'static str) -> bool {
    state.push();
    for chr_expected in str.chars() {
        if let Some(CharIndex {
            char: chr_found, ..
        }) = state.char_iter.next()
        {
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

/// Read in a token representing an identifier which starts with a letter or
/// underscore and is followed by zero or more letters, numbers, or underscores.
fn tokenize_ident<'src>(state: &mut TokenizeIter<'src>) -> Option<&'src str> {
    state.push();
    if !state
        .char_iter
        .next()
        .is_some_and(|ci| ci.char.is_alphabetic() || ci.char == '_')
    {
        state.pop_rewind();
        return None;
    }
    while state
        .next_if(|chr| chr.is_alphanumeric() || chr == '_')
        .is_some()
    {
        continue;
    }

    Some(state.pop_continue_as_str())
}

/// Read in all of the upcoming whitespace, returning None if no whitespace is
/// founde, otherwise returning with whether there was a newline in the space.
fn tokenize_whitespace(state: &mut TokenizeIter) -> Option<bool> {
    if !state.peek_next().is_some_and(|ci| ci.char.is_whitespace()) {
        return None;
    }
    let mut has_newline = false;
    while let Some(chr) = state.next_if(|char| char.is_whitespace()) {
        has_newline |= chr == '\n';
    }
    Some(has_newline)
}

/// Tokenize numbers of all kind,
fn tokenize_numeric<'src>(state: &mut TokenizeIter<'src>) -> Option<TokenContent<'src>> {
    state.push();
    macro_rules! fail {
        () => {{
            state.pop_rewind();
            return None;
        }};
    }
    macro_rules! unwrap_or_fail {
        ($x:expr) => {
            if let Some(x) = $x {
                x
            } else {
                fail!()
            }
        };
    }
    let zero = {
        let next = unwrap_or_fail!(state.char_iter.next());
        if next.char.is_ascii_digit() {
            next.char == '0'
        } else {
            fail!();
        }
    };
    let mut matching = true;
    let mut decimal;
    let mut exp;
    let base: u8;
    let allow_exp;

    // second digit
    if let Some(c) = state.next_if(|chr| chr.is_ascii_alphanumeric() || chr == '.') {
        base = match (zero, c) {
            (true, 'x') => 16,
            (true, 'o') => 8,
            (true, 'b') => 2,
            _ => 10,
        };
        decimal = c == '.';
        exp = c == 'e' || c == 'E';
        allow_exp = c.is_ascii_digit() || c == '.';
    } else {
        matching = false;
        base = 10;

        decimal = false;
        exp = false;
        allow_exp = false;
    };

    // integer part
    while matching && !exp && !decimal {
        if let Some(c) = state.next_if(|c| c.is_ascii_alphanumeric() || c == '.') {
            if c == '.' {
                decimal = true;
            }
            if allow_exp && (c == 'e' || c == 'E') {
                exp = true;
            }
        } else {
            matching = false;
        }
    }
    // decimal part
    if decimal {
        while matching && !exp {
            if let Some(c) = state.next_if(|c| c.is_ascii_alphanumeric()) {
                if allow_exp && (c == 'e' || c == 'E') {
                    exp = true;
                }
            } else {
                matching = false;
            }
        }
    }
    // exponential part (the `e+123` in `1.23e+123`)
    if exp {
        if state
            .next_if(|c| c.is_ascii_alphanumeric() || c == '+' || c == '-')
            .is_some()
        {
            while state.next_if(|c| c.is_alphanumeric()).is_some() {
                continue;
            }
        }
    }

    Some(TokenContent::Number {
        decimal,
        exp,
        radix: base,
        unparsed: state.pop_continue_as_str(),
    })
}

/// Parse string literals.
fn tokenize_string<'src>(state: &mut TokenizeIter<'src>) -> Option<TokenContent<'src>> {
    let start_interpolation = state.next_if(|c| c == '"' || c == '`')? == '`';
    state.push();
    let mut had_escape = false;
    let mut missing_closing = false;
    let (unescaped, end_interpolation) = loop {
        if had_escape {
            had_escape = false;
            if state.char_iter.next().is_none() {
                missing_closing = true;
                break (state.pop_continue_as_str(), false);
            }
        } else {
            if let Some(CharIndex { char, .. }) = state.char_iter.peek_next() {
                match char {
                    '`' | '"' => {
                        let str = state.pop_continue_as_str();
                        let is_interp = char == '`';
                        let _ = state.char_iter.next();
                        break (str, is_interp);
                    }
                    '\\' => {
                        had_escape = true;
                    }
                    _ => {}
                }
                let _ = state.char_iter.next();
            } else {
                missing_closing = true;
                break (state.pop_continue_as_str(), false);
            }
        }
    };

    Some(TokenContent::Str {
        unescaped,
        interpolation: (start_interpolation, end_interpolation),
        missing_closing,
    })
}

/// Tokenize inline (`// ...`) and multiline (`/* */`) comments.
fn tokenize_comment<'src>(state: &mut TokenizeIter<'src>) -> Option<TokenContent<'src>> {
    state.push();
    if state.next_if(|c| c == '/').is_none() {
        state.pop_rewind();
        return None;
    }
    fn multiline<'src>(state: &mut TokenizeIter<'src>) -> Option<TokenContent<'src>> {
        state.next_if(|c| c == '*')?;
        let documenting = state.next_if(|c| c == '*').is_some();
        if documenting && state.next_if(|c| c == '/').is_some() {
            // special case!
            // comment was actually /**/, which is empty, and is not actually a doc comment.
            return Some(TokenContent::Comment {
                content: "",
                documenting: false,
                missing_closing: false,
            });
        }

        let mut depth: u32 = 1;
        let mut just_slash = false;
        let mut just_star = false;
        state.push();
        while let Some(CharIndex { char, .. }) = state.char_iter.next() {
            if just_slash {
                if char == '*' {
                    // `/*`
                    depth += 1;
                }
                just_slash = false;
            } else if just_star {
                if char == '/' {
                    //  `*/`
                    depth -= 1;
                    if depth == 0 {
                        break;
                    }
                }
                just_star = false;
            } else {
                just_slash = char == '/';
                just_star = char == '*';
            }
        }
        Some(TokenContent::Comment {
            content: state.pop_continue_as_str().trim_end_matches("*/"),
            documenting,
            missing_closing: depth != 0,
        })
    }
    fn inline<'src>(state: &mut TokenizeIter<'src>) -> Option<TokenContent<'src>> {
        state.next_if(|c| c == '/')?;
        let documenting = state.next_if(|c| c == '/').is_some();

        state.push();
        while state.next_if(|c| c != '\n').is_some() {
            // consume until we hit a newline
        }

        Some(TokenContent::Comment {
            content: state.pop_continue_as_str(),
            documenting,
            missing_closing: false,
        })
    }
    if let Some(res) = multiline(state).or_else(|| inline(state)) {
        let _ = state.pop_continue();
        Some(res)
    } else {
        state.pop_rewind();
        None
    }
}

/// Match the next token in the source file.
fn tokenize_next<'src>(state: &mut TokenizeIter<'src>) -> Option<TokenContent<'src>> {
    macro_rules! switch {
        ( [$($pos:tt)+] else [ $($neg:tt)* ] ) => {
            $($pos)*
        };
        ( [  ] else [ $($neg:tt)* ] ) => {
            $($neg)*
        };
    }
    macro_rules! first_match {
        (
            [$state:expr]
            $(
                $try_tokenize:expr $( => | $arg:tt | $to_token_content:expr )?
            ),* $(,)?
        ) => {
            $(
                if let Some(x) = $try_tokenize ( $state ) {
                    switch! {
                        [$( let $arg = x; Some($to_token_content) )?]
                        else [ Some(x) ]
                    }
                } else
            )* {
                None
            }
        };
    }
    first_match! {
        [state]
        tokenize_whitespace => |has_linebreak| TokenContent::Space { has_linebreak },
        tokenize_comment,
        tokenize_matches_any::<TKeyword> => |keyword| TokenContent::Keyword(keyword),
        tokenize_matches_any::<TSymbol> => |symbol| TokenContent::Symbol(symbol),
        tokenize_matches_any::<bool> => |bool| TokenContent::Bool(bool),
        tokenize_numeric,
        tokenize_string,
        tokenize_ident => |ident| TokenContent::Identifier(ident),

    }
}

#[allow(unused)]
pub fn tokenize<'src>(src: &'src str) -> Vec<Token<'src>> {
    TokenizeIter::new(src, tokenize_next).collect()
}

#[cfg(test)]
mod test {
    use crate::front::tokenize::{tokenize, Loc, TKeyword, TSymbol, TokenContent};

    #[test]
    fn string_literals() {
        assert_eq!(
            &tokenize("\"hello`")
                .into_iter()
                .map(|v| v.content)
                .collect::<Vec<_>>()[..],
            &[TokenContent::Str {
                unescaped: "hello",
                interpolation: (false, true),
                missing_closing: false
            },][..]
        );
    }

    #[test]
    fn general_tokenization_test() {
        assert_eq!(
            &tokenize("for+=while/%let/* /* */ */true jhdkdhk *")
                .into_iter()
                .map(|v| v.content)
                .collect::<Vec<_>>()[..],
            &[
                TokenContent::Keyword(TKeyword::For),
                TokenContent::Symbol(TSymbol::AddAssign),
                TokenContent::Keyword(TKeyword::While),
                TokenContent::Symbol(TSymbol::Remainder),
                TokenContent::Keyword(TKeyword::Let),
                TokenContent::Comment {
                    content: " /* */ ",
                    documenting: false,
                    missing_closing: false
                },
                TokenContent::Bool(true),
                TokenContent::Space {
                    has_linebreak: false
                },
                TokenContent::Identifier("jhdkdhk"),
                TokenContent::Space {
                    has_linebreak: false
                },
                TokenContent::Symbol(TSymbol::Asterisk),
            ][..]
        );
    }
    #[test]
    fn loc() {
        assert_eq!(
            &tokenize("+abc+=\"def\"")
                .into_iter()
                .map(|v| v.loc)
                .collect::<Vec<_>>()[..],
            &[
                Loc {
                    start: 0,
                    length: 1
                },
                Loc {
                    start: 1,
                    length: 3
                },
                Loc {
                    start: 4,
                    length: 2
                },
                Loc {
                    start: 6,
                    length: 5
                },
            ][..]
        )
    }
}
