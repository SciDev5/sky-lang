use super::ops::SLOperator;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum SLToken<'a> {
    Space { hard: bool },
    Keyword(Keyword),
    Identifier(&'a str),
    Bool(bool),
    Int { value: i128, imaginary: bool },
    Float { value: f64, imaginary: bool },
    NumLiteralInvalid(&'a str),
    BracketOpen(BracketType),
    BracketClose(BracketType),
    Separator(SeparatorType),
    Symbol(SLSymbol),
    Unknown(&'a str),
    Comment { content: &'a str, documenting: bool },
}

pub enum SLTokenBreakType {
    None,
    Break { hard: bool },
}
impl SLTokenBreakType {
    pub fn is_hard(self) -> bool {
        matches!(self, SLTokenBreakType::Break { hard: true })
    }
    pub fn is_soft(self) -> bool {
        matches!(self, SLTokenBreakType::Break { hard: false })
    }
    pub fn is_some(self) -> bool {
        matches!(self, SLTokenBreakType::Break { .. })
    }
}

impl<'a> SLToken<'a> {
    pub fn break_type(&self) -> SLTokenBreakType {
        match self {
            Self::Comment { .. } => SLTokenBreakType::Break { hard: false },
            Self::Space { hard } => SLTokenBreakType::Break { hard: *hard },
            _ => SLTokenBreakType::None,
        }
    }
    pub fn is_whitespace(&self) -> bool {
        matches!(self, SLToken::Space { .. })
    }
    pub fn is_break(&self) -> bool {
        self.break_type().is_some()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AngleBracketShape {
    /** `>` symbol, which could be transformed into `SLOperator::GreaterThan` or `BracketOpen(BracketType::Angle)` */
    OpenOrLessThan,
    /** `<` symbol, which could be transformed into `SLOperator::LessThan` or `BracketClose(BracketType::Angle)` */
    CloseOrGreaterThan,
}
impl AngleBracketShape {
    pub fn bracket_type(self) -> BracketType {
        BracketType::Angle
    }
    pub fn is_open_bracket(self) -> bool {
        match self {
            Self::OpenOrLessThan => true,
            Self::CloseOrGreaterThan => false,
        }
    }
    pub fn to_operator(self) -> SLOperator {
        match self {
            Self::CloseOrGreaterThan => SLOperator::GreaterThan,
            Self::OpenOrLessThan => SLOperator::LessThan,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BracketType {
    Square,
    Paren,
    Curly,
    Angle,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SeparatorType {
    Comma,
    Period,
    Semicolon,
    Colon,
    ThinArrowRight,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SyntacticSugarType {
    /** `#` hash/pound symbol, used to denote array/matrix/tensor stuff mostly. */
    Hash,
}

macro_rules! impl_tokenizer {
    {
        $struct_visibility: vis $struct_name: ident .
        $fn_name: ident <$lifetime: tt> (program: &str) -> Vec<$T: ty> ;
        unknown_token ($unk_text_name: ident) => $unknown: expr;
        [ $($subtokenizer: ident),+ $(,)? ]
    } => {
        paste::paste!{
            $struct_visibility struct $struct_name {
                re_combined: regex::Regex,
                $( // Store per-subtokenizer regexes in variables named `re_subtokenizer_<name>`
                    #[allow(non_snake_case)]
                    [< re_subtokenizer_ $subtokenizer >] : regex::Regex,
                )+
            }
        }
        impl $struct_name {
            pub fn new() -> Self {
                paste::paste!{
                    Self {
                        re_combined: regex::Regex::new(
                            &[$($subtokenizer.re_combined()),+].join("|")
                        ).expect("generated tokenizer regex was invalid"),
                        $(
                            [< re_subtokenizer_ $subtokenizer >] : regex::Regex::new(
                                & ["^(",& $subtokenizer.re_combined(),")$"].join("")
                            ).expect("generated tokenizer regex was invalid"),
                        )+
                    }
                }
            }
            pub fn $fn_name<$lifetime> (&self, program: & $lifetime str) -> Vec<$T> {
                let mut last_end = 0;
                let mut tokens = vec![];

                for matched in self.re_combined.find_iter(program) {
                    // Detect skipped characters and mark them as "unknown"
                    if matched.start() > last_end {
                        let $unk_text_name = &program[last_end..matched.start()];
                        tokens.push($unknown);
                    }
                    last_end = matched.end();

                    // Match by each of the inner parsers.
                    let matched_str = matched.as_str();
                    $({
                        if paste::paste!{ self.[< re_subtokenizer_ $subtokenizer >] }.is_match(matched_str) {
                            tokens.push($subtokenizer.match_it(matched_str));
                            continue
                        }
                    })+
                }

                tokens
            }
        }
    };
}
macro_rules! gen_tokenizer_struct {
    {
        $struct_name: ident, $outer_enum_type: ident;
        $re_combined_impl: expr;
        $match_it_param_name: ident => $match_it_impl: expr
    } => {
        #[allow(non_camel_case_types)]
        struct $struct_name;
        impl $struct_name {
            fn re_combined(&self) -> String {
                $re_combined_impl
            }
            fn match_it<'a>(&self, $match_it_param_name: &'a str) -> $outer_enum_type<'a> {
                $match_it_impl
            }
        }
    };
}
macro_rules! subtokenize_sub_enum {
    {
        $name: ident;
        $outer_enum_type: ident : $outer_enum_entry: expr;
        $($literal: expr => $inner_val: expr),+ $(,)?
    } => {
        gen_tokenizer_struct! {
            $name, $outer_enum_type;
            [$(regex::escape($literal)),+].join("|");
            matched_str => match matched_str {
                $(
                    $literal => {
                        let it = $inner_val;
                        $outer_enum_entry(it)
                    },
                )+
                _ => panic!("should not be possible"),
            }
        }
    };
}
macro_rules! subtokenize_sub_enum_words {
    {
        $name: ident;
        $outer_enum_type: ident : $outer_enum_entry: expr;
        $($literal: expr => $inner_val: expr),+ $(,)?
    } => {
        gen_tokenizer_struct! {
            $name, $outer_enum_type;
            [$( [r"\b",&regex::escape($literal),r"\b"].join("")  ),+].join("|");
            matched_str => match matched_str {
                $(
                    $literal => {
                        let it = $inner_val;
                        $outer_enum_entry(it)
                    },
                )+
                _ => panic!("should not be possible"),
            }
        }
    };
}
macro_rules! subtokenize_fn {
    {
        $name: ident;
        $outer_enum_type: ident;
        $re: expr => |$match_it_param_name: ident| $callback: expr
    } => {
        gen_tokenizer_struct! {
            $name, $outer_enum_type;
            $re.to_string();
            $match_it_param_name => $callback
        }
    };
}
macro_rules! subtokenize_bracket_sub_enum {
    {
        $name: ident;
        $outer_enum_type: ident : ($outer_enum_entry_open: expr, $outer_enum_entry_close: expr $(,)?);
        $($literal_open: expr, $literal_close: expr => $inner_val: expr),+ $(,)?
    } => {
        gen_tokenizer_struct! {
            $name, $outer_enum_type;
            [$(regex::escape($literal_open),regex::escape($literal_close)),+].join("|");
            matched_str => match matched_str {
                $(
                    $literal_open => {
                        let it = $inner_val;
                        $outer_enum_entry_open(it)
                    },
                    $literal_close => {
                        let it = $inner_val;
                        $outer_enum_entry_close(it)
                    },
                )+
                _ => panic!("should not be possible"),
            }
        }
    };
}

subtokenize_fn! {
    tokenize_comment;
    SLToken;
    r"\/\/[^\n]*|\/\*(.|\s)*\*\/" => |str| {
        if str.starts_with("///") {
            SLToken::Comment { content: str, documenting: true } // str will be cleaned in doc comment formatting
        } else if str.starts_with("//") {
            SLToken::Comment { content: str.strip_prefix("//").unwrap().trim(), documenting: false }
        } else if str.starts_with("/**") && str != "/**/" {
            SLToken::Comment { content: str, documenting: true } // str will be cleaned in doc comment formatting
        } else if str.starts_with("/*") {
            SLToken::Comment { content: str.strip_prefix("/*").unwrap().strip_suffix("*/").unwrap().trim(), documenting: false }
        } else {
            panic!("tokenize_comment -> should be impossible unless the regex is wrong");
        }
    }
}

macro_rules! gen_Keyword {
    ($(
        $(#[doc = $doc: expr])?
        $ident: ident ( $key_str: expr )
    ),* $(,)?) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        pub enum Keyword {
            $(
                $(#[doc = $doc])?
                $ident
            ),*
        }

        subtokenize_sub_enum_words! {
            tokenize_keyword;
            SLToken : |keyword| SLToken::Keyword(keyword);
            $($key_str => Keyword:: $ident ),*,
        }
    };
}
gen_Keyword! {
    /// Declare variable
    VarDeclare ("let"),
    /// Declare constant variable
    VarConstDeclare ("const"),
    /// Delete/destroy variable
    VarDestroy ("del"),
    /// Function return value
    Return ("return"),
    /// For loop keyword
    LoopFor ("for"),
    /// In keyword
    In ("in"),
    /// While loop keyword
    LoopWhile ("while"),
    /// Forever loop keyword
    LoopForever ("loop"),
    /// Break out of loop
    LoopBreak ("break"),
    /// Continue to next loop iteration
    LoopContinue ("continue"),
    /// Conditional
    ConditionalIf ("if"),
    /// Conditional else branch
    ConditionalElse ("else"),
    /// Function definition
    FunctionDefinition ("fn"),
    /// Structured data type definition
    StructDefinition ("struct"),
    /// Struct tuple data definition keyword
    StructTuple ("tuple"),
    /// Enum definition
    EnumDefinition ("enum"),
}

macro_rules! gen_SLSymbol {
    ($(
        $ident: ident ( $key_str: expr $(, op = $op: expr)? $(, assign_op = $assign_op: expr)?)
    ),* $(,)?) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        pub enum SLSymbol {
            $($ident),*
        }
        impl SLSymbol {
            pub fn to_operator(self) -> Option<SLOperator> {
                match self {
                    $(
                        Self:: $ident => {
                            None $( .or({
                                use SLOperator::*;
                                Some($op)
                            }) )?
                        }
                    )*
                }
            }
            pub fn to_assignment_operator(self) -> Option<Option<SLOperator>> {
                match self {
                    $(
                        Self:: $ident => {
                            None $( .or({
                                #[allow(unused)] // assign_op = None will not reference, but this is still convenient
                                use SLOperator::*;
                                Some($assign_op)
                            }) )?
                        }
                    )*
                }
            }
        }


        subtokenize_sub_enum! {
            tokenize_symbols;
            SLToken : |sep| SLToken::Symbol(sep);
            $($key_str => SLSymbol:: $ident ),*,
        }
    };
}
gen_SLSymbol! {
    // operators with longer sequences go first in order to make sure they don't
    // get mixed up with shorter ops that start with the same thing.

    Shl (">>", op = Shl),
    Shr ("<<", op = Shr),

    Equal ("==", op = Equal),
    GreaterEqual (">=", op = GreaterEqual),
    LessEqual ("<=", op = LessEqual),
    NotEqual ("!=", op = NotEqual),

    AngleOpen ("<", op = LessThan),
    AngleClose (">", op = GreaterThan),

    MatExp ("^^", op = MatExp),
    MatTimes ("**", op = MatTimes),
    Inverse ("_/", op = Inverse),

    Assign ("=", assign_op = None), // `None` here means no associated operator to assignment like the `+` in `+=`
    AssignPlus ("+=", assign_op = Some(Plus)),
    AssignMinus ("-=", assign_op = Some(Minus)),
    AssignTimes ("*=", assign_op = Some(Times)),
    AssignDiv ("/=", assign_op = Some(Div)),
    AssignRemainder ("/%=", assign_op = Some(Remainder)),
    AssignModulo ("%=", assign_op = Some(Modulo)),
    AssignXor ("~=", assign_op = Some(Xor)),
    AssignAnd ("&=", assign_op = Some(And)),
    AssignOr ("|=", assign_op = Some(Or)),


    HermitianConjugate ("'", op = HermitianConjugate),
    Transpose ("\"", op = Transpose),

    Not ("!", op = Not),
    Xor ("~", op = Xor),
    Or ("|", op = Or),
    And ("&", op = And),

    Plus ("+", op = Plus),
    Minus ("-", op = Minus),
    Times ("*", op = Times),
    Div ("/", op = Div),
    Exp ("^", op = Exp),
    Remainder ("/%", op = Remainder),
    Modulo ("%", op = Modulo),

    PropertyAccess ("."),

    Hash ("#"),
}

subtokenize_sub_enum! {
    tokenize_separators;
    SLToken : |sep| SLToken::Separator(sep);
    "," => SeparatorType::Comma,
    ";" => SeparatorType::Semicolon,
    ":" => SeparatorType::Colon,
    "->" => SeparatorType::ThinArrowRight,
}
subtokenize_fn! {
    tokenize_whitespace;
    SLToken;
    r"\s+" => |str| SLToken::Space { hard: str.contains("\n") }
}
subtokenize_bracket_sub_enum! {
    tokenize_brackets;
    SLToken : (
        |ty| SLToken::BracketOpen(ty),
        |ty| SLToken::BracketClose(ty),
    );
    "[", "]" => BracketType::Square,
    "(", ")" => BracketType::Paren,
    "{", "}" => BracketType::Curly,
}
subtokenize_fn! {
    tokenize_number;
    SLToken;
    // forgive the nightmare regex, it matches ints and floats with an optional "i" at the end.
    r"(\b|^)?\d*(\d|\.\d+([eE][+-]\d+)?)( ?i)?(\b|$)" => |str| {
        let str_trimmed = str.trim_end_matches('i').trim_start_matches(&['-','+']).trim();

        let imaginary = str.ends_with("i");

        let floating_point = str.contains(".");
        if floating_point {
            // float
            if let Ok(value) = str_trimmed.parse::<f64>() {
                SLToken::Float { value, imaginary }
            } else {
                SLToken::NumLiteralInvalid(str)
            }
        } else {
            // int
            if let Ok(value) = str_trimmed.parse::<i128>() {
                SLToken::Int { value, imaginary }
            } else {
                SLToken::NumLiteralInvalid(str)
            }
        }
    }
}
subtokenize_sub_enum! {
    tokenize_boolean;
    SLToken : |b| SLToken::Bool(b);
    "true" => true,
    "false" => false,
}
subtokenize_fn! {
    tokenize_identifiers;
    SLToken;
    r"(^|\b)[a-zA-Z_][a-zA-Z_0-9]*(\b|$)" => |str| {
        SLToken::Identifier(str)
    }
}
impl_tokenizer! {
    pub SLTokenizer.tokenize<'a>(program: &str) -> Vec<SLToken<'a>>;
    unknown_token (txt) => SLToken::Unknown(txt);
    [
        // comments
        tokenize_comment,

        // key symbols
        tokenize_brackets,
        tokenize_separators,
        tokenize_symbols,
        tokenize_keyword,

        // many-valued symbols
        tokenize_boolean,
        tokenize_number,
        tokenize_identifiers,

        // whitespace
        tokenize_whitespace,
    ]
}

#[cfg(test)]
mod test {
    use crate::parse::tokenization::{Keyword, SLSymbol};

    use super::{SLToken, SLTokenizer};

    #[test]
    fn tokenization() {
        let t = SLTokenizer::new();
        assert_eq!(
            t.tokenize(" for-3+东西69420621i[  \n5.32e+43i[(];^^ide_nt"),
            vec![
                SLToken::Space { hard: false },
                SLToken::Keyword(Keyword::LoopFor),
                SLToken::Symbol(SLSymbol::Minus),
                SLToken::Int {
                    value: 3,
                    imaginary: false
                },
                SLToken::Symbol(SLSymbol::Plus),
                SLToken::Unknown("东西"),
                SLToken::Int {
                    value: 69420621,
                    imaginary: true
                },
                SLToken::BracketOpen(super::BracketType::Square),
                SLToken::Space { hard: true },
                SLToken::Float {
                    value: 5.32e+43,
                    imaginary: true
                },
                SLToken::BracketOpen(super::BracketType::Square),
                SLToken::BracketOpen(super::BracketType::Paren),
                SLToken::BracketClose(super::BracketType::Square),
                SLToken::Separator(super::SeparatorType::Semicolon),
                SLToken::Symbol(SLSymbol::MatExp),
                SLToken::Identifier("ide_nt"),
            ],
        );
    }
}
