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
    Operator(SLOperator),
    AmbiguityAngleBracket(AngleBracketShape),
    SyntacticSugar(SyntacticSugarType),
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
pub enum Keyword {
    /** `let` Declare variable */
    VarDeclare,
    /** `const` Declare constant variable */
    VarConstDeclare,
    /** `del` Delete/destroy variable */
    VarDestroy,
    /** `return` Function return value */
    Return,
    /** `for` For loop keyword */
    LoopFor,
    /** `in` In keyword */
    In,
    /** `while` While loop keyword */
    LoopWhile,
    /** `loop` Forever loop keyword */
    LoopForever,
    /** `break` Break out of loop */
    LoopBreak,
    /** `continue` Continue to next loop iteration */
    LoopContinue,
    /** `if` Conditional */
    ConditionalIf,
    /** `elif` Conditional else if branch */
    ConditionalElseIf,
    /** `else` Conditional else branch */
    ConditionalElse,
    /** `fn` Function definition */
    FunctionDefinition,
    /** `struct` Structured data type definition */
    StructDefinition,
    /** `tuple` Struct tuple data definition keyword */
    StructTuple,
    /** `enum` Enum definition */
    EnumDefinition,
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
subtokenize_sub_enum! {
    tokenize_separators;
    SLToken : |sep| SLToken::Separator(sep);
    "," => SeparatorType::Comma,
    "." => SeparatorType::Period,
    ";" => SeparatorType::Semicolon,
    ":" => SeparatorType::Colon,
    "->" => SeparatorType::ThinArrowRight,
}
subtokenize_sub_enum! {
    tokenize_ambiguity_angle_bracket;
    SLToken : |which| SLToken::AmbiguityAngleBracket(which);
    "<" => AngleBracketShape::OpenOrLessThan,
    ">" => AngleBracketShape::CloseOrGreaterThan,
}
subtokenize_sub_enum! {
    tokenize_syntactic_sugar;
    SLToken : |it| SLToken::SyntacticSugar(it);
    "#" => SyntacticSugarType::Hash,
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
    tokenize_ops;
    SLToken : |op| SLToken::Operator(op);
    // operators with longer sequences go first in order to make sure they don't
    // get mixed up with shorter ops that start with the same thing.

    ">>" => SLOperator::Shl,
    "<<" => SLOperator::Shr,

    "~~" => SLOperator::Xor,
    "||" => SLOperator::Or,
    "&&" => SLOperator::And,

    "==" => SLOperator::Equal,
    ">=" => SLOperator::GreaterEqual,
    "<=" => SLOperator::LessEqual,
    "!=" => SLOperator::NotEqual,

    "^^" => SLOperator::MatExp,

    "!" => SLOperator::Not,
    "'" => SLOperator::HermitianConjugate,
    "\"" => SLOperator::Transpose,
    "$" => SLOperator::Inverse,

    "~" => SLOperator::BitXor,
    "|" => SLOperator::BitOr,
    "&" => SLOperator::BitAnd,

    "+" => SLOperator::Plus,
    "-" => SLOperator::Minus,
    "*" => SLOperator::ScalarTimes,
    "/" => SLOperator::ScalarDiv,
    "^" => SLOperator::ScalarExp,
    "%" => SLOperator::ScalarModulo,
    "@" => SLOperator::MatTimes,

    "=" => SLOperator::Assign,
    // "<" => SLOperator::LessThan,     // specifically not included
    // ">" => SLOperator::GreaterThan, //  see `tokenize_ambiguity_angle_bracket`

}
subtokenize_sub_enum_words! {
    tokenize_keyword;
    SLToken : |op| SLToken::Keyword(op);
    "let" => Keyword::VarDeclare,
    "const" => Keyword::VarConstDeclare,
    "del" => Keyword::VarDestroy,
    "for" => Keyword::LoopFor,
    "in" => Keyword::In,
    "while" => Keyword::LoopWhile,
    "loop" => Keyword::LoopForever,
    "break" => Keyword::LoopBreak,
    "continue" => Keyword::LoopContinue,
    "if" => Keyword::ConditionalIf,
    "elif" => Keyword::ConditionalElseIf,
    "else" => Keyword::ConditionalElse,
    "fn" => Keyword::FunctionDefinition,
    "struct" => Keyword::StructDefinition,
    "tuple" => Keyword::StructTuple,
    "enum" => Keyword::EnumDefinition,
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
        tokenize_ops,
        tokenize_syntactic_sugar,
        tokenize_ambiguity_angle_bracket,
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
    use crate::parse::{ops::SLOperator, tokenization::Keyword};

    use super::{SLToken, SLTokenizer};

    #[test]
    fn tokenization() {
        let t = SLTokenizer::new();
        assert_eq!(
            t.tokenize(" for-3+东西69420621i[  \n5.32e+43i[(];^^ide_nt"),
            vec![
                SLToken::Space { hard: false },
                SLToken::Keyword(Keyword::LoopFor),
                SLToken::Operator(SLOperator::Minus),
                SLToken::Int {
                    value: 3,
                    imaginary: false
                },
                SLToken::Operator(SLOperator::Plus),
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
                SLToken::Operator(SLOperator::MatExp),
                SLToken::Identifier("ide_nt"),
            ],
        );
    }
}
