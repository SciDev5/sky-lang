use super::ops::SLOperator;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum SLToken<'a> {
    Space {
        hard: bool,
    },
    Keyword(Keyword),
    Ident(&'a str),
    /// (value, is_imaginary)
    Int(i128, bool),
    /// (value, is_imaginary)
    Float(f64, bool),
    NumLiteralInvalid(&'a str),
    BracketOpen(BracketType),
    BracketClose(BracketType),
    Separator(SeparatorType),
    Operator(SLOperator),
    Unk(&'a str),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Keyword {
    /** `let` Declare variable */
    VarDeclare,
    /** `del` Delete/destroy variable */
    VarDestroy,
    /** `for` For loop keyword */
    LoopFor,
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
}
impl Keyword {
    fn from_str(str: &str) -> Option<Keyword> {
        match str {
            "let" => Some(Self::VarDeclare),
            "del" => Some(Self::VarDestroy),
            "for" => Some(Self::LoopFor),
            "while" => Some(Self::LoopWhile),
            "loop" => Some(Self::LoopForever),
            "break" => Some(Self::LoopBreak),
            "continue" => Some(Self::LoopContinue),
            "if" => Some(Self::ConditionalIf),
            "elif" => Some(Self::ConditionalElseIf),
            "else" => Some(Self::ConditionalElse),
            "fn" => Some(Self::FunctionDefinition),
            _ => None,
        }
    }
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BracketType {
    Square,
    Paren,
    Curly,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SeparatorType {
    Comma,
    Semicolon,
}

macro_rules! restr_whitespace {
    () => {
        r#"\s+"#
    };
}
macro_rules! restr_int {
    () => {
        r#"[0-9]+(\b|$)"#
    };
}
macro_rules! restr_int_imag {
    () => {
        r#"[0-9]+i(\b|$)"#
    };
}
macro_rules! restr_float {
    () => {
        r#"[0-9]*\.[0-9]+([eE][+-]?[0-9]+)?(\b|$)"#
    };
}
macro_rules! restr_float_imag {
    () => {
        r#"[0-9]*\.[0-9]+([eE][+-]?[0-9]+)?i(\b|$)"#
    };
}
macro_rules! restr_ident {
    () => {
        r#"(^|\b)[a-zA-Z_][a-zA-Z_0-9]*(\b|$)"#
    };
}
macro_rules! restr_brackets {
    () => {
        r#"[\[\]\(\)\{\}]"#
    };
}
macro_rules! restr_symbols {
    () => {
        r#"[=!<>]=|&&|\|\||<<|>>|\^\^|[,;]|[+\-*\/^%@'"$~=!<>\|&]"#
    };
}

macro_rules! re_main {
    () => {
        regex::Regex::new(concat!(
            restr_whitespace!(),
            "|",
            restr_float!(),
            "|",
            restr_float_imag!(),
            "|",
            restr_int!(),
            "|",
            restr_int_imag!(),
            "|",
            restr_brackets!(),
            "|",
            restr_symbols!(),
            "|",
            restr_ident!(),
        ))
        .unwrap()
    };
}

macro_rules! re_particle {
    ($core: expr) => {
        regex::Regex::new(concat!("^(", $core, ")$")).unwrap()
    };
}

#[derive(Clone)]
struct SLTokenizerRegexes {
    global_token: regex::Regex,
    part_whitespace: regex::Regex,
    part_ident: regex::Regex,
    part_int: regex::Regex,
    part_int_imag: regex::Regex,
    part_float: regex::Regex,
    part_float_imag: regex::Regex,
}
impl SLTokenizerRegexes {
    fn new() -> Self {
        Self {
            global_token: re_main!(),
            part_whitespace: re_particle!(restr_whitespace!()),
            part_ident: re_particle!(restr_ident!()),
            part_int: re_particle!(restr_int!()),
            part_int_imag: re_particle!(restr_int_imag!()),
            part_float: re_particle!(restr_float!()),
            part_float_imag: re_particle!(restr_float_imag!()),
        }
    }
}

pub struct SLTokenizer {
    re: SLTokenizerRegexes,
}

impl SLTokenizer {
    pub fn new() -> Self {
        Self {
            re: SLTokenizerRegexes::new(),
        }
    }
    pub fn tokenize<'a>(&self, prog: &'a str) -> Vec<SLToken<'a>> {
        let mut last_end = 0;
        let mut tokens = vec![];

        for matched in self.re.global_token.find_iter(prog) {
            if matched.start() > last_end {
                tokens.push(SLToken::Unk(&prog[last_end..matched.start()]));
            }
            last_end = matched.end();

            let str = matched.as_str();

            if str.len() == 1 {
                if let Some(token) = match str {
                    "[" => Some(SLToken::BracketOpen(BracketType::Square)),
                    "]" => Some(SLToken::BracketClose(BracketType::Square)),
                    "(" => Some(SLToken::BracketOpen(BracketType::Paren)),
                    ")" => Some(SLToken::BracketClose(BracketType::Paren)),
                    "{" => Some(SLToken::BracketOpen(BracketType::Curly)),
                    "}" => Some(SLToken::BracketClose(BracketType::Curly)),

                    "," => Some(SLToken::Separator(SeparatorType::Comma)),
                    ";" => Some(SLToken::Separator(SeparatorType::Semicolon)),

                    "!" => Some(SLToken::Operator(SLOperator::Not)),
                    "'" => Some(SLToken::Operator(SLOperator::HermitianConjugate)),
                    "\"" => Some(SLToken::Operator(SLOperator::Transpose)),
                    "$" => Some(SLToken::Operator(SLOperator::Inverse)),

                    "~" => Some(SLToken::Operator(SLOperator::BitXor)),
                    "|" => Some(SLToken::Operator(SLOperator::BitOr)),
                    "&" => Some(SLToken::Operator(SLOperator::BitAnd)),

                    "+" => Some(SLToken::Operator(SLOperator::Plus)),
                    "-" => Some(SLToken::Operator(SLOperator::Minus)),
                    "*" => Some(SLToken::Operator(SLOperator::ScalarTimes)),
                    "/" => Some(SLToken::Operator(SLOperator::ScalarDiv)),
                    "^" => Some(SLToken::Operator(SLOperator::ScalarExp)),
                    "%" => Some(SLToken::Operator(SLOperator::ScalarModulo)),
                    "@" => Some(SLToken::Operator(SLOperator::MatTimes)),

                    "=" => Some(SLToken::Operator(SLOperator::Assign)),
                    "<" => Some(SLToken::Operator(SLOperator::LessThan)),
                    ">" => Some(SLToken::Operator(SLOperator::GreaterThan)),

                    _ => None,
                } {
                    tokens.push(token);
                    continue;
                }
            }
            if str.len() == 2 {
                if let Some(token) = match str {
                    ">>" => Some(SLToken::Operator(SLOperator::Shl)),
                    "<<" => Some(SLToken::Operator(SLOperator::Shr)),

                    "~~" => Some(SLToken::Operator(SLOperator::Xor)),
                    "||" => Some(SLToken::Operator(SLOperator::Or)),
                    "&&" => Some(SLToken::Operator(SLOperator::And)),

                    "==" => Some(SLToken::Operator(SLOperator::Equal)),
                    ">=" => Some(SLToken::Operator(SLOperator::GreaterEqual)),
                    "<=" => Some(SLToken::Operator(SLOperator::LessEqual)),
                    "!=" => Some(SLToken::Operator(SLOperator::NotEqual)),

                    "^^" => Some(SLToken::Operator(SLOperator::MatExp)),

                    _ => None,
                } {
                    tokens.push(token);
                    continue;
                }
            }

            tokens.push(if self.re.part_whitespace.is_match(str) {
                SLToken::Space { hard: str.contains("\n") }
            } else if self.re.part_float.is_match(str) {
                if let Ok(val) = str.parse::<f64>() {
                    SLToken::Float(val, false)
                } else {
                    SLToken::NumLiteralInvalid(str)
                }
            } else if self.re.part_float_imag.is_match(str) {
                // `str.len()` is valid here because /[0-9+\-eEi]/ is all ascii. 
                if let Ok(val) = (&str[0..str.len()-1]).parse::<f64>() {
                    SLToken::Float(val, true)
                } else {
                    SLToken::NumLiteralInvalid(str)
                }
            } else if self.re.part_int.is_match(str) {
                if let Ok(val) = str.parse::<i128>() {
                    SLToken::Int(val, false)
                } else {
                    SLToken::NumLiteralInvalid(str)
                }
            } else if self.re.part_int_imag.is_match(str) {
                // `str.len()` is valid here because /[0-9\-i]/ is all ascii. 
                if let Ok(val) = (&str[0..str.len()-1]).parse::<i128>()  {
                    SLToken::Int(val, true)
                } else {
                    SLToken::NumLiteralInvalid(str)
                }
            } else if self.re.part_ident.is_match(str) {
                if let Some(keyword) = Keyword::from_str(str) {
                    SLToken::Keyword(keyword)
                } else {
                    SLToken::Ident(str)
                }
            } else {
                panic!("should not be possible (if it is, tokenization has the wrong regular expressions in it)");
            });
        }

        tokens
    }
}

#[cfg(test)]
mod test {
    use crate::language::ops::SLOperator;

    use super::{SLToken, SLTokenizer};

    #[test]
    fn tokenization() {
        let t = SLTokenizer::new();
        assert_eq!(
            t.tokenize(" -3+东西69420621i[  \n5.32e+43i[(];^^ide_nt"),
            vec![
                SLToken::Space { hard: false },
                SLToken::Int(-3, false),
                SLToken::Operator(SLOperator::Plus),
                SLToken::Unk("东西"),
                SLToken::Int(69420621, true),
                SLToken::BracketOpen(super::BracketType::Square),
                SLToken::Space { hard: true },
                SLToken::Float(5.32e+43, true),
                SLToken::BracketOpen(super::BracketType::Square),
                SLToken::BracketOpen(super::BracketType::Paren),
                SLToken::BracketClose(super::BracketType::Square),
                SLToken::Separator(super::SeparatorType::Semicolon),
                SLToken::Operator(SLOperator::MatExp),
                SLToken::Ident("ide_nt"),
            ],
        );
    }
}
