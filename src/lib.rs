#[derive(Debug, Clone, Copy, PartialEq)]
pub enum SLToken<'a> {
    Space,
    Keyword(Keyword),
    Ident(&'a str),
    Int(i128),
    IntImag(i128),
    NumLiteralInvalid(&'a str),
    Float(f64),
    FloatImag(f64),
    BracketOpen(BracketType),
    BracketClose(BracketType),
    Separator(SeparatorType),
    Operator(OperatorType),
    Unk(&'a str),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Keyword {}
impl Keyword {
    fn from_str(str: &str) -> Option<Keyword> {
        match str {
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
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OperatorType {
    /** `x ~ y` exclusive or (bitwise, boolean) */
    Xor,
    /** `! x` not (bitwise, boolean) */
    Not,
    /** `x | y` or (bitwise) */
    BitOr,
    /** `x & y` and (bitwise) */
    BitAnd,
    /** `x || y` or (boolean) */
    Or,
    /** `x && y` and (boolean) */
    And,
    /** `x >> y` left shift */
    Shl,
    /** `x << y` right shift */
    Shr,

    /** `x + y` addition */
    Plus,
    /** `x - y` subtraction */
    Minus,
    /** `x * y` scalar / elementwise multiply */
    ScalarTimes,
    /** `x / y` scalar / elementwise divide */
    ScalarDiv,
    /** `x ^ y` scalar / elementwise exponentiation */
    ScalarExp,
    /** `x % y` modulo, using floormod */
    ScalarModulo,

    /** `x @ y` matrix multiplication */
    MatTimes,
    /** `x ^^ y` matrix exponentiation */
    MatExp,
    /** `x '` hermitian conjugate (transpose + complex conjugate) */
    HermitianConjugate,
    /** `x "` non-complex-conjugating transpose */
    Transpose,
    /** `x $` matrix inverse */
    Inverse,

    /** `x = y` variable assignment */
    Assign,

    /** `x == y` equals comparison */
    Equal,
    /** `x >= y` greater than or equals comparison */
    GreaterEqual,
    /** `x <= y` less than or equals comparison */
    LessEqual,
    /** `x < y` less than comparison */
    LessThan,
    /** `x > y` greater than comparison */
    GreaterThan,
    /** `x != y` not equals comparison */
    NotEqual,
}

macro_rules! restr_whitespace {
    () => {
        r#"\s+"#
    };
}
macro_rules! restr_int {
    () => {
        r#"[+-]?[0-9]+(\b|$)"#
    };
}
macro_rules! restr_int_imag {
    () => {
        r#"[+-]?[0-9]+i(\b|$)"#
    };
}
macro_rules! restr_float {
    () => {
        r#"[+-]?[0-9]*\.[0-9]+([eE][+-]?[0-9]+)?(\b|$)"#
    };
}
macro_rules! restr_float_imag {
    () => {
        r#"[+-]?[0-9]*\.[0-9]+([eE][+-]?[0-9]+)?i(\b|$)"#
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

                    "~" => Some(SLToken::Operator(OperatorType::Xor)),
                    "!" => Some(SLToken::Operator(OperatorType::Not)),
                    "|" => Some(SLToken::Operator(OperatorType::BitOr)),
                    "&" => Some(SLToken::Operator(OperatorType::BitAnd)),
                    "+" => Some(SLToken::Operator(OperatorType::Plus)),
                    "-" => Some(SLToken::Operator(OperatorType::Minus)),
                    "*" => Some(SLToken::Operator(OperatorType::ScalarTimes)),
                    "/" => Some(SLToken::Operator(OperatorType::ScalarDiv)),
                    "^" => Some(SLToken::Operator(OperatorType::ScalarExp)),
                    "%" => Some(SLToken::Operator(OperatorType::ScalarModulo)),
                    "@" => Some(SLToken::Operator(OperatorType::MatTimes)),
                    "'" => Some(SLToken::Operator(OperatorType::HermitianConjugate)),
                    "\"" => Some(SLToken::Operator(OperatorType::Transpose)),
                    "$" => Some(SLToken::Operator(OperatorType::Inverse)),
                    "=" => Some(SLToken::Operator(OperatorType::Assign)),
                    "<" => Some(SLToken::Operator(OperatorType::LessThan)),
                    ">" => Some(SLToken::Operator(OperatorType::GreaterThan)),

                    _ => None,
                } {
                    tokens.push(token);
                    continue;
                }
            }
            if str.len() == 2 {
                if let Some(token) = match str {
                    "||" => Some(SLToken::Operator(OperatorType::Or)),
                    "&&" => Some(SLToken::Operator(OperatorType::And)),
                    ">>" => Some(SLToken::Operator(OperatorType::Shl)),
                    "<<" => Some(SLToken::Operator(OperatorType::Shr)),
                    "^^" => Some(SLToken::Operator(OperatorType::MatExp)),
                    "==" => Some(SLToken::Operator(OperatorType::Equal)),
                    ">=" => Some(SLToken::Operator(OperatorType::GreaterEqual)),
                    "<=" => Some(SLToken::Operator(OperatorType::LessEqual)),
                    "!=" => Some(SLToken::Operator(OperatorType::NotEqual)),

                    _ => None,
                } {
                    tokens.push(token);
                    continue;
                }
            }

            tokens.push(if self.re.part_whitespace.is_match(str) {
                SLToken::Space
            } else if self.re.part_float.is_match(str) {
                if let Ok(val) = str.parse::<f64>() {
                    SLToken::Float(val)
                } else {
                    SLToken::NumLiteralInvalid(str)
                }
            } else if self.re.part_float_imag.is_match(str) {
                // `str.len()` is valid here because /[0-9+\-eEi]/ is all ascii. 
                if let Ok(val) = (&str[0..str.len()-1]).parse::<f64>() {
                    SLToken::FloatImag(val)
                } else {
                    SLToken::NumLiteralInvalid(str)
                }
            } else if self.re.part_int.is_match(str) {
                if let Ok(val) = str.parse::<i128>() {
                    SLToken::Int(val)
                } else {
                    SLToken::NumLiteralInvalid(str)
                }
            } else if self.re.part_int_imag.is_match(str) {
                // `str.len()` is valid here because /[0-9\-i]/ is all ascii. 
                if let Ok(val) = (&str[0..str.len()-1]).parse::<i128>()  {
                    SLToken::IntImag(val)
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
    use crate::{SLToken, SLTokenizer};

    #[test]
    fn tokenization() {
        let t = SLTokenizer::new();
        assert_eq!(
            t.tokenize(" -3+东西69420621i[5.32e+43i[(];^^ide_nt"),
            vec![
                SLToken::Space,
                SLToken::Int(-3),
                SLToken::Operator(crate::OperatorType::Plus),
                SLToken::Unk("东西"),
                SLToken::IntImag(69420621),
                SLToken::BracketOpen(crate::BracketType::Square),
                SLToken::FloatImag(5.32e+43),
                SLToken::BracketOpen(crate::BracketType::Square),
                SLToken::BracketOpen(crate::BracketType::Paren),
                SLToken::BracketClose(crate::BracketType::Square),
                SLToken::Separator(crate::SeparatorType::Semicolon),
                SLToken::Operator(crate::OperatorType::MatExp),
                SLToken::Ident("ide_nt"),
            ],
        );
    }
}
