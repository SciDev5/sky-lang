#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SLToken<'a> {
    Space,
    Keyword(Keyword),
    Ident(&'a str),
    Int(i128),
    IntTooBig(&'a str),
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
    /** `+` */
    Plus,
    /** `-` */
    Minus,
    /** `*` */
    ScalarTimes,
    /** `/` */
    ScalarDiv,
    /** `@` */
    MatTimes,
    /** `'` */
    Transpose,
    /** `~` */
    HermitianConjugate,
    /** `=` */
    Assign,
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
macro_rules! restr_ident {
    () => {
        r#"(^|\b)[a-zA-Z_][a-zA-Z_0-9]*(\b|$)"#
    };
}
macro_rules! restr_symbols {
    () => {
        r#"[\[\]\(\)\{\},;+\-*/@~'=]"#
    };
}

macro_rules! re_main {
    () => {
        regex::Regex::new(concat!(
            restr_whitespace!(),
            "|",
            restr_int!(),
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
}
impl SLTokenizerRegexes {
    fn new() -> Self {
        Self {
            global_token: re_main!(),
            part_whitespace: re_particle!(restr_whitespace!()),
            part_ident: re_particle!(restr_ident!()),
            part_int: re_particle!(restr_int!()),
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

                    "+" => Some(SLToken::Operator(OperatorType::Plus)),
                    "-" => Some(SLToken::Operator(OperatorType::Minus)),
                    "*" => Some(SLToken::Operator(OperatorType::ScalarTimes)),
                    "/" => Some(SLToken::Operator(OperatorType::ScalarDiv)),

                    "@" => Some(SLToken::Operator(OperatorType::MatTimes)),
                    "'" => Some(SLToken::Operator(OperatorType::Transpose)),
                    "~" => Some(SLToken::Operator(OperatorType::HermitianConjugate)),

                    "=" => Some(SLToken::Operator(OperatorType::Assign)),

                    _ => None,
                } {
                    tokens.push(token);
                    continue;
                }
            }

            tokens.push(if self.re.part_whitespace.is_match(str) {
                SLToken::Space
            } else if self.re.part_int.is_match(str) {
                if let Ok(val) = i128::from_str_radix(str, 10) {
                    SLToken::Int(val)
                } else {
                    SLToken::IntTooBig(str)
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
    use crate::{SLTokenizer, SLToken};

    #[test]
    fn tokenization() {
        let t = SLTokenizer::new();
        assert_eq!(
            t.tokenize(" -3+东西[[(];ide_nt"),
            vec![
                SLToken::Space,
                SLToken::Int(-3),
                SLToken::Operator(crate::OperatorType::Plus),
                SLToken::Unk("东西"),
                SLToken::BracketOpen(crate::BracketType::Square),
                SLToken::BracketOpen(crate::BracketType::Square),
                SLToken::BracketOpen(crate::BracketType::Paren),
                SLToken::BracketClose(crate::BracketType::Square),
                SLToken::Separator(crate::SeparatorType::Semicolon),
                SLToken::Ident("ide_nt"),
            ],
        );
    }
}
