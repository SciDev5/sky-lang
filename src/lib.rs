pub type SLIRIdent = Box<str>;

#[derive(Debug, Clone)]
pub enum SLIRVarAccessExpression {
    Ident(SLIRIdent),
}
#[derive(Debug, Clone)]
pub enum SLIRExpression {
    VarRead(SLIRVarAccessExpression),
    BinaryOp(OperatorType, Box<SLIRExpression>, Box<SLIRExpression>),
    UnaryOp(OperatorType, Box<SLIRExpression>),
    Literal(SLIRLiteral),
}
#[derive(Debug, Clone)]
pub enum SLIRStatement {
    VarDeclare(SLIRIdent),
    VarAssign(SLIRVarAccessExpression, Box<SLIRExpression>),
    Expr(Box<SLIRExpression>),
}
#[derive(Debug, Clone)]
pub enum SLIRLiteral {
    Int { re: i128, im: i128 },
    Float { re: f64, im: f64 },
}

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
    Operator(OperatorType),
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
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OperatorType {
    /** `! x` not (bitwise, boolean) */
    Not,

    /** `x '` hermitian conjugate (transpose + complex conjugate) */
    HermitianConjugate,
    /** `x "` non-complex-conjugating transpose */
    Transpose,
    /** `x $` matrix inverse */
    Inverse,

    /** `x ~ y` exclusive or (bitwise) */
    BitXor,
    /** `x | y` or (bitwise) */
    BitOr,
    /** `x & y` and (bitwise) */
    BitAnd,
    /** `x ~~ y` exclusive or (boolean) */
    Xor,
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
impl OperatorType {
    fn is_prefix(self) -> bool {
        match self {
            Self::Not | Self::Plus | Self::Minus => true,
            _ => false,
        }
    }
    fn is_postfix(self) -> bool {
        match self {
            Self::HermitianConjugate | Self::Transpose | Self::Inverse => true,
            _ => false,
        }
    }
    fn is_infix(self) -> bool {
        match self {
            Self::Xor
            | Self::BitOr
            | Self::BitAnd
            | Self::Or
            | Self::And
            | Self::Shl
            | Self::Shr
            | Self::Plus
            | Self::Minus
            | Self::ScalarTimes
            | Self::ScalarDiv
            | Self::ScalarExp
            | Self::ScalarModulo
            | Self::MatTimes
            | Self::MatExp
            | Self::Equal
            | Self::GreaterEqual
            | Self::LessEqual
            | Self::LessThan
            | Self::GreaterThan
            | Self::NotEqual => true,
            _ => false,
        }
    }
    fn precedence(self) -> u8 {
        match self {
            // comparison -> 0
            Self::Equal => 0,
            Self::NotEqual => 0,
            Self::GreaterEqual => 1,
            Self::LessEqual => 1,
            Self::LessThan => 1,
            Self::GreaterThan => 1,

            // boolean -> 10
            Self::Or => 10,
            Self::Xor => 11,
            Self::And => 12,
            Self::Not => 15,

            // addition -> 20
            Self::Plus => 20,
            Self::Minus => 20,
            // multiplication -> 30
            Self::MatTimes => 30,
            Self::ScalarTimes => 35,
            Self::ScalarDiv => 35,
            Self::ScalarModulo => 35,
            // exponentiation -> 40
            Self::MatExp => 40,
            Self::ScalarExp => 45,
            // bitmath -> 50
            Self::BitOr => 50,
            Self::BitXor => 51,
            Self::BitAnd => 52,
            Self::Shl => 55,
            Self::Shr => 55,

            // modifiers -> 60
            Self::HermitianConjugate => 60,
            Self::Transpose => 60,
            Self::Inverse => 60,

            Self::Assign => panic!("never part of expressions"),
        }
    }
    fn right_associative(self) -> bool {
        match self {
            Self::MatExp => true,
            Self::ScalarExp => true,
            _ => false,
        }
    }
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

                    "!" => Some(SLToken::Operator(OperatorType::Not)),
                    "'" => Some(SLToken::Operator(OperatorType::HermitianConjugate)),
                    "\"" => Some(SLToken::Operator(OperatorType::Transpose)),
                    "$" => Some(SLToken::Operator(OperatorType::Inverse)),

                    "~" => Some(SLToken::Operator(OperatorType::BitXor)),
                    "|" => Some(SLToken::Operator(OperatorType::BitOr)),
                    "&" => Some(SLToken::Operator(OperatorType::BitAnd)),

                    "+" => Some(SLToken::Operator(OperatorType::Plus)),
                    "-" => Some(SLToken::Operator(OperatorType::Minus)),
                    "*" => Some(SLToken::Operator(OperatorType::ScalarTimes)),
                    "/" => Some(SLToken::Operator(OperatorType::ScalarDiv)),
                    "^" => Some(SLToken::Operator(OperatorType::ScalarExp)),
                    "%" => Some(SLToken::Operator(OperatorType::ScalarModulo)),
                    "@" => Some(SLToken::Operator(OperatorType::MatTimes)),

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
                    ">>" => Some(SLToken::Operator(OperatorType::Shl)),
                    "<<" => Some(SLToken::Operator(OperatorType::Shr)),

                    "~~" => Some(SLToken::Operator(OperatorType::Xor)),
                    "||" => Some(SLToken::Operator(OperatorType::Or)),
                    "&&" => Some(SLToken::Operator(OperatorType::And)),

                    "==" => Some(SLToken::Operator(OperatorType::Equal)),
                    ">=" => Some(SLToken::Operator(OperatorType::GreaterEqual)),
                    "<=" => Some(SLToken::Operator(OperatorType::LessEqual)),
                    "!=" => Some(SLToken::Operator(OperatorType::NotEqual)),

                    "^^" => Some(SLToken::Operator(OperatorType::MatExp)),

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

pub mod parser {
    use std::iter::Peekable;

    use crate::{
        BracketType, OperatorType, SLIRExpression, SLIRLiteral, SLIRStatement,
        SLIRVarAccessExpression, SLToken,
    };

    pub fn parse<'a>(tokens: Vec<SLToken<'a>>) -> Result<Vec<SLIRStatement>, ()> {
        // TODO proper errors
        let mut statements = vec![];
        let mut tokens = tokens.into_iter().peekable();
        loop {
            skip_whitespace(&mut tokens);
            if tokens.peek().is_none() {
                break Ok(statements); // Hit end without issue.
            }
            if let Some((next, tokens_advanced)) = next_statement(tokens.clone()) {
                statements.push(next);
                tokens = tokens_advanced;
            } else {
                break Err(());
            }
        }
    }
    fn next_statement<'a, I: Iterator<Item = SLToken<'a>>>(
        tokens: Peekable<I>,
    ) -> Option<(SLIRStatement, Peekable<I>)> {
        // TODO not just expressions
        let (expr, tokens) = next_expression(tokens)?;
        Some((SLIRStatement::Expr(Box::new(expr)), tokens))
    }
    fn next_expression<'a, I: Iterator<Item = SLToken<'a>>>(
        tokens: Peekable<I>,
    ) -> Option<(SLIRExpression, Peekable<I>)> {
        /*
        A finite state machine should be able to handle expression parsing.

        >> List of possible expressions
        <expr> = <literal>
        <expr> = <expr> <infix> <expr>
        <expr> = <prefix> <expr>
        <expr> = <expr> <postfix>
        <expr> = "(" <expr> ")"
        <expr> = "[" ((<expr> ",")... ";")... "]"

        >> Whole thing in a regex-like format
        $ = (
            L |
            I |
            P $ A? |
            (
                "(" $ ")" |
                "[" ($ S)* "]"
            ) A?
        ) (I $)*

        >> Broken apart into separate smaller functions, seen below in code.
        expr = expr_no_infix (I expr_no_infix)*
        expr_no_infix = (
            L |
            I |
            expr_prefix |  >> initiated by Any Operator (matching will determine if valid)
            expr_grouping  >> initiated by "(" | "["
        )
        expr_prefix = P expr_no_infix A?
        expr_grouping = (
            expr_paren |    >> initiated by "("
            expr_array      >> initiated by "["
        ) A?
        expr_paren = "(" expr ")"
        expr_array = "[" (expr S)* "]"

        I'm qualified bet.
        */

        let mut infix_expr = vec![];

        let (expr, mut tokens) = next_expression_no_infix(tokens)?;

        infix_expr.push(ShuntingYardObj::Expr(expr));

        skip_whitespace(&mut tokens);
        while let Some(SLToken::Operator(op)) = tokens.peek().copied() {
            if !op.is_infix() {
                break;
            }
            tokens.next();
            let (expr, tokens_) = next_expression_no_infix(tokens)?;
            tokens = tokens_;
            infix_expr.push(ShuntingYardObj::Op(op));
            infix_expr.push(ShuntingYardObj::Expr(expr));
            skip_whitespace(&mut tokens); // skip whitespace before peeking the next token in the `while let`
        }

        let mut postfix_expr_reversed = shunting_yard(infix_expr.into_iter()).into_iter().rev();

        Some((
            treeify_reversed_postfix_ops(&mut postfix_expr_reversed),
            tokens,
        ))
    }
    fn next_expression_no_infix<'a, I: Iterator<Item = SLToken<'a>>>(
        mut tokens: Peekable<I>,
    ) -> Option<(SLIRExpression, Peekable<I>)> {
        /*
        expr_no_infix = (
            L |
            I |
            expr_prefix |  >> initiated by Any Operator (matching will determine if valid)
            expr_grouping  >> initiated by "(" | "["
        )
        */
        skip_whitespace(&mut tokens);
        let next = tokens.next()?;
        match next {
            SLToken::BracketOpen(bracket_type) => next_expression_grouping(bracket_type, tokens),
            SLToken::BracketClose(_) => None, // can't start expression with a closing bracket
            SLToken::Separator(_) => None,    // can't start expression with a separator

            SLToken::Float(_, _) | SLToken::Int(_, _) => next_expression_literal_num(next, tokens),

            SLToken::Ident(name) => Some((
                SLIRExpression::VarRead(SLIRVarAccessExpression::Ident(
                    name.to_string().into_boxed_str(),
                )),
                tokens,
            )),

            SLToken::Operator(op) => next_expression_prefix(op, tokens),

            // keywords not expected here
            SLToken::Keyword(_) => None,
            // always invalid
            SLToken::NumLiteralInvalid(_) => None,
            SLToken::Unk(_) => None,
            // should have skipped all spaces
            SLToken::Space { .. } => panic!("impossible, just skipped all whitespace"),
        }
    }
    fn next_expression_literal_num<'a, I: Iterator<Item = SLToken<'a>>>(
        initial: SLToken<'a>,
        tokens: Peekable<I>,
    ) -> Option<(SLIRExpression, Peekable<I>)> {
        let val = match initial {
            SLToken::Float(v, imag) => {
                if imag {
                    SLIRLiteral::Float { re: 0.0, im: v }
                } else {
                    SLIRLiteral::Float { re: v, im: 0.0 }
                }
            }
            SLToken::Int(v, imag) => {
                if imag {
                    SLIRLiteral::Int { re: 0, im: v }
                } else {
                    SLIRLiteral::Int { re: v, im: 0 }
                }
            }
            _ => {
                return None;
            }
        };
        // TODO: automatically merge stuff like 1+2i into a single primitive
        Some((SLIRExpression::Literal(val), tokens))
    }
    fn next_expression_prefix<'a, I: Iterator<Item = SLToken<'a>>>(
        op: OperatorType,
        tokens: Peekable<I>,
    ) -> Option<(SLIRExpression, Peekable<I>)> {
        // expr_prefix = P expr_no_infix A?

        if !op.is_prefix() {
            // Not scientifically possible!
            return None; // (i lied, it actually is if someone writes like `(^3)`. `^` is not a prefix operator)
        }

        let (mut expr, mut tokens) = next_expression_no_infix(tokens)?;

        for postfix_op in next_if_postfix_ops(&mut tokens) {
            expr = SLIRExpression::UnaryOp(postfix_op, Box::new(expr))
        }

        Some((SLIRExpression::UnaryOp(op, Box::new(expr)), tokens))
    }
    fn next_expression_grouping<'a, I: Iterator<Item = SLToken<'a>>>(
        bracket_type: BracketType,
        tokens: Peekable<I>,
    ) -> Option<(SLIRExpression, Peekable<I>)> {
        /*
        expr_grouping = (
            expr_paren |    >> initiated by "("
            expr_array      >> initiated by "["
        ) A?
        */
        let (mut expr, mut tokens) = match bracket_type {
            BracketType::Curly => todo!(),
            BracketType::Paren => next_expression_paren(tokens),
            BracketType::Square => next_expression_array(tokens),
        }?;

        for op in next_if_postfix_ops(&mut tokens) {
            expr = SLIRExpression::UnaryOp(op, Box::new(expr))
        }

        Some((expr, tokens))
    }
    fn next_expression_paren<'a, I: Iterator<Item = SLToken<'a>>>(
        tokens: Peekable<I>,
    ) -> Option<(SLIRExpression, Peekable<I>)> {
        // expr_paren = "(" expr ")"
        let (expr, mut tokens) = next_expression(tokens)?;

        skip_whitespace(&mut tokens);
        if let Some(SLToken::BracketClose(BracketType::Paren)) = tokens.next() {
            Some((expr, tokens))
        } else {
            None
        }
    }
    fn next_expression_array<'a, I: Iterator<Item = SLToken<'a>>>(
        tokens: Peekable<I>,
    ) -> Option<(SLIRExpression, Peekable<I>)> {
        // TODO expr_array = "[" (expr S)* "]"
        dbg!(tokens.collect::<Vec<_>>());
        todo!("SLParser::next_expression_array")
    }
    fn next_if_postfix_ops<'a, I: Iterator<Item = SLToken<'a>>>(
        tokens: &mut Peekable<I>,
    ) -> Vec<OperatorType> {
        let mut ops = vec![];
        loop {
            skip_soft_whitespace(tokens);
            if let Some(SLToken::Operator(op)) = tokens.peek().copied() {
                if op.is_postfix() {
                    tokens.next();
                    ops.push(op);
                    continue;
                }
            }
            break;
        }
        ops
    }

    /// Skips whitespace, returning true if there was a "hard" space (aka. a line break)
    /// and false if no skip or "soft" whitespace.
    fn skip_whitespace<'a, I: Iterator<Item = SLToken<'a>>>(tokens: &mut Peekable<I>) -> bool {
        if let Some(SLToken::Space { hard }) = tokens.peek().copied() {
            tokens.next();
            return hard;
        }
        return false;
    }
    /// Skips soft whitespace (eg. spaces/tabs but not line breaks).
    fn skip_soft_whitespace<'a, I: Iterator<Item = SLToken<'a>>>(tokens: &mut Peekable<I>) {
        if let Some(SLToken::Space { hard }) = tokens.peek().copied() {
            if !hard {
                tokens.next();
            }
        }
    }

    enum ShuntingYardObj {
        Expr(SLIRExpression),
        Op(OperatorType),
    }

    fn shunting_yard(input_stream: impl Iterator<Item = ShuntingYardObj>) -> Vec<ShuntingYardObj> {
        let mut output_queue = vec![];
        let mut operator_stack = vec![];

        for token in input_stream {
            match token {
                ShuntingYardObj::Expr(expr) => output_queue.push(ShuntingYardObj::Expr(expr)),
                ShuntingYardObj::Op(op) => {
                    while let Some(&top_op) = operator_stack.last() {
                        if shunting_yard_should_pop_operator(&op, &top_op) {
                            output_queue.push(ShuntingYardObj::Op(operator_stack.pop().unwrap()));
                        } else {
                            break;
                        }
                    }
                    operator_stack.push(op);
                }
            }
        }

        while let Some(op) = operator_stack.pop() {
            output_queue.push(ShuntingYardObj::Op(op));
        }

        output_queue
    }

    fn shunting_yard_should_pop_operator(new_op: &OperatorType, top_op: &OperatorType) -> bool {
        if new_op.right_associative() {
            new_op.precedence() < top_op.precedence()
        } else {
            new_op.precedence() <= top_op.precedence()
        }
    }

    fn treeify_reversed_postfix_ops(
        stream: &mut impl Iterator<Item = ShuntingYardObj>,
    ) -> SLIRExpression {
        match stream
            .next()
            .expect("treeifying postfix output of shunting yard failed: ran out of nodes")
        {
            ShuntingYardObj::Expr(expr) => expr,
            ShuntingYardObj::Op(op) => {
                // Remember we're reading the list in reverse.
                let second = treeify_reversed_postfix_ops(stream);
                let first = treeify_reversed_postfix_ops(stream);
                SLIRExpression::BinaryOp(op, Box::new(first), Box::new(second))
            }
        }
    }
}

#[cfg(test)]
mod test {
    use crate::{SLToken, SLTokenizer};

    #[test]
    fn tokenization() {
        let t = SLTokenizer::new();
        assert_eq!(
            t.tokenize(" -3+东西69420621i[  \n5.32e+43i[(];^^ide_nt"),
            vec![
                SLToken::Space { hard: false },
                SLToken::Int(-3, false),
                SLToken::Operator(crate::OperatorType::Plus),
                SLToken::Unk("东西"),
                SLToken::Int(69420621, true),
                SLToken::BracketOpen(crate::BracketType::Square),
                SLToken::Space { hard: true },
                SLToken::Float(5.32e+43, true),
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
