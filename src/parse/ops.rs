use crate::math::shunting_yard::ShuntingYardOperator;

#[derive(Debug, Hash, Clone, Copy, PartialEq, Eq)]
pub enum SLOperator {
    // BOOLEAN / BITWISE OPERATORS
    /** `! x` not (bitwise, boolean) */
    Not,
    /** `x ~ y` exclusive or (boolean and bitwise) */
    Xor,
    /** `x | y` or (boolean and bitwise) */
    Or,
    /** `x & y` and (boolean and bitwise) */
    And,
    /** `x >> y` left shift */
    Shl,
    /** `x << y` right shift */
    Shr,

    // COMPARISON
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

    // NUMERIC
    /** `x + y` addition */
    Plus,
    /** `x - y` subtraction */
    Minus,
    /** `x * y` scalar / elementwise multiply */
    Times,
    /** `x / y` scalar / elementwise divide */
    Div,
    /** `x ^ y` scalar / elementwise exponentiation */
    Exp,
    /** `x % y` modulus (not remainder), using floormod */
    Modulo,
    /** `x /% y` remainder after division */
    Remainder,

    // MATRIX
    /** `x '` hermitian conjugate (transpose + complex conjugate) */
    HermitianConjugate,
    /** `x "` non-complex-conjugating transpose */
    Transpose,
    /** `_/ x` multiplicative inverse */
    Inverse,
    /** `x ** y` matrix multiplication */
    MatTimes,
    /** `x ^^ y` matrix exponentiation */
    MatExp,
}
impl SLOperator {
    pub fn is_prefix(self) -> bool {
        match self {
            Self::Not | Self::Plus | Self::Minus | Self::Inverse => true,
            _ => false,
        }
    }
    pub fn is_postfix(self) -> bool {
        match self {
            Self::HermitianConjugate | Self::Transpose => true,
            _ => false,
        }
    }
    pub fn is_infix(self) -> bool {
        match self {
            Self::Xor
            | Self::Or
            | Self::And
            | Self::Shl
            | Self::Shr
            | Self::Plus
            | Self::Minus
            | Self::Times
            | Self::Div
            | Self::Exp
            | Self::Modulo
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
}
impl ShuntingYardOperator for SLOperator {
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
            Self::Shl => 16,
            Self::Shr => 16,

            // addition -> 20
            Self::Plus => 20,
            Self::Minus => 20,
            // multiplication -> 30
            Self::MatTimes => 30,
            Self::Times => 35,
            Self::Div => 35,
            Self::Modulo => 35,
            Self::Remainder => 35,
            // exponentiation -> 40
            Self::MatExp => 40,
            Self::Exp => 45,

            // postfix -> 50
            Self::HermitianConjugate => 50,
            Self::Transpose => 50,
            Self::Inverse => 50,
        }
    }
    fn right_associative(self) -> bool {
        match self {
            Self::MatExp => true,
            Self::Exp => true,
            _ => false,
        }
    }
}
