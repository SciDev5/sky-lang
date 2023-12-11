use crate::math::shunting_yard::ShuntingYardOperator;


#[derive(Debug, Hash, Clone, Copy, PartialEq, Eq)]
pub enum SLOperator {
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
impl SLOperator {
    pub fn is_prefix(self) -> bool {
        match self {
            Self::Not | Self::Plus | Self::Minus => true,
            _ => false,
        }
    }
    pub fn is_postfix(self) -> bool {
        match self {
            Self::HermitianConjugate | Self::Transpose | Self::Inverse => true,
            _ => false,
        }
    }
    pub fn is_infix(self) -> bool {
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