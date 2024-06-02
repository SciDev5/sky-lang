use super::tokenize::TInfixOperatorType;

#[derive(Debug, Clone)]
enum ASTLiteral {
    /// Integer of any size (up to `2^128-1`).
    Int(u128),
    /// Negative integer of any size (stored as negative, down to `-2^63`)
    NInt(i128),
    /// Floating point number, stored in both supported formats for when the
    /// type system decides which one to use.
    Float(f64, f32),
    /// Simple boolean.
    Bool(bool),
    /// String (with `\n` and similar escaped), with no templating going on.
    String(String),
}

#[derive(Debug, Clone)]
enum ASTDeclr<'src> {}
#[derive(Debug, Clone)]
enum ASTStmt<'src> {
    VarDeclare {
        destruct: Option<ASTVarDestructure<'src>>,
        ty: Option<ASTType<'src>>,
        initializer: Option<Expr<'src>>,
    },
    SubBlocked(ASTSubBlocked<'src>),
    Expr(ASTExpr<'src>),
}
#[derive(Debug, Clone)]
enum ASTExpr<'src> {
    Literal {
        value: ASTLiteral,
    },
    SubBlocked(ASTSubBlocked<'src>),
    PostfixBlock {
        inner: Expr<'src>,
        postfix: ASTPostfixBlock<'src>,
    },
}
#[derive(Debug, Clone)]
enum ASTPostfixBlock<'src> {
    Call { args: Vec<Expr<'src>> },
    Index { args: Vec<Expr<'src>> },
    AnonFunc { func: ASTAnonymousFunction<'src> },
}
#[derive(Debug, Clone)]
enum ASTSubBlocked<'src> {
    /// `if x { ... }`
    If {
        condition: Option<Expr<'src>>,
        block: Option<Block<'src>>,
        /// `else if y { ... }`
        elifs: Option<Vec<(Option<Expr<'src>>, Option<Block<'src>>)>>,
        /// `else { ... }`
        else_block: Option<Block<'src>>,
    },
    /// `loop { ... }`
    Loop { block: Option<Block<'src>> },
    /// `while x { ... }`
    While {
        condition: Option<Expr<'src>>,
        block: Option<Block<'src>>,
        /// Optional else block which allows this to return
        /// a value when used as an expression.
        else_block: Option<Block<'src>>,
    },
    /// `for x in y { ... }`
    For {
        var_destruct: Option<ASTVarDestructure<'src>>,
        var_ty: Option<ASTType<'src>>,
        block: Option<Block<'src>>,
        /// Optional else block which allows this to return
        /// a value when used as an expression.
        else_block: Option<Block<'src>>,
    },
}

/// `\(a: ty0) -> ty1 { ... }`
#[derive(Debug, Clone)]
struct ASTAnonymousFunction<'src> {
    args: Option<Vec<(&'src str, ASTType<'src>)>>,
    return_ty: Option<ASTType<'src>>,
    block: Option<Block<'src>>,
}

#[derive(Debug, Clone)]
enum ASTType<'src> {
    /// A simple named type, like `int` or `BinaryTree`.
    Named { name: &'src str },
    /// Types of the form `Inner.Name`
    Access {
        name: &'src str,
        inner: Box<ASTType<'src>>,
    },
    /// types of the form `A<B,C,D=E>
    TypeParam {
        inner: Box<ASTType<'src>>,
        params: Vec<ASTType<'src>>,
        named_params: Option<Vec<(Option<&'src str>, Option<ASTType<'src>>)>>,
    },
    //// not included for the time being, as they are aliases for other types
    //// and I need to decide how to handle that:
    // Ref {
    //     name: &'src str,
    //     mutable: bool,
    //     inner: Box<ASTType<'src>>,
    // },
    // Tuple {
    //     name: &'src str,
    //     inner: Vec<ASTType<'src>>,
    // },
    // Array {
    //     inner: Box<ASTType<'src>>,
    // },
}

#[derive(Debug, Clone)]
enum ASTVarDestructure<'src> {
    Name(&'src str),
}

type Block<'src> = Vec<ASTStmt<'src>>;
type Expr<'src> = Box<ASTExpr<'src>>;
