use std::collections::HashMap;

#[derive(Debug, Clone)]
enum ASTDeclr<'src> {
    Function(ASTFunction<'src>),
    Trait(ASTTrait<'src>),
    Data(ASTData<'src>),
    FreeImpl(ASTFreeImpl<'src>),
}
#[derive(Debug, Clone)]
enum ASTStmt<'src> {
    StaticDeclare(Box<ASTDeclr<'src>>),
    VarDeclare {
        declr: Option<ASTTypedDestructure<'src>>,
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
    Lambda(ASTLambda<'src>),
    SubBlocked(ASTSubBlocked<'src>),
    PostfixBlock {
        inner: Expr<'src>,
        postfix: ASTPostfixBlock<'src>,
    },
}
type Block<'src> = Vec<ASTStmt<'src>>;
type Expr<'src> = Box<ASTExpr<'src>>;

#[derive(Debug, Clone)]
enum ASTPostfixBlock<'src> {
    /// `x ( a, b, c )`
    Call { args: Vec<ASTExpr<'src>> },
    /// `x [ a, b, c ]`
    Index { args: Vec<ASTExpr<'src>> },
    /// `x { a, b -> c }``
    Lambda { func: ASTLambda<'src> },
    /// `x.{ a = b, c }`
    DataStructInit {
        entries: HashMap<&'src str, ASTExpr<'src>>,
    },
    /// `x.( a, b, c)`
    DataTupleInit { entries: Vec<ASTExpr<'src>> },
}
#[derive(Debug, Clone)]
enum ASTSubBlocked<'src> {
    /// `{ ... }`
    Block { block: Block<'src> },
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

#[derive(Debug, Clone)]
struct ASTTemplateBound<'src> {
    name: &'src str,
    bounds: Vec<ASTType<'src>>,
}
type Templates<'src> = Vec<ASTTemplateBound<'src>>;

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

/// `\(a: ty0) -> ty1 { ... }` or `x { \a: ty0 -> ... }`
#[derive(Debug, Clone)]
struct ASTLambda<'src> {
    has_self_arg: bool,
    args: Vec<ASTTypedDestructure<'src>>,
    return_ty: Option<ASTType<'src>>,
    block: Option<Block<'src>>,
}

#[derive(Debug, Clone)]
struct ASTFunction<'src> {
    name: Option<&'src str>,
    doc: Option<ASTDoc>,
    lambda: ASTLambda<'src>,
}

#[derive(Debug, Clone)]
struct ASTDoc {
    processed_text: String,
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
#[derive(Debug, Clone)]
struct ASTTypedDestructure<'src> {
    destructure: ASTVarDestructure<'src>,
    ty: Option<ASTType<'src>>,
}

#[derive(Debug, Clone)]
struct ASTTrait<'src> {
    name: Option<&'src str>,
    doc: Option<ASTDoc>,
    templates: Templates<'src>,
    bounds: Option<ASTType<'src>>,
    functions: Vec<ASTFunction<'src>>,
}

#[derive(Debug, Clone)]
struct ASTData<'src> {
    name: Option<&'src str>,
    doc: Option<ASTDoc>,
    templates: Templates<'src>,
    attatched_impls: Vec<ASTImpl<'src>>,
}
#[derive(Debug, Clone)]
enum ASTDataContents<'src> {
    Unit,
    Struct {
        properties: Vec<ASTDataProperty<'src>>,
    },
    Tuple {
        properties: Vec<ASTType<'src>>,
    },
    Enum {
        variants: Vec<ASTEnumVariant<'src>>,
    },
}
#[derive(Debug, Clone)]
struct ASTEnumVariant<'src> {
    name: &'src str,
    doc: Option<ASTDoc>,
    which: ASTEnumVariantType<'src>,
}
#[derive(Debug, Clone)]
enum ASTEnumVariantType<'src> {
    Unit,
    Struct {
        properties: Vec<ASTDataProperty<'src>>,
    },
    Tuple {
        properties: Vec<ASTType<'src>>,
    },
}
#[derive(Debug, Clone)]
struct ASTFreeImpl<'src> {
    target: ASTType<'src>,
    attatched_impl: ASTImpl<'src>,
}

#[derive(Debug, Clone)]
struct ASTDataProperty<'src> {
    name: &'src str,
    ty: ASTType<'src>,
    doc: Option<ASTDoc>,
}

#[derive(Debug, Clone)]
struct ASTImpl<'src> {
    templates: Templates<'src>,
    target_trait: Option<ASTType<'src>>,
    functions: Vec<ASTFunction<'src>>,
}
