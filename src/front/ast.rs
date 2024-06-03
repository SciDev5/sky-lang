use super::source::Loc;

#[derive(Debug, Clone)]
pub struct ASTSourceFile<'src> {
    pub body: Vec<ASTStmt<'src>>,
}

#[derive(Debug, Clone)]
pub enum ASTDeclr<'src> {
    Function(ASTFunction<'src>),
    Const(ASTConst<'src>),
    Trait(ASTTrait<'src>),
    Data(ASTData<'src>),
    FreeImpl(ASTFreeImpl<'src>),
    Import(ASTImport<'src>),
}
#[derive(Debug, Clone)]
pub enum ASTStmt<'src> {
    StaticDeclare(Box<ASTDeclr<'src>>),
    VarDeclare(ASTVarDeclare<'src>),
    SubBlocked(ASTSubBlocked<'src>),
    Expr(ASTExpr<'src>),
}
#[derive(Debug, Clone)]
pub enum ASTExpr<'src> {
    Literal {
        loc: Loc,
        value: ASTLiteral,
    },
    Lambda(ASTLambda<'src>),
    SubBlocked(ASTSubBlocked<'src>),
    PostfixBlock {
        inner: Expr<'src>,
        postfix: (ASTPostfixBlock<'src>, Loc),
    },
}
type Block<'src> = (Vec<ASTStmt<'src>>, Loc);
type Expr<'src> = Box<ASTExpr<'src>>;

#[derive(Debug, Clone)]
pub enum ASTPostfixBlock<'src> {
    /// `x ( a, b, c )`
    Call { args: Vec<ASTExpr<'src>> },
    /// `x [ a, b, c ]`
    Index { args: Vec<ASTExpr<'src>> },
    /// `x { a, b -> c }``
    Lambda { func: ASTLambda<'src> },
    /// `x.{ a = b, c }`
    DataStructInit {
        entries: Vec<((&'src str, Loc), ASTExpr<'src>)>,
    },
    /// `x.( a, b, c)`
    DataTupleInit { entries: Vec<ASTExpr<'src>> },
}
#[derive(Debug, Clone)]
pub enum ASTSubBlocked<'src> {
    /// `{ ... }`
    Block { block: Block<'src> },
    /// `if x { ... }`
    If {
        loc: Loc,
        condition: Option<Expr<'src>>,
        block: Option<Block<'src>>,
        /// `else if y { ... }`
        elifs: Option<Vec<(Option<Expr<'src>>, Option<Block<'src>>)>>,
        /// `else { ... }`
        else_block: Option<Block<'src>>,
    },
    /// `loop { ... }`
    Loop {
        loc: Loc,
        block: Option<Block<'src>>,
    },
    /// `while x { ... }`
    While {
        loc: Loc,
        condition: Option<Expr<'src>>,
        block: Option<Block<'src>>,
        /// Optional else block which allows this to return
        /// a value when used as an expression.
        else_block: Option<Block<'src>>,
    },
    /// `for x in y { ... }`
    For {
        loc: Loc,
        var_destruct: Option<ASTVarDestructure<'src>>,
        var_ty: Option<ASTType<'src>>,
        block: Option<Block<'src>>,
        /// Optional else block which allows this to return
        /// a value when used as an expression.
        else_block: Option<Block<'src>>,
    },
}

#[derive(Debug, Clone)]
pub struct ASTTemplateBound<'src> {
    pub loc: Loc,
    pub name: &'src str,
    pub bounds: Vec<(ASTType<'src>, Loc)>,
}
type Templates<'src> = Vec<ASTTemplateBound<'src>>;

#[derive(Debug, Clone)]
pub enum ASTLiteral {
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
pub struct ASTVarDeclare<'src> {
    pub loc: Loc,
    pub declr: Option<ASTTypedDestructure<'src>>,
    pub initializer: Option<Expr<'src>>,
}

/// `\(a: ty0) -> ty1 { ... }` or `x { \a: ty0 -> ... }`
#[derive(Debug, Clone)]
pub struct ASTLambda<'src> {
    pub loc: Loc,
    pub has_self_arg: bool,
    pub args: Vec<ASTTypedDestructure<'src>>,
    pub return_ty: Option<ASTType<'src>>,
    pub block: Option<Block<'src>>,
}

#[derive(Debug, Clone)]
pub struct ASTFunction<'src> {
    pub name: Option<&'src str>,
    pub doc: Option<ASTDoc>,
    pub lambda: ASTLambda<'src>,
}

#[derive(Debug, Clone)]
pub struct ASTImport<'src> {
    pub loc: Loc,
    pub tree: ASTImportTree<'src>,
}
#[derive(Debug, Clone)]
pub enum ASTImportTree<'src> {
    /// `abc` or `abc.<...>`
    Name {
        loc: Loc,
        name: &'src str,
        child: Option<Box<ASTImportTree<'src>>>,
    },
    /// `abc.[ ... ]`
    Group {
        loc: Loc,
        name: &'src str,
        children: Vec<(Loc, ASTImportTree<'src>)>,
    },
    /// `*`
    ToAll(Loc),
    /// `self`
    ToSelf(Loc),
    /// `super`
    ToSuper(Loc),
    /// `root`
    ToRoot(Loc),
}

#[derive(Debug, Clone)]
pub struct ASTDoc {
    pub loc: Loc,
    pub processed_text: String,
}

#[derive(Debug, Clone)]
pub enum ASTType<'src> {
    /// A simple named type, like `int` or `BinaryTree`.
    Named { name: &'src str, loc: Loc },
    /// Types of the form `Inner.Name`
    Access {
        name_loc: Loc,
        name: &'src str,
        inner: Box<ASTType<'src>>,
    },
    /// types of the form `A<B,C,D=E>
    TypeParam {
        loc: Loc,
        inner: Box<ASTType<'src>>,
        params: Vec<ASTType<'src>>,
        named_params: Option<Vec<((&'src str, Loc), Option<ASTType<'src>>)>>,
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
pub enum ASTVarDestructure<'src> {
    Name { loc: Loc, name: &'src str },
}
#[derive(Debug, Clone)]
pub struct ASTTypedDestructure<'src> {
    pub destructure: ASTVarDestructure<'src>,
    pub ty: Option<ASTType<'src>>,
}

#[derive(Debug, Clone)]
pub struct ASTConst<'src> {
    pub loc: Loc,
    pub name: Option<&'src str>,
    pub ty: Option<ASTType<'src>>,
    pub value: Option<ASTExpr<'src>>,
}

#[derive(Debug, Clone)]
pub struct ASTTrait<'src> {
    pub loc: Loc,
    pub name: Option<&'src str>,
    pub doc: Option<ASTDoc>,
    pub templates: Templates<'src>,
    pub bounds: Option<ASTType<'src>>,
    pub functions: Vec<ASTFunction<'src>>,
    pub consts: Vec<ASTConst<'src>>,
    // [TODO:0000] type aliases
}

#[derive(Debug, Clone)]
pub struct ASTData<'src> {
    pub loc: Loc,
    pub name: Option<&'src str>,
    pub doc: Option<ASTDoc>,
    pub templates: Templates<'src>,
    pub attatched_impls: Vec<ASTImpl<'src>>,
    pub consts: Vec<ASTConst<'src>>,
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
    pub loc: Loc,
    pub name: &'src str,
    pub doc: Option<ASTDoc>,
    pub which: ASTEnumVariantType<'src>,
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
pub struct ASTFreeImpl<'src> {
    pub target: ASTType<'src>,
    pub attatched_impl: ASTImpl<'src>,
}

#[derive(Debug, Clone)]
pub struct ASTDataProperty<'src> {
    pub loc: Loc,
    pub name: &'src str,
    pub ty: ASTType<'src>,
    pub doc: Option<ASTDoc>,
}

#[derive(Debug, Clone)]
pub struct ASTImpl<'src> {
    pub loc: Loc,
    pub templates: Templates<'src>,
    pub target_trait: Option<ASTType<'src>>,
    pub functions: Vec<ASTFunction<'src>>,
}
