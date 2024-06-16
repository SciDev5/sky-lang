//! The definition of the abstract syntax tree for the language.
//!
//! This represents the text content of the program as a tree of nodes representing
//! distinct syntactic elements of the language, such as if statements, expressions,
//! and literals.

use crate::{impl_hasloc_simple, middle::scope::LocallyScoped};

use super::{
    source::{HasLoc, Loc},
    tokenize::{TInfixOperatorType, TPostfixOperatorType, TPrefixOperatorType},
};

/// The definition of the abstract syntax tree for a given source file and all its
/// contents.
///
/// The AST represents the text content of the program as a tree of nodes representing
/// distinct syntactic elements of the language, such as if statements, expressions,
/// and literals.
#[derive(Debug, Clone, PartialEq)]
pub struct ASTSourceFile<'src> {
    pub body: Vec<ASTStmt<'src>>,
    pub scope: LocallyScoped<'src>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ASTDeclr<'src> {
    Function(ASTFunction<'src>),
    Const(ASTConst<'src>),
    Trait(ASTTrait<'src>),
    Data(ASTData<'src>),
    FreeImpl(ASTFreeImpl<'src>),
    Import(ASTImport<'src>),
    TypeAlias(ASTTypeAlias<'src>),
}
#[derive(Debug, Clone, PartialEq)]
pub enum ASTStmt<'src> {
    StaticDeclare(Box<ASTDeclr<'src>>),
    VarDeclare(ASTVarDeclare<'src>),
    VarAssign {
        loc: Loc,
        assign_op: Option<TInfixOperatorType>,
        lhs: Expr<'src>,
        rhs: ASTFallible<Expr<'src>>,
    },
    SubBlocked(ASTSubBlocked<'src>),
    Expr(ASTExpr<'src>),
}
#[derive(Debug, Clone, PartialEq)]
pub enum ASTExpr<'src> {
    Literal {
        loc: Loc,
        value: ASTLiteral<'src>,
    },
    Ident(ASTIdent<'src>),
    Lambda(ASTLambda<'src>),
    SubBlocked(ASTSubBlocked<'src>),
    PostfixBlock {
        inner: Expr<'src>,
        postfix: (ASTPostfixBlock<'src>, Loc),
    },
    OpPrefix {
        loc: Loc,
        inner: Expr<'src>,
        op: (TPrefixOperatorType, Loc),
    },
    OpPostfix {
        loc: Loc,
        inner: Expr<'src>,
        op: (TPostfixOperatorType, Loc),
    },
    OpInfix {
        loc: Loc,
        inner: (Expr<'src>, Expr<'src>),
        op: (TInfixOperatorType, Loc),
    },
    Parentheses {
        loc: Loc,
        inner: ASTFallible<Expr<'src>>,
    },
    Array {
        loc: Loc,
        inner: Vec<ASTFallible<ASTExpr<'src>>>,
        /// `true` &rarr; `(a,b,c)`, represents a tuple.
        /// `false` &rarr; `[a,b,c]`, represents an array ref.
        is_tuple: bool,
    },

    Return {
        loc: Loc,
        inner: Option<Expr<'src>>,
    },
    Break {
        loc: Loc,
        inner: Option<Expr<'src>>,
    },
    Continue {
        loc: Loc,
    },
}
impl<'src> HasLoc for ASTExpr<'src> {
    fn loc(&self) -> Loc {
        match self {
            Self::Literal { loc, .. }
            | Self::Ident(ASTIdent { loc, .. })
            | Self::OpInfix { loc, .. }
            | Self::OpPrefix { loc, .. }
            | Self::OpPostfix { loc, .. }
            | Self::Parentheses { loc, .. }
            | Self::Return { loc, .. }
            | Self::Break { loc, .. }
            | Self::Continue { loc }
            | Self::Array { loc, .. } => *loc,
            Self::PostfixBlock { inner, postfix } => inner.loc().merge(postfix.1),
            Self::Lambda(lambda) => lambda.loc(),
            Self::SubBlocked(sb) => sb.loc(),
        }
    }
}
type Expr<'src> = Box<ASTExpr<'src>>;

#[derive(Debug, Clone, PartialEq)]
pub struct ASTBlock<'src> {
    pub loc: Loc,
    pub body: Vec<ASTStmt<'src>>,
    pub scope: LocallyScoped<'src>,
}
impl_hasloc_simple!(ASTBlock<'src>);

#[derive(Debug, Clone, PartialEq)]
pub enum ASTPostfixBlock<'src> {
    /// `x ( a, b, c )`
    Call {
        args: Vec<ASTFallible<ASTExpr<'src>>>,
    },
    /// `x [ a, b, c ]`
    Index {
        args: Vec<ASTFallible<ASTExpr<'src>>>,
    },
    /// `x { a, b -> c }``
    Lambda { lambda: ASTLambda<'src> },
    /// `x.{ a = b, c }`
    DataStructInit {
        entries: Vec<(ASTName<'src>, ASTFallible<ASTExpr<'src>>)>,
    },
    /// `x.( a, b, c)`
    DataTupleInit {
        entries: Vec<ASTFallible<ASTExpr<'src>>>,
    },
    /// `x.abc`
    PropertyAccess { name: ASTName<'src> },
}
#[derive(Debug, Clone, PartialEq)]
pub enum ASTSubBlocked<'src> {
    /// `{ ... }`
    Block { block: ASTBlock<'src> },
    /// `if x { ... }`
    If {
        loc: Loc,
        condition: ASTFallible<Expr<'src>>,
        block: ASTFallible<ASTBlock<'src>>,
        /// `else if y { ... }`
        elifs: Vec<(ASTFallible<Expr<'src>>, ASTFallible<ASTBlock<'src>>)>,
        /// `else { ... }`
        else_block: Option<ASTFallible<ASTBlock<'src>>>,
    },
    /// `loop { ... }`
    Loop {
        loc: Loc,
        block: ASTFallible<ASTBlock<'src>>,
    },
    /// `while x { ... }`
    While {
        loc: Loc,
        condition: ASTFallible<Expr<'src>>,
        block: ASTFallible<ASTBlock<'src>>,
        /// Optional else block which allows this to return
        /// a value when used as an expression.
        else_block: Option<ASTFallible<ASTBlock<'src>>>,
    },
    /// `for x in y { ... }`
    For {
        loc: Loc,
        var: ASTFallible<ASTTypedDestructure<'src>>,
        iterator: ASTFallible<Expr<'src>>,
        block: ASTFallible<ASTBlock<'src>>,
        /// Optional else block which allows this to return
        /// a value when used as an expression.
        else_block: Option<ASTFallible<ASTBlock<'src>>>,
    },
}
impl<'src> HasLoc for ASTSubBlocked<'src> {
    fn loc(&self) -> Loc {
        match self {
            Self::If { loc, .. }
            | Self::Loop { loc, .. }
            | Self::While { loc, .. }
            | Self::For { loc, .. }
            | Self::Block {
                block: ASTBlock { loc, .. },
            } => *loc,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTTemplateBound<'src> {
    pub loc: Loc,
    pub name: ASTName<'src>,
    pub bounds: Vec<ASTType<'src>>,
}
pub type ASTTemplates<'src> = Option<(Vec<ASTTemplateBound<'src>>, Loc)>;

#[derive(Debug, Clone, PartialEq)]
pub enum ASTLiteral<'src> {
    /// Integer of any size (up to `2^128-1`).
    Int(u128),
    /// Negative integer of any size (stored as negative, down to `-2^63`)
    NInt(i128),
    /// Invalid integer
    IntInvalid(&'src str, ASTDiagnosticRef),
    /// Floating point number, stored in both supported formats for when the
    /// type system decides which one to use.
    Float(f64, f32),
    /// Invalid float
    FloatInvalid(&'src str, ASTDiagnosticRef),
    /// Simple boolean.
    Bool(bool),
    /// String (with `\n` and similar escaped), with no templating going on.
    String(String),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTVarDeclare<'src> {
    pub loc: Loc,
    pub declr: ASTFallible<ASTTypedDestructure<'src>>,
    pub initializer: Option<ASTFallible<Expr<'src>>>,
}
impl_hasloc_simple!(ASTVarDeclare<'src>);

/// `\(a: ty0) -> ty1 { ... }` or `x { \a: ty0 -> ... }`
#[derive(Debug, Clone, PartialEq)]
pub struct ASTLambda<'src> {
    pub loc: Loc,
    // pub templates: TemplateBounds<'src>,
    pub args: Option<Vec<ASTFallible<ASTTypedDestructure<'src>>>>,
    pub ty_return: Option<ASTFallible<ASTType<'src>>>,
    pub block: ASTBlock<'src>,
}
impl_hasloc_simple!(ASTLambda<'src>);

#[derive(Debug, Clone, PartialEq)]
pub struct ASTFunction<'src> {
    pub loc: Loc,
    pub annot: ASTAnnot,
    pub name: ASTFallible<ASTName<'src>>,
    pub templates: ASTTemplates<'src>,
    pub args: Vec<ASTFallible<ASTTypedDestructure<'src>>>,
    pub ty_return: Option<ASTFallible<ASTType<'src>>>,
    pub block: Option<ASTBlock<'src>>,
}
impl_hasloc_simple!(ASTFunction<'src>);

#[derive(Debug, Clone, PartialEq)]
pub struct ASTImport<'src> {
    pub loc: Loc,
    pub tree: ASTFallible<ASTImportTree<'src>>,
}
#[derive(Debug, Clone, PartialEq)]
pub enum ASTImportTree<'src> {
    /// `abc` or `abc.<...>`
    Name {
        loc: Loc,
        ident: ASTIdent<'src>,
        inner: Option<ASTFallible<Box<ASTImportTree<'src>>>>,
    },
    /// `abc.[ ... ]`
    Group {
        loc: Loc,
        ident: ASTIdent<'src>,
        inner: Vec<ASTImportTree<'src>>,
    },
    /// `*`
    ToAll(Loc),
}
impl<'src> HasLoc for ASTImportTree<'src> {
    fn loc(&self) -> Loc {
        match self {
            Self::Name { loc, .. } | Self::Group { loc, .. } | Self::ToAll(loc) => *loc,
        }
    }
}

/// Raw names, like [`ASTIdent`], but doesn't match keywords like `self`, `super`, etc.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ASTName<'src> {
    pub loc: Loc,
    pub value: &'src str,
}
impl_hasloc_simple!(ASTName<'src>);
/// Identifiers, including some keywords like `self`, `super`, etc.
#[derive(Debug, Clone, PartialEq)]
pub struct ASTIdent<'src> {
    pub loc: Loc,
    pub value: ASTIdentValue<'src>,
}
impl_hasloc_simple!(ASTIdent<'src>);
#[derive(Debug, Clone, PartialEq)]
pub enum ASTIdentValue<'src> {
    Name(&'src str),
    Super,
    Root,
    SelfVar,
    SelfTy,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTAnnot {
    /// If public keyword, this is the location of that keyword.
    pub is_public: Option<Loc>,
    pub doc: Vec<ASTDoc>,
    // TODO attr macros
}
impl ASTAnnot {
    pub fn take(&mut self) -> Self {
        let mut replacement = Self {
            doc: Vec::new(),
            is_public: None,
        };
        std::mem::swap(self, &mut replacement);
        replacement
    }
}
#[derive(Debug, Clone, PartialEq)]
pub struct ASTDoc {
    pub loc: Loc,
    pub processed_text: String,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ASTType<'src> {
    /// Shorthand for the common unit type
    Unit { loc: Loc },
    /// A simple named type, like `int` or `BinaryTree`.
    Ident(ASTIdent<'src>),
    /// Types of the form `Inner.Name`
    Access {
        ident: ASTIdent<'src>,
        inner: Box<ASTType<'src>>,
    },
    /// types of the form `A<B,C,D=E>
    TypeParam {
        loc: Loc,
        inner: Box<ASTType<'src>>,
        params: Vec<ASTFallible<ASTType<'src>>>,
        named_params: Vec<(ASTName<'src>, ASTFallible<ASTType<'src>>)>,
    },
    /// Types of the form `(A,B,C)`
    Tuple {
        loc: Loc,
        inner: Vec<ASTFallible<ASTType<'src>>>,
    },
    /// Parentheses in types. Used for disambiguation.
    Paren {
        loc: Loc,
        inner: ASTFallible<Box<ASTType<'src>>>,
    },
    //// TODO not included for the time being, as they are aliases for other types
    //// and I need to decide how to handle that:
    // Ref {
    //     mutable: bool,
    //     inner: Box<ASTType<'src>>,
    // },
    // Array {
    //     inner: Box<ASTType<'src>>,
    // },
}
impl<'src> HasLoc for ASTType<'src> {
    fn loc(&self) -> Loc {
        match self {
            Self::Ident(ASTIdent { loc, .. })
            | Self::Unit { loc }
            | Self::TypeParam { loc, .. }
            | Self::Tuple { loc, .. }
            | Self::Paren { loc, .. } => *loc,
            Self::Access { ident, inner, .. } => ident.loc().merge(inner.loc()),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ASTDestructure<'src> {
    Name(ASTName<'src>),
    Paren {
        loc: Loc,
        inner: ASTFallible<Box<ASTDestructure<'src>>>,
    },
    Tuple {
        loc: Loc,
        inner: Vec<ASTFallible<ASTDestructure<'src>>>,
    },
    // TODO Struct destructures not included because they're rare and a pain to implement
}
impl<'src> HasLoc for ASTDestructure<'src> {
    fn loc(&self) -> Loc {
        match self {
            Self::Name(ASTName { loc, .. }) | Self::Tuple { loc, .. } | Self::Paren { loc, .. } => {
                *loc
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTTypedDestructure<'src> {
    pub destructure: ASTDestructure<'src>,
    pub ty: Option<ASTFallible<ASTType<'src>>>,
}
impl<'src> HasLoc for ASTTypedDestructure<'src> {
    fn loc(&self) -> Loc {
        self.destructure.loc().merge_some(
            self.ty
                .as_ref()
                .and_then(|f| f.as_ref().ok())
                .map(HasLoc::loc),
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTConst<'src> {
    pub loc: Loc,
    pub annot: ASTAnnot,
    pub name: ASTFallible<ASTName<'src>>,
    pub ty: Option<ASTFallible<ASTType<'src>>>,
    pub value: ASTFallible<ASTExpr<'src>>,
}
impl_hasloc_simple!(ASTConst<'src>);

#[derive(Debug, Clone, PartialEq)]
pub struct ASTTrait<'src> {
    pub loc: Loc,
    pub annot: ASTAnnot,
    pub name: ASTFallible<ASTName<'src>>,
    pub templates: ASTTemplates<'src>,
    pub bounds: Vec<ASTType<'src>>,
    pub contents: ASTImplContents<'src>,
}
impl_hasloc_simple!(ASTTrait<'src>);

#[derive(Debug, Clone, PartialEq)]
pub struct ASTData<'src> {
    pub loc: Loc,
    pub annot: ASTAnnot,
    pub name: ASTFallible<ASTName<'src>>,
    pub templates: ASTTemplates<'src>,
    pub contents: ASTDataContents<'src>,
    pub attatched_impls: Vec<ASTImpl<'src>>,
}
impl_hasloc_simple!(ASTData<'src>);

#[derive(Debug, Clone, PartialEq)]
pub enum ASTDataContents<'src> {
    Unit,
    Abstract,
    Inherit,
    Struct {
        properties: Vec<ASTDataProperty<'src>>,
    },
    Tuple {
        properties: Vec<ASTFallible<ASTType<'src>>>,
    },
    Enum {
        variants: Vec<ASTEnumVariant<'src>>,
    },
}
#[derive(Debug, Clone, PartialEq)]
pub struct ASTEnumVariant<'src> {
    pub loc: Loc,
    pub annot: ASTAnnot,
    pub name: ASTName<'src>,
    pub contents: ASTEnumVariantType<'src>,
}
#[derive(Debug, Clone, PartialEq)]
pub enum ASTEnumVariantType<'src> {
    Unit,
    Struct {
        properties: Vec<ASTDataProperty<'src>>,
    },
    Tuple {
        properties: Vec<ASTFallible<ASTType<'src>>>,
    },
}
#[derive(Debug, Clone, PartialEq)]
pub struct ASTTypeAlias<'src> {
    pub loc: Loc,
    pub annot: ASTAnnot,
    pub templates: ASTTemplates<'src>,
    pub name: ASTFallible<ASTName<'src>>,
    pub value: ASTFallible<ASTType<'src>>,
}
impl_hasloc_simple!(ASTTypeAlias<'src>);

#[derive(Debug, Clone, PartialEq)]
pub struct ASTFreeImpl<'src> {
    pub target: ASTFallible<ASTType<'src>>,
    pub attatched_impl: ASTImpl<'src>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTDataProperty<'src> {
    pub loc: Loc,
    pub annot: ASTAnnot,
    pub name: ASTName<'src>,
    pub ty: Option<ASTFallible<ASTType<'src>>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTImpl<'src> {
    pub loc: Loc,
    pub templates: ASTTemplates<'src>,
    pub target_trait: Option<ASTType<'src>>,
    pub contents: ASTImplContents<'src>,
}
impl_hasloc_simple!(ASTImpl<'src>);

#[derive(Debug, Clone, PartialEq)]
pub struct ASTImplContents<'src> {
    pub loc: Loc,
    pub functions: Vec<ASTFunction<'src>>,
    pub consts: Vec<ASTConst<'src>>,
    pub types: Vec<ASTTypeAlias<'src>>,
}
impl_hasloc_simple!(ASTImplContents<'src>);

pub type ASTFallible<T> = Result<T, ASTDiagnosticRef>;
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ASTDiagnosticRef {
    pub parse_diagnostic_id: usize,
}
