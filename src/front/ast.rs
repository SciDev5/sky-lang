//! The definition of the abstract syntax tree for the language.
//!
//! This represents the text content of the program as a tree of nodes representing
//! distinct syntactic elements of the language, such as if statements, expressions,
//! and literals.

use std::collections::HashMap;

use crate::{
    back::BackendId,
    impl_hasloc_simple,
    lint::diagnostic::{DiagnosticId, Fallible},
    middle::{
        module::ModuleParts,
        statics::scopes::{ScopeId, Scopes},
    },
    modularity::Id,
};

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
    pub scope: ScopeId,
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
        rhs: Fallible<Expr<'src>>,
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
        inner: Fallible<Expr<'src>>,
    },
    Array {
        loc: Loc,
        inner: Vec<Fallible<ASTExpr<'src>>>,
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
pub struct ASTScope<'src> {
    pub backend_id: BackendId,
    pub mod_id: usize,
    pub functions: HashMap<&'src str, Id>,
    pub datas: HashMap<&'src str, Id>,
    pub traits: HashMap<&'src str, Id>,
    pub consts: HashMap<&'src str, Id>,
    pub typealiases: HashMap<&'src str, Id>,
    pub imports: Vec<ASTImportTree<'src>>,
}
impl<'src> ASTScope<'src> {
    pub fn new_empty() -> Self {
        Self {
            backend_id: usize::MAX,
            mod_id: usize::MAX,
            functions: HashMap::new(),
            datas: HashMap::new(),
            traits: HashMap::new(),
            consts: HashMap::new(),
            typealiases: HashMap::new(),
            imports: Vec::new(),
        }
    }
    pub fn attatch_backend_ids(
        modules: &Vec<ModuleParts<'src>>,
        scopes: &mut Scopes<ASTScope<'src>>,
    ) {
        let backend_id_lookup = modules
            .iter()
            .enumerate()
            .flat_map(|(mod_id, ModuleParts { parts, .. })| {
                parts
                    .iter()
                    .zip(std::iter::repeat(mod_id))
                    .map(|((backend_id, src), mod_id)| (src.scope, (mod_id, *backend_id)))
            })
            .collect::<HashMap<_, _>>();
        scopes.modify_contextual(
            |scope, (mod_id, backend_id), _| {
                scope.backend_id = *backend_id;
                scope.mod_id = *mod_id;
            },
            &backend_id_lookup,
        )
    }
}

#[derive(Debug)]
pub struct ASTStatics<'src> {
    pub functions: Vec<ASTFunction<'src>>,
    pub datas: Vec<ASTData<'src>>,
    pub traits: Vec<ASTTrait<'src>>,
    pub consts: Vec<ASTConst<'src>>,
    pub typealiases: Vec<ASTTypeAlias<'src>>,
    pub free_impls: Vec<ASTFreeImpl<'src>>,
}

impl<'src> ASTStatics<'src> {
    pub fn new() -> Self {
        Self {
            functions: Vec::new(),
            datas: Vec::new(),
            traits: Vec::new(),
            consts: Vec::new(),
            typealiases: Vec::new(),
            free_impls: Vec::new(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTBlock<'src> {
    pub loc: Loc,
    pub body: Vec<ASTStmt<'src>>,
    pub scope: ScopeId,
}
impl_hasloc_simple!(ASTBlock<'src>);

#[derive(Debug, Clone, PartialEq)]
pub enum ASTPostfixBlock<'src> {
    /// `x ( a, b, c )`
    Call { args: Vec<Fallible<ASTExpr<'src>>> },
    /// `x [ a, b, c ]`
    Index { args: Vec<Fallible<ASTExpr<'src>>> },
    /// `x { a, b -> c }``
    Lambda { lambda: ASTLambda<'src> },
    /// `x.{ a = b, c }`
    DataStructInit {
        entries: Vec<(ASTName<'src>, Fallible<ASTExpr<'src>>)>,
    },
    /// `x.( a, b, c)`
    DataTupleInit {
        entries: Vec<Fallible<ASTExpr<'src>>>,
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
        condition: Fallible<Expr<'src>>,
        block: Fallible<ASTBlock<'src>>,
        /// `else if y { ... }`
        elifs: Vec<(Fallible<Expr<'src>>, Fallible<ASTBlock<'src>>)>,
        /// `else { ... }`
        else_block: Option<Fallible<ASTBlock<'src>>>,
    },
    /// `loop { ... }`
    Loop {
        loc: Loc,
        block: Fallible<ASTBlock<'src>>,
    },
    /// `while x { ... }`
    While {
        loc: Loc,
        condition: Fallible<Expr<'src>>,
        block: Fallible<ASTBlock<'src>>,
        /// Optional else block which allows this to return
        /// a value when used as an expression.
        else_block: Option<Fallible<ASTBlock<'src>>>,
    },
    /// `for x in y { ... }`
    For {
        loc: Loc,
        var: Fallible<ASTTypedDestructure<'src>>,
        iterator: Fallible<Expr<'src>>,
        block: Fallible<ASTBlock<'src>>,
        /// Optional else block which allows this to return
        /// a value when used as an expression.
        else_block: Option<Fallible<ASTBlock<'src>>>,
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
    IntInvalid(&'src str, DiagnosticId),
    /// Floating point number, stored in both supported formats for when the
    /// type system decides which one to use.
    Float(f64, f32),
    /// Invalid float
    FloatInvalid(&'src str, DiagnosticId),
    /// Simple boolean.
    Bool(bool),
    /// String (with `\n` and similar escaped), with no templating going on.
    String(String),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTVarDeclare<'src> {
    pub loc: Loc,
    pub declr: Fallible<ASTTypedDestructure<'src>>,
    pub initializer: Option<Fallible<Expr<'src>>>,
}
impl_hasloc_simple!(ASTVarDeclare<'src>);

/// `\(a: ty0) -> ty1 { ... }` or `x { \a: ty0 -> ... }`
#[derive(Debug, Clone, PartialEq)]
pub struct ASTLambda<'src> {
    pub loc: Loc,
    // pub templates: TemplateBounds<'src>,
    pub args: Option<Vec<Fallible<ASTTypedDestructure<'src>>>>,
    pub ty_return: Option<Fallible<ASTType<'src>>>,
    pub block: ASTBlock<'src>,
}
impl_hasloc_simple!(ASTLambda<'src>);

#[derive(Debug, Clone, PartialEq)]
pub struct ASTFunction<'src> {
    pub loc: Loc,
    pub containing_scope: ScopeId,
    pub annot: ASTAnnot,
    pub name: Fallible<ASTName<'src>>,
    pub templates: ASTTemplates<'src>,
    pub args: Vec<Fallible<ASTTypedDestructure<'src>>>,
    pub ty_return: Option<Fallible<ASTType<'src>>>,
    pub block: Option<ASTBlock<'src>>,
}
impl_hasloc_simple!(ASTFunction<'src>);

#[derive(Debug, Clone, PartialEq)]
pub struct ASTImport<'src> {
    pub containing_scope: ScopeId,
    pub loc: Loc,
    pub tree: Fallible<ASTImportTree<'src>>,
}
#[derive(Debug, Clone, PartialEq)]
pub enum ASTImportTree<'src> {
    /// `abc` or `abc.<...>`
    Name {
        loc: Loc,
        ident: ASTIdent<'src>,
        inner: Option<Fallible<Box<ASTImportTree<'src>>>>,
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
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ASTIdent<'src> {
    pub loc: Loc,
    pub value: ASTIdentValue<'src>,
}
impl_hasloc_simple!(ASTIdent<'src>);
#[derive(Debug, Clone, Copy, PartialEq)]
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
        params: Vec<Fallible<ASTType<'src>>>,
        named_params: Vec<(ASTName<'src>, Fallible<ASTType<'src>>)>,
    },
    /// Types of the form `(A,B,C)`
    Tuple {
        loc: Loc,
        inner: Vec<Fallible<ASTType<'src>>>,
    },
    /// Parentheses in types. Used for disambiguation.
    Paren {
        loc: Loc,
        inner: Fallible<Box<ASTType<'src>>>,
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
        inner: Fallible<Box<ASTDestructure<'src>>>,
    },
    Tuple {
        loc: Loc,
        inner: Vec<Fallible<ASTDestructure<'src>>>,
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
    pub ty: Option<Fallible<ASTType<'src>>>,
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
    pub containing_scope: ScopeId,
    pub annot: ASTAnnot,
    pub name: Fallible<ASTName<'src>>,
    pub ty: Option<Fallible<ASTType<'src>>>,
    pub value: Option<Fallible<ASTExpr<'src>>>,
}
impl_hasloc_simple!(ASTConst<'src>);
#[derive(Debug, Clone, PartialEq)]
pub struct ASTTraitConst<'src> {
    pub loc: Loc,
    pub containing_scope: ScopeId,
    pub annot: ASTAnnot,
    pub name: Fallible<ASTName<'src>>,
    pub ty: Fallible<ASTType<'src>>,
}
impl_hasloc_simple!(ASTTraitConst<'src>);

#[derive(Debug, Clone, PartialEq)]
pub struct ASTTrait<'src> {
    pub loc: Loc,
    pub containing_scope: ScopeId,
    pub annot: ASTAnnot,
    pub name: Fallible<ASTName<'src>>,
    pub templates: ASTTemplates<'src>,
    pub bounds: Vec<ASTType<'src>>,

    pub functions: Vec<ASTFunction<'src>>,
    pub consts: Vec<ASTTraitConst<'src>>,
    pub types: Vec<ASTTraitTypeAlias<'src>>,
}
impl_hasloc_simple!(ASTTrait<'src>);

#[derive(Debug, Clone, PartialEq)]
pub struct ASTData<'src> {
    pub loc: Loc,
    pub containing_scope: ScopeId,
    pub annot: ASTAnnot,
    pub name: Fallible<ASTName<'src>>,
    pub templates: ASTTemplates<'src>,
    pub contents: ASTDataContents<'src>,
    pub attatched_impls: Vec<ASTImpl<'src>>,
}
impl_hasloc_simple!(ASTData<'src>);

#[derive(Debug, Clone, PartialEq)]
pub enum ASTDataContents<'src> {
    Unit,
    Abstract,
    Struct {
        properties: Vec<ASTDataProperty<'src>>,
    },
    Tuple {
        properties: Vec<Fallible<ASTType<'src>>>,
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
        properties: Vec<Fallible<ASTType<'src>>>,
    },
}
#[derive(Debug, Clone, PartialEq)]
pub struct ASTTypeAlias<'src> {
    pub loc: Loc,
    pub containing_scope: ScopeId,
    pub annot: ASTAnnot,
    pub templates: ASTTemplates<'src>,
    pub name: Fallible<ASTName<'src>>,
    pub value: Fallible<ASTType<'src>>,
}
impl_hasloc_simple!(ASTTypeAlias<'src>);
#[derive(Debug, Clone, PartialEq)]
pub struct ASTTraitTypeAlias<'src> {
    pub loc: Loc,
    pub containing_scope: ScopeId,
    pub annot: ASTAnnot,
    pub name: Fallible<ASTName<'src>>,
    pub bounds: Vec<ASTType<'src>>,
}
impl_hasloc_simple!(ASTTraitTypeAlias<'src>);

#[derive(Debug, Clone, PartialEq)]
pub struct ASTFreeImpl<'src> {
    pub containing_scope: ScopeId,
    pub target: Fallible<ASTType<'src>>,
    pub attatched_impl: ASTImpl<'src>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTDataProperty<'src> {
    pub loc: Loc,
    pub annot: ASTAnnot,
    pub name: ASTName<'src>,
    pub ty: Option<Fallible<ASTType<'src>>>,
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
