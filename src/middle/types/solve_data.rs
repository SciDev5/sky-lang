use crate::{
    front::{
        ast::{ASTIdent, ASTName},
        source::{HasLoc, Loc},
        tokenize::{TInfixOperatorType, TPostfixOperatorType, TPrefixOperatorType},
    },
    impl_hasloc_simple,
    lint::diagnostic::{DiagnosticId, Fallible},
    middle::statics::scopes::ScopeId,
    modularity::Id,
};

use super::TypeDatalike;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SolvingType {
    Unknown,
    Intlike,
    Floatlike,
    Solved(TypeDatalike),
}

pub type LocalVarId = usize;
/// A number representing how many levels of loops deep a control flow command (like `break`) should act.
///
/// 0 is outermost loop, 1 is the loop inside that, all the way until the loop that directly contians the command.
pub type LoopLevel = usize;

#[derive(Debug, Clone, PartialEq)]
pub enum SolvingStmt<'src> {
    VarDeclare(SolvingVarDeclare<'src>),
    VarAssign {
        loc: Loc,
        assign_op: Option<TInfixOperatorType>,
        lhs: Box<SolvingExpr<'src>>,
        rhs: Fallible<Box<SolvingExpr<'src>>>,
    },
    SubBlocked(SolvingSubBlocked<'src>),
    Expr(SolvingExpr<'src>),
}
#[derive(Debug, Clone, PartialEq)]
pub enum SolvingExpr<'src> {
    Fail {
        loc: Loc,
        after: Vec<SolvingExpr<'src>>,
        because: DiagnosticId,
        ty_eval: SolvingType,
    },
    Literal {
        loc: Loc,
        value: SolvingLiteral,
    },
    Ident(ASTIdent<'src>),
    Lambda(SolvingLambda<'src>),
    SubBlocked(SolvingSubBlocked<'src>),
    PostfixBlock {
        inner: Box<SolvingExpr<'src>>,
        postfix: (SolvingPostfixBlock<'src>, Loc),
    },
    OpPrefix {
        loc: Loc,
        inner: Box<SolvingExpr<'src>>,
        op: (TPrefixOperatorType, Loc),
    },
    OpPostfix {
        loc: Loc,
        inner: Box<SolvingExpr<'src>>,
        op: (TPostfixOperatorType, Loc),
    },
    OpInfix {
        loc: Loc,
        inner: (Box<SolvingExpr<'src>>, Box<SolvingExpr<'src>>),
        op: (TInfixOperatorType, Loc),
    },
    Array {
        loc: Loc,
        inner: Vec<Fallible<SolvingExpr<'src>>>,
        /// `true` &rarr; `(a,b,c)`, represents a tuple.
        /// `false` &rarr; `[a,b,c]`, represents an array ref.
        is_tuple: bool,
    },

    Call {
        loc: Loc,
        template_values: Vec<SolvingType>,
        fn_id: Id,
        args: Vec<SolvingExpr<'src>>,
    },
    CallTrait {
        loc: Loc,
        reifier: TypeDatalike,
        template_values: Vec<SolvingType>,
        trait_id: Id,
        fn_id: usize,
        args: Vec<SolvingExpr<'src>>,
    },

    Return {
        loc: Loc,
        inner: Option<Box<SolvingExpr<'src>>>,
    },
    Break {
        loc: Loc,
        inner: Option<Box<SolvingExpr<'src>>>,
        target: LoopLevel,
    },
    Continue {
        loc: Loc,
        target: LoopLevel,
    },
}
impl<'src> HasLoc for SolvingExpr<'src> {
    fn loc(&self) -> Loc {
        match self {
            Self::Literal { loc, .. }
            | Self::Call { loc, .. }
            | Self::CallTrait { loc, .. }
            | Self::Fail { loc, .. }
            | Self::Ident(ASTIdent { loc, .. })
            | Self::OpInfix { loc, .. }
            | Self::OpPrefix { loc, .. }
            | Self::OpPostfix { loc, .. }
            | Self::Return { loc, .. }
            | Self::Break { loc, .. }
            | Self::Continue { loc, .. }
            | Self::Array { loc, .. } => *loc,
            Self::PostfixBlock { inner, postfix } => inner.loc().merge(postfix.1),
            Self::Lambda(lambda) => lambda.loc(),
            Self::SubBlocked(sb) => sb.loc(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct SolvingBlock<'src> {
    pub loc: Loc,
    pub body: Vec<SolvingStmt<'src>>,
    pub scope: ScopeId,
}
impl_hasloc_simple!(SolvingBlock<'src>);

#[derive(Debug, Clone, PartialEq)]
pub enum SolvingPostfixBlock<'src> {
    /// `x ( a, b, c )`
    Call {
        args: Vec<Fallible<SolvingExpr<'src>>>,
    },
    /// `x [ a, b, c ]`
    Index {
        args: Vec<Fallible<SolvingExpr<'src>>>,
    },
    /// `x { a, b -> c }``
    Lambda { lambda: SolvingLambda<'src> },
    /// `x.{ a = b, c }`
    DataStructInit {
        entries: Vec<(ASTName<'src>, Fallible<SolvingExpr<'src>>)>,
    },
    /// `x.( a, b, c)`
    DataTupleInit {
        entries: Vec<Fallible<SolvingExpr<'src>>>,
    },
    /// `x.abc`
    PropertyAccess { name: ASTName<'src> },
}
#[derive(Debug, Clone, PartialEq)]
pub enum SolvingSubBlocked<'src> {
    /// `{ ... }`
    Block { block: SolvingBlock<'src> },
    /// `if x { ... }`
    If {
        loc: Loc,
        condition: Fallible<Box<SolvingExpr<'src>>>,
        block: Fallible<SolvingBlock<'src>>,
        /// `else if y { ... }`
        elifs: Vec<(
            Fallible<Box<SolvingExpr<'src>>>,
            Fallible<SolvingBlock<'src>>,
        )>,
        /// `else { ... }`
        else_block: Option<Fallible<SolvingBlock<'src>>>,
    },
    /// `loop { ... }`
    Loop {
        loc: Loc,
        block: Fallible<SolvingBlock<'src>>,
    },
    /// `while x { ... }`
    While {
        loc: Loc,
        condition: Fallible<Box<SolvingExpr<'src>>>,
        block: Fallible<SolvingBlock<'src>>,
        /// Optional else block which allows this to return
        /// a value when used as an expression.
        else_block: Option<Fallible<SolvingBlock<'src>>>,
    },
    /// `for x in y { ... }`
    For {
        loc: Loc,
        var: Fallible<SolvingTypedDestructure>,
        iterator: Fallible<Box<SolvingExpr<'src>>>,
        block: Fallible<SolvingBlock<'src>>,
        /// Optional else block which allows this to return
        /// a value when used as an expression.
        else_block: Option<Fallible<SolvingBlock<'src>>>,
    },
}
impl<'src> HasLoc for SolvingSubBlocked<'src> {
    fn loc(&self) -> Loc {
        match self {
            Self::If { loc, .. }
            | Self::Loop { loc, .. }
            | Self::While { loc, .. }
            | Self::For { loc, .. }
            | Self::Block {
                block: SolvingBlock { loc, .. },
            } => *loc,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum SolvingLiteral {
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

#[derive(Debug, Clone, PartialEq)]
pub struct SolvingVarDeclare<'src> {
    pub loc: Loc,
    pub declr: Fallible<SolvingTypedDestructure>,
    pub initializer: Option<Fallible<Box<SolvingExpr<'src>>>>,
}
impl_hasloc_simple!(SolvingVarDeclare<'src>);

/// `\(a: ty0) -> ty1 { ... }` or `x { \a: ty0 -> ... }`
#[derive(Debug, Clone, PartialEq)]
pub struct SolvingLambda<'src> {
    pub loc: Loc,
    // pub templates: TemplateBounds<'src>,
    pub args: Option<Vec<Fallible<SolvingTypedDestructure>>>,
    pub ty_return: Option<Fallible<TypeDatalike>>,
    pub block: SolvingBlock<'src>,
}
impl_hasloc_simple!(SolvingLambda<'src>);

#[derive(Debug, Clone, PartialEq)]
pub enum SolvingDestructure {
    Name {
        loc: Loc,
        var_id: LocalVarId,
    },
    Paren {
        loc: Loc,
        inner: Fallible<Box<SolvingDestructure>>,
    },
    Tuple {
        loc: Loc,
        inner: Vec<Fallible<SolvingDestructure>>,
    },
    // TODO Struct destructures not included because they're rare and a pain to implement
}
impl<'src> HasLoc for SolvingDestructure {
    fn loc(&self) -> Loc {
        match self {
            Self::Name { loc, .. } | Self::Tuple { loc, .. } | Self::Paren { loc, .. } => *loc,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct SolvingTypedDestructure {
    pub destructure: SolvingDestructure,
    pub ty: Option<Fallible<TypeDatalike>>,
}
impl HasLoc for SolvingTypedDestructure {
    fn loc(&self) -> Loc {
        self.destructure.loc().merge_some(
            self.ty
                .as_ref()
                .and_then(|f| f.as_ref().ok())
                .map(HasLoc::loc),
        )
    }
}
