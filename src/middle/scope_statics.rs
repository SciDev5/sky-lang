use std::collections::HashMap;

use crate::{
    front::{
        ast::{
            ASTBlock, ASTConst, ASTData, ASTDeclr, ASTExpr, ASTFallible, ASTFreeImpl, ASTFunction,
            ASTImpl, ASTImplContents, ASTImportTree, ASTLambda, ASTName, ASTPostfixBlock,
            ASTSourceFile, ASTStmt, ASTSubBlocked, ASTTrait, ASTTypeAlias, ASTVarDeclare,
        },
        source::{HasLoc, Loc},
    },
    middle::resolution_diagnostics::ResolutionDiagnostic,
    modularity::Id,
};

use super::resolution_diagnostics::ResolutionDiagnostics;

#[derive(Debug)]
pub struct ASTStatics<'src> {
    pub functions: Vec<ASTFunction<'src>>,
    pub datas: Vec<ASTData<'src>>,
    pub traits: Vec<ASTTrait<'src>>,
    pub consts: Vec<ASTConst<'src>>,
    pub typealiases: Vec<ASTTypeAlias<'src>>,
    pub freeimpls: Vec<ASTFreeImpl<'src>>,
}

impl<'src> ASTStatics<'src> {
    pub fn new() -> Self {
        Self {
            functions: Vec::new(),
            datas: Vec::new(),
            traits: Vec::new(),
            consts: Vec::new(),
            typealiases: Vec::new(),
            freeimpls: Vec::new(),
        }
    }
}

/// Contains lists of statics contained in this scope (not including child scopes.)
///
/// Note that these do not represent the final ids that these names will point to, as
/// there will be some shifts in ids when merging several items into their cross-platform
/// version, and when removing unused items.
#[derive(Debug, Clone, PartialEq)]
pub struct LocallyScoped<'src> {
    pub functions: HashMap<&'src str, Id>,
    pub datas: HashMap<&'src str, Id>,
    pub traits: HashMap<&'src str, Id>,
    pub modules: HashMap<&'src str, Id>,
    pub consts: HashMap<&'src str, Id>,
    pub typealiases: HashMap<&'src str, Id>,
    pub imports: Vec<ASTImportTree<'src>>,
    pub nameless: Vec<LocallyDefinedNameless>,
}
impl<'src> LocallyScoped<'src> {
    pub fn new_empty() -> Self {
        Self {
            functions: HashMap::new(),
            datas: HashMap::new(),
            traits: HashMap::new(),
            modules: HashMap::new(),
            consts: HashMap::new(),
            typealiases: HashMap::new(),
            imports: Vec::new(),
            nameless: Vec::new(),
        }
    }
}
#[derive(Debug, Clone, PartialEq)]
pub enum LocallyDefinedNameless {
    Function(Id),
    Data(Id),
    Trait(Id),
    Module(Id),
    Const(Id),
    TypeAlias(Id),
    FreeImpl(Id),
}

/// Run the scoping / statics extraction step.
///
/// Moves static declarations (eg. function definitions, data definitions, etc.) into their
/// own list (one for the entire module), recording the index it was inserted at as its id.
/// This id is then recorded into the scope that the declaration was made in into a map by
/// the name it was created under, (such as "hello" in the case of `fn hello()`).
///
/// Each `scope_<whatever>` function handles this task for different kinds of data.
#[allow(unused)]
pub fn scope_src<'src>(
    src: &mut ASTSourceFile<'src>,
    statics: &mut ASTStatics<'src>,
) -> Vec<(ResolutionDiagnostic, Loc)> {
    let mut diagnostics = ResolutionDiagnostics::new();
    scope_stmt(&mut src.body, &mut src.scope, statics, &mut diagnostics);
    diagnostics.diagnostics
}

/// Register this static into the list of statics and add its id to the local scope.
/// (This will additionally run the `scope_<whatever>` on the contents of the declarations,
/// such as associated function bodies).
fn register_static<'src>(
    declr: ASTDeclr<'src>,
    scope: &mut LocallyScoped<'src>,
    statics: &mut ASTStatics<'src>,
    diagnostics: &mut ResolutionDiagnostics,
) {
    fn push_get_id<T>(target: &mut Vec<T>, value: T) -> Id {
        let id = target.len();
        target.push(value);
        Id::Local { id }
    }
    fn insert<'src, T: HasLoc, F: Fn(Id) -> LocallyDefinedNameless>(
        name: ASTFallible<ASTName<'src>>,
        value: T,
        scope: &mut HashMap<&'src str, Id>,
        statics: &mut Vec<T>,
        to_nameless: F,
        nameless: &mut Vec<LocallyDefinedNameless>,
        diagnostics: &mut ResolutionDiagnostics,
    ) {
        let loc = value.loc();
        let id = push_get_id(statics, value);
        if let Ok(ASTName { value: name, .. }) = name {
            scope
                .entry(name)
                .and_modify(|_| {
                    diagnostics.raise(ResolutionDiagnostic::DuplicateName, loc);
                })
                .or_insert(id);
        } else {
            nameless.push(to_nameless(id));
        }
    }
    macro_rules! insert {
        ($ast:expr, $k:ident, $to_nameless:expr) => {
            insert(
                $ast.name,
                $ast,
                &mut scope.$k,
                &mut statics.$k,
                $to_nameless,
                &mut scope.nameless,
                diagnostics,
            )
        };
    }
    match declr {
        ASTDeclr::Import(ast_import) => {
            if let Ok(import_tree) = ast_import.tree {
                scope.imports.push(import_tree);
            } else {
                // TODO maybe throw the error? for now, too lazy
            }
        }
        ASTDeclr::FreeImpl(mut ast) => {
            scope_impl(&mut ast.attatched_impl, statics, diagnostics);
            scope
                .nameless
                .push(LocallyDefinedNameless::FreeImpl(push_get_id(
                    &mut statics.freeimpls,
                    ast,
                )));
        }
        ASTDeclr::Function(mut ast) => {
            scope_function(&mut ast, statics, diagnostics);
            insert!(ast, functions, LocallyDefinedNameless::Function);
        }
        ASTDeclr::Const(mut ast) => {
            scope_const(&mut ast, scope, statics, diagnostics);
            insert!(ast, consts, LocallyDefinedNameless::Const);
        }
        ASTDeclr::Trait(mut ast) => {
            scope_trait(&mut ast, statics, diagnostics);
            insert!(ast, traits, LocallyDefinedNameless::Trait);
        }
        ASTDeclr::Data(mut ast) => {
            scope_data(&mut ast, statics, diagnostics);
            insert!(ast, datas, LocallyDefinedNameless::Data);
        }
        ASTDeclr::TypeAlias(ast) => {
            // types doesnt need to have its contents scoped there isnt anything in its contents to scope.
            insert!(ast, typealiases, LocallyDefinedNameless::TypeAlias);
        }
    }
}

fn scope_block<'src>(
    block: &mut ASTBlock<'src>,
    statics: &mut ASTStatics<'src>,
    diagnostics: &mut ResolutionDiagnostics,
) {
    scope_stmt(&mut block.body, &mut block.scope, statics, diagnostics)
}

fn scope_stmt<'src>(
    stmts: &mut Vec<ASTStmt<'src>>,
    scope: &mut LocallyScoped<'src>,
    statics: &mut ASTStatics<'src>,
    diagnostics: &mut ResolutionDiagnostics,
) {
    *stmts = stmts
        .drain(..)
        .filter_map(|stmt| {
            Some(match stmt {
                ASTStmt::StaticDeclare(declr) => {
                    register_static(*declr, scope, statics, diagnostics);
                    return None;
                }
                mut stmt => {
                    match &mut stmt {
                        ASTStmt::SubBlocked(sb) => {
                            scope_subblocked(sb, scope, statics, diagnostics)
                        }
                        ASTStmt::Expr(expr) => scope_expr(expr, scope, statics, diagnostics),
                        ASTStmt::VarAssign { lhs, rhs, .. } => {
                            scope_expr(lhs.as_mut(), scope, statics, diagnostics);
                            if let Ok(rhs) = rhs {
                                scope_expr(rhs.as_mut(), scope, statics, diagnostics);
                            }
                        }
                        ASTStmt::VarDeclare(ASTVarDeclare { initializer, .. }) => {
                            if let Some(Ok(initializer)) = initializer {
                                scope_expr(initializer.as_mut(), scope, statics, diagnostics);
                            }
                        }
                        ASTStmt::StaticDeclare(_) => unreachable!(),
                    };
                    stmt
                }
            })
        })
        .collect::<Vec<_>>();
}

fn scope_expr<'src>(
    expr: &mut ASTExpr<'src>,
    scope: &mut LocallyScoped<'src>,
    statics: &mut ASTStatics<'src>,
    diagnostics: &mut ResolutionDiagnostics,
) {
    match expr {
        ASTExpr::Lambda(lambda) => scope_lambda(lambda, scope, statics, diagnostics),
        ASTExpr::SubBlocked(sb) => scope_subblocked(sb, scope, statics, diagnostics),
        ASTExpr::PostfixBlock {
            inner,
            postfix: (postfix, _),
        } => {
            scope_expr(inner.as_mut(), scope, statics, diagnostics);
            scope_exprpostfix(postfix, scope, statics, diagnostics);
        }
        ASTExpr::OpInfix {
            inner: (lhs, rhs), ..
        } => {
            scope_expr(lhs.as_mut(), scope, statics, diagnostics);
            scope_expr(rhs.as_mut(), scope, statics, diagnostics);
        }
        ASTExpr::Array { inner, .. } => scope_expr_iter(
            inner.iter_mut().filter_map(|arg| arg.as_mut().ok()),
            scope,
            statics,
            diagnostics,
        ),

        ASTExpr::OpPrefix { inner, .. }
        | ASTExpr::OpPostfix { inner, .. }
        | ASTExpr::Parentheses {
            inner: Ok(inner), ..
        }
        | ASTExpr::Return {
            inner: Some(inner), ..
        }
        | ASTExpr::Break {
            inner: Some(inner), ..
        } => scope_expr(inner.as_mut(), scope, statics, diagnostics),

        ASTExpr::Literal { .. }
        | ASTExpr::Ident { .. }
        | ASTExpr::Parentheses { inner: Err(_), .. }
        | ASTExpr::Return { inner: None, .. }
        | ASTExpr::Break { inner: None, .. }
        | ASTExpr::Continue { .. } => {}
    }
}
fn scope_subblocked<'src>(
    sb: &mut ASTSubBlocked<'src>,
    scope: &mut LocallyScoped<'src>,
    statics: &mut ASTStatics<'src>,
    diagnostics: &mut ResolutionDiagnostics,
) {
    match sb {
        ASTSubBlocked::Block { block } => scope_block(block, statics, diagnostics),
        ASTSubBlocked::If {
            condition,
            block,
            elifs,
            else_block,
            ..
        } => {
            if let Ok(condition) = condition {
                scope_expr(condition.as_mut(), scope, statics, diagnostics);
            }
            if let Ok(block) = block {
                scope_block(block, statics, diagnostics);
            }
            for elif in elifs {
                if let (Ok(condition), _) = elif {
                    scope_expr(condition.as_mut(), scope, statics, diagnostics);
                }
            }
            if let Some(Ok(else_block)) = else_block {
                scope_block(else_block, statics, diagnostics);
            }
        }
        ASTSubBlocked::Loop { block, .. } => {
            if let Ok(block) = block {
                scope_block(block, statics, diagnostics);
            }
        }
        ASTSubBlocked::While {
            condition,
            block,
            else_block,
            ..
        } => {
            if let Ok(condition) = condition {
                scope_expr(condition.as_mut(), scope, statics, diagnostics);
            }
            if let Ok(block) = block {
                scope_block(block, statics, diagnostics);
            }
            if let Some(Ok(else_block)) = else_block {
                scope_block(else_block, statics, diagnostics);
            }
        }
        ASTSubBlocked::For {
            iterator,
            block,
            else_block,
            ..
        } => {
            if let Ok(iterator) = iterator {
                scope_expr(iterator.as_mut(), scope, statics, diagnostics);
            }
            if let Ok(block) = block {
                scope_block(block, statics, diagnostics);
            }
            if let Some(Ok(else_block)) = else_block {
                scope_block(else_block, statics, diagnostics);
            }
        }
    }
}

fn scope_exprpostfix<'src>(
    postfix: &mut ASTPostfixBlock<'src>,
    scope: &mut LocallyScoped<'src>,
    statics: &mut ASTStatics<'src>,
    diagnostics: &mut ResolutionDiagnostics,
) {
    match postfix {
        ASTPostfixBlock::Call { args } => scope_expr_iter(
            args.iter_mut().filter_map(|arg| arg.as_mut().ok()),
            scope,
            statics,
            diagnostics,
        ),
        ASTPostfixBlock::Index { args } => scope_expr_iter(
            args.iter_mut().filter_map(|arg| arg.as_mut().ok()),
            scope,
            statics,
            diagnostics,
        ),
        ASTPostfixBlock::Lambda { lambda } => scope_lambda(lambda, scope, statics, diagnostics),
        ASTPostfixBlock::DataStructInit { entries } => scope_expr_iter(
            entries.iter_mut().filter_map(|(_, ent)| ent.as_mut().ok()),
            scope,
            statics,
            diagnostics,
        ),
        ASTPostfixBlock::DataTupleInit { entries } => scope_expr_iter(
            entries.iter_mut().filter_map(|arg| arg.as_mut().ok()),
            scope,
            statics,
            diagnostics,
        ),
        ASTPostfixBlock::PropertyAccess { .. } => {}
    }
}

fn scope_expr_iter<'src: 'a, 'a>(
    exprs: impl Iterator<Item = &'a mut ASTExpr<'src>>,
    scope: &mut LocallyScoped<'src>,
    statics: &mut ASTStatics<'src>,
    diagnostics: &mut ResolutionDiagnostics,
) {
    for expr in exprs {
        scope_expr(expr, scope, statics, diagnostics);
    }
}

fn scope_lambda<'src>(
    lambda: &mut ASTLambda<'src>,
    _scope: &mut LocallyScoped<'src>,
    statics: &mut ASTStatics<'src>,
    diagnostics: &mut ResolutionDiagnostics,
) {
    scope_block(&mut lambda.block, statics, diagnostics)
}

fn scope_impl<'src>(
    impl_: &mut ASTImpl<'src>,
    statics: &mut ASTStatics<'src>,
    diagnostics: &mut ResolutionDiagnostics,
) {
    scope_impl_contents(&mut impl_.contents, statics, diagnostics)
}
fn scope_impl_contents<'src>(
    contents: &mut ASTImplContents<'src>,
    statics: &mut ASTStatics<'src>,
    diagnostics: &mut ResolutionDiagnostics,
) {
    let mut scope = LocallyScoped::new_empty();
    for const_ in &mut contents.consts {
        scope_const(const_, &mut scope, statics, diagnostics);
    }
    for function in &mut contents.functions {
        scope_function(function, statics, diagnostics);
    }
    // types doesnt need to have its contents scoped there isnt anything in its contents to scope.
}

fn scope_function<'src>(
    function: &mut ASTFunction<'src>,
    statics: &mut ASTStatics<'src>,
    diagnostics: &mut ResolutionDiagnostics,
) {
    if let Some(block) = &mut function.block {
        scope_block(block, statics, diagnostics);
    }
}

fn scope_trait<'src>(
    trait_: &mut ASTTrait<'src>,
    statics: &mut ASTStatics<'src>,
    diagnostics: &mut ResolutionDiagnostics,
) {
    scope_impl_contents(&mut trait_.contents, statics, diagnostics)
}

fn scope_data<'src>(
    data: &mut ASTData<'src>,
    statics: &mut ASTStatics<'src>,
    diagnostics: &mut ResolutionDiagnostics,
) {
    for impl_ in &mut data.attatched_impls {
        scope_impl_contents(&mut impl_.contents, statics, diagnostics);
    }
}

fn scope_const<'src>(
    const_: &mut ASTConst<'src>,
    scope: &mut LocallyScoped<'src>,
    statics: &mut ASTStatics<'src>,
    diagnostics: &mut ResolutionDiagnostics,
) {
    if let Ok(value) = &mut const_.value {
        scope_expr(value, scope, statics, diagnostics);
    }
}
