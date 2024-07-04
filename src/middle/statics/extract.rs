use std::collections::HashMap;

use crate::{
    front::{
        ast::{
            ASTBlock, ASTConst, ASTData, ASTDeclr, ASTExpr, ASTFunction, ASTImpl, ASTImplContents,
            ASTLambda, ASTName, ASTPostfixBlock, ASTScope, ASTSourceFile, ASTStatics, ASTStmt,
            ASTSubBlocked, ASTTrait, ASTVarDeclare,
        },
        source::HasLoc,
    },
    lint::diagnostic::{Diagnostics, Fallible},
    middle::resolution_diagnostics::ResolutionDiagnostic,
    modularity::Id,
};

use super::scopes::{ScopeId, Scopes};

struct ScopeExtractor<'a, 'src> {
    scopes: &'a mut Scopes<ASTScope<'src>>,
    statics: &'a mut ASTStatics<'src>,
    diagnostics: &'a mut Diagnostics,
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
pub fn scope_extract_src<'src>(
    src: &mut ASTSourceFile<'src>,
    scopes: &mut Scopes<ASTScope<'src>>,
    statics: &mut ASTStatics<'src>,
    diagnostics: &mut Diagnostics,
) {
    ScopeExtractor {
        scopes,
        statics,
        diagnostics,
    }
    .scope_src(src)
}

impl<'a, 'src> ScopeExtractor<'a, 'src> {
    pub fn scope_src(&mut self, src: &mut ASTSourceFile<'src>) {
        self.scope_stmt(&mut src.body, src.scope)
    }

    /// Register this static into the list of statics and add its id to the local scope.
    /// (This will additionally run the `scope_<whatever>` on the contents of the declarations,
    /// such as associated function bodies).
    fn register_static(&mut self, declr: ASTDeclr<'src>, scope: ScopeId) {
        fn push_get_id<T>(target: &mut Vec<T>, value: T) -> Id {
            let id = target.len();
            target.push(value);
            Id::Local { id }
        }
        fn insert<'src, T: HasLoc>(
            name: Fallible<ASTName<'src>>,
            value: T,
            scope: &mut HashMap<&'src str, Id>,
            statics: &mut Vec<T>,
            diagnostics: &mut Diagnostics,
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
                // missing name
                // already handled this as a parser error
            }
        }
        macro_rules! insert {
            ($ast:expr, $k:ident) => {
                insert(
                    $ast.name,
                    $ast,
                    &mut self.scopes.get_or_insert(scope, ASTScope::new_empty()).$k,
                    &mut self.statics.$k,
                    self.diagnostics,
                )
            };
        }
        match declr {
            ASTDeclr::Import(ast_import) => {
                if let Ok(import_tree) = ast_import.tree {
                    self.scopes
                        .get_or_insert(scope, ASTScope::new_empty())
                        .imports
                        .push(import_tree);
                } else {
                    // TODO maybe throw the error? for now, too lazy
                }
            }
            ASTDeclr::FreeImpl(mut ast) => {
                self.scope_impl(&mut ast.attatched_impl, scope);
                self.statics.free_impls.push(ast);
            }
            ASTDeclr::Function(mut ast) => {
                self.scope_function(&mut ast);
                insert!(ast, functions);
            }
            ASTDeclr::Const(mut ast) => {
                self.scope_const(&mut ast, scope);
                insert!(ast, consts);
            }
            ASTDeclr::Trait(mut ast) => {
                self.scope_trait(&mut ast);
                insert!(ast, traits);
            }
            ASTDeclr::Data(mut ast) => {
                self.scope_data(&mut ast);
                insert!(ast, datas);
            }
            ASTDeclr::TypeAlias(ast) => {
                // types doesnt need to have its contents scoped there isnt anything in its contents to scope.
                insert!(ast, typealiases);
            }
        }
    }

    fn scope_block(&mut self, block: &mut ASTBlock<'src>) {
        self.scope_stmt(&mut block.body, block.scope)
    }

    fn scope_stmt(&mut self, stmts: &mut Vec<ASTStmt<'src>>, scope: ScopeId) {
        *stmts = stmts
            .drain(..)
            .filter_map(|stmt| {
                Some(match stmt {
                    ASTStmt::StaticDeclare(declr) => {
                        self.register_static(*declr, scope);
                        return None;
                    }
                    mut stmt => {
                        match &mut stmt {
                            ASTStmt::SubBlocked(sb) => self.scope_subblocked(sb, scope),
                            ASTStmt::Expr(expr) => self.scope_expr(expr, scope),
                            ASTStmt::VarAssign { lhs, rhs, .. } => {
                                self.scope_expr(lhs.as_mut(), scope);
                                if let Ok(rhs) = rhs {
                                    self.scope_expr(rhs.as_mut(), scope);
                                }
                            }
                            ASTStmt::VarDeclare(ASTVarDeclare { initializer, .. }) => {
                                if let Some(Ok(initializer)) = initializer {
                                    self.scope_expr(initializer.as_mut(), scope);
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

    fn scope_expr(&mut self, expr: &mut ASTExpr<'src>, scope: ScopeId) {
        match expr {
            ASTExpr::Lambda(lambda) => self.scope_lambda(lambda, scope),
            ASTExpr::SubBlocked(sb) => self.scope_subblocked(sb, scope),
            ASTExpr::PostfixBlock {
                inner,
                postfix: (postfix, _),
            } => {
                self.scope_expr(inner.as_mut(), scope);
                self.scope_exprpostfix(postfix, scope);
            }
            ASTExpr::OpInfix {
                inner: (lhs, rhs), ..
            } => {
                self.scope_expr(lhs.as_mut(), scope);
                self.scope_expr(rhs.as_mut(), scope);
            }
            ASTExpr::Array { inner, .. } => {
                self.scope_expr_iter(inner.iter_mut().filter_map(|arg| arg.as_mut().ok()), scope)
            }

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
            } => self.scope_expr(inner.as_mut(), scope),

            ASTExpr::Literal { .. }
            | ASTExpr::Ident { .. }
            | ASTExpr::Parentheses { inner: Err(_), .. }
            | ASTExpr::Return { inner: None, .. }
            | ASTExpr::Break { inner: None, .. }
            | ASTExpr::Continue { .. } => {}
        }
    }
    fn scope_subblocked(&mut self, sb: &mut ASTSubBlocked<'src>, scope: ScopeId) {
        match sb {
            ASTSubBlocked::Block { block } => self.scope_block(block),
            ASTSubBlocked::If {
                condition,
                block,
                elifs,
                else_block,
                ..
            } => {
                if let Ok(condition) = condition {
                    self.scope_expr(condition.as_mut(), scope);
                }
                if let Ok(block) = block {
                    self.scope_block(block);
                }
                for elif in elifs {
                    if let (Ok(condition), _) = elif {
                        self.scope_expr(condition.as_mut(), scope);
                    }
                }
                if let Some(Ok(else_block)) = else_block {
                    self.scope_block(else_block);
                }
            }
            ASTSubBlocked::Loop { block, .. } => {
                if let Ok(block) = block {
                    self.scope_block(block);
                }
            }
            ASTSubBlocked::While {
                condition,
                block,
                else_block,
                ..
            } => {
                if let Ok(condition) = condition {
                    self.scope_expr(condition.as_mut(), scope);
                }
                if let Ok(block) = block {
                    self.scope_block(block);
                }
                if let Some(Ok(else_block)) = else_block {
                    self.scope_block(else_block);
                }
            }
            ASTSubBlocked::For {
                iterator,
                block,
                else_block,
                ..
            } => {
                if let Ok(iterator) = iterator {
                    self.scope_expr(iterator.as_mut(), scope);
                }
                if let Ok(block) = block {
                    self.scope_block(block);
                }
                if let Some(Ok(else_block)) = else_block {
                    self.scope_block(else_block);
                }
            }
        }
    }

    fn scope_exprpostfix(&mut self, postfix: &mut ASTPostfixBlock<'src>, scope: ScopeId) {
        match postfix {
            ASTPostfixBlock::Call { args } => {
                self.scope_expr_iter(args.iter_mut().filter_map(|arg| arg.as_mut().ok()), scope)
            }
            ASTPostfixBlock::Index { args } => {
                self.scope_expr_iter(args.iter_mut().filter_map(|arg| arg.as_mut().ok()), scope)
            }
            ASTPostfixBlock::Lambda { lambda } => self.scope_lambda(lambda, scope),
            ASTPostfixBlock::DataStructInit { entries } => self.scope_expr_iter(
                entries.iter_mut().filter_map(|(_, ent)| ent.as_mut().ok()),
                scope,
            ),
            ASTPostfixBlock::DataTupleInit { entries } => self.scope_expr_iter(
                entries.iter_mut().filter_map(|arg| arg.as_mut().ok()),
                scope,
            ),
            ASTPostfixBlock::PropertyAccess { .. } => {}
        }
    }

    fn scope_expr_iter<'b>(
        &mut self,
        exprs: impl Iterator<Item = &'b mut ASTExpr<'src>>,
        scope: ScopeId,
    ) where
        'src: 'b,
    {
        for expr in exprs {
            self.scope_expr(expr, scope);
        }
    }

    fn scope_lambda(&mut self, lambda: &mut ASTLambda<'src>, _scope: ScopeId) {
        self.scope_block(&mut lambda.block)
    }

    fn scope_impl(&mut self, impl_: &mut ASTImpl<'src>, scope: ScopeId) {
        self.scope_impl_contents(&mut impl_.contents, scope)
    }
    fn scope_impl_contents(&mut self, contents: &mut ASTImplContents<'src>, scope: ScopeId) {
        for const_ in &mut contents.consts {
            self.scope_const(const_, scope);
        }
        for function in &mut contents.functions {
            self.scope_function(function);
        }
        // types doesnt need to have its contents scoped there isnt anything in its contents to scope.
    }

    fn scope_function(&mut self, function: &mut ASTFunction<'src>) {
        if let Some(block) = &mut function.block {
            self.scope_block(block);
        }
    }

    fn scope_trait(&mut self, trait_: &mut ASTTrait<'src>) {
        for function in &mut trait_.functions {
            self.scope_function(function);
        }
        // both types and consts dont need to have their contents scoped there isnt anything in their contents to scope.
    }

    fn scope_data(&mut self, data: &mut ASTData<'src>) {
        for impl_ in &mut data.attatched_impls {
            self.scope_impl_contents(&mut impl_.contents, data.containing_scope);
        }
    }

    fn scope_const(&mut self, const_: &mut ASTConst<'src>, scope: ScopeId) {
        if let Some(Ok(value)) = &mut const_.value {
            self.scope_expr(value, scope);
        }
    }
}
