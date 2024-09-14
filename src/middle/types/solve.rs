use std::collections::HashMap;

use crate::{
    back::BackendId,
    front::ast::{ASTBlock, ASTExpr, ASTStmt, ASTSubBlocked, ASTType, ASTVarDeclare},
    lint::diagnostic::Fallible,
    middle::{
        core::Core,
        statics::{
            scopes::ScopeId,
            verify_merge::{TemplateNames, TemplateNamesRef, TraitAssociatedTyIdMap},
            FunctionGeneric, FunctionVariant, UnresolvedStatics,
        },
        types::convert_type_datalike,
    },
};

use super::{
    solve_data::{
        SolvingBlock, SolvingExpr, SolvingStmt, SolvingSubBlocked, SolvingType, SolvingVarDeclare,
    },
    StaticsInfo, TypeDatalike,
};

/*



GC'd

@inline;
fn write<T>(ptr: RefMut<T>, val: T) {
    $js_literal"`(ptr)`=`val`"
}


let x
let y

x = y    //    write( (&mo x), *(&y) )    //    x = y


*/

struct TypeSolvingFunctionBody<'src> {
    template_names_variant: Option<TemplateNames<'src>>,
    ty_locals: Vec<SolvingType>,
    body: SolvingBlock<'src>,
}

struct PrepareContext<'a, 'src> {
    scope: ScopeId,
    templates: &'a TemplateNamesRef<'src>,
    self_ty: &'a Option<TypeDatalike>,
    trait_associated_ty_id_map: &'a TraitAssociatedTyIdMap<'src>,
    statics_info: StaticsInfo<'a, 'src>,

    ty_locals: Vec<Vec<SolvingType>>,
    locals_lookup: Vec<HashMap<&'a str, usize>>,

    core: &'a Core,
}
impl<'a, 'src> PrepareContext<'a, 'src> {
    fn convert_ty(&mut self, ty: ASTType<'src>) -> Fallible<TypeDatalike> {
        convert_type_datalike(
            ty,
            self.scope,
            self.templates,
            self.self_ty.as_ref(),
            self.trait_associated_ty_id_map,
            &mut self.statics_info,
        )
    }
    fn prepare_for_solver(
        &mut self,
        statics: &mut UnresolvedStatics<'src>,
    ) -> Vec<FunctionGeneric<Option<TypeDatalike>, TypeSolvingFunctionBody<'a>>> {
        let mut functions_in = Vec::new();
        std::mem::swap(&mut statics.functions, &mut functions_in);

        let functions = functions_in
            .into_iter()
            .map(
                |FunctionGeneric {
                     annot,
                     name,
                     templates,
                     ty_args,
                     ty_return,
                     ty_self,
                     base_target,
                     variants,
                 }| {
                    FunctionGeneric {
                        annot,
                        name,
                        templates,
                        ty_args,
                        ty_return,
                        ty_self,
                        base_target,
                        variants: variants
                            .into_iter()
                            .map(|(backend_id, variant)| {
                                (
                                    backend_id,
                                    self.prepare_function_variant(backend_id, variant),
                                )
                            })
                            .collect(),
                    }
                },
            )
            .collect::<Vec<_>>();

        return functions;
    }
    fn prepare_function_variant(
        &mut self,
        backend_id: BackendId,
        FunctionVariant {
            loc,
            args,
            body: (body, template_names_variant),
        }: FunctionVariant<(ASTBlock<'src>, Option<TemplateNames<'src>>)>,
    ) -> FunctionVariant<TypeSolvingFunctionBody<'a>> {
        self.locals_lookup.clear();
        self.ty_locals.push(Vec::new());
        let body = self.prepare_block(body);
        let ty_locals = self.ty_locals.pop().unwrap();
        FunctionVariant {
            loc,
            args,
            body: TypeSolvingFunctionBody {
                template_names_variant,
                body,
                ty_locals,
            },
        }
    }
    fn prepare_block(&mut self, block: ASTBlock<'src>) -> SolvingBlock<'src> {
        self.guard_scope(|ctx| {
            todo!();
        })
    }
    fn guard_scope<T>(&mut self, mut f: impl FnMut(&mut Self) -> T) -> T {
        let scope_outer = self.scope;
        self.locals_lookup.push(HashMap::new());

        let res = f(self);

        self.scope = scope_outer;
        self.locals_lookup.pop();

        res
    }

    fn prepare_stmt(&mut self, stmt: ASTStmt<'src>) -> SolvingStmt<'src> {
        match stmt {
            ASTStmt::Expr(expr) => SolvingStmt::Expr(self.prepare_expr(expr)),
            ASTStmt::SubBlocked(sb) => SolvingStmt::SubBlocked(self.prepare_subblocked(sb)),
            ASTStmt::VarAssign {
                loc,
                assign_op,
                lhs,
                rhs,
            } => {
                dbg!("todo! pointers/references");
                SolvingStmt::VarAssign {
                    loc,
                    assign_op,
                    lhs: Box::new(self.prepare_expr(*lhs)),
                    rhs: rhs.map(|rhs| Box::new(self.prepare_expr(*rhs))),
                }
            }
            ASTStmt::VarDeclare(var_declare) => {
                SolvingStmt::VarDeclare(self.prepare_var_declare(var_declare))
            }
            ASTStmt::StaticDeclare(_) => unreachable!(),
        }
    }
    fn prepare_var_declare(&mut self, var_declare: ASTVarDeclare<'src>) -> SolvingVarDeclare<'src> {
        // var_declare.declr.unwrap()
        todo!()
    }
    fn prepare_expr(&mut self, expr: ASTExpr<'src>) -> SolvingExpr<'src> {
        todo!()
    }
    fn prepare_subblocked(&mut self, sb: ASTSubBlocked<'src>) -> SolvingSubBlocked<'src> {
        todo!()
    }
}
