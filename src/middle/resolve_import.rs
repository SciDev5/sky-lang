//! This module is responsible for adding the outputs from import statments like `import a.b.c` into
//! the scoped name to id mappings.
//!
//!

// Weird things:
// - need reexports
//

use std::collections::HashMap;

use crate::{
    back::BackendId,
    front::{
        ast::{ASTIdent, ASTIdentValue, ASTImportTree, ASTScope},
        source::Loc,
    },
    lint::diagnostic::{DiagnosticContent, Diagnostics, ToDiagnostic},
    modularity::Id,
};

use super::statics::{merge::MergedStatics, module::ModuleExports, scopes::Scopes};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ImportDiagnostic {
    IllegalRoot,
    IllegalSelf,
    IllegalSuper,
    DuplicateName,
    DuplicateSelfInListImport,
    IllegalInitial,
    WrongSelfKeyIdent,
    NoSuperAvailable,
    NameForImportNotFound,
    NameMemberForImportNotFound,
    ImportDependencyFailed,
    RecursiveProblem,
}
impl ToDiagnostic for ImportDiagnostic {
    fn to_content(self) -> DiagnosticContent {
        DiagnosticContent::Import(self)
    }
}

struct Import<'src> {
    backend_id: BackendId,
    import_tree: ImportTree<'src>,
    base: ImportingId,
}
struct ImportTree<'src> {
    loc: Loc,
    which: ImportTreeWhich<'src>,
}
enum ImportTreeWhich<'src> {
    /// `.abc`
    Name {
        ident: &'src str,
        dep_id: ImportDependencyId,
    },
    /// `.abc.<...>`
    NameWithChild {
        ident: &'src str,
        inner: Box<ImportTree<'src>>,
    },
    /// `.[ ... ]`
    Group {
        export_self: Option<ImportDependencyId>,
        inner: Vec<ImportTree<'src>>,
    },
    /// `.*`
    ToAll,
}

struct ImportingScope<'src> {
    backend_id: BackendId,
    named_items: HashMap<&'src str, ImportingId>,
}
#[derive(Debug, Clone, PartialEq)]
pub struct ImportedScope<'src> {
    pub backend_id: BackendId,
    pub functions: HashMap<&'src str, Id>,
    pub datas: HashMap<&'src str, Id>,
    pub traits: HashMap<&'src str, Id>,
    pub consts: HashMap<&'src str, Id>,
    pub typealiases: HashMap<&'src str, Id>,
    pub modules: HashMap<&'src str, Id>,
}

type ImportDependencyId = usize;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ImportingId {
    Module(Id),
    Function(Id),
    Data(Id),
    Trait(Id),
    Const(Id),
    TypeAlias(Id),
    Import(ImportDependencyId),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ImportDependencyResolutionStatus {
    NotResolved,
    Failed,
    Succeeded(ImportingId),
}
impl ImportDependencyResolutionStatus {
    fn is_resolved(self) -> bool {
        self != Self::NotResolved
    }
    fn succeeded(self) -> Option<ImportingId> {
        match self {
            Self::Succeeded(id) => Some(id),
            _ => None,
        }
    }
}

fn convert_ast_importtree<'src>(
    ast: ASTImportTree<'src>,
    backend_id: usize,
    diagnostics: &mut Diagnostics,
    named_items: &mut HashMap<&'src str, ImportingId>,
    import_dependency_resolved: &mut Vec<ImportDependencyResolutionStatus>,
) -> Option<ImportTree<'src>> {
    fn define_import_dependency<'src>(
        name: &'src str,
        loc: Loc,
        named_items: &mut HashMap<&'src str, ImportingId>,
        import_dependency_resolved: &mut Vec<ImportDependencyResolutionStatus>,
        diagnostics: &mut Diagnostics,
    ) -> ImportDependencyId {
        let id = import_dependency_resolved.len();
        import_dependency_resolved.push(ImportDependencyResolutionStatus::NotResolved);
        named_items
            .entry(name)
            .and_modify(|_| diagnostics.raise(ImportDiagnostic::DuplicateName, loc))
            .or_insert(ImportingId::Import(id));
        id
    }

    Some(match ast {
        ASTImportTree::Name {
            loc,
            ident:
                ASTIdent {
                    value: ASTIdentValue::Name(ident),
                    ..
                },
            inner: None,
        } => ImportTree {
            loc,
            which: ImportTreeWhich::Name {
                ident,
                dep_id: define_import_dependency(
                    ident,
                    loc,
                    named_items,
                    import_dependency_resolved,
                    diagnostics,
                ),
            },
        },
        ASTImportTree::ToAll(loc) => ImportTree {
            loc,
            which: ImportTreeWhich::ToAll,
        },
        ASTImportTree::Name {
            loc,
            ident:
                ASTIdent {
                    value: ASTIdentValue::Name(ident),
                    ..
                },
            inner: Some(inner),
        } => ImportTree {
            loc,
            which: ImportTreeWhich::NameWithChild {
                ident,
                inner: Box::new(convert_ast_importtree(
                    *inner.ok()?,
                    backend_id,
                    diagnostics,
                    named_items,
                    import_dependency_resolved,
                )?),
            },
        },
        ASTImportTree::Group {
            loc,
            ident:
                ASTIdent {
                    value: ASTIdentValue::Name(ident),
                    ..
                },
            inner,
        } => {
            let mut export_self = None;
            let inner = inner
                .into_iter()
                .filter_map(|ast| match ast {
                    ASTImportTree::Name {
                        loc,
                        ident:
                            ASTIdent {
                                value: ASTIdentValue::SelfVar,
                                ..
                            },
                        inner: None,
                    } => {
                        if export_self.is_none() {
                            export_self = Some(define_import_dependency(
                                ident,
                                loc,
                                named_items,
                                import_dependency_resolved,
                                diagnostics,
                            ));
                        } else {
                            diagnostics.raise(ImportDiagnostic::DuplicateSelfInListImport, loc);
                            return None;
                        }
                        None
                    }
                    ast => convert_ast_importtree(
                        ast,
                        backend_id,
                        diagnostics,
                        named_items,
                        import_dependency_resolved,
                    ),
                })
                .collect::<Vec<_>>();

            ImportTree {
                loc,
                which: ImportTreeWhich::NameWithChild {
                    ident,
                    inner: Box::new(ImportTree {
                        loc,
                        which: ImportTreeWhich::Group { export_self, inner },
                    }),
                },
            }
        }
        ASTImportTree::Name { loc, ident, .. } | ASTImportTree::Group { loc, ident, .. } => {
            match ident.value {
                ASTIdentValue::Root => diagnostics.raise(ImportDiagnostic::IllegalRoot, loc),
                ASTIdentValue::SelfTy | ASTIdentValue::SelfVar => {
                    diagnostics.raise(ImportDiagnostic::IllegalSelf, loc)
                }
                ASTIdentValue::Super => diagnostics.raise(ImportDiagnostic::IllegalSuper, loc),
                ASTIdentValue::Name(_) => unreachable!(),
            }
            return None;
        }
    })
}

enum NameOrModuleId<'src> {
    Name(&'src str),
    ModuleId(usize),
}

fn convert_ast_importtree_base<'src>(
    ast: ASTImportTree<'src>,
    backend_id: usize,
    module_id: usize,
    module_exports: &Vec<ModuleExports<'src>>,
    named_items: &mut HashMap<&'src str, ImportingId>,
    diagnostics: &mut Diagnostics,
    import_dependency_resolved: &mut Vec<ImportDependencyResolutionStatus>,
) -> Option<(NameOrModuleId<'src>, ImportTree<'src>)> {
    match ast {
        ASTImportTree::ToAll(loc)
        | ASTImportTree::Group { loc, .. }
        | ASTImportTree::Name {
            loc, inner: None, ..
        } => {
            diagnostics.raise(ImportDiagnostic::IllegalInitial, loc);
            return None;
        }
        ASTImportTree::Name {
            loc,
            ident,
            inner: Some(inner),
        } => {
            let mut inner = *inner.ok()?;
            let mut base = match ident.value {
                ASTIdentValue::SelfTy => {
                    diagnostics.raise(ImportDiagnostic::WrongSelfKeyIdent, loc);
                    return None;
                }
                ASTIdentValue::Root => NameOrModuleId::ModuleId(0), // 0 is always root
                ASTIdentValue::Super => {
                    if let Some(parent_id) = module_exports[module_id].parent {
                        NameOrModuleId::ModuleId(parent_id)
                    } else {
                        diagnostics.raise(ImportDiagnostic::NoSuperAvailable, loc);
                        return None;
                    }
                }
                ASTIdentValue::SelfVar => NameOrModuleId::ModuleId(module_id),
                ASTIdentValue::Name(name) => NameOrModuleId::Name(name),
            };

            loop {
                match (inner, base) {
                    (
                        ASTImportTree::Name {
                            loc,
                            ident:
                                ASTIdent {
                                    value: ASTIdentValue::Super,
                                    ..
                                },
                            inner: Some(inner_),
                        },
                        NameOrModuleId::ModuleId(module_id),
                    ) => {
                        inner = *inner_.ok()?;
                        if let Some(parent_id) = module_exports[module_id].parent {
                            base = NameOrModuleId::ModuleId(parent_id);
                        } else {
                            diagnostics.raise(ImportDiagnostic::NoSuperAvailable, loc);
                            return None;
                        }
                    }
                    (inner_, base_) => {
                        inner = inner_;
                        base = base_;
                        break;
                    }
                }
            }

            convert_ast_importtree(
                inner,
                backend_id,
                diagnostics,
                named_items,
                import_dependency_resolved,
            )
            .map(|import_tree| (base, import_tree))
        }
    }
}

fn lookup_by_name<'src>(
    named_items: &HashMap<&'src str, ImportingId>,
    parent_scopes: &[ImportingScope],
    name: &'src str,
) -> Option<ImportingId> {
    if let Some(r) = named_items.get(name) {
        return Some(*r);
    }
    parent_scopes
        .iter()
        .find_map(|scope| scope.named_items.get(name))
        .copied()
}

fn lookup_by_name_in<'src>(
    statics: &MergedStatics<'src>,
    module_exports: &Vec<ModuleExports<'src>>,
    import_dependency_resolved: &Vec<ImportDependencyResolutionStatus>,
    base: ImportingId,
    name: &'src str,
    loc: Loc,
    diagnostics: &mut Diagnostics,
) -> Option<Result<ImportingId, ()>> {
    let base = match base {
        ImportingId::Import(import_dep_id) => {
            let import_depenency = import_dependency_resolved[import_dep_id];
            if import_depenency.is_resolved() {
                match import_depenency.succeeded() {
                    Some(base) => base,
                    None => {
                        diagnostics.raise(ImportDiagnostic::ImportDependencyFailed, loc);
                        return Some(Err(()));
                    }
                }
            } else {
                return None;
            }
        }
        v => v,
    };
    Some(match base {
        ImportingId::Module(Id::Local { id: module_id }) => {
            let exports = &module_exports[module_id];

            let module_ = exports
                .children
                .get(name)
                .copied()
                .map(|id| ImportingId::Module(Id::Local { id }));
            let data_ = exports.datas.get(name).copied().map(ImportingId::Data);
            let trait_ = exports.traits.get(name).copied().map(ImportingId::Trait);
            let const_ = exports.consts.get(name).copied().map(ImportingId::Const);
            let typealias_ = exports
                .typealiases
                .get(name)
                .copied()
                .map(ImportingId::TypeAlias);
            let function_ = exports
                .functions
                .get(name)
                .copied()
                .map(ImportingId::Function);

            if let Some(id) = module_
                .or(data_)
                .or(trait_)
                .or(const_)
                .or(typealias_)
                .or(function_)
            {
                Ok(id)
            } else {
                diagnostics.raise(ImportDiagnostic::NameMemberForImportNotFound, loc);
                Err(())
            }
        }
        ImportingId::Module(Id::Dependency { package_id, id }) => {
            todo!("imports from dependencies");
        }
        _ => {
            diagnostics.raise(ImportDiagnostic::NameMemberForImportNotFound, loc);
            Err(())
        }
    })
}

fn fail_import_recursive<'src>(
    tree: ImportTree<'src>,
    import_dependency_resolved: &mut Vec<ImportDependencyResolutionStatus>,
) {
    match tree.which {
        ImportTreeWhich::Name { dep_id, .. } => {
            import_dependency_resolved[dep_id] = ImportDependencyResolutionStatus::Failed
        }
        ImportTreeWhich::ToAll => {}
        ImportTreeWhich::Group { export_self, inner } => {
            if let Some(export_self) = export_self {
                import_dependency_resolved[export_self] = ImportDependencyResolutionStatus::Failed;
            }
            for subtree in inner {
                fail_import_recursive(subtree, import_dependency_resolved);
            }
        }
        ImportTreeWhich::NameWithChild { inner, .. } => {
            fail_import_recursive(*inner, import_dependency_resolved)
        }
    }
}

pub fn resolve_imports<'src>(
    scopes: Scopes<ASTScope<'src>>,
    statics: &MergedStatics<'src>,
    module_exports: &Vec<ModuleExports<'src>>,
    diagnostics: &mut Diagnostics,
) -> Scopes<ImportedScope<'src>> {
    let moduleid_by_scopeid = module_exports
        .iter()
        .enumerate()
        .flat_map(|(i, exports)| {
            exports
                .sources
                .iter()
                .zip(std::iter::repeat(i))
                .map(|((_, src), i)| (src.scope, i))
        })
        .collect::<HashMap<_, _>>();

    let mut import_dependency_resolved = Vec::new();

    let mut unfinished_imports = Vec::new();

    let scopes = scopes.map_contextual(
        |scope, module_id, parent_scopes| {
            let mut named_items = HashMap::new();

            macro_rules! add_to_named_items {
                ($x:ident, $importing_id_ty:ident) => {
                    for (name, id) in scope.$x {
                        let didnt_replace = named_items
                            .insert(name, ImportingId::$importing_id_ty(id))
                            .is_none();
                        assert!(didnt_replace);
                    }
                };
            }
            add_to_named_items!(functions, Function);
            add_to_named_items!(datas, Data);
            add_to_named_items!(traits, Trait);
            add_to_named_items!(consts, Const);
            add_to_named_items!(typealiases, TypeAlias);

            for (name, child_module_id) in &module_exports[*module_id].children {
                let didnt_replace = named_items
                    .insert(
                        *name,
                        ImportingId::Module(Id::Local {
                            id: *child_module_id,
                        }),
                    )
                    .is_none();
                assert!(didnt_replace);
            }

            let mut imports = Vec::new();
            for import in scope.imports {
                let import = convert_ast_importtree_base(
                    import,
                    scope.backend_id,
                    *module_id,
                    module_exports,
                    &mut named_items,
                    diagnostics,
                    &mut import_dependency_resolved,
                );
                if let Some(import) = import {
                    imports.push(import);
                }
            }

            unfinished_imports.extend(imports.into_iter().filter_map(
                |(name_or_moduleid, import_tree)| {
                    let base = match name_or_moduleid {
                        NameOrModuleId::ModuleId(module_id) => {
                            ImportingId::Module(Id::Local { id: module_id })
                        }
                        NameOrModuleId::Name(name) => {
                            if let Some(id) = lookup_by_name(&mut named_items, parent_scopes, name)
                            {
                                id
                            } else {
                                diagnostics.raise(
                                    ImportDiagnostic::NameForImportNotFound,
                                    import_tree.loc,
                                );
                                fail_import_recursive(import_tree, &mut import_dependency_resolved);
                                return None;
                            }
                        }
                    };
                    Some(Import {
                        backend_id: scope.backend_id,
                        import_tree,
                        base,
                    })
                },
            ));

            ImportingScope {
                named_items,
                backend_id: scope.backend_id,
            }
        },
        &moduleid_by_scopeid,
    );

    loop {
        let mut changed = false;
        let mut new = Vec::with_capacity(unfinished_imports.len());

        for import in unfinished_imports {
            let Import {
                backend_id,
                import_tree,
                base,
            } = import;
            let loc = import_tree.loc;
            match import_tree.which {
                ImportTreeWhich::ToAll => {
                    todo!("- hevent done -------------------------------------------")
                }
                ImportTreeWhich::Group { export_self, inner } => {
                    // solved groups get split up into pieces
                    changed = true;

                    if let Some(export_self_id) = export_self {
                        import_dependency_resolved[export_self_id] =
                            ImportDependencyResolutionStatus::Succeeded(base);
                    }

                    for import_tree in inner {
                        new.push(Import {
                            backend_id,
                            import_tree,
                            base,
                        });
                    }
                }
                ImportTreeWhich::Name { ident, dep_id } => {
                    if let Some(r) = lookup_by_name_in(
                        statics,
                        module_exports,
                        &import_dependency_resolved,
                        base,
                        ident,
                        loc,
                        diagnostics,
                    ) {
                        import_dependency_resolved[dep_id] = match r {
                            Ok(id) => {
                                // resolved, mark as such
                                ImportDependencyResolutionStatus::Succeeded(id)
                            }
                            Err(_) => {
                                // failed (so close!)
                                diagnostics
                                    .raise(ImportDiagnostic::NameMemberForImportNotFound, loc);
                                ImportDependencyResolutionStatus::Failed
                            }
                        }
                    } else {
                        // put it back unchanged
                        new.push(Import {
                            backend_id,
                            import_tree: ImportTree {
                                loc,
                                which: ImportTreeWhich::Name { ident, dep_id },
                            },
                            base,
                        });
                    }
                }
                ImportTreeWhich::NameWithChild { ident, inner } => {
                    if let Some(r) = lookup_by_name_in(
                        statics,
                        module_exports,
                        &import_dependency_resolved,
                        base,
                        ident,
                        loc,
                        diagnostics,
                    ) {
                        match r {
                            Ok(new_base) => {
                                // resolved, mark as such
                                new.push(Import {
                                    backend_id,
                                    import_tree: *inner,
                                    base: new_base,
                                });
                            }
                            Err(_) => {
                                // failed recursively mark as failed
                                diagnostics
                                    .raise(ImportDiagnostic::NameMemberForImportNotFound, loc);
                                fail_import_recursive(*inner, &mut import_dependency_resolved);
                            }
                        }
                    } else {
                        // put it back unchanged
                        new.push(Import {
                            backend_id,
                            import_tree: ImportTree {
                                loc,
                                which: ImportTreeWhich::NameWithChild { ident, inner },
                            },
                            base,
                        });
                    }
                }
            }
        }

        unfinished_imports = new;

        if changed == false {
            break;
        }
    }

    if unfinished_imports.len() > 0 {
        for import in unfinished_imports {
            diagnostics.raise(ImportDiagnostic::RecursiveProblem, import.import_tree.loc);
            fail_import_recursive(import.import_tree, &mut import_dependency_resolved);
        }
    }

    scopes.map(
        |ImportingScope {
             backend_id,
             named_items,
         }| {
            let mut scope = ImportedScope {
                backend_id,
                functions: HashMap::new(),
                datas: HashMap::new(),
                traits: HashMap::new(),
                consts: HashMap::new(),
                typealiases: HashMap::new(),
                modules: HashMap::new(),
            };
            for (name, id) in named_items {
                let id = match id {
                    ImportingId::Import(dep_id) => {
                        if let Some(id) = import_dependency_resolved[dep_id].succeeded() {
                            id
                        } else {
                            continue;
                        }
                    }
                    id => id,
                };
                let (map, id) = match id {
                    ImportingId::Function(id) => (&mut scope.functions, id),
                    ImportingId::Data(id) => (&mut scope.datas, id),
                    ImportingId::Trait(id) => (&mut scope.traits, id),
                    ImportingId::Const(id) => (&mut scope.consts, id),
                    ImportingId::TypeAlias(id) => (&mut scope.typealiases, id),
                    ImportingId::Module(id) => (&mut scope.modules, id),
                    ImportingId::Import(_) => unreachable!(),
                };
                map.insert(name, id);
            }
            scope
        },
    )
}
