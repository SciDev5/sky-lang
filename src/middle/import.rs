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
    lint::diagnostic::{DiagnosticContent, Diagnostics, Fallible, ToDiagnostic},
    modularity::Id,
};

use super::{
    module::{LocalModule, ModuleExports},
    statics::scopes::{ScopeId, Scopes},
};

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
    ExpectedDataFoundModule,
    ExpectedDataOrModule,
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
    mod_id: usize,
    named_items: HashMap<&'src str, ImportingId>,
}
#[derive(Debug, Clone, PartialEq)]
pub struct ImportedScope<'src> {
    pub backend_id: BackendId,
    pub mod_id: usize,
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
) -> Fallible<ImportTree<'src>> {
    fn define_import_dependency<'src>(
        name: &'src str,
        loc: Loc,
        named_items: &mut HashMap<&'src str, ImportingId>,
        import_dependency_resolved: &mut Vec<ImportDependencyResolutionStatus>,
        diagnostics: &mut Diagnostics,
    ) -> ImportDependencyId {
        println!("todo! no reexports problem also here -------------------------------------");

        let id = import_dependency_resolved.len();
        import_dependency_resolved.push(ImportDependencyResolutionStatus::NotResolved);
        named_items
            .entry(name)
            .and_modify(|_| {
                diagnostics.raise(ImportDiagnostic::DuplicateName, loc);
            })
            .or_insert(ImportingId::Import(id));
        id
    }

    Ok(match ast {
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
                    *inner?,
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
                            let _ =
                                diagnostics.raise(ImportDiagnostic::DuplicateSelfInListImport, loc);
                        }
                        None
                    }
                    ast => convert_ast_importtree(
                        ast,
                        backend_id,
                        diagnostics,
                        named_items,
                        import_dependency_resolved,
                    )
                    .ok(),
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
            return Err(match ident.value {
                ASTIdentValue::Root => diagnostics.raise(ImportDiagnostic::IllegalRoot, loc),
                ASTIdentValue::SelfTy | ASTIdentValue::SelfVar => {
                    diagnostics.raise(ImportDiagnostic::IllegalSelf, loc)
                }
                ASTIdentValue::Super => diagnostics.raise(ImportDiagnostic::IllegalSuper, loc),
                ASTIdentValue::Name(_) => unreachable!(),
            });
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
    module_exports: &Vec<LocalModule<'src>>,
    named_items: &mut HashMap<&'src str, ImportingId>,
    diagnostics: &mut Diagnostics,
    import_dependency_resolved: &mut Vec<ImportDependencyResolutionStatus>,
) -> Fallible<(NameOrModuleId<'src>, ImportTree<'src>)> {
    match ast {
        ASTImportTree::ToAll(loc)
        | ASTImportTree::Group { loc, .. }
        | ASTImportTree::Name {
            loc, inner: None, ..
        } => {
            return Err(diagnostics.raise(ImportDiagnostic::IllegalInitial, loc));
        }
        ASTImportTree::Name {
            loc,
            ident,
            inner: Some(inner),
        } => {
            let mut inner = *inner?;
            let mut base = match ident.value {
                ASTIdentValue::SelfTy => {
                    return Err(diagnostics.raise(ImportDiagnostic::WrongSelfKeyIdent, loc));
                }
                ASTIdentValue::Root => NameOrModuleId::ModuleId(0), // 0 is always root
                ASTIdentValue::Super => {
                    if let Some(parent_id) = module_exports[module_id].parent {
                        NameOrModuleId::ModuleId(parent_id)
                    } else {
                        return Err(diagnostics.raise(ImportDiagnostic::NoSuperAvailable, loc));
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
                        inner = *inner_?;
                        if let Some(parent_id) = module_exports[module_id].parent {
                            base = NameOrModuleId::ModuleId(parent_id);
                        } else {
                            return Err(diagnostics.raise(ImportDiagnostic::NoSuperAvailable, loc));
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

// todo! something might be wrong here with importing reexports?
fn lookup_by_name_in<'src>(
    module_exports: &Vec<LocalModule<'src>>,
    import_dependency_resolved: &Vec<ImportDependencyResolutionStatus>,
    base: ImportingId,
    name: &'src str,
    loc: Loc,
    diagnostics: &mut Diagnostics,
) -> Option<Fallible<ImportingId>> {
    let base = match base {
        ImportingId::Import(import_dep_id) => {
            let import_depenency = import_dependency_resolved[import_dep_id];
            if import_depenency.is_resolved() {
                match import_depenency.succeeded() {
                    Some(base) => base,
                    None => {
                        return Some(Err(
                            diagnostics.raise(ImportDiagnostic::ImportDependencyFailed, loc)
                        ));
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
            let exports = &module_exports[module_id].exports;

            let module_ = exports.modules.get(name).copied().map(ImportingId::Module);
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
                Err(diagnostics.raise(ImportDiagnostic::NameMemberForImportNotFound, loc))
            }
        }
        ImportingId::Module(Id::Dependency { package_id, id }) => {
            todo!("imports from dependencies");
        }
        _ => Err(diagnostics.raise(ImportDiagnostic::NameMemberForImportNotFound, loc)),
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
    modules: &mut Vec<LocalModule<'src>>,
    diagnostics: &mut Diagnostics,
) -> Scopes<ImportedScope<'src>> {
    let moduleid_by_scopeid = modules
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

            for (name, child_module_id) in &modules[*module_id].children {
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
                    modules,
                    &mut named_items,
                    diagnostics,
                    &mut import_dependency_resolved,
                );
                if let Ok(import) = import {
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
                mod_id: scope.mod_id,
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
                        modules,
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
                        modules,
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
             mod_id,
             named_items,
         }| {
            let mut scope = ImportedScope {
                backend_id,
                mod_id,
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

// todo! public/private
pub struct ExportsLookup<'src> {
    local: Vec<ModuleExports<'src>>,
    // todo! dependency exports
}
impl<'src> ExportsLookup<'src> {
    fn new(local_modules: &Vec<LocalModule<'src>>) -> Self {
        Self {
            local: local_modules
                .iter()
                .map(|module| {
                    let mut exports = module.exports.clone();
                    // modules also export their children for access for local
                    for (child_name, child_id) in &module.children {
                        exports
                            .modules
                            .entry(*child_name)
                            .or_insert(Id::Local { id: *child_id });
                        // does not result in errors if something shadows local modlue children exports
                    }
                    exports
                })
                .collect(),
        }
    }
    fn get_module(&self, mod_id: Id) -> &ModuleExports<'src> {
        match mod_id {
            Id::Local { id } => &self.local[id],
            Id::Dependency { .. } => todo!("exports from modules in dependencies"),
        }
    }
}

pub fn resolve_data<'src>(
    idents: Vec<ASTIdent<'src>>,
    scopes: &Scopes<ImportedScope<'src>>,
    scope: ScopeId,
    local_modules: &Vec<LocalModule<'src>>,
    mod_id: usize,
    exports: &ExportsLookup<'src>,
    diagnostics: &mut Diagnostics,
) -> Fallible<((Id, Loc), Vec<(&'src str, Loc)>)> {
    assert!(idents.len() > 0);

    fn into_names<'src>(
        idents: impl Iterator<Item = ASTIdent<'src>>,
        diagnostics: &mut Diagnostics,
    ) -> Fallible<(Vec<&'src str>, Vec<Loc>)> {
        let mut names = Vec::new();
        let mut locs = Vec::new();
        for ident in idents {
            names.push(match ident.value {
                ASTIdentValue::Name(name) => name,
                ASTIdentValue::Root => {
                    return Err(diagnostics.raise(ImportDiagnostic::IllegalRoot, ident.loc));
                }
                ASTIdentValue::SelfTy | ASTIdentValue::SelfVar => {
                    return Err(diagnostics.raise(ImportDiagnostic::IllegalSelf, ident.loc));
                }
                ASTIdentValue::Super => {
                    return Err(diagnostics.raise(ImportDiagnostic::IllegalSuper, ident.loc));
                }
            });
            locs.push(ident.loc);
        }
        Ok((names, locs))
    }

    let (id, n_skip) = match &idents[0].value {
        ASTIdentValue::Name(_) => {
            let (names, locs) = into_names(idents.into_iter(), diagnostics)?;
            let (id, trailing_skip) =
                resolve_data_named(&names, &locs, scopes, scope, exports, diagnostics)?;
            return Ok((
                id,
                names
                    .into_iter()
                    .zip(locs.into_iter())
                    .skip(trailing_skip)
                    .collect(),
            ));
        }
        ASTIdentValue::SelfTy => {
            panic!("`Self` should be handled by caller");
        }
        ASTIdentValue::SelfVar => {
            return Err(diagnostics.raise(ImportDiagnostic::IllegalSelf, idents[0].loc));
        }
        ASTIdentValue::Root => {
            (Id::Local { id: 0 }, 1) // root is always zero
        }
        ASTIdentValue::Super => {
            let mut id = mod_id;
            let mut i = 0;
            for name in &idents {
                if name.value == ASTIdentValue::Super {
                    i += 1;
                    if let Some(id_parent) = local_modules[id].parent {
                        id = id_parent
                    } else {
                        return Err(diagnostics.raise(ImportDiagnostic::NoSuperAvailable, name.loc));
                    }
                } else {
                    break;
                }
            }
            (Id::Local { id }, i)
        }
    };

    let (names, locs) = into_names(idents.into_iter().skip(n_skip), diagnostics)?;

    let (id, trailing_skip) = resolve_data_from_module(id, &names, &locs, exports, diagnostics)?;

    return Ok((
        id,
        names
            .into_iter()
            .zip(locs.into_iter())
            .skip(trailing_skip)
            .collect(),
    ));
}
pub fn resolve_data_from_module<'a, 'src>(
    mut id: Id,
    names: &'a [&'src str],
    locs: &'a [Loc],
    exports: &ExportsLookup<'src>,
    diagnostics: &mut Diagnostics,
) -> Fallible<((Id, Loc), usize)> {
    let mut i = 1;

    loop {
        if i >= names.len() {
            return Err(diagnostics.raise(
                ImportDiagnostic::ExpectedDataFoundModule,
                locs[names.len() - 1],
            ));
        }
        let exports = exports.get_module(id);
        if let Some(id_next) = exports.modules.get(names[i]) {
            id = *id_next;
            i += 1;
        } else if let Some(id_next) = exports.datas.get(names[i]) {
            id = *id_next;
            break;
        } else {
            return Err(diagnostics.raise(ImportDiagnostic::ExpectedDataOrModule, locs[i]));
        }
    }
    Ok(((id, locs[0].merge(locs[i - 1])), i))
}
pub fn resolve_data_named<'a, 'src>(
    names: &'a [&'src str],
    locs: &'a [Loc],
    scopes: &Scopes<ImportedScope<'src>>,
    scope: ScopeId,
    exports: &ExportsLookup<'src>,
    diagnostics: &mut Diagnostics,
) -> Fallible<((Id, Loc), usize)> {
    assert!(names.len() > 0);
    assert_eq!(names.len(), locs.len());
    Ok(
        if let Some((id, is_module)) = scopes.find_map(scope, |scope| {
            if let Some(id) = scope.datas.get(names[0]) {
                Some((*id, false))
            } else if let Some(id) = scope.modules.get(names[0]) {
                Some((*id, true))
            } else {
                None
            }
        }) {
            if is_module {
                resolve_data_from_module(id, names, locs, exports, diagnostics)?
            } else {
                // is data
                ((id, locs[0]), 1)
            }
        } else {
            return Err(diagnostics.raise(ImportDiagnostic::ExpectedDataOrModule, locs[0]));
        },
    )
}
