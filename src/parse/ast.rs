use std::{collections::HashMap, fmt::Debug};

use num::complex::Complex64;

use crate::{
    build::module_tree::{
        ModuleTree, PackageRef, SubModuleCodeRef, SubModuleEntryInfo, SubModuleType,
    },
    common::{
        backend::{BackendId, BackendsIndex, PlatformInfo},
        common_module::DocComment,
        IdentStr,
    },
    math::tensor::Tensor,
};

use super::{
    macros::MacroCall,
    ops::SLOperator,
    raw_module::{RMTemplateDef, RMType},
};

#[derive(Debug, Clone, PartialEq)]
pub enum ASTVarAccessExpression {
    Var {
        ident: IdentStr,
    },
    Index {
        object: Box<ASTExpression>,
        indices: Vec<ASTExpression>,
    },
    PropertyAccess {
        object: Box<ASTExpression>,
        property_ident: IdentStr,
    },
}

/*

"a + b" ok

"a(b)" ambiguous, either tuple struct init or function call, but can be handled later in raw_2_common

ambiguous, either struct init or function call and needs to be parsed differently in each case:
    "a { b }"

non-ambiguous if ranges not allowed on top level:
    "a { b: c }"

non-ambiguous:
    "a { -> b: c }"
    "a { 3 }"
    "a() { b: c }"
    "a { b, c }"

*/

#[derive(Debug, Clone, PartialEq)]
pub struct ASTOptionallyTypedIdent {
    pub ident: IdentStr,
    pub ty: Option<RMType>,
}
#[derive(Debug, Clone, PartialEq)]
pub struct ASTTypedIdent {
    pub ident: IdentStr,
    pub ty: RMType,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTAnonymousFunction {
    pub params: Option<Vec<ASTOptionallyTypedIdent>>,
    pub block: ASTBlock,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTBlockStructInit {
    pub properties: Vec<(IdentStr, ASTExpression)>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ASTCompoundPostfixContents {
    /// Function calls
    /// > `target (arg_a, arg_b, arc_c, ...)`
    /// > `target (arg_a, arg_b, arc_c, ...) { param_a, param_b, ... -> code_block }`
    /// > `target { param_a, param_b, ... -> code_block }`
    Call(Vec<ASTExpression>, Option<ASTAnonymousFunction>),
    /// Tuple struct initializations.
    /// > `target.(arg_a, arg_b, arc_c, ...)`
    TupleStructInit(Vec<ASTExpression>),
    /// Struct initialization
    /// > `struct_ident.{ property_a: expr_a, property_b, ... }`
    BlockStructInit(ASTBlockStructInit),
    /// Indexing call
    /// > `target [arg_a, arg_b, ...]`
    Index(Vec<ASTExpression>),
    /// Property access
    /// > `target. property``
    PropertyAccess(IdentStr),
}

#[derive(Debug, Clone, PartialEq)]
pub enum ASTExpression {
    InlineMacroCall(MacroCall<ASTExpression>),

    VarDeclare {
        doc_comment: DocComment,
        ident: IdentStr,
        writable: bool,
        initial_value: Option<Box<ASTExpression>>,
        ty: Option<RMType>,
    },
    Assign {
        target: ASTVarAccessExpression,
        op: Option<SLOperator>,
        value: Box<ASTExpression>,
    },

    Ident(IdentStr),

    Literal(ASTLiteral),
    Range {
        start: Option<Box<ASTExpression>>,
        step: Option<Box<ASTExpression>>,
        end: Option<Box<ASTExpression>>,
    },
    Array(ASTArray),
    AnonymousFunction(ASTAnonymousFunction),

    BinaryOp {
        op: SLOperator,
        lhs: Box<ASTExpression>,
        rhs: Box<ASTExpression>,
    },
    UnaryOp {
        op: SLOperator,
        value: Box<ASTExpression>,
    },
    CompoundPostfix {
        target: Box<ASTExpression>,
        contents: ASTCompoundPostfixContents,
    },

    Conditional {
        condition: Box<ASTExpression>,
        block: ASTBlock,
        elifs: Vec<(ASTExpression, ASTBlock)>,
        else_block: Option<ASTBlock>,
    },
    Loop {
        block: ASTBlock,
    },
    LoopWhile {
        condition: Box<ASTExpression>,
        block: ASTBlock,
        else_block: Option<ASTBlock>,
    },
    For {
        loop_var: (IdentStr, Option<RMType>),
        iterable: Box<ASTExpression>,
        block: ASTBlock,
        else_block: Option<ASTBlock>,
    },
    Break(Option<Box<ASTExpression>>),
    Continue,

    Return(Option<Box<ASTExpression>>),
    FunctionDefinition(ASTFunctionDefinition),

    StructDefinition {
        attrs: Vec<MacroCall<ASTExpression>>,
        doc_comment: DocComment,
        is_exported: bool,
        ident: IdentStr,
        properties: Vec<(IdentStr, DocComment, RMType)>,
        // TODO associated functionality
        impl_functions: HashMap<IdentStr, ASTFunctionDefinition>,
        impl_traits: HashMap<IdentStr, ASTTraitImpl>,
    },
    TraitDefinition {
        attrs: Vec<MacroCall<ASTExpression>>,
        doc_comment: DocComment,
        is_exported: bool,
        ident: IdentStr,
        bounds: (),
        // TODO associated types and consts
        functions: HashMap<IdentStr, ASTFunctionDefinition>,
    },

    Import {
        include_paths: Vec<Vec<IdentStr>>,
    },
}

pub type ASTBlock = Vec<ASTExpression>;

#[derive(Debug, Clone, PartialEq)]
pub struct ASTFunctionDefinition {
    pub attrs: Vec<MacroCall<ASTExpression>>,
    pub doc_comment: DocComment,
    pub is_exported: bool,
    pub ident: IdentStr,
    pub is_member: bool,
    pub params: Vec<ASTTypedIdent>,
    pub return_ty: Option<RMType>,
    pub block: Option<ASTBlock>,
    pub can_be_disembodied: bool,
    pub local_template_defs: Vec<RMTemplateDef>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTTraitImpl {
    pub trait_ident: IdentStr,
    // TODO associated types and consts
    pub functions: HashMap<IdentStr, ASTFunctionDefinition>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ASTLiteral {
    Int(i128),
    Float(f64),
    Complex(Complex64),
    Bool(bool),
    String(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum ASTArray {
    List(Vec<ASTExpression>),
    Matrix(Tensor<ASTExpression>),
    Tensor(Tensor<ASTExpression>),
}

#[derive(Debug)]
pub struct ASTSubModule {
    pub block: ASTBlock,
    pub mod_type: SubModuleEntryInfo,
}

#[derive(Debug)]
pub struct ASTModule {
    pub modules: Vec<ASTSubModule>,
    pub submodule_tree: ModuleTree,

    pub base_supported_backend: PlatformInfo,
}
pub enum PreASTSubModule {
    Common(ASTBlock),
    Multiplatform {
        common: ASTBlock,
        platform_specific: HashMap<BackendId, ASTBlock>,
    },
}
impl ASTModule {
    pub fn new(
        base_supported_backend: BackendId,
        iter: impl Iterator<Item = (Vec<IdentStr>, PreASTSubModule)>,
        backends_index: &mut BackendsIndex,
        libs: impl Iterator<Item = (IdentStr, PackageRef)>,
    ) -> Self {
        backends_index
            .lookup_or_load(base_supported_backend)
            .expect("// TODO handle required backend doesn't exist");

        let mut modules = vec![];
        let mut submodule_tree = ModuleTree::new();
        for (name, package) in libs {
            if !submodule_tree.add_dependency(name, package) {
                panic!("// TODO handle failed to construct dependencies");
            }
        }
        for (path, block) in iter {
            let i = modules.len();
            let (id, code_ref) = submodule_tree.entry_at_root(&path);
            *code_ref = match block {
                PreASTSubModule::Common(block) => {
                    modules.push(ASTSubModule {
                        block,
                        mod_type: SubModuleEntryInfo {
                            ty: SubModuleType::Common,
                            id,
                        },
                    });
                    SubModuleCodeRef::Common { module_id: i }
                }
                PreASTSubModule::Multiplatform {
                    common,
                    platform_specific: platform_specific_in,
                } => {
                    modules.push(ASTSubModule {
                        block: common,
                        mod_type: SubModuleEntryInfo {
                            ty: SubModuleType::MultiplatformCommon,
                            id,
                        },
                    });
                    let mut platform_specific = HashMap::new();
                    for (backend_id, block) in platform_specific_in {
                        let i1 = modules.len();
                        let Some(backend) = backends_index.lookup_or_load(backend_id) else {
                            eprintln!("// TODO warn if backend cant be found");
                            continue;
                        };
                        dbg!("HERE", backend);
                        modules.push(ASTSubModule {
                            block,
                            mod_type: SubModuleEntryInfo {
                                ty: SubModuleType::MultiplatformSpecific(backend),
                                id,
                            },
                        });
                        platform_specific.insert(backend_id, i1);
                    }
                    SubModuleCodeRef::Multiplatform {
                        common_module_id: i,
                        platform_specific,
                    }
                }
            };
        }
        Self {
            modules,
            submodule_tree,
            base_supported_backend: backends_index
                .lookup(base_supported_backend)
                .expect("// TODO error if base backend cant be found"),
        }
    }
}
