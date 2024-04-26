use std::{collections::HashMap, rc::Rc};

use crate::{
    build::module_tree::{PackageId, PackageRef},
    common::{
        backend::{BackendId, BackendsIndex},
        common_module::CommonModule,
        IdentStr,
    },
    parse::{
        ast::{ASTBlock, ASTModule, PreASTSubModule},
        ast_2_raw::ast_2_raw,
        raw_2_common::raw_2_common,
        tokenization::SLTokenizer,
    },
};

pub struct TestSys {
    tokenizer: SLTokenizer,
    backends_index: BackendsIndex,
    pub modules: Vec<Rc<CommonModule>>,
    name_to_mod_lut: HashMap<IdentStr, usize>,
}

impl TestSys {
    pub fn new() -> Self {
        Self {
            tokenizer: SLTokenizer::new(),
            backends_index: BackendsIndex::new([].into_iter()),
            modules: Vec::new(),
            name_to_mod_lut: HashMap::new(),
        }
    }

    fn tokenize_and_parse(&self, src: &str) -> ASTBlock {
        let tokens = self.tokenizer.tokenize(src);
        for (i, token) in tokens.iter().enumerate() {
            println!("TOKEN[{}] | {:?}", i, &token);
        }
        let (parsed, diagnostics) = crate::parse::parser::parse(tokens);
        // dbg!(diagnostics);
        let parsed = parsed.unwrap();
        // for (i, expr) in parsed.iter().enumerate() {
        //     println!("[{}] {:?}", i, expr);
        // }
        parsed
    }

    fn src_to_ast(&self, path: &[&str], src: M) -> (Vec<IdentStr>, PreASTSubModule) {
        (
            path.into_iter().map(|it| it.to_string()).collect(),
            match src {
                M::Common(src) => PreASTSubModule::Common(self.tokenize_and_parse(src)),
                M::Multiplatform {
                    common,
                    platform_specific,
                } => PreASTSubModule::Multiplatform {
                    common: self.tokenize_and_parse(common),
                    platform_specific: platform_specific
                        .iter()
                        .copied()
                        .map(|(id, src)| (id, self.tokenize_and_parse(src)))
                        .collect(),
                },
            },
        )
    }

    pub fn testcompile_commonmodule(
        &mut self,
        mod_name: &'static str,
        base_supported_backend: BackendId,
        dependencies: &[&str],
        src: &[(&[&str], M)],
    ) {
        let src = src
            .iter()
            .copied()
            .map(|(path, src)| self.src_to_ast(path, src))
            .collect::<Vec<_>>();
        let modules = &self.modules;
        let ix = &mut self.backends_index;
        let name_to_mod_lut = &self.name_to_mod_lut;
        let ast = ASTModule::new(
            base_supported_backend,
            src.into_iter(),
            ix,
            dependencies.into_iter().map(|it| it.to_string()).map(|it| {
                let id = *name_to_mod_lut.get(&it).unwrap();
                (
                    it,
                    PackageRef {
                        id: PackageId(id),
                        module: modules[id].clone(),
                    },
                )
            }),
        );
        // dbg!(&ast);

        let raw = ast_2_raw(ast, &self.backends_index);
        // dbg!(raw);
        let common = raw_2_common(raw);
        // dbg!(common);

        self.name_to_mod_lut
            .insert(mod_name.to_string(), self.modules.len());
        self.modules.push(Rc::new(common));
    }
}

#[derive(Debug, Clone, Copy)]
pub enum M {
    Common(&'static str),
    Multiplatform {
        common: &'static str,
        platform_specific: &'static [(BackendId, &'static str)],
    },
}
