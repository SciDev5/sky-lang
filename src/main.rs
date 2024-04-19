use std::{collections::HashMap, rc::Rc};

use skylab::{
    build::module_tree::{PackageId, PackageRef},
    common::{
        backend::{BackendCompiler, BackendId, BackendsIndex, CommonBackend},
        common_module::CommonModule,
        IdentStr,
    },
    dbg_bytecode_module_code,
    interpreter::{compile::InterpreterBackend, gc::GarbageCollector, interpreter::execute},
    parse::{
        ast::{ASTBlock, ASTModule, PreASTSubModule},
        ast_2_raw::ast_2_raw,
        raw_2_common::raw_2_common,
        tokenization::SLTokenizer,
    },
};

fn main() {
    /* Work in progress
            ___       ___
           /  /      /  /  _________    ____      _____      ____
          /  /      /  /   |__   __|   / __ \    |  __ \    / __ \
         /  /      /  /       | |     | |  | |   | |  | |  | |  | |
        /  /      /  /        | |     | |  | |   | |  | |  | |  | |
       /  /      /  /         | |     | |__| |   | |__| |  | |__| |
      /  /      /  /          |_|      \____/    |_____/    \____/
     /__/      /__/

    */

    let mut sys = TestSys::new();

    sys.testcompile_commonmodule(
        "main",
        CommonBackend::ID,
        &[],
        &[
            (
                &[],
                M::Common(
                    r"
import mod_a

fn b() = mod_a.b()

pub fn c() = 3


b()
                    ",
                ),
            ),
            (
                &["mod_a"],
                M::Multiplatform {
                    common: r"
import root.c

pub fn b() = c()
                    ",
                    platform_specific: &[(
                        InterpreterBackend::ID,
                        r"
import root.c

pub fn b() {
    c()
    4
}
                        ",
                    )],
                },
            ),
        ],
    );

    let bytecode = InterpreterBackend.compile(&sys.modules);
    dbg_bytecode_module_code!(bytecode);

    let mut gc = GarbageCollector::new();
    let return_val = execute(&bytecode, &mut gc);

    dbg!(return_val).unwrap();
}

struct TestSys {
    tokenizer: SLTokenizer,
    backends_index: BackendsIndex,
    modules: Vec<Rc<CommonModule>>,
    name_to_mod_lut: HashMap<IdentStr, usize>,
}

impl TestSys {
    fn new() -> Self {
        Self {
            tokenizer: SLTokenizer::new(),
            backends_index: BackendsIndex::new([].into_iter()),
            modules: Vec::new(),
            name_to_mod_lut: HashMap::new(),
        }
    }

    fn tokenize_and_parse(&self, src: &str) -> ASTBlock {
        let tokens = self.tokenizer.tokenize(src);
        // for (i, token) in tokens.iter().enumerate() {
        //     println!("TOKEN[{}] | {:?}", i, &token);
        // }
        let (parsed, diagnostics) = skylab::parse::parser::parse(tokens);
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

    fn testcompile_commonmodule(
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
enum M {
    Common(&'static str),
    Multiplatform {
        common: &'static str,
        platform_specific: &'static [(BackendId, &'static str)],
    },
}
