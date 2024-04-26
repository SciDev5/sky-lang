use skylab::{
    common::backend::{BackendCompiler, CommonBackend},
    dbg_bytecode_module_code,
    example_code_test_utils::{TestSys, M},
    interpreter::{compile::InterpreterBackend, gc::GarbageCollector, interpreter::execute},
};

fn main() {
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

pub fn c() {
    if true {
        3
    } else if false {
        let a = 4
        a
    } else {
        5
    }
}

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
                        r#"
import root.c

pub fn b() {
    c()
    4
}
                        "#,
                    )],
                },
            ),
        ],
    );

    let bytecode = InterpreterBackend.compile(&sys.modules, ());
    dbg_bytecode_module_code!(bytecode);

    let mut gc = GarbageCollector::new();
    let return_val = execute(&bytecode, &mut gc);

    dbg!(return_val).unwrap();
}
