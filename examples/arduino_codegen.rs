use std::{path::PathBuf, str::FromStr};

use skylab::{
    codegen::cpp::CPPCodegenBackend,
    common::backend::BackendCompiler,
    example_code_test_utils::{TestSys, M},
};

fn main() {
    let mut sys = TestSys::new();

    sys.testcompile_commonmodule(
        "bind_arduino",
        CPPCodegenBackend::ID,
        &[],
        &[(&[], M::Common(include_str!("./libs/bind_arduino/.sl")))],
    );
    sys.testcompile_commonmodule(
        "prizm",
        CPPCodegenBackend::ID,
        &["bind_arduino"],
        &[(&[], M::Common(include_str!("./libs/prizm/.sl")))],
    );

    sys.testcompile_commonmodule(
        "main",
        CPPCodegenBackend::ID,
        &["bind_arduino", "prizm"],
        &[(&[], M::Common(include_str!("./arduino_codegen.sl")))],
    );

    let (content, header) = CPPCodegenBackend.compile(
        &sys.modules,
        (PathBuf::from_str("./skylang_arduino_example").unwrap(),),
    );

    if !PathBuf::from("./examples/target/skylang_arduino_example").is_dir() {
        std::fs::create_dir("./examples/target/skylang_arduino_example").unwrap();
    }
    std::fs::write(
        "./examples/target/skylang_arduino_example/skylang_arduino_example.ino",
        content.as_str(),
    )
    .unwrap();
    std::fs::write(
        "./examples/target/skylang_arduino_example/skylang_arduino_example.h",
        header.as_str(),
    )
    .unwrap();

    println!("------------- skylang_arduino_example.ino ---------------");
    println!("{}", content);
    println!("-------------- skylang_arduino_example.h ----------------");
    println!("{}", header);
}
