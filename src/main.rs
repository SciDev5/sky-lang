use skylab::{parse::{ast_2_raw::ast_2_raw, raw_2_common::raw_2_common}, interpreter::{compile::compile_interpreter_bytecode_module, interpreter::execute, gc::GarbageCollector}};


fn main() {
    let t = skylab::parse::tokenization::SLTokenizer::new();
    let tokens = t.tokenize("69.420");
    let parsed = skylab::parse::parser::parse(tokens).unwrap();
    let common = raw_2_common(ast_2_raw(parsed));
    let bytecode = compile_interpreter_bytecode_module(common);

    let mut gc = GarbageCollector::new();
    let return_val = execute(&bytecode, &mut gc);

    dbg!(return_val);

    // let serialized_code = skylab::interpreter::interpreter::serialize_program(parsed);

    // for (i, cmd) in serialized_code.iter().enumerate() {
    //     println!("{} :: {:?}", i, cmd);
    // }

    // let mut gc = GarbageCollector::new();
    // dbg!(skylab::interpreter::interpreter::execute_serialized(
    //     serialized_code,
    //     ScopeStackFrame::base(),
    //     &mut gc
    // ))
    // .unwrap();
    // // dbg!(&gc);
}
