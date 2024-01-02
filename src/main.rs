use skylab::{
    interpreter::{
        compile::compile_interpreter_bytecode_module, gc::GarbageCollector, interpreter::execute,
    },
    parse::{ast_2_raw::ast_2_raw, raw_2_common::raw_2_common}, dbg_bytecode_module_code,
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
    let t = skylab::parse::tokenization::SLTokenizer::new();
    let tokens = t.tokenize(
        r"

        fn k() {
            3
        }

        fn k(b: int) {
            -b + k()
        }

        let a = k(k())
        let b = a + 1

        if b < a {
            1 + k(b * a)
        } else {
            2
        }

        
    ",
    );
    let parsed = skylab::parse::parser::parse(tokens).unwrap();
    let common = raw_2_common(ast_2_raw(parsed));
    let bytecode = compile_interpreter_bytecode_module(common);
    dbg_bytecode_module_code!(bytecode);

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
