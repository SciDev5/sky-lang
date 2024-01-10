use skylab::{
    dbg_bytecode_module_code,
    interpreter::{
        compile::compile_interpreter_bytecode_module, gc::GarbageCollector, interpreter::execute,
    },
    parse::{ast_2_raw::ast_2_raw, raw_2_common::raw_2_common},
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

        
        struct Hello {
            a: int
            /// a property documenting comment
            b: float
            // d: Hello
        }

        let a = Hello {
            a: 4 + 4, 
            // c: 3.3
            b: 2.4 
        }

        a

        // fn k() {
        //     3
        // }

        // fn k(b: int) {
        //     -b + k()
        // }

        // let a = k(k())
        // let b = a + 1

        // let some_result = if b > a {
        //     1 + k(b * a)
        // } else {
        //     2
        // } // -> 4

        // let a = 5
        // let b = 1
        // loop {
        //     if a == 1 {
        //         break b
        //     }
        //     b = b * a
        //     a = a - 1
        // } + some_result // -> 5! + 4 = 124
    ",
    );
    // let tokens = t.tokenize(
    //     r"
    //     let a: bool = 3

    //     let b: int
    //     b = false
    //     b = 2

    //     let c
    //     c = true

    //     let d = b + c
    // ",
    // );
    let parsed = skylab::parse::parser::parse(tokens).unwrap();
    let common = raw_2_common(ast_2_raw(parsed));
    dbg!(&common.structs);
    let bytecode = compile_interpreter_bytecode_module(common);
    dbg_bytecode_module_code!(bytecode);

    let mut gc = GarbageCollector::new();
    let return_val = execute(&bytecode, &mut gc);

    dbg!(return_val).unwrap();

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
