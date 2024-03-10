use skylab::{
    dbg_bytecode_module_code,
    interpreter::{
        compile::compile_interpreter_bytecode_module, gc::GarbageCollector, interpreter::execute,
    },
    parse::{ast_2_raw::ast_2_raw, ast_module::ASTModule, raw_2_common::raw_2_common},
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
    // let tokens = t.tokenize("import a.b.c.d.e");
    let tokens = t.tokenize("import a.b.c.[d,e] let x = d.f()");
    //     let tokens = t.tokenize(
    //         r#"
    // /////////////////////////////

    // fn add_0(a: int, b: int) = a + b
    // fn add_1(a: int, b: int) {
    //     a + b
    // }
    // fn add_2(a: int, b: int) {
    //     return a + b
    // }

    // struct Vec2 {
    //     x: int
    //     y: int

    //     fn add(self: Vec2, other: Vec2) = Vec2.{
    //         x: self.x + other.x
    //         y: self.y + other.y
    //     }
    // }

    // let a = Vec2.{ x: 1, y: 2 }
    // let b = Vec2.{ x: 3, y: 4 }

    // let c = a.add(b)

    // add_0(c.x, 2) == add_1(c.x, 2) &&
    //     add_1(c.x, 2) == add_2(c.x, 2)

    // /////////////////////////////
    // "#,
    //     );
    //     let tokens = t.tokenize(
    //         r"

    // let a: int = 1 + k(1)
    // /*{
    //     4 + 3
    // }*/
    // let j = if a < 0 {
    //     69
    // } else if 4 < 32 {
    //     420
    // } else {
    //     58913
    // } * 10000

    // // -a(2,4)[b]'
    // struct Hello {
    //     a: int
    //     /// a property documenting comment
    //     b: float
    //     // d: Hello

    //     fn a(d: int) = d * 2
    // }

    // // let x = 3 + 3
    // let a = Hello.{
    //     a: 4 + 4,
    //     // c: 3.3
    //     b: 2.4
    // }
    // // { it -> 3 }

    // //    let x = 3 + 3
    // a

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
    // while a > 1 {
    //     b = b * a
    //     a = a - 1
    // }

    // a = 5
    // let c_ = 1
    // let c = while a > 1 {
    //     c_ = c_ * a
    //     a = a - 1
    //     // break 9999999
    // } else {
    //     c_
    // }

    // b + c + some_result + j // -> 5! + 5! + 4 + 420*10000 = 4200244
    // // loop {
    // //     if a == 1 {
    // //         break b
    // //     }
    // //     b = b * a
    // //     a = a - 1
    // // } + some_result + j // -> 5! + 4 + 420*10000 = 4200124

    //            ", // */
    //     );
    //     let tokens = t.tokenize(
    //         r"
    // struct A {
    //     x: int
    //     y: int

    //     fn a(self: A, x: int) = x * self.x + self.y
    // }

    // let b = A.{ x: 2, y: 3 }

    // b.a(100)
    // ",
    //     );
    for (i, token) in tokens.iter().enumerate() {
        println!("TOKEN[{}] | {:?}", i, &token);
    }
    // let _ = dbg!(parser::parse(tokens));
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

    let (parsed, diagnostics) = skylab::parse::parser::parse(tokens);
    dbg!(diagnostics);
    let parsed = parsed.unwrap();
    for (i, expr) in parsed.into_iter().enumerate() {
        println!("[{}] {:?}", i, expr);
    }
    // let ast = ASTModule::new([(vec![], parsed)].into_iter());
    // let common = raw_2_common(ast_2_raw(ast));
    // dbg!(&common.structs);
    // let bytecode = compile_interpreter_bytecode_module(common);
    // dbg_bytecode_module_code!(bytecode);

    // let mut gc = GarbageCollector::new();
    // let return_val = execute(&bytecode, &mut gc);

    // dbg!(return_val).unwrap();
}
