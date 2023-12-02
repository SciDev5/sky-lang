use skylab::interpreter::{interpreter::ScopeStackFrame, gc::GarbageCollector};

fn main() {
    let t = skylab::language::tokenization::SLTokenizer::new();
    // dbg!(t.tokenize(r#"
    // let a = 4;
    // 3 + [4,5,6] @ [3;2;1]
    // "#));
    // let tokens = dbg!(t.tokenize("2+3*a^b^c+#[2, 4; a + b^2, []]"));
    // let tokens = dbg!(t.tokenize("#<2> [[1,2],[3,4]]"));
    // let tokens = dbg!(t.tokenize("{ 3 + 3 }"));
    // let tokens = dbg!(t.tokenize(r"
    //     let k = { s -> s }
    //     k(1)
    // "));
    // let tokens = dbg!(t.tokenize("(2+3)[1]"));
    // let tokens = dbg!(t.tokenize("{ a, b -> a + b } /* jkd */[:,9](k,3)"));
    // let tokens = dbg!(t.tokenize("if k == 3 { e } else { 4 }"));
    // let tokens = dbg!(t.tokenize("loop { if k == 3 { e } }"));
    // let tokens = dbg!(t.tokenize(
    //     r"
    //     /// whaa function
    //     /// :3
    //     /**
    //      * oijfogijogoijgoij
    //      */
    //     fn helloworld(k) {
    //         if k + 3 > 2 {
    //             print(k)
    //             1
    //         } else {
    //             let i = 0
    //             loop {
    //                 i = i + 1

    //                 // break
    //                 if i == 10 {
    //                 }
    //             }
    //         }
    //     }
    // "
    // ));

    // let tokens = dbg!(t.tokenize("a[1,2:3 + 3,:4,5:,:, 0::2]"));
    // let tokens = dbg!(t.tokenize(r"
    //      { s -> s } (1)
    // "));
    // let tokens = dbg!(t.tokenize(r"
    //     let x = 3 + (-4) // should be -1
    //     let b = [1,2,3]
    //     /// functions!!! :3c
    //     fn add5(x) {
    //         x + 5
    //     }
    //     let z = if false {
    //         1
    //     } elif false {
    //         2
    //     } elif false {
    //         3
    //     } elif true {
    //         add5(x) // should be -1+5 = 4
    //     } elif false {
    //         5
    //     } else {
    //         6
    //     } + 100

    //     let r = [z,b]

    //     z
    // "));
    let tokens = dbg!(t.tokenize(
        r"
        // fn whatever(inner_fun) {
        //     inner_fun(4)
        // }

        // let k = whatever { x -> x + 2 }

        // if true {
        //     if (whatever { _ -> true }) {
        //         if true {
        //             if false {
        //                 1
        //             } else {
        //                 k
        //             }
        //         } else {
        //             3
        //         }
        //     }
        // }

        // let i = 0
        // let k = 1
        // loop {
        //     i = i + 1
        //     k = k * i
        //     if i >= 5 {
        //         break k
        //     }
        // }

        let a = 1 + 3i

        a * a'
    "
    ));

    let parsed = dbg!(skylab::language::parser::parse(tokens));

    let serialized_code = skylab::interpreter::interpreter::serialize_program(parsed.unwrap());

    for (i, cmd) in serialized_code.iter().enumerate() {
        println!("{} :: {:?}", i, cmd);
    }

    let mut gc = GarbageCollector::new();
    dbg!(skylab::interpreter::interpreter::execute_serialized(
        serialized_code,
        ScopeStackFrame::base(),
        &mut gc
    ))
    .unwrap();
    // dbg!(&gc);
}
