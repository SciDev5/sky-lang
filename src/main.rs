fn main() {
    let t = skylab::language::tokenization::SLTokenizer::new();
    // dbg!(t.tokenize(r#"
    // let a = 4;
    // 3 + [4,5,6] @ [3;2;1]
    // "#));
    // let tokens = dbg!(t.tokenize("2+3*a^b^c+#[2, 4; a + b^2, []]"));
    // let tokens = dbg!(t.tokenize("#<2> [[1,2],[3,4]]"));
    // let tokens = dbg!(t.tokenize("{ 3 + 3 }"));
    let tokens = dbg!(t.tokenize(r"
        let k = { s -> s + 1 }
        k(1)
    "));
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

    let parsed = dbg!(skylab::language::parser::parse(tokens));

    let serialized_code = dbg!(skylab::interpreter::interpreter::serialize_program(parsed.unwrap()));
}
