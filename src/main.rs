fn main() {
    let t = skylab::language::tokenization::SLTokenizer::new();
    // dbg!(t.tokenize(r#"
    // let a = 4;
    // 3 + [4,5,6] @ [3;2;1]
    // "#));
    let tokens = dbg!(t.tokenize("2+3*a^b^c+#[2, 4; a + b^2, []]"));

    let _parsed = dbg!(skylab::language::parser::parse(tokens));
}
