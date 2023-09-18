fn main() {
    let t = skylab::SLTokenizer::new();
    dbg!(t.tokenize("3+[4,5,6] @ [3;2;1]"));
}
