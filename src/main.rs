
fn main() {
    let t = skylab::parse::tokenization::SLTokenizer::new();
    let tokens = dbg!(t.tokenize(r"
    
    let a = 3
    
    let b = a + 4

    if b > a {
        b = 2
    }
    
    "));

    let parsed = skylab::parse::parser::parse(tokens).unwrap();

    dbg!(parsed);

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
