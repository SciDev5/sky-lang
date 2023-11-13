
pub enum ShuntingYardObj<E, O: ShuntingYardOperator> {
    Expr(E),
    Op(O),
}
pub trait ShuntingYardOperator: Copy {
    fn right_associative(self) -> bool;
    fn precedence(self) -> u8;
}

pub fn treeify_infix<E,O: ShuntingYardOperator,F: Fn(O,E,E)->E>(
    stream: &mut impl Iterator<Item = ShuntingYardObj<E,O>>,
    f: &F,
) -> E {
    let postfix = shunting_yard_infix_to_postfix(stream);
    treeify_reversed_postfix(&mut postfix.into_iter().rev(), f)
}

/// An implementation of [Shunting Yard](https://en.wikipedia.org/wiki/Shunting_yard_algorithm#The_algorithm_in_detail)
/// 
/// Converts infix-representation of values and operators into postfix. 
fn shunting_yard_infix_to_postfix<E,O:ShuntingYardOperator>(input_stream: impl Iterator<Item = ShuntingYardObj<E,O>>) -> Vec<ShuntingYardObj<E,O>> {
    let mut output_queue = vec![];
    let mut operator_stack = vec![];

    for token in input_stream {
        match token {
            ShuntingYardObj::Expr(expr) => output_queue.push(ShuntingYardObj::Expr(expr)),
            ShuntingYardObj::Op(op) => {
                while let Some(&top_op) = operator_stack.last() {
                    if shunting_yard_should_pop_operator(&op, &top_op) {
                        output_queue.push(ShuntingYardObj::Op(operator_stack.pop().unwrap()));
                    } else {
                        break;
                    }
                }
                operator_stack.push(op);
            }
        }
    }

    while let Some(op) = operator_stack.pop() {
        output_queue.push(ShuntingYardObj::Op(op));
    }

    output_queue
}
fn shunting_yard_should_pop_operator<O:ShuntingYardOperator>(new_op: &O, top_op: &O) -> bool {
    if new_op.right_associative() {
        new_op.precedence() < top_op.precedence()
    } else {
        new_op.precedence() <= top_op.precedence()
    }
}

/// Transform a reversed stream of postfix order operators into a tree structure.
fn treeify_reversed_postfix<E,O: ShuntingYardOperator,F: Fn(O,E,E)->E>(
    stream: &mut impl Iterator<Item = ShuntingYardObj<E,O>>,
    f: &F,
) -> E {
    match stream
        .next()
        .expect("treeifying postfix output of shunting yard failed: ran out of nodes")
    {
        ShuntingYardObj::Expr(expr) => expr,
        ShuntingYardObj::Op(op) => {
            // Remember we're reading the list in reverse.
            let second = treeify_reversed_postfix(stream,f);
            let first = treeify_reversed_postfix(stream,f);
            f(op, first, second)
        }
    }
}