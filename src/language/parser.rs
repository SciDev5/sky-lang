use std::iter::Peekable;

use crate::math::tensor::Tensor;

use super::{
    ops::SLOperator,
    slir::{SLIRArray, SLIRExpression, SLIRLiteral, SLIRStatement, SLIRVarAccessExpression},
    tokenization::{BracketType, SLToken, SeparatorType},
};

pub fn parse<'a>(tokens: Vec<SLToken<'a>>) -> Result<Vec<SLIRStatement>, ()> {
    // TODO proper errors
    let mut statements = vec![];
    let mut tokens = tokens.into_iter().peekable();
    loop {
        skip_whitespace(&mut tokens);
        if tokens.peek().is_none() {
            break Ok(statements); // Hit end without issue.
        }
        if let Some((next, tokens_advanced)) = next_statement(tokens.clone()) {
            statements.push(next);
            tokens = tokens_advanced;
        } else {
            break Err(());
        }
    }
}

fn next_statement<'a, I: Iterator<Item = SLToken<'a>>>(
    tokens: Peekable<I>,
) -> Option<(SLIRStatement, Peekable<I>)> {
    // TODO not just expressions
    let (expr, tokens) = next_expression(tokens)?;
    Some((SLIRStatement::Expr(Box::new(expr)), tokens))
}

fn next_expression<'a, I: Iterator<Item = SLToken<'a>>>(
    tokens: Peekable<I>,
) -> Option<(SLIRExpression, Peekable<I>)> {
    /*
    A finite state machine should be able to handle expression parsing.

    >> List of possible expressions
    <expr> = <literal>
    <expr> = <expr> <infix> <expr>
    <expr> = <prefix> <expr>
    <expr> = <expr> <postfix>
    <expr> = "(" <expr> ")"
    <expr> = "[" ((<expr> ",")... ";")... "]"

    >> Whole thing in a regex-like format
    $ = (
        L |
        I |
        P $ A? |
        (
            "(" $ ")" |
            "[" ($ S)* "]"
        ) A?
    ) (I $)*

    >> Broken apart into separate smaller functions, seen below in code.
    expr = expr_no_infix (I expr_no_infix)*
    expr_no_infix = (
        L |
        I |
        expr_prefix |  >> initiated by Any Operator (matching will determine if valid)
        expr_grouping  >> initiated by "(" | "["
    )
    expr_prefix = P expr_no_infix A?
    expr_grouping = (
        expr_paren |    >> initiated by "("
        expr_array      >> initiated by "["
    ) A?
    expr_paren = "(" expr ")"
    expr_array = "[" (expr S)* "]"

    I'm qualified bet.
    */

    let mut infix_expr = vec![];

    let (expr, mut tokens) = next_expression_no_infix(tokens)?;

    infix_expr.push(ShuntingYardObj::Expr(expr));

    skip_whitespace(&mut tokens);
    while let Some(SLToken::Operator(op)) = tokens.peek().copied() {
        if !op.is_infix() {
            break;
        }
        tokens.next();
        let (expr, tokens_) = next_expression_no_infix(tokens)?;
        tokens = tokens_;
        infix_expr.push(ShuntingYardObj::Op(op));
        infix_expr.push(ShuntingYardObj::Expr(expr));
        skip_whitespace(&mut tokens); // skip whitespace before peeking the next token in the `while let`
    }

    let mut postfix_expr_reversed = shunting_yard(infix_expr.into_iter()).into_iter().rev();

    Some((
        treeify_reversed_postfix_ops(&mut postfix_expr_reversed),
        tokens,
    ))
}

fn next_expression_no_infix<'a, I: Iterator<Item = SLToken<'a>>>(
    mut tokens: Peekable<I>,
) -> Option<(SLIRExpression, Peekable<I>)> {
    /*
    expr_no_infix = (
        L |
        I |
        expr_prefix |  >> initiated by Any Operator (matching will determine if valid)
        expr_grouping  >> initiated by "(" | "["
    )
    */
    skip_whitespace(&mut tokens);
    let next = tokens.next()?;
    match next {
        SLToken::BracketOpen(bracket_type) => next_expression_grouping(bracket_type, tokens),
        SLToken::BracketClose(_) => None, // can't start expression with a closing bracket
        SLToken::Separator(_) => None,    // can't start expression with a separator

        SLToken::Float(_, _) | SLToken::Int(_, _) => next_expression_literal_num(next, tokens),

        SLToken::Ident(name) => Some((
            SLIRExpression::VarRead(SLIRVarAccessExpression::Ident(
                name.to_string().into_boxed_str(),
            )),
            tokens,
        )),

        SLToken::Operator(op) => next_expression_prefix(op, tokens),

        // keywords not expected here
        SLToken::Keyword(_) => None,
        // always invalid
        SLToken::NumLiteralInvalid(_) => None,
        SLToken::Unk(_) => None,
        // should have skipped all spaces
        SLToken::Space { .. } => panic!("impossible, just skipped all whitespace"),
    }
}

fn next_expression_literal_num<'a, I: Iterator<Item = SLToken<'a>>>(
    initial: SLToken<'a>,
    tokens: Peekable<I>,
) -> Option<(SLIRExpression, Peekable<I>)> {
    let val = match initial {
        SLToken::Float(v, imag) => {
            if imag {
                SLIRLiteral::Float { re: 0.0, im: v }
            } else {
                SLIRLiteral::Float { re: v, im: 0.0 }
            }
        }
        SLToken::Int(v, imag) => {
            if imag {
                SLIRLiteral::Int { re: 0, im: v }
            } else {
                SLIRLiteral::Int { re: v, im: 0 }
            }
        }
        _ => {
            return None;
        }
    };
    // TODO: automatically merge stuff like 1+2i into a single primitive
    Some((SLIRExpression::Literal(val), tokens))
}

fn next_expression_prefix<'a, I: Iterator<Item = SLToken<'a>>>(
    op: SLOperator,
    tokens: Peekable<I>,
) -> Option<(SLIRExpression, Peekable<I>)> {
    // expr_prefix = P expr_no_infix A?

    if !op.is_prefix() {
        // Not scientifically possible!
        return None; // (i lied, it actually is if someone writes like `(^3)`. `^` is not a prefix operator)
    }

    let (mut expr, mut tokens) = next_expression_no_infix(tokens)?;

    for postfix_op in next_if_postfix_ops(&mut tokens) {
        expr = SLIRExpression::UnaryOp(postfix_op, Box::new(expr))
    }

    Some((SLIRExpression::UnaryOp(op, Box::new(expr)), tokens))
}

fn next_expression_grouping<'a, I: Iterator<Item = SLToken<'a>>>(
    bracket_type: BracketType,
    tokens: Peekable<I>,
) -> Option<(SLIRExpression, Peekable<I>)> {
    /*
    expr_grouping = (
        expr_paren |    >> initiated by "("
        expr_array      >> initiated by "["
    ) A?
    */
    let (mut expr, mut tokens) = match bracket_type {
        BracketType::Curly => todo!(),
        BracketType::Paren => next_expression_paren(tokens),
        BracketType::Square => next_expression_array(tokens),
    }?;

    for op in next_if_postfix_ops(&mut tokens) {
        expr = SLIRExpression::UnaryOp(op, Box::new(expr))
    }

    Some((expr, tokens))
}

fn next_expression_paren<'a, I: Iterator<Item = SLToken<'a>>>(
    tokens: Peekable<I>,
) -> Option<(SLIRExpression, Peekable<I>)> {
    // expr_paren = "(" expr ")"
    let (expr, mut tokens) = next_expression(tokens)?;

    skip_whitespace(&mut tokens);
    if let Some(SLToken::BracketClose(BracketType::Paren)) = tokens.next() {
        Some((expr, tokens))
    } else {
        None
    }
}

fn next_expression_array<'a, I: Iterator<Item = SLToken<'a>>>(
    tokens: Peekable<I>,
) -> Option<(SLIRExpression, Peekable<I>)> {
    let mut tokens = tokens;
    // TODO expr_array = "[" (expr S)* "]"
    let mut elts = vec![];
    loop {
        skip_whitespace(&mut tokens);
        let next_elt = match tokens.peek() {
            Some(SLToken::BracketClose(BracketType::Square)) => {
                tokens.next();
                break;
            }
            Some(_) => {
                if let Some(next) = next_expression(tokens) {
                    next
                } else {
                    // Failed to match inner expression.
                    return None;
                }
            }
            None => {
                // Unexpected end.
                return None;
            }
        };
        tokens = next_elt.1;
        let next_elt = next_elt.0;

        skip_whitespace(&mut tokens);
        let sep = match tokens.peek() {
            Some(SLToken::Separator(typ)) => {
                let typ = *typ;
                tokens.next();
                Some(typ)
            }
            _ => None,
        }
        .unwrap_or(super::tokenization::SeparatorType::Comma); // spaces become implicit commas

        elts.push((next_elt, sep));
    }

    if elts.len() == 0 {
        return Some((
            SLIRExpression::Array(SLIRArray(Tensor::new_matrix::<0, 0>([]))),
            tokens,
        ));
    }

    let width = elts
        .iter()
        .enumerate()
        .find(|(_, (_, styp))| {
            if let SeparatorType::Semicolon = styp {
                true
            } else {
                false
            }
        })
        .map(|(i, _)| i + 1)
        .unwrap_or(elts.len());
    dbg!(width);
    dbg!(elts.len());
    if elts.len() % width != 0 {
        return None; // Invalid array length, guaranteed illegal/ragged
    }
    let height = elts.len() / width;
    for (i, (_, styp)) in elts[..elts.len() - 1].iter().enumerate() {
        let is_row_break = match styp {
            SeparatorType::Comma => false,
            SeparatorType::Semicolon => true,
        };
        if is_row_break != ((i + width - 1) % width == 0) {
            // Ragged (inconsistent width), not allowed.
            return None;
        }
    }

    let matrix =
        Tensor::new_matrix_iter(&mut elts.into_iter().map(|(expr, _)| expr), width, height);

    Some((SLIRExpression::Array(SLIRArray(matrix)), tokens))
}

fn next_if_postfix_ops<'a, I: Iterator<Item = SLToken<'a>>>(
    tokens: &mut Peekable<I>,
) -> Vec<SLOperator> {
    let mut ops = vec![];
    loop {
        skip_soft_whitespace(tokens);
        if let Some(SLToken::Operator(op)) = tokens.peek().copied() {
            if op.is_postfix() {
                tokens.next();
                ops.push(op);
                continue;
            }
        }
        break;
    }
    ops
}

/// Skips whitespace, returning true if there was a "hard" space (aka. a line break)
/// and false if no skip or "soft" whitespace.
fn skip_whitespace<'a, I: Iterator<Item = SLToken<'a>>>(tokens: &mut Peekable<I>) -> bool {
    if let Some(SLToken::Space { hard }) = tokens.peek().copied() {
        tokens.next();
        return hard;
    }
    return false;
}

/// Skips soft whitespace (eg. spaces/tabs but not line breaks).
fn skip_soft_whitespace<'a, I: Iterator<Item = SLToken<'a>>>(tokens: &mut Peekable<I>) {
    if let Some(SLToken::Space { hard }) = tokens.peek().copied() {
        if !hard {
            tokens.next();
        }
    }
}

enum ShuntingYardObj {
    Expr(SLIRExpression),
    Op(SLOperator),
}

fn shunting_yard(input_stream: impl Iterator<Item = ShuntingYardObj>) -> Vec<ShuntingYardObj> {
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

fn shunting_yard_should_pop_operator(new_op: &SLOperator, top_op: &SLOperator) -> bool {
    if new_op.right_associative() {
        new_op.precedence() < top_op.precedence()
    } else {
        new_op.precedence() <= top_op.precedence()
    }
}

fn treeify_reversed_postfix_ops(
    stream: &mut impl Iterator<Item = ShuntingYardObj>,
) -> SLIRExpression {
    match stream
        .next()
        .expect("treeifying postfix output of shunting yard failed: ran out of nodes")
    {
        ShuntingYardObj::Expr(expr) => expr,
        ShuntingYardObj::Op(op) => {
            // Remember we're reading the list in reverse.
            let second = treeify_reversed_postfix_ops(stream);
            let first = treeify_reversed_postfix_ops(stream);
            SLIRExpression::BinaryOp(op, Box::new(first), Box::new(second))
        }
    }
}
