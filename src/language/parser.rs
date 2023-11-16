use super::{
    slir::SLIRBlock,
    tokenization::SLToken,
};

#[derive(Debug, Clone, Copy)]
pub struct Tokens<'vec, 'token_content> {
    index: usize,
    tokens: &'vec [SLToken<'token_content>],
}

impl<'a, 'b> Tokens<'a, 'b> {
    pub fn new(tokens: &'a [SLToken<'b>]) -> Self {
        Self { index: 0, tokens }
    }
    fn peek(&self) -> Option<&'a SLToken<'b>> {
        self.tokens.get(self.index)
    }
    fn peek_skip_whitespace(&self) -> Option<&'a SLToken<'b>> {
        self.peek_slice()
            .iter()
            .find(|token| !token.is_whitespace())
    }
    fn peek_skip_break(&self) -> Option<&'a SLToken<'b>> {
        self.peek_slice().iter().find(|token| !token.is_break())
    }
    fn peek_slice(&self) -> &'a [SLToken<'b>] {
        if self.index < self.tokens.len() {
            &self.tokens[self.index..]
        } else {
            &[]
        }
    }
    fn peek_is_hard_break(&self) -> bool {
        self.peek()
            .map_or(false, |it| it.break_type().is_hard())
    }
    fn next(&mut self) -> Option<&'a SLToken<'b>> {
        let output = self.tokens.get(self.index);
        self.index += 1;
        return output;
    }
    fn next_skip_break(&mut self) -> Option<&'a SLToken<'b>> {
        loop {
            match self.next()? {
                token if token.is_break() => continue,
                token => break Some(token),
            }
        }
    }
    fn next_skip_soft_break(&mut self) -> Option<&'a SLToken<'b>> {
        loop {
            match self.next()? {
                token if token.break_type().is_soft() => continue,
                token => break Some(token),
            }
        }
    }
    fn next_skip_break_if<F: Fn(Option<&SLToken<'b>>) -> bool>(&mut self, f: F) -> bool {
        let mut self_advanced = *self;
        let matched = f(self_advanced.next_skip_break());
        if matched {
            *self = self_advanced
        }
        matched
    }
    fn next_skip_soft_break_if<F: Fn(Option<&SLToken<'b>>) -> bool>(&mut self, f: F) -> bool {
        let mut self_advanced = *self;
        let matched = f(self_advanced.next_skip_soft_break());
        if matched {
            *self = self_advanced
        }
        matched
    }
    fn step_back(&mut self) {
        self.index = self.index.saturating_sub(1);
    }
    /// Run the parser function passed in and advance the token index if successful.
    fn next_parse<T, F: Fn(Self) -> Option<(Self, T)>>(&mut self, parser: F) -> Option<T> {
        let (tokens_out, data_out) = parser(*self)?;
        *self = tokens_out;
        Some(data_out)
    }
    fn next_match_skip_break<F: Fn(&'a SLToken<'b>) -> bool>(&mut self, f: F) -> Option<&'a SLToken<'b>> {
        let token = self.next_skip_break()?;
        if f(token) {
            Some(token)
        } else {
            None
        }
    }
}

macro_rules! next_match_skip_break {
    ($tokens: ident, $pattern:pat $(if $guard:expr)? $(,)?) => {
        if let Some(token) = $tokens.next_skip_break() {
            match &token {
                $pattern $(if $guard)? => Some(token),
                _ => None,
            }
        } else {
            None
        }
    };
}

macro_rules! parse_rule {
    // parse_rule!(fn name<T: Default, R>(tokens) -> matched: T, _failed { T::default() })
    (
        // code documenting comment
        $(#[$comments_and_attrss:meta])*
        // generated function name and visibility modifier (ex. pub)
        $vis: vis fn $name: ident
        // optional additional function generic type/lifetime parameters.
        $(<$($typ: tt),*>)?
        // function parameter names, usually just `tokens`.
        ($tokens: ident $(, $(($param: ident) : $param_ty: ty)+ $(,)?)?)
        ->
        // `matched : $ty`, creates a macro named `matched!` that takes the current value of tokens and data and returns.
        $macro_name_return_match: ident : $ty: ty,
        // `failed: None`, creates a macro named `failed!` that
        $macro_name_return_fail: ident : None
        // function contents (value is type $ty, can `return None` to abort early)
        $code: block
    ) => {
        $(#[$comments_and_attrss])*
        $vis fn $name <'tv, 'tc, $($typ),*> (
            #[allow(unused_mut)]
            mut $tokens : Tokens<'tv, 'tc>,
            $($($param : $param_ty),+)?
        ) -> Option<(Tokens<'tv, 'tc>, $ty)> {
            macro_rules! $macro_name_return_match {
                ($data: ident) => {
                    return Some(($tokens, $data))
                };
                (tokens = $tokens_override: ident, $data: ident) => {
                    return Some(($tokens_override, $data))
                };
            }
            macro_rules! $macro_name_return_fail {
                () => { return None };
            }
            let _res: $ty = $code;
            #[allow(unreachable_code)]
            Some(($tokens, _res))
        }
    }
}

macro_rules! parse_match_first {
    ($tokens: ident; $($parser: ident => |$param: ident| $success_transform: expr ),+ $(,)?) => {
        $(
            if let Some($param) = $tokens.next_parse($parser) {
                Some($success_transform)
            } else
        )+ {
            None
        }
    };
}

mod expr {
    use super::Tokens;
    use crate::{
        language::{
            ops::SLOperator,
            parser::parse_block,
            slir::{SLIRArray, SLIRExpression, SLIRIdent, SLIRLiteral},
            tokenization::{
                AngleBracketShape, BracketType, SLToken, SeparatorType, SyntacticSugarType,
            },
        },
        math::{
            shunting_yard::{treeify_infix, ShuntingYardObj},
            tensor::Tensor,
        },
    };

    /*
    A finite state machine should be able to handle expression parsing.

    ///////////////// List of possible expressions /////////////////
    <expr> = <literal>
    <expr> = <identifier>
    <expr> = <expr> <infix> <expr>
    <expr> = <prefix> <expr>
    <expr> = <expr> (<postfix> | ("[" (<expr>)","* "]") | ("(" (<expr>)","* ")"))
    <expr> = <block>
    <expr> = "{" ( (<identifier> (":" <type>)? )","+ (",")? "->")? <expr> "}"
    <expr> = "(" <expr> ")"
    <expr> = "[" (<expr>)","* ","? "]"
    <expr> = "#" "[" ( (<expr>)","* )";"* ";"? "]"
    <expr> = "#" "<" [rank] ">" "[" // nested arrays to depth [rank] // "]"
    // <expr> = <<ARRAY COMPREHENSIONS>>

    ///////////////// Expression parsing in smaller chunks /////////////////

    <expr> = <expr_infix> | <range>
    <expr_infix> = <expr_no_infix> (<infix> <expr_no_infix>)*

    <expr_no_infix> = <prefix>* (
        | <literal>
        | <identifier>
        | <expr_grouping>
        | <expr_anonfunc>
        | <expr_extras>
    ) <postfix>*

    <expr_grouping> = (
        //// parentheses
        | "(" <expr> ")"
        //// normal arrays/lists/whatever
        | "[" (<expr>)","* ","? "]"
        //// matrices
        | "#" [" ( (<expr>)","* )";"* ";"? "]"
        //// tensors
        | "#" "<" [rank] ">" "[" // nested arrays to depth [rank] // "]"
    )

    <expr_anonfunc> = "{" ( (<identifier> (":" <type>)? )","+ (",")? "->")? <expr> "}"

    <prefix> = (
        | <postfix_operator>   // ~operators_and_such
    )

    <postfix> = (
        | <postfix_operator>   // operators_and_such'
        | <postfix_func_call> //  function_calls(like, this, 4)
        | <postfix_index>    //   indexing_data[i,j,k]
    )
    <postfix_func_call> = "(" (<expr>)","* (",")? ")"
    <postfix_index> = "[" (<expr>)","* (",")? "]"

    <range> = (
        | (<expr>)? ":" (<expr>)? ( ":" (<expr>)? )?
    )

    <expr_extras> = (
        // TODO
        // ... array comprehensions and such ...
    )
    */

    parse_rule! {
        /// Parse all expressions.
        ///
        /// ```plaintext
        /// <expr> = <expr_infix> | <range>
        /// ```
        pub fn parse_expr(tokens) -> _matched: SLIRExpression, _failed: None {
            parse_match_first!(tokens;
                parse_expr_rangeto => |it| it,
                parse_expr_infixed => |it| it,
            )?
        }
    }
    parse_rule! {
        /// Parse expressions.
        ///
        /// ```plaintext
        /// <expr_infix> = <expr_no_infix> (<infix> <expr_no_infix>)*
        /// ```
        pub fn parse_expr_infixed(tokens) -> _matched: SLIRExpression, failed: None {
            let first_expr = tokens.next_parse(parse_expr_no_infix)?;

            let mut infix = vec![ShuntingYardObj::Expr(first_expr)];

            while let Some(op) = tokens.next_parse(parse_expr_infix_op) {
                if let Some(expr) = tokens.next_parse(parse_expr_no_infix) {
                    infix.push(ShuntingYardObj::Op(op));
                    infix.push(ShuntingYardObj::Expr(expr));
                } else {
                    // saw infix operator follwed by something unexpected.
                    failed!();
                }
            }

            treeify_infix(&mut infix.into_iter(), &|op, a, b| {
                SLIRExpression::BinaryOp(op, Box::new(a), Box::new(b))
            })
        }
    }
    parse_rule! {
        /// Parse the next infix operator token, such as `+`, `*`, `||` or the like
        fn parse_expr_infix_op(tokens) -> matched: SLOperator, failed: None {
            match tokens.next_skip_break()? {
                SLToken::Operator(op) => {
                    if op.is_infix() {
                        let result = *op;
                        matched!(result);
                    } else {
                        failed!();
                    }
                }
                SLToken::AmbiguityAngleBracket(b) => {
                    let op = b.to_operator();
                    matched!(op);
                }
                _ => failed!(),
            }
        }
    }
    parse_rule! {
        /// Parse expressions, excluding expressions with infix operators at the top level.
        ///
        /// ```plaintext
        /// <expr_no_infix> = <prefix>* (
        ///     | <literal>
        ///     | <identifier>
        ///     | <expr_grouping>
        ///     | <expr_extras>
        /// ) <postfix>*
        /// ```
        fn parse_expr_no_infix(tokens) -> _matched: SLIRExpression, _failed: None {
            // prefixes
            let mut prefixes = vec![];
            while let Some(prefix) = tokens.next_parse(parse_expr_prefix) {
                prefixes.push(prefix);
            }
            // main data
            let expr_main = parse_match_first!(
                tokens;
                parse_expr_literal => |literal| SLIRExpression::Literal(literal),
                parse_expr_identifier => |ident| SLIRExpression::Read(ident),
                parse_expr_anonfunc => |it| it,
                parse_expr_grouping => |it| it,
            )?;
            // postfixes
            let mut postfixes = vec![];
            while let Some(postfix) = tokens.next_parse(parse_expr_postfix) {
                postfixes.push(postfix);
            }
            //
            let mut expr = expr_main;
            for affix in postfixes.into_iter().chain(prefixes.into_iter().rev()) {
                expr = affix.apply_to_expr(expr);
            }
            expr
        }
    }
    enum ExprAffix {
        Op(SLOperator),
        Indexing(Vec<SLIRExpression>),
        FunctionCall(Vec<SLIRExpression>), // TODO template types, callback block
    }
    impl ExprAffix {
        fn apply_to_expr(self, expr: SLIRExpression) -> SLIRExpression {
            let expr = Box::new(expr);
            match self {
                ExprAffix::Op(op) => SLIRExpression::UnaryOp(op, expr),
                ExprAffix::FunctionCall(arguments) => SLIRExpression::Call {
                    callable: expr,
                    arguments,
                },
                ExprAffix::Indexing(indices) => SLIRExpression::Index { expr, indices },
            }
        }
    }
    parse_rule! {
        fn parse_expr_prefix(tokens) -> matched: ExprAffix, failed: None {
            match tokens.next_skip_break()? {
                SLToken::Operator(op) => {
                    if op.is_prefix() {
                        let result = ExprAffix::Op(*op);
                        matched!(result);
                    } else {
                        failed!();
                    }
                }
                _ => failed!(),
            }
        }
    }
    parse_rule! {
        ///
        /// Parse postfixes/suffixes/whateveryouwannacallem. This is shockingly complicated,
        /// because there is also function calls (`...[...]`), indexing  (`...[]...]`), and others
        /// beyond just operators like `'` (hermitian conjugate).
        ///
        /// ```plaintext
        /// <postfix> = (
        ///     | <postfix_operator>   // operators_and_such'
        ///     | <postfix_func_call> //  function_calls(param_a * 69 + 420, 4)
        ///     | <postfix_index>    //   indexing_data[i,j,k]
        /// )
        /// ```
        fn parse_expr_postfix(tokens) -> matched: ExprAffix, failed: None {
            // only skipping soft breaks, postfixes dangling after newlines are hard
            // to spot or often not actually meant to be postfixes.
            match tokens.next_skip_soft_break()? {
                SLToken::Operator(op) => {
                    if op.is_postfix() {
                        let result = ExprAffix::Op(*op);
                        matched!(result);
                    } else {
                        failed!();
                    }
                }
                SLToken::BracketOpen(BracketType::Square) => {
                    let indices = tokens.next_parse(|tokens| parse_list(tokens,
                        &parse_expr,
                        |token| matches!(token, SLToken::Separator(SeparatorType::Comma)),
                        |token| matches!(token, SLToken::BracketClose(BracketType::Square)),
                    ))?;
                    let result = ExprAffix::Indexing(indices);
                    matched!(result);
                }
                SLToken::BracketOpen(BracketType::Paren) => {
                    let arguments = tokens.next_parse(|tokens| parse_list(tokens,
                        &parse_expr,
                        |token| matches!(token, SLToken::Separator(SeparatorType::Comma)),
                        |token| matches!(token, SLToken::BracketClose(BracketType::Paren)),
                    ))?;
                    let result = ExprAffix::FunctionCall(arguments);
                    matched!(result);
                }
                // TODO function calls with callback blocks like the_func { it -> it[1] }
                _ => failed!(),
            }
        }
    }
    parse_rule! {
        /// Parse range expressions, like `1:4`, `-5:2:8`
        ///
        /// ```plaintext
        /// <range> = (
        ///     | (<expr>)? ":" (<expr>)? ( ":" (<expr>)? )?
        /// )
        /// ```
        fn parse_expr_rangeto(tokens) -> _matched: SLIRExpression, _failed: None {
            let expr_first = tokens.next_parse(parse_expr_infixed);
            let has_first_colon = tokens.next_skip_soft_break_if(|it| matches!(it, Some(SLToken::Separator(SeparatorType::Colon))));
            if has_first_colon {
                let expr_second = if tokens.peek_is_hard_break() { None } else { tokens.next_parse(parse_expr_infixed) };

                let has_second_colon = tokens.next_skip_soft_break_if(|it| matches!(it, Some(SLToken::Separator(SeparatorType::Colon))));
                if has_second_colon {
                    let expr_third = (if tokens.peek_is_hard_break() { None } else { tokens.next_parse(parse_expr_infixed) })?;

                    // a:b:step
                    SLIRExpression::Range { start: expr_first.map(Box::new), end: Some(Box::new(expr_third)), step: expr_second.map(Box::new) }
                } else {
                    // a:b
                    SLIRExpression::Range { start: expr_first.map(Box::new), end: expr_second.map(Box::new), step: None }
                }
            } else {
                // no range
                _failed!();
            }
        }
    }
    ///
    /// Parses a list structure from tokens, by matching against a data, separator, and end pattern.
    ///
    /// NOTICE: this does not capture the INITIAL token, only separator and ending tokens.
    ///
    /// Ex:
    ///
    /// ```plaintext
    /// let tokens = parse("1,3,4,5]6,7,8")
    /// parse_list(tokens, &parse_number, |it| matches!(it,COMMA), |it| matches!(it, CLOSE_BRACKET))
    /// ```
    /// returns `vec![1,2,3,4,5]`, tokens = `6,7,8`
    ///
    fn parse_list<
        'a,
        'b,
        T,
        Match: Fn(Tokens<'a, 'b>) -> Option<(Tokens<'a, 'b>, T)>,
        MatchSep: Fn(&SLToken<'b>) -> bool,
        MatchEnd: Fn(&SLToken<'b>) -> bool,
    >(
        mut tokens: Tokens<'a, 'b>,
        fn_match: &Match,
        fn_match_sep: MatchSep,
        fn_match_end: MatchEnd,
    ) -> Option<(Tokens<'a, 'b>, Vec<T>)> {
        if fn_match_end(tokens.peek_skip_break()?) {
            tokens.next_skip_break();
            return Some((tokens, vec![]));
        }

        let mut contents = vec![];
        loop {
            if tokens
                .peek_skip_break()
                .map(|v| fn_match_end(v))
                .unwrap_or(false)
                && tokens.peek_is_hard_break()
            {
                // allow trailing separators if there's a hard break before the end token.
                break;
            }
            contents.push(tokens.next_parse(fn_match)?);
            let next = tokens.next_skip_break()?;
            if fn_match_end(next) {
                break;
            } else if fn_match_sep(next) {
                continue;
            } else {
                return None;
            }
        }

        Some((tokens, contents))
    }
    parse_rule! {
        fn parse_expr_grouping(tokens) -> _matched: SLIRExpression, failed: None {
            let had_array_symbol = tokens.next_skip_break_if(|token| matches!(token, Some(SLToken::SyntacticSugar(SyntacticSugarType::Hash))));
            match (
                had_array_symbol,
                (if had_array_symbol {
                    tokens.next_skip_soft_break()
                } else {
                    tokens.next_skip_break()
                })?,
            ) {
                // matrix `#< $rank >[ $($( $data_expr ),* );* $(;)? ]`
                (true, SLToken::BracketOpen(BracketType::Square)) => {
                    let mat = {
                        let data = tokens.next_parse(|tokens| {
                            parse_list(
                                tokens,
                                &|mut tokens| { // capture one row ... , ... , ... ]
                                    let res = tokens.next_parse(|tokens| {
                                        parse_list(
                                            tokens,
                                            &parse_expr,
                                            |token| matches!(token, SLToken::Separator(SeparatorType::Comma)),
                                            |token| matches!(token, SLToken::Separator(SeparatorType::Semicolon) | SLToken::BracketClose(BracketType::Square)),
                                        )
                                    })?;
                                    tokens.step_back(); // step back so the outer `parse_list` can read the separator that ended the row.
                                    Some((tokens, res))
                                },
                                |token| matches!(token, SLToken::Separator(SeparatorType::Semicolon)),
                                |token| matches!(token, SLToken::BracketClose(BracketType::Square)),
                            )
                        })?;

                        if data.len() == 0 {
                            Tensor::new_matrix::<0,0>([])
                        } else {
                            let width = data[0].len();
                            let height = data.len();
                            if !data.iter().all(|row| row.len() == width) {
                                failed!();
                            }
                            Tensor::new_matrix_iter(&mut data.into_iter().flat_map(|row|row.into_iter()), width, height)
                        }
                    };
                    SLIRExpression::Array(SLIRArray::Matrix(mat))
                }
                // tensor `#< $rank >[ $... ]` something-something it's recursive.
                (
                    true,
                    SLToken::BracketOpen(BracketType::Angle)
                    | SLToken::AmbiguityAngleBracket(AngleBracketShape::OpenOrLessThan),
                ) => {
                    let rank = if let Some(SLToken::Int { value, imaginary }) = tokens.next_skip_break() {
                        if *imaginary || *value < 0 || *value > std::usize::MAX as i128 {
                            failed!()
                        }
                        *value as usize
                    } else {
                        failed!()
                    };
                    if !matches!(tokens.next_skip_break(),
                        Some(SLToken::BracketClose(BracketType::Angle)
                            | SLToken::AmbiguityAngleBracket(AngleBracketShape::CloseOrGreaterThan))
                    ) {
                        failed!()
                    }
                    let (contents, dim) = tokens.next_parse(|tokens| parse_expr_tensor_entries(tokens, rank))?;

                    SLIRExpression::Array(SLIRArray::Tensor(Tensor::new_raw(contents, dim)))
                }

                // TODO array comprehension
                // (also probably move it to it's own parser function)

                // list/array `[ $( $data_expr ),* $(,)? ]`
                (false, SLToken::BracketOpen(BracketType::Square)) => {
                    SLIRExpression::Array(SLIRArray::List(tokens.next_parse(|tokens| {
                        parse_list(
                            tokens,
                            &parse_expr,
                            |token| matches!(token, SLToken::Separator(SeparatorType::Comma)),
                            |token| matches!(token, SLToken::BracketClose(BracketType::Square)),
                        )
                    })?))
                }

                // parentheses `( $data_expr )`
                (false, SLToken::BracketOpen(BracketType::Paren)) => {
                    let result = tokens.next_parse(parse_expr)?;
                    if !matches!(
                        tokens.next_skip_break(),
                        Some(SLToken::BracketClose(BracketType::Paren))
                    ) {
                        // failed to match closing parentheses
                        failed!()
                    }
                    result
                }
                _ => failed!(),
            }
        }
    }
    parse_rule! {
        fn parse_expr_tensor_entries(tokens, (rank): usize) -> _matched: (Vec<SLIRExpression>, Vec<usize>), failed: None {
            if rank == 0 {
                (vec![tokens.next_parse(parse_expr)?], vec![])
            } else {
                if !matches!(
                    tokens.next_skip_break(),
                    Some(SLToken::BracketOpen(BracketType::Square))
                ) {
                    // expect "["
                    failed!();
                }
                let parsed = tokens
                    .next_parse(|tokens| {
                        parse_list(
                            tokens,
                            &|tokens| parse_expr_tensor_entries(tokens, rank - 1),
                            |token| matches!(token, SLToken::Separator(SeparatorType::Comma)),
                            |token| matches!(token, SLToken::BracketClose(BracketType::Square)),
                        )
                    })?;
                if parsed.is_empty() {
                    (vec![], vec![0; rank])
                } else {
                    let mut dim = parsed[0].1.clone();
                    if !parsed.iter().all(|(_,dim_other)| &dim == dim_other) {
                        // ragged tensors are not allowed.
                        failed!();
                    }
                    dim.push(parsed.len());
                    (parsed.into_iter().flat_map(|(data,_)| data).collect(), dim)
                }
            }
        }
    }
    parse_rule! {
        /// Parse anonymous function expressions.
        ///
        /// ```plaintext
        /// <expr_anonfunc> = "{" ( (<identifier> (":" <type>)? )","+ (",")? "->")? <expr> "}"
        /// ```
        fn parse_expr_anonfunc(tokens) -> _matched: SLIRExpression, failed: None {
            if !tokens.next_skip_break_if(|it| matches!(it, Some(SLToken::BracketOpen(BracketType::Curly)))) {
                // anonymous functions must start with `{`
                failed!();
            }
            let params = tokens.next_parse(parse_expr_anonfunc_parameters).unwrap_or(vec![]);
            let block = tokens.next_parse(parse_block)?;
            if !tokens.next_skip_break_if(|it| matches!(it, Some(SLToken::BracketClose(BracketType::Curly)))) {
                // anonymous functions must start with `{`
                failed!();
            }

            SLIRExpression::AnonymousFunction { params, block }
        }
    }
    parse_rule! {
        fn parse_expr_anonfunc_parameters(tokens) -> _matched: Vec<SLIRIdent>, _failed: None {
            tokens.next_parse(|tokens| {
                parse_list(
                    tokens,
                    &parse_expr_anonfunc_parameter_entry,
                    |token| matches!(token, SLToken::Separator(SeparatorType::Comma)),
                    |token| matches!(token, SLToken::Separator(SeparatorType::ThinArrowRight)),
                )
            })?
        }
    }
    parse_rule! {
        fn parse_expr_anonfunc_parameter_entry(tokens) -> _matched: SLIRIdent, _failed: None {
            let ident = tokens.next_parse(parse_expr_identifier)?;
            // TODO anonymous function parameter types
            ident
        }
    }
    parse_rule! {
        /// Parse literal values in expressions, such as `133.45` and `"hello world"`
        fn parse_expr_literal(tokens) -> _matched: SLIRLiteral, failed: None {
            match tokens.next_skip_break()? {
                SLToken::Float { value, imaginary: false } => SLIRLiteral::Float { re: *value, im: 0.0 },
                SLToken::Float { value, imaginary: true } => SLIRLiteral::Float { re: 0.0, im: *value },
                SLToken::Int { value, imaginary: false } => SLIRLiteral::Int { re: *value, im: 0 },
                SLToken::Int { value, imaginary: true } => SLIRLiteral::Int { re: 0, im: *value },
                // TODO strings
                _ => failed!(),
            }
        }
    }
    parse_rule! {
        /// Parse identifiers in expressions.
        ///
        /// Identifiers are named variables and funcitons.
        fn parse_expr_identifier(tokens) -> _matched: SLIRIdent, failed: None {
            match tokens.next_skip_break()? {
                SLToken::Identifier(str) => {
                    str.to_string().into_boxed_str()
                }
                _ => failed!(),
            }
        }
    }
}

mod statement {
    use crate::language::{
        parser::{expr, Tokens},
        slir::{SLIRStatement, SLIRBlock}, tokenization::{SLToken, BracketType},
    };

    parse_rule! {
        pub fn parse_statement(tokens) -> _matched: SLIRStatement, _failed: None {
            SLIRStatement::Expr(Box::new(tokens.next_parse(expr::parse_expr)?))
        }
    }

    parse_rule! {
        /// Parse a block bounded by curly braces
        fn parse_curly_block(tokens) -> _matched: SLIRBlock, _failed: None {
            next_match_skip_break!(tokens, SLToken::BracketOpen(BracketType::Curly))?;
            let block = tokens.next_parse(super::parse_block)?;
            next_match_skip_break!(tokens, SLToken::BracketClose(BracketType::Curly))?;
            block
        }
    }
}

parse_rule! {
    fn parse_block(tokens) -> _matched: SLIRBlock, _failed: None {
        let mut exprs = vec![];

        while let Some(statement) = tokens.next_parse(statement::parse_statement)  {
            exprs.push(statement)
        }

        SLIRBlock(exprs)
    }
}

pub fn parse<'a>(tokens: Vec<SLToken<'a>>) -> Result<SLIRBlock, ()> {
    let mut tokens = Tokens::new(&tokens);

    tokens.next_parse(parse_block).ok_or(())
}

#[cfg(test)]
mod d {
    use crate::language::{parser::Tokens, tokenization::SLToken};

    #[test]
    fn parser_tokens_advancing_and_peeking() {
        let a = SLToken::Unknown("a");
        let b = SLToken::Unknown("b");
        let c = SLToken::Unknown("c");
        let space_soft = SLToken::Space { hard: false };
        let space_hard = SLToken::Space { hard: true };
        let vv = [a, space_soft, b, space_hard, c];
        let v = Tokens::new(&vv);

        let mut v0 = v;
        assert_eq!(v0.peek(), Some(&a));
        assert_eq!(v0.next(), Some(&a));
        assert_eq!(v0.next(), Some(&space_soft));
        assert_eq!(v0.next(), Some(&b));
        assert_eq!(v0.peek(), Some(&space_hard));

        let mut v1 = v;

        assert_eq!(v1.peek_skip_break(), Some(&a));
        assert_eq!(v1.next_skip_break(), Some(&a));
        //
        assert_eq!(v1.peek(), Some(&space_soft));
        assert_eq!(v1.peek_skip_break(), Some(&b));
        assert_eq!(v1.next_skip_break(), Some(&b));
        //
        assert_eq!(v1.peek(), Some(&space_hard));
        assert_eq!(v1.peek_skip_break(), Some(&c));
        assert_eq!(v1.next_skip_break(), Some(&c));
        //
        assert_eq!(v1.peek(), None);
        assert_eq!(v1.peek_skip_break(), None);

        let mut v2 = v;
        assert_eq!(v2.next_skip_soft_break(), Some(&a));
        assert_eq!(v2.peek_is_hard_break(), false);
        assert_eq!(v2.next_skip_soft_break(), Some(&b));
        assert_eq!(v2.peek_is_hard_break(), true);
        assert_eq!(v2.next_skip_soft_break(), Some(&space_hard));
        assert_eq!(v2.next_skip_soft_break(), Some(&c));
    }
}
