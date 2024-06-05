use crate::{
    front::tokenize::TSymbol,
    lint::diagnostic::{DiagnosticContent, ToDiagnostic},
};

use super::{
    ast::{
        ASTBlock, ASTDeclr, ASTDestructure, ASTExpr, ASTSourceFile, ASTStmt, ASTSubBlocked,
        ASTType, ASTTypedDestructure, ASTVarDeclare,
    },
    source::{HasLoc, Loc},
    tokenize::{TBracketType, TKeyword, Token, TokenContent},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ParseDiagnostic {
    // grouping syntax //
    ExpectedClosing,
    MissingClosing { expected: TBracketType },

    // code block syntax //
    ExpectedStmt,

    // subblock syntax //
    ExpectedElseBlock,
    ExpectedIfCondition,
    ExpectedIfBlock,
    ExpectedLoopBlock,
    ExpectedWhileCondition,
    ExpectedWhileBlock,
    ExpectedForBlock,
    ExpectedForIn,
    ExpectedForIterator,
    ExpectedForVar,

    // variable declaration/assignment syntax //
    ExpectedVarDefDestructure,
    ExpectedAssignValue,
    ExpectedDestructureTupleEnt,
    ExpectedDestructureTupleComma,

    // type syntax //
    ExpectedTemplateTypeEntry,
    ExpectedTemplateTypeAfterEq,
    TemplateOrderOrderedFoundAfterNamed,
}
impl ToDiagnostic for ParseDiagnostic {
    fn to_content(self) -> DiagnosticContent {
        DiagnosticContent::Parse(self)
    }
}

/// Bracket states with higher priority can terminate lower priority bracket states.
/// For example because `{` has higher priority than `(`, `{ ( } )` is treated like `{ () }`.
fn bracket_priority(which: TBracketType) -> u8 {
    match which {
        TBracketType::Curly => 3,
        TBracketType::Angle => 2,
        TBracketType::Paren => 1,
        TBracketType::Square => 0,
    }
}

enum HelperParenOrTuple<T> {
    Paren(Box<T>),
    Tuple(Vec<T>),
}

macro_rules! loc_of {
    (
        $has_loc_0: expr
        $(, $has_loc: expr)* $(,)?
    ) => {
        $has_loc_0.as_ref().map(HasLoc::loc)
        $(
            .or_else(|| $has_loc.as_ref().map(HasLoc::loc))
        )*
    };
}

struct Parser<'a, 'src> {
    /// Reference to the tokenized source code.
    tokens: &'a [Token<'src>],
    /// Index into [`Self::tokens`], points to what is returned upon the next call to [`Self::next_raw`].
    i: usize,
    /// A list of diagnostics that is produced by the parsing process  (syntax errors, mostly).
    diagnostics: Vec<(ParseDiagnostic, Loc)>,
    /// A stack containing saved copies of `(self.i, self.diagnostics.len())`.
    /// Not to be modified outside [`Self::guard_state`].
    state_stack: Vec<(usize, usize)>,
    /// A stack containing a list of all brackets we are inside of right now.
    bracket_stack: Vec<TBracketType>,
}
impl<'a, 'src> Parser<'a, 'src> {
    fn new(tokens: &'a [Token<'src>]) -> Self {
        Self {
            tokens,
            i: 0,
            diagnostics: Vec::new(),
            state_stack: Vec::new(),
            bracket_stack: Vec::new(),
        }
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////////////////
    //
    //   WHAT DOES `Option<AST...>` MEAN HERE?:
    //
    //     - If a parse function returns `None` it must have no side effects, including:
    //     >> "NONE means the region to match is NOT PRESENT".
    //        - no raising diagnostics
    //        - no stepping the token iterators (if stepping ocurred it must be rolled back)
    //
    //     - If a parse function returns `Some` that signifies a successful match.
    //     >> "SOME means the region IS PRESENT, but may contain recoverable errors"
    //        - invalid syntax is allowed up to tolerance within successfully matched ranges,
    //          and these regions are marked by raising syntax diagnostics and by subbing `None`
    //          into the fields of the valid outer AST node.
    //
    //

    ////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Iteration and state utilities
    //

    /// Consume only whitespace, stops before consuming any other kinds of tokens.
    fn consume_whitespace(&mut self) -> bool {
        let mut newline = false;
        while let Some(Token {
            content: TokenContent::Space { has_linebreak },
            ..
        }) = self.tokens.get(self.i)
        {
            newline |= has_linebreak;
            self.i += 1;
        }
        newline
    }
    /// Returns the next token, without skipping whitespace or stopping at closing brackets.
    fn next_simple(&mut self) -> Option<Token<'src>> {
        let token = *self.tokens.get(self.i)?;
        self.i += 1;
        Some(token)
    }
    /// Consume and return the next token, skipping whitespace and returning `None` if the end of the source or if
    /// a valid closing bracket is reached.
    /// Additionally, a boolean flag representing whether any newlines were consumed is returned alongside the token.
    ///
    /// NOTE: Will consume whitespace even if `None` is returned.
    fn next(&mut self) -> Option<(Token<'src>, bool)> {
        let mut newline = false;
        Some(loop {
            match self.tokens.get(self.i).copied()? {
                Token {
                    content: TokenContent::Space { has_linebreak },
                    ..
                } => {
                    newline |= has_linebreak;
                    self.i += 1;
                }
                Token {
                    content: TokenContent::Symbol(s),
                    ..
                } if s.as_bracket_close().is_some_and(|bracket_closing| {
                    let closing_bracket_priority = bracket_priority(bracket_closing);
                    self.bracket_stack
                        .iter()
                        .rev()
                        .copied()
                        .find_map(|b| {
                            if bracket_closing == b {
                                // matched closing brackes act like EOF
                                Some(true)
                            } else if closing_bracket_priority < bracket_priority(b) {
                                // weak closing brackets in a strong boundary (like the `)` in `{)}`) get consumed.
                                Some(false)
                            } else {
                                // strong closing brackets in weak boundary go on to outer ones until something hits.
                                None
                            }
                        })
                        .unwrap_or(false) // otherwise, just consume this, dont treat like EOF.
                }) =>
                {
                    return None
                }
                v => {
                    self.i += 1;
                    break (v, newline);
                }
            }
        })
    }

    /// Peeks the next token using [`Self::next`], and calls the given lambda on it.
    /// If either [`Self::next`] or the lambda return `None`, reverts state. Otherwise, returns the resultant value.
    ///
    /// ```
    /// let loc = super::source::Loc { start: 0, length: 0 };
    /// let tokens = [
    ///     TokenContent::Identifier("a"),
    ///     TokenContent::Space { has_linebreak: true },
    ///     TokenContent::Identifier("b"),
    ///     TokenContent::Space { has_linebreak: false },
    ///     TokenContent::Identifier("c"),
    ///     TokenContent::Bool(true),
    ///     TokenContent::Bool(false),
    /// ].map(|content| Token { loc, content });
    ///
    ///
    /// let mut iter = ParseIter::new(&tokens, 0);
    ///
    /// // match based on whether there's a leading newline
    /// let matcher_1: fn(_, _) -> _ = |t, n| match (t, n) {
    ///     (TokenContent::Identifier(name), true) => Some(name),
    ///     _ => None,
    /// };
    /// assert_eq!(iter.next_if_peek_and(matcher_1), None); // no match, does not consume
    /// let _ = iter.next_w(); // manually consume `a`
    /// assert_eq!(iter.next_if_peek_and(matcher_1), Some(("b", loc))); // matches and consumes `b`
    /// assert_eq!(iter.next_if_peek_and(matcher_1), None); // no match, does not consume
    /// let _ = iter.next_w(); // manually consume `c`
    ///
    /// // match based on token content
    /// let matcher_2: fn(TokenContent, _) -> _ = |t, _| match t {
    ///     TokenContent::Bool(true) => Some("matched!"),
    ///     TokenContent::Bool(false) => None,
    ///     _ => None,
    /// };
    /// assert_eq!(iter.next_if_peek_and(matcher_2), Some(("matched!", loc))); // consumes
    /// assert_eq!(iter.next_if_peek_and(matcher_2), None); // does not consume
    /// assert_eq!(iter.next(), Some(Token { loc, content: TokenContent::Bool(false) })); // final token is still there
    /// ```
    fn next_if_peek_and<T, F: Fn(TokenContent<'src>, bool) -> Option<T>>(
        &mut self,
        f: F,
    ) -> Option<(T, Loc)> {
        let i_prev = self.i;
        if let Some(v) = self
            .next()
            .and_then(|(token, newline)| f(token.content, newline).map(|v| (v, token.loc)))
        {
            Some(v)
        } else {
            self.i = i_prev;
            None
        }
    }
    /// Like [`Self::next_if_peek_and`], but it uses [`Self::next_simple`] instead of [`Self::next`].
    fn next_simple_if_peek_and<T, F: Fn(TokenContent<'src>) -> Option<T>>(
        &mut self,
        f: F,
    ) -> Option<(T, Loc)> {
        let i_prev = self.i;
        if let Some(v) = self
            .next_simple()
            .and_then(|token| f(token.content).map(|v| (v, token.loc)))
        {
            Some(v)
        } else {
            self.i = i_prev;
            None
        }
    }

    /// Consumes the next token if it matches the given content.
    fn next_if_eq(&mut self, target: TokenContent) -> Option<Loc> {
        let i_prev = self.i;
        if let Some(Token { loc, .. }) = self.next_simple().filter(|token| token.content == target)
        {
            Some(loc)
        } else {
            self.i = i_prev;
            None
        }
    }

    /// Reverts the state to as it was at the beginning of this call if the given function
    /// returns `None`, undoing all side effects (iterator state and raised diagnostics).
    #[inline]
    fn guard_state<T, F: Fn(&mut Self) -> Option<T>>(&mut self, f: F) -> Option<T> {
        self.state_stack.push((self.i, self.diagnostics.len()));
        let value = f(self);
        let popped_state = self
            .state_stack
            .pop()
            .expect("state_stack was drained! should not be possible");
        if value.is_none() {
            let (revert_i, revert_diagnostics_len) = popped_state;
            self.i = revert_i;
            self.diagnostics.resize_with(revert_diagnostics_len, || {
                unreachable!("somehow diagnostics were drained");
            });
        }
        value
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Helper parsers
    //

    /// Parse a pair of brackets, like `{ ... }` or `( ... )`.
    ///
    /// Returns `None` if and only if the next token is not the specified opening bracket. Otherwise
    /// it returns `Some(parse_contents(self))`.
    ///
    /// ## Diagnostics:
    /// - Contents parser is expected to consume the entire contents, progressing until [`Self::next`]
    /// returns `None`. If tokens remain before the closing bracket, they are consumed and
    /// [`ParseDiagnostic::ExpectedClosing`] is raised.
    /// - If the closing bracket mismatches what was expected, [`ParseDiagnostic::MissingClosing`] is raised.
    fn parse_brackets<T, F: Fn(&mut Self) -> T>(
        &mut self,
        bracket_ty: TBracketType,
        allow_leading_newline: bool,
        parse_contents: F,
    ) -> Option<(T, Loc)> {
        // try to match opening bracket and short to None if not found.
        let loc_open = self
            .next_if_peek_and(|token, newline| match token {
                _ if newline && !allow_leading_newline => None,
                TokenContent::Symbol(s) if s.as_bracket_open() == Some(bracket_ty) => Some(()),
                _ => None,
            })?
            .1;
        // push state
        self.bracket_stack.push(bracket_ty);
        // eval content parser
        let value = parse_contents(self);

        // match any extra junk and consume extra whitespace
        self.consume_whitespace();
        let i0 = self.i;
        while self.next().is_some() {}
        if self.i > i0 {
            let loc = self.tokens[i0].loc.merge(self.tokens[self.i - 1].loc);
            self.raise(ParseDiagnostic::ExpectedClosing, loc)
        }

        // consume closing paren if correct, else raise diagnostic
        let loc_close = if let Some((_, loc)) = self.next_simple_if_peek_and(|token| match token {
            TokenContent::Symbol(s) if s.as_bracket_close() == Some(bracket_ty) => Some(()),
            _ => None,
        }) {
            loc
        } else {
            let loc = self.tokens[self.i - 1].loc;
            self.raise(
                ParseDiagnostic::MissingClosing {
                    expected: bracket_ty,
                },
                Loc {
                    start: loc.start + loc.length,
                    length: 0,
                },
            );
            loc
        };

        // pop state and return
        self.bracket_stack.pop();
        Some((value, loc_open.merge(loc_close)))
    }

    /// Parses `T`s until it hits EOF or closing bracket. Sequences that don't match will be
    /// raised as the given [`ParseDiagnostic`]
    fn parse_list<T, F: Fn(&mut Self) -> Option<T>>(
        &mut self,
        parse_contents: F,
        fail_diagnostic: &'static ParseDiagnostic,
    ) -> Vec<T> {
        let mut out = Vec::new();
        let mut failing: Option<(Loc, Loc)> = None;
        loop {
            if let Some(matched) = parse_contents(self) {
                out.push(matched);
                if let Some((start, end)) = failing.take() {
                    self.raise(*fail_diagnostic, start.merge(end));
                }
            } else {
                match self.next() {
                    None => {
                        if let Some((start, end)) = failing {
                            self.raise(*fail_diagnostic, start.merge(end));
                        }
                        break;
                    }
                    Some((Token { loc, .. }, _)) => {
                        if let Some((_, end)) = &mut failing {
                            *end = loc;
                        } else {
                            failing = Some((loc, loc));
                        }
                    }
                }
            }
        }
        out
    }

    #[inline]
    fn raise(&mut self, diagnostic: ParseDiagnostic, loc: Loc) {
        self.diagnostics.push((diagnostic, loc));
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Language parsers
    //

    fn parse_source_file(&mut self) -> ASTSourceFile<'src> {
        ASTSourceFile {
            body: self.parse_stmt_list(),
        }
    }

    fn parse_stmt(&mut self) -> Option<ASTStmt<'src>> {
        if let Some(expr) = self.parse_expr() {
            if let Some(assign_symbol_loc) = self.next_if_eq(TokenContent::Symbol(TSymbol::Assign))
            {
                // ASTStmt::VarAssign //
                let rhs = self.parse_expr();
                if rhs.is_none() {
                    self.raise(
                        ParseDiagnostic::ExpectedAssignValue,
                        expr.loc().merge(assign_symbol_loc),
                    );
                }
                Some(ASTStmt::VarAssign {
                    loc: expr
                        .loc()
                        .merge(rhs.as_ref().map(|it| it.loc()).unwrap_or(assign_symbol_loc)),
                    lhs: Some(Box::new(expr)),
                    rhs: rhs.map(Box::new),
                })
            } else {
                // ASTStmt::Expr //
                Some(ASTStmt::Expr(expr))
            }
        } else if let Some(subblocked) = self.parse_subblocked() {
            // ASTStmt::SubBlocked //
            Some(ASTStmt::SubBlocked(subblocked))
        } else if let Some(var_declare) = self.parse_var_declare() {
            // ASTStmt::VarDeclare //
            Some(ASTStmt::VarDeclare(var_declare))
        } else if let Some(static_declare) = self.parse_static_declaration() {
            // ASTStmt::StaticDeclare //
            Some(ASTStmt::StaticDeclare(Box::new(static_declare)))
        } else {
            // no match
            None
        }
    }
    fn parse_stmt_list(&mut self) -> Vec<ASTStmt<'src>> {
        self.parse_list(Self::parse_stmt, &ParseDiagnostic::ExpectedStmt)
    }
    fn parse_block(&mut self) -> Option<ASTBlock<'src>> {
        self.parse_brackets(TBracketType::Curly, true, |p| p.parse_stmt_list())
            .map(|(body, loc)| ASTBlock { loc, body })
    }

    #[inline]
    fn _parse_expr_generic<const BEFORE_BLOCK: bool>(&mut self) -> Option<ASTExpr<'src>> {
        todo!();
    }
    fn parse_expr_before_block(&mut self) -> Option<ASTExpr<'src>> {
        self._parse_expr_generic::<true>()
    }
    fn parse_expr(&mut self) -> Option<ASTExpr<'src>> {
        self._parse_expr_generic::<false>()
    }

    fn parse_type(&mut self) -> Option<ASTType<'src>> {
        let mut current = self._parse_type_no_postfix()?;
        loop {
            if let Some((name, loc_name)) = self.guard_state(|p| {
                p.next_if_eq(TokenContent::Symbol(TSymbol::Period))?;
                p.parse_ident()
            }) {
                // ASTType::Access //
                current = ASTType::Access {
                    loc_name,
                    name,
                    inner: Box::new(current),
                };
            } else if let Some(((params, named_params), loc)) =
                self.parse_brackets(TBracketType::Angle, true, |p| {
                    // ASTType::TypeParam //
                    let s = p.parse_list(
                        |p| {
                            let key = p.guard_state(|p| {
                                let (key, loc_key) = p.parse_ident()?;
                                let loc_eq = p.next_if_eq(TokenContent::Symbol(TSymbol::Assign))?;
                                Some((key, loc_key, loc_eq))
                            });
                            let ty = p.parse_type();

                            match (key, ty) {
                                (None, None) => None,
                                (Some((_, loc_key, loc_eq)), None) => {
                                    p.raise(
                                        ParseDiagnostic::ExpectedTemplateTypeAfterEq,
                                        loc_key.merge(loc_eq),
                                    );
                                    Some((key, None))
                                }
                                (key, Some(ty)) => Some((key, Some(ty))),
                            }
                        },
                        &ParseDiagnostic::ExpectedTemplateTypeEntry,
                    );
                    let mut params = Vec::new();
                    let mut named_params = None;

                    for (key, ty) in s {
                        if let Some((key, loc_key, _)) = key {
                            named_params
                                .get_or_insert_with(Vec::new)
                                .push(((key, loc_key), ty));
                        } else {
                            let ty = ty.unwrap();
                            if named_params.is_some() {
                                p.raise(
                                    ParseDiagnostic::TemplateOrderOrderedFoundAfterNamed,
                                    ty.loc(),
                                );
                            }
                            params.push(ty);
                        }
                    }

                    (params, named_params)
                })
            {
                current = ASTType::TypeParam {
                    loc,
                    inner: Box::new(current),
                    params,
                    named_params,
                };
            } else {
                break;
            }
        }
        Some(current)
    }
    fn _parse_type_no_postfix(&mut self) -> Option<ASTType<'src>> {
        if let Some((name, loc)) = self.parse_ident() {
            // ASTType::Named //
            Some(ASTType::Named { name, loc })
        } else if let Some((inner, loc)) = self.parse_paren_or_tuple(|p| p.parse_type()) {
            Some(match inner {
                // ASTType::Paren //
                HelperParenOrTuple::Paren(inner) => ASTType::Paren { loc, inner },
                // ASTType::Tuple //
                HelperParenOrTuple::Tuple(inner) => ASTType::Tuple { loc, inner },
            })
        } else {
            None
        }
    }

    fn parse_static_declaration(&mut self) -> Option<ASTDeclr<'src>> {
        // ASTDeclr::Function //
        // ASTDeclr::Const //
        // ASTDeclr::Trait //
        // ASTDeclr::Data //
        // ASTDeclr::FreeImpl //
        // ASTDeclr::Import //
        todo!()
    }

    fn parse_subblocked(&mut self) -> Option<ASTSubBlocked<'src>> {
        if let Some(block) = self.parse_block() {
            // ASTSubBlocked::Block //
            Some(ASTSubBlocked::Block { block })
        } else if let Some((keyword, loc_start)) = self.next_if_peek_and(|token, _| match token {
            TokenContent::Keyword(keyword)
                if matches!(
                    keyword,
                    TKeyword::If | TKeyword::Loop | TKeyword::While | TKeyword::For
                ) =>
            {
                Some(keyword)
            }
            _ => None,
        }) {
            match keyword {
                TKeyword::If => {
                    // ASTSubBlocked::If //

                    let condition = self.parse_expr_before_block();
                    let block = self.parse_block();
                    if condition.is_none() {
                        self.raise(ParseDiagnostic::ExpectedIfCondition, loc_start)
                    }
                    if block.is_none() {
                        self.raise(
                            ParseDiagnostic::ExpectedIfBlock,
                            loc_start.merge_some(loc_of!(condition)),
                        )
                    }
                    let mut loc_end = loc_of!(block, condition).unwrap_or(loc_start);
                    let mut elifs = None;
                    let mut else_block = None;

                    while self._parse_subblocked_elseif(&mut elifs, &mut else_block, &mut loc_end) {
                    }

                    Some(ASTSubBlocked::If {
                        loc: loc_start.merge(loc_end),
                        condition: condition.map(Box::new),
                        block,
                        elifs,
                        else_block,
                    })
                }
                TKeyword::Loop => {
                    // ASTSubBlocked::Loop //

                    let block = self.parse_block();
                    if block.is_none() {
                        self.raise(ParseDiagnostic::ExpectedLoopBlock, loc_start)
                    }

                    Some(ASTSubBlocked::Loop {
                        loc: loc_start.merge_some(loc_of!(block)),
                        block,
                    })
                }
                TKeyword::While => {
                    // ASTSubBlocked::While //

                    let condition = self.parse_expr_before_block();
                    let block = self.parse_block();
                    if condition.is_none() {
                        self.raise(ParseDiagnostic::ExpectedWhileCondition, loc_start)
                    }
                    if block.is_none() {
                        self.raise(
                            ParseDiagnostic::ExpectedWhileBlock,
                            loc_start.merge_some(loc_of!(condition)),
                        )
                    }
                    let mut loc_end = loc_of!(block, condition).unwrap_or(loc_start);
                    let else_block = self._parse_subblocked_else(&mut loc_end);

                    Some(ASTSubBlocked::While {
                        loc: loc_start.merge(loc_end),
                        condition: condition.map(Box::new),
                        block,
                        else_block,
                    })
                }
                TKeyword::For => {
                    // ASTSubBlocked::For //

                    let var = self.parse_typed_destructure();
                    let keyword_in = self.next_if_eq(TokenContent::Keyword(TKeyword::In));
                    let iterator = self.parse_expr_before_block();
                    let block = self.parse_block();

                    if var.is_none() {
                        self.raise(ParseDiagnostic::ExpectedForVar, loc_start)
                    }
                    if keyword_in.is_none() {
                        self.raise(
                            ParseDiagnostic::ExpectedForIn,
                            loc_start.merge_some(loc_of!(var)),
                        )
                    }
                    if iterator.is_none() {
                        self.raise(
                            ParseDiagnostic::ExpectedForIterator,
                            loc_start.merge_some(loc_of!(keyword_in, var)),
                        )
                    }
                    if block.is_none() {
                        self.raise(
                            ParseDiagnostic::ExpectedForBlock,
                            loc_start.merge_some(loc_of!(iterator, keyword_in, var)),
                        )
                    }

                    let mut loc_end = loc_of!(block, iterator, var)
                        .or(keyword_in)
                        .unwrap_or(loc_start);
                    let else_block = self._parse_subblocked_else(&mut loc_end);

                    Some(ASTSubBlocked::For {
                        loc: loc_start.merge(loc_end),
                        var,
                        iterator: iterator.map(Box::new),
                        block,
                        else_block,
                    })
                }
                _ => unreachable!(),
            }
        } else {
            None
        }
    }
    fn _parse_subblocked_elseif(
        &mut self,
        elifs: &mut Option<Vec<(Option<Box<ASTExpr<'src>>>, Option<ASTBlock<'src>>)>>,
        else_block: &mut Option<ASTBlock<'src>>,
        loc_end: &mut Loc,
    ) -> bool {
        let loc_else =
            if let Some(loc_else) = self.next_if_eq(TokenContent::Keyword(TKeyword::Else)) {
                loc_else
            } else {
                return false;
            };

        if let Some(loc_if) = self.next_if_eq(TokenContent::Keyword(TKeyword::If)) {
            // matching `else if ...`
            let condition = self.parse_expr_before_block();
            let block = self.parse_block();
            if condition.is_none() {
                self.raise(ParseDiagnostic::ExpectedIfCondition, loc_if)
            }
            if block.is_none() {
                self.raise(
                    ParseDiagnostic::ExpectedIfBlock,
                    loc_if.merge_some(loc_of!(condition)),
                )
            }
            *loc_end = loc_of!(block, condition).unwrap_or(loc_if);
            elifs
                .get_or_insert_with(Vec::new)
                .push((condition.map(Box::new), block));
            true
        } else {
            // matching `else ...`
            *else_block = self.parse_block();
            if let Some(else_block) = else_block {
                *loc_end = else_block.loc;
            } else {
                self.raise(ParseDiagnostic::ExpectedElseBlock, loc_else);
                *loc_end = loc_else;
            }
            false
        }
    }
    fn _parse_subblocked_else(&mut self, loc_end: &mut Loc) -> Option<ASTBlock<'src>> {
        let loc_else = self.next_if_eq(TokenContent::Keyword(TKeyword::Else))?;

        let else_block = self.parse_block();
        if let Some(else_block) = &else_block {
            *loc_end = else_block.loc;
        } else {
            self.raise(ParseDiagnostic::ExpectedElseBlock, loc_else);
            *loc_end = loc_else;
        }
        else_block
    }

    fn parse_var_declare(&mut self) -> Option<ASTVarDeclare<'src>> {
        let loc_start = self.next_if_eq(TokenContent::Keyword(TKeyword::Let))?;

        let declr = self.parse_typed_destructure();
        if declr.is_none() {
            self.raise(ParseDiagnostic::ExpectedVarDefDestructure, loc_start);
        }
        let (initializer, loc_assign_symbol) = if let Some(loc_assign_symbol) =
            self.next_if_eq(TokenContent::Symbol(TSymbol::Assign))
        {
            let expr = self.parse_expr();
            if expr.is_none() {
                self.raise(ParseDiagnostic::ExpectedAssignValue, loc_assign_symbol);
            }
            (expr, Some(loc_assign_symbol))
        } else {
            (None, None)
        };

        Some(ASTVarDeclare {
            loc: loc_start.merge_some(loc_of!(initializer, loc_assign_symbol, declr)),
            declr,
            initializer: initializer.map(Box::new),
        })
    }

    fn parse_typed_destructure(&mut self) -> Option<ASTTypedDestructure<'src>> {
        Some(ASTTypedDestructure {
            destructure: self.parse_destructure()?,
            ty: self.parse_type(),
        })
    }
    fn parse_destructure(&mut self) -> Option<ASTDestructure<'src>> {
        if let Some((name, loc)) = self.parse_ident() {
            Some(ASTDestructure::Name { loc, name })
        } else if let Some((inner, loc)) = self.parse_paren_or_tuple(|p| p.parse_destructure()) {
            Some(match inner {
                HelperParenOrTuple::Paren(inner) => ASTDestructure::Paren { loc, inner },
                HelperParenOrTuple::Tuple(inner) => ASTDestructure::Tuple { loc, inner },
            })
        } else {
            None
        }
    }

    /// A special parser program for expressions of the form `(a,b,c)` (tuples) or `(x)` (simple grouping).
    /// This warranted a special case because it is just so long and convoluted lmao.
    fn parse_paren_or_tuple<T: HasLoc, F: Fn(&mut Self) -> Option<T>>(
        &mut self,
        parse_contents: F,
    ) -> Option<(HelperParenOrTuple<T>, Loc)> {
        self.guard_state(|p| {
            p.parse_brackets(TBracketType::Paren, true, |p| {
                let first = parse_contents(p)?;
                Some(
                    if p.next_if_eq(TokenContent::Symbol(TSymbol::Comma)).is_some() {
                        // Tuple //
                        let list = p.parse_list(
                            |p| {
                                Some((
                                    parse_contents(p)?,
                                    p.next_if_eq(TokenContent::Symbol(TSymbol::Comma)).is_some(),
                                ))
                            },
                            &ParseDiagnostic::ExpectedDestructureTupleEnt,
                        );
                        let len = list.len();
                        HelperParenOrTuple::Tuple(
                            list.into_iter()
                                .enumerate()
                                .map(|(i, (inner, had_comma))| {
                                    if !had_comma && i != len - 1 {
                                        p.raise(
                                            ParseDiagnostic::ExpectedDestructureTupleComma,
                                            inner.loc().new_from_end(),
                                        )
                                    }
                                    inner
                                })
                                .collect(),
                        )
                    } else {
                        // Paren //
                        HelperParenOrTuple::Paren(Box::new(first))
                    },
                )
            })
            .and_then(|(it, loc)| Some((it?, loc)))
        })
    }

    fn parse_ident(&mut self) -> Option<(&'src str, Loc)> {
        self.next_if_peek_and(|token, _| match token {
            TokenContent::Identifier(ident) => Some(ident),
            _ => None,
        })
    }
}

pub fn parse_from_source<'src>(src: &'src str) -> ASTSourceFile<'src> {
    Parser::new(&super::tokenize::tokenize(src)).parse_source_file()
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Testing

#[cfg(test)]
mod test {
    use crate::front::{
        parse::{ParseDiagnostic, Parser},
        source::Loc,
        tokenize::{tokenize, TBracketType, TokenContent},
    };

    #[test]
    fn mismatched_brackets_automatically_close() {
        let tokens = tokenize("{ ( 3 }");
        let mut p = Parser::new(&tokens);
        assert_eq!(
            p.parse_brackets(TBracketType::Curly, true, |p| {
                p.parse_brackets(TBracketType::Paren, true, |p| {
                    p.next().map(|it| it.0.content)
                })
                .map(|it| it.0)
            })
            .map(|it| it.0),
            Some(Some(Some(TokenContent::Number {
                decimal: false,
                exp: false,
                base: 10,
                unparsed: "3"
            })))
        );
        assert_eq!(
            p.diagnostics.as_slice(),
            [(
                ParseDiagnostic::MissingClosing {
                    expected: TBracketType::Paren
                },
                Loc {
                    start: 6,
                    length: 0
                }
            )]
            .as_slice()
        );
    }
}
