use std::num::IntErrorKind;

use crate::{
    front::{
        ast::{ASTEnumVariant, ASTLiteral},
        tokenize::{TInfixOperatorType, TPostfixOperatorType, TPrefixOperatorType, TSymbol},
    },
    lint::diagnostic::{DiagnosticContent, DiagnosticId, Diagnostics, Fallible, ToDiagnostic},
    middle::statics::scopes::{ScopeId, Scopes},
};

use super::{
    ast::{
        ASTAnnot, ASTBlock, ASTConst, ASTData, ASTDataContents, ASTDataProperty, ASTDeclr,
        ASTDestructure, ASTDoc, ASTEnumVariantType, ASTExpr, ASTFreeImpl, ASTFunction, ASTIdent,
        ASTIdentValue, ASTImpl, ASTImplContents, ASTImport, ASTImportTree, ASTLambda,
        ASTMacroInvocation, ASTMacroInvocationBody, ASTName, ASTPostfixBlock, ASTScope,
        ASTSourceFile, ASTStmt, ASTSubBlocked, ASTTemplateBound, ASTTemplates, ASTTrait,
        ASTTraitConst, ASTTraitTypeAlias, ASTType, ASTTypeAlias, ASTTypedDestructure,
        ASTVarDeclare,
    },
    source::{HasLoc, Loc, SourceFileId},
    tokenize::{TBracketType, TKeyword, Token, TokenContent},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ParseDiagnostic {
    // comment syntax //
    CommentMissingClosing,

    // expr syntax //
    IntOverflowPos,
    IntOverflowNeg,
    StringMissingClosing,
    StringIllegalInterpolation,
    ExpectedExpr,
    ExpectedFunctionCallArgument,
    ExpectedIndexCallArgument,
    ExpectedPostfixAfterDot,
    ExpectedValueStructInit,
    ExpectedValueTupleInit,
    ExpectedValueStructInitAssign,
    ExpectedValueStructInitValue,
    UnexpectedOperator,

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
    ExpectedDestructure,
    ExpectedDestructureInner,
    ExpectedAssignValue,
    ExpectedTupleEntry,
    ExpectedConstName,

    // type syntax //
    ExpectedTypeAfterColon,
    ExpectedTypeInner,
    ExpectedTemplateTypeEntry,
    ExpectedTemplateTypeAfterEq,
    TemplateOrderOrderedFoundAfterNamed,
    ExpectedBound,
    ExpectedBoundEntry,
    ExpectedBoundUnionSep,

    // import syntax //
    ExpectedImportTreeBase,
    ExpectedImportTreeInner,
    ExpectedImportTreeAfterDot,

    // function syntax //
    ExpectedLambdaArgument,
    ExpectedLambdaBody,
    ExpectedFunctionName,
    ExpectedFunctionArgument,
    ExpectedFunctionArgumentList,
    ExpectedFunctionBodyExprAfterEq,

    // trait/data/impl syntax //
    ExpectedTraitName,
    ExpectedTraitBody,
    ExpectedDataName,
    ExpectedDataBody,
    ExpectedDataStructBody,
    ExpectedDataProperty,
    ExpectedDataTupleBody,
    ExpectedDataTupleTypeEntry,
    ExpectedImplTargetAfterFor,
    ExpectedImplTarget,
    ExpectedImplContents,
    ExpectedImplContentEntry,
    ExpectedTypealiasName,
    ExpectedTypealiasValue,
    ExpectedTraitConstType,

    // macro invocation //
    ExpectedMacroName,
    ExpectedMacroBody,
    NonexistentMacro,
    MacroPatternFailure,
    IllegalCoreOverride,
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
        TBracketType::LambdaArgs => 3,
        TBracketType::Curly => 4,
        TBracketType::Angle => 2,
        TBracketType::Paren => 1,
        TBracketType::Square => 0,
    }
}

enum HelperParenOrTuple<T> {
    Paren(Fallible<Box<T>>),
    Tuple(Vec<Fallible<T>>),
}
enum AorB<A, B> {
    A(A),
    B(B),
}

macro_rules! merge {
    ($x_0:expr $(, $x:expr )* $(,)?) => {
        $x_0
        $( .or_else(|| $x) )*
    };
}
macro_rules! loco {
    ($x:expr) => {
        $x.as_ref().map(HasLoc::loc)
    };
}
macro_rules! locr {
    ($x:expr) => {
        $x.as_ref().ok().map(HasLoc::loc)
    };
}

struct Parser<'a, 'src> {
    /// Reference to the tokenized source code.
    tokens: &'a [Token<'src>],
    /// Index into [`Self::tokens`], points to what is returned upon the next call to [`Self::next_raw`].
    i: usize,

    diagnostics: &'a mut Diagnostics,
    scopes: &'a mut Scopes<ASTScope<'src>>,
    /// Stores all the ids for stacks currently in scope.
    scope_stack: Vec<ScopeId>,

    /// A stack containing saved copies of `(self.i, self.diagnostics.len())`.
    /// Not to be modified outside [`Self::guard_state`].
    state_stack: Vec<(usize, usize)>,
    /// A stack containing a list of all brackets we are inside of right now.
    bracket_stack: Vec<TBracketType>,
}
impl<'a, 'src> Parser<'a, 'src> {
    fn new(
        tokens: &'a [Token<'src>],
        diagnostics: &'a mut Diagnostics,
        scopes: &'a mut Scopes<ASTScope<'src>>,
    ) -> Self {
        Self {
            tokens,
            i: 0,
            diagnostics,
            scopes,
            scope_stack: Vec::new(),
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
        loop {
            match self.tokens.get(self.i) {
                Some(Token {
                    content: TokenContent::Space { has_linebreak },
                    ..
                }) => {
                    newline |= has_linebreak;
                    self.i += 1;
                }
                Some(Token {
                    content:
                        TokenContent::Comment {
                            missing_closing, ..
                        },
                    loc,
                }) => {
                    self.i += 1;

                    if *missing_closing {
                        // we are consuming here, so this error must be reported
                        self.raise(ParseDiagnostic::CommentMissingClosing, *loc);
                    }
                }
                _ => break,
            }
        }
        newline
    }
    /// Returns the next token, without skipping whitespace or stopping at closing brackets.
    ///
    /// Should not be called without consuming whitespace beforehand, as it does not handle unclosed comments.
    fn next_simple(&mut self) -> Option<Token<'src>> {
        let token = *self.tokens.get(self.i)?;
        self.i += 1;
        Some(token)
    }
    /// Consume and return the next token, skipping whitespace/comments and returning `None` if the end of the source or if
    /// a valid closing bracket is reached.
    /// Additionally, a boolean flag representing whether any newlines were consumed is returned alongside the token.
    fn next(&mut self) -> Option<(Token<'src>, bool)> {
        self.guard_state(Self::_next_internal)
    }
    fn _next_internal(&mut self) -> Option<(Token<'src>, bool)> {
        let mut newline = false;
        Some(loop {
            match self.tokens.get(self.i).copied()? {
                Token {
                    content:
                        TokenContent::Comment {
                            missing_closing, ..
                        },
                    loc,
                } => {
                    // skip comments
                    self.i += 1;

                    if missing_closing {
                        // we are consuming here, so this error must be reported
                        self.raise(ParseDiagnostic::CommentMissingClosing, loc);
                    }
                }
                Token {
                    content: TokenContent::Space { has_linebreak },
                    ..
                } => {
                    // skip whitespace
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
    /// ```ignore
    /// let loc = Loc { start: 0, length: 0 };
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
        self.guard_state(|p| {
            p._next_internal()
                .and_then(|(token, newline)| f(token.content, newline).map(|v| (v, token.loc)))
        })
    }
    /// Like [`Self::next_if_peek_and`], but it uses [`Self::next_simple`] instead of [`Self::next`].
    ///
    /// Should not be called without consuming whitespace beforehand, as it does not handle unclosed comments.
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
        self.guard_state(|p| {
            p._next_internal()
                .filter(|(token, _)| token.content == target)
                .map(|(Token { loc, .. }, _)| loc)
        })
    }

    /// Peeks ahead to see if there is a linebreak coming up before the next non-empty token.
    fn peek_has_linebreak(&mut self) -> bool {
        let mut has_newline = false;
        let mut i = self.i;
        while let Some(newline) = self
            .tokens
            .get(i)
            .and_then(|Token { content, .. }| match content {
                TokenContent::Comment { content, .. } => Some(content.contains('\n')),
                TokenContent::Space { has_linebreak } => Some(*has_linebreak),
                _ => None,
            })
        {
            has_newline |= newline;
            i += 1;
        }
        has_newline
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
            self.diagnostics.resize_shrink(revert_diagnostics_len);
        }
        value
    }
    /// Reverts the state to as it was at the beginning of this call if the given function
    /// returns `None`, undoing all side effects (iterator state and raised diagnostics).
    #[inline]
    fn peek<T, F: Fn(&mut Self) -> T>(&mut self, f: F) -> T {
        self.state_stack.push((self.i, self.diagnostics.len()));
        let value = f(self);
        let popped_state = self
            .state_stack
            .pop()
            .expect("state_stack was drained! should not be possible");
        let (revert_i, revert_diagnostics_len) = popped_state;
        self.i = revert_i;
        self.diagnostics.resize_shrink(revert_diagnostics_len);
        value
    }

    /// Creates a new empty scope for tracking statics, with the parent scope set
    /// to the current scope, and makes it the current scope while the given callback
    /// is running.
    #[inline]
    fn guard_scope<T, F: Fn(&mut Parser<'a, 'src>, ScopeId) -> T>(&mut self, f: F) -> T {
        let scope_id = self.new_empty_scope();

        self.scope_stack.push(scope_id);
        let res = f(self, scope_id);
        self.scope_stack.pop();

        res
    }
    /// Creates a new empty scope for tracking statics, with the parent scope set
    /// to the current scope. Does not add this scope to the scope stack.
    fn new_empty_scope(&mut self) -> ScopeId {
        let current_innermost_scope = self.scope_stack.last().copied();
        self.scopes.register(current_innermost_scope)
    }
    /// Get the current scope.
    ///
    /// Panics:
    /// - If there are no scopes currently present. (This should only be the case when calling [`Self::parse_source_file`])
    fn current_scope(&mut self) -> ScopeId {
        *self.scope_stack.last().unwrap()
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
            self.raise(ParseDiagnostic::ExpectedClosing, loc);
        }
        self.consume_whitespace();

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
                loc.new_from_end(),
            );
            loc
        };

        // pop state and return
        self.bracket_stack.pop();
        Some((value, loc_open.merge(loc_close)))
    }

    fn parse_either<A, B, FA: Fn(&mut Self) -> Option<A>, FB: Fn(&mut Self) -> Option<B>>(
        &mut self,
        parse_a: FA,
        parse_b: FB,
    ) -> Option<AorB<A, B>> {
        if let Some(a) = parse_a(self) {
            Some(AorB::A(a))
        } else if let Some(b) = parse_b(self) {
            Some(AorB::B(b))
        } else {
            None
        }
    }

    /// Parses the inner parser, marking all tokens that it didnt match beforehand as invalid.
    ///
    /// NOTE: doesnt abide by the `None` means no side effects rule.
    fn fail_until_parse<T, F: Fn(&mut Self) -> Option<T>>(
        &mut self,
        parse_inner: F,
        fail_diagnostic: &'static ParseDiagnostic,
    ) -> Option<T> {
        if let Some(res) = parse_inner(self) {
            return Some(res);
        }
        let loc_start = self.next()?.0.loc;
        let mut loc_end = loc_start;
        let res = loop {
            if let Some(res) = parse_inner(self) {
                break res;
            }
            loc_end = self.next()?.0.loc;
        };
        self.raise(*fail_diagnostic, loc_start.merge(loc_end));
        Some(res)
    }
    /// Parses the inner parser, marking all tokens that it didnt match beforehand as invalid.
    ///
    /// If this matches nothing, it will return `Err()`, if the remaining sequence in the region is empty it will panic.
    fn fail_until_parse_nonempty<T, F: Fn(&mut Self) -> Option<T>>(
        &mut self,
        parse_inner: F,
        fail_diagnostic: &'static ParseDiagnostic,
    ) -> Fallible<T> {
        if let Some(res) = parse_inner(self) {
            return Ok(res);
        }
        let loc_start = self
            .next()
            .expect("fail_until_parse_nonempty used in empty region")
            .0
            .loc;
        let mut loc_end = loc_start;
        let res = loop {
            if let Some(res) = parse_inner(self) {
                break res;
            }
            loc_end = self
                .next()
                .ok_or_else(|| self.raise(*fail_diagnostic, loc_start.merge(loc_end)))?
                .0
                .loc;
        };
        self.raise(*fail_diagnostic, loc_start.merge(loc_end));
        Ok(res)
    }

    /// Parses `T`s until it hits EOF or closing bracket. Sequences that don't match will be
    /// raised as the given [`ParseDiagnostic`]
    fn parse_list<T, F: Fn(&mut Self) -> Option<T>>(
        &mut self,
        parse_inner: F,
        fail_diagnostic: &'static ParseDiagnostic,
    ) -> Vec<T> {
        let mut out = Vec::new();
        while let Some(inner) = self.fail_until_parse(|p| parse_inner(p), fail_diagnostic) {
            out.push(inner)
        }
        out
    }
    fn parse_list_require_sep<T: HasLoc, F: Fn(&mut Self) -> Option<T>>(
        &mut self,
        parse_inner: F,
        separator: TokenContent<'src>,
        ent_fail_diagnostic: &'static ParseDiagnostic,
        sep_fail_diagnostic: &'static ParseDiagnostic,
        loc_prev_in: Loc,
    ) -> (Vec<T>, Loc) {
        let mut out = Vec::new();
        let mut expecting_sep = false;
        let mut loc_prev: Loc = loc_prev_in;
        loop {
            if expecting_sep {
                // expecting separator
                if let Some(inner) = parse_inner(self) {
                    // found entry
                    expecting_sep = true;
                    let loc = inner.loc();
                    self.raise(
                        *sep_fail_diagnostic,
                        loc_prev.new_from_end().merge(loc.new_from_start()),
                    );
                    loc_prev = loc;
                    out.push(inner);
                } else if let Some(loc_sep) =
                    self.fail_until_parse(|p| p.next_if_eq(separator), sep_fail_diagnostic)
                {
                    // found separator
                    expecting_sep = false;
                    loc_prev = loc_sep;
                } else {
                    break;
                }
            } else {
                // expecting entry
                if let Some(inner) = self.fail_until_parse(
                    |p| p.parse_either(|p| parse_inner(p), |p| p.next_if_eq(separator)),
                    ent_fail_diagnostic,
                ) {
                    match inner {
                        AorB::A(inner) => {
                            // found entry
                            loc_prev = inner.loc();
                            out.push(inner);
                            expecting_sep = true;
                        }
                        AorB::B(loc_sep) => {
                            // found sep
                            expecting_sep = false;

                            self.raise(
                                *ent_fail_diagnostic,
                                loc_prev.new_from_end().merge(loc_sep.new_from_start()),
                            );
                            loc_prev = loc_sep;
                        }
                    }
                } else {
                    // nothing matched, raise an error anyway- for gits and shiggles, ofc.
                    self.raise(*ent_fail_diagnostic, loc_prev.new_from_end());
                    break;
                }
            }
        }

        (out, loc_prev_in.new_from_end().merge(loc_prev))
    }

    /// This gets its own function because it's so common.
    fn parse_list_commasep<T, F: Fn(&mut Self) -> Option<T>>(
        &mut self,
        parse_inner: F,
        fail_diagnostic: &'static ParseDiagnostic,
    ) -> Vec<Fallible<T>> {
        let mut out = Vec::new();
        let mut expecting_comma = false;
        for aorb in self.parse_list(
            |p| {
                p.parse_either(
                    |p| parse_inner(p),
                    |p| p.next_if_eq(TokenContent::Symbol(TSymbol::Comma)),
                )
            },
            fail_diagnostic,
        ) {
            match (aorb, expecting_comma) {
                (AorB::A(inner), _) => {
                    // found inner
                    // .. always ok
                    // missing commas are ignored, as commas are optional
                    expecting_comma = true;
                    out.push(Ok(inner))
                }
                (AorB::B(_), true) => {
                    // expected comma found comma
                    // .. all is well in the world
                    expecting_comma = false;
                }
                (AorB::B(loc_comma), false) => {
                    // expected inner found comma
                    // !! this is not allowed and the writer should feel bad
                    out.push(Err(self.raise(*fail_diagnostic, loc_comma.new_from_start())));
                }
            }
        }

        out
    }

    #[inline]
    fn raise(&mut self, diagnostic: ParseDiagnostic, loc: Loc) -> DiagnosticId {
        self.diagnostics.raise(diagnostic, loc)
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Language parsers
    //

    fn parse_source_file(&mut self) -> ASTSourceFile<'src> {
        self.guard_scope(|p, scope| ASTSourceFile {
            body: p.parse_stmt_list(),
            scope,
        })
    }

    fn parse_stmt(&mut self) -> Option<ASTStmt<'src>> {
        if let Some(expr) = self.parse_expr_non_greedy() {
            if let Some((assign_op, assign_symbol_loc)) =
                self.next_if_peek_and(|token, _| match token {
                    TokenContent::Symbol(symbol) => symbol.as_assign_op(),
                    _ => None,
                })
            {
                // ASTStmt::VarAssign //
                let rhs = self.parse_expr_non_greedy().ok_or_else(|| {
                    self.raise(
                        ParseDiagnostic::ExpectedAssignValue,
                        expr.loc().merge(assign_symbol_loc),
                    )
                });
                Some(ASTStmt::VarAssign {
                    loc: expr
                        .loc()
                        .merge(rhs.as_ref().map(|it| it.loc()).unwrap_or(assign_symbol_loc)),
                    assign_op,
                    lhs: Box::new(expr),
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
        self.parse_brackets(TBracketType::Curly, true, |p| {
            p.guard_scope(|p, scope| (p.parse_stmt_list(), scope))
        })
        .map(|((body, scope), loc)| ASTBlock { loc, body, scope })
    }

    fn parse_expr_before_block(&mut self) -> Option<ASTExpr<'src>> {
        self._parse_expr_generic::<true, true>()
    }
    fn parse_expr_non_greedy(&mut self) -> Option<ASTExpr<'src>> {
        self._parse_expr_generic::<false, false>()
    }
    fn parse_expr_greedy(&mut self) -> Option<ASTExpr<'src>> {
        self._parse_expr_generic::<false, true>()
    }
    #[inline]
    fn _parse_expr_generic<const BEFORE_BLOCK: bool, const GREEDY_MATCH: bool>(
        &mut self,
    ) -> Option<ASTExpr<'src>> {
        // ASTExpr::OpPrefix //
        // ASTExpr::OpPostfix //
        // ASTExpr::OpInfix //

        /*


        a + b

        a +
        b

        a
        + b


        a * b

        a *
        b

        a
        * b

        a : * : b
        ( sip sip sip ) -> ipp
        a : ( *( :b) )

        * - *
        ( si ip si )


        ( si si [si ip] ip ip)


        ( [si sip sip ip] ip ip)

        ( s si s [i] pi p )  -> (sssipp)

        ( si pi )  -> si | (ip)

         */
        type TS = TPostfixOperatorType;
        type TI = TInfixOperatorType;
        type TP = TPrefixOperatorType;

        let (mut p, mut expr_prev): (Vec<(TP, Loc)>, ASTExpr) = self.guard_state(|parser| {
            let p = std::iter::repeat(())
                .map_while(|_| {
                    parser.next_if_peek_and(|token, _| match token {
                        TokenContent::Symbol(s) => s.as_prefix_op(),
                        _ => None,
                    })
                })
                .collect();
            let expr_prev = parser._parse_expr_with_postfix_blocks::<GREEDY_MATCH>()?;
            Some((p, expr_prev))
        })?;
        let mut s: Vec<(TS, Loc)> = Vec::new();

        let mut si: Option<((TS, TI), Loc)> = None;
        let mut i: Option<(TI, Loc)> = None;
        let mut sip: Vec<((TS, TI, TP), Loc)> = Vec::new();

        #[derive(Debug)]
        enum MSYOp {
            S(TS),
            I(TI),
            P(TP),
        }
        #[derive(Debug)]
        enum MSYObj<'src> {
            Expr(ASTExpr<'src>),
            Op(MSYOp, Loc),
        }
        let mut msy_in: Vec<MSYObj<'src>> =
            Vec::from_iter(p.drain(..).map(|(op, loc)| MSYObj::Op(MSYOp::P(op), loc)));

        fn drain_si_to_s(
            s: &mut Vec<(TS, Loc)>,
            si: &mut Option<((TS, TI), Loc)>,
            sip: &mut Vec<((TS, TI, TP), Loc)>,
        ) {
            s.extend(si.take().map(|((s, _), loc)| (s, loc)).into_iter());
            s.extend(sip.drain(..).map(|((s, _, _), loc)| (s, loc)).into_iter());
        }
        fn choose_i(
            s: &mut Vec<(TS, Loc)>,
            i: &mut Option<(TI, Loc)>,
            p: &mut Vec<(TP, Loc)>,
            si: &mut Option<((TS, TI), Loc)>,
            ip: Option<((TI, TP), Loc)>,
            sip: &mut Vec<((TS, TI, TP), Loc)>,
        ) -> bool {
            fn si_to_s(((s, _), loc): ((TS, TI), Loc)) -> (TS, Loc) {
                (s, loc)
            }
            fn si_to_i(((_, i), loc): ((TS, TI), Loc)) -> (TI, Loc) {
                (i, loc)
            }
            fn ip_to_i(((i, _), loc): ((TI, TP), Loc)) -> (TI, Loc) {
                (i, loc)
            }
            fn ip_to_p(((_, p), loc): ((TI, TP), Loc)) -> (TP, Loc) {
                (p, loc)
            }
            fn sip_to_s(((s, _, _), loc): ((TS, TI, TP), Loc)) -> (TS, Loc) {
                (s, loc)
            }
            fn sip_to_i(((_, i, _), loc): ((TS, TI, TP), Loc)) -> (TI, Loc) {
                (i, loc)
            }
            fn sip_to_p(((_, _, p), loc): ((TS, TI, TP), Loc)) -> (TP, Loc) {
                (p, loc)
            }
            assert!(p.is_empty());
            assert!(i.is_none());
            let si = si.take().map(|si| (si.0 .1.precedence(), si));
            let ip = ip.map(|ip| (ip.0 .0.precedence(), ip));
            let sip_i = sip
                .iter()
                .map(|((_, i_, _), _)| i_.precedence())
                .enumerate()
                .min_by_key(|(_, pr)| *pr);

            macro_rules! sip_to_s_i_and_p {
                ($sip_i: expr) => {
                    s.extend(sip.drain(..$sip_i).map(sip_to_s));
                    *i = Some(sip_to_i(sip.remove(0)));
                    p.extend(sip.drain(..).map(sip_to_p));
                };
            }

            match (si, ip, sip_i) {
                (Some((pr_si, si)), Some((pr_ip, ip)), Some((sip_i, pr_sip))) => {
                    if pr_si < pr_ip && pr_si < pr_sip {
                        // s[i] sip ip
                        *i = Some(si_to_i(si));
                        p.extend(sip.drain(..).map(sip_to_p));
                        p.push(ip_to_p(ip));
                    } else if pr_ip < pr_si && pr_ip < pr_sip {
                        // si sip [i]p
                        s.push(si_to_s(si));
                        s.extend(sip.drain(..).map(sip_to_s));
                        *i = Some(ip_to_i(ip));
                    } else {
                        // si s[i]p ip
                        s.push(si_to_s(si));
                        sip_to_s_i_and_p!(sip_i);
                        p.push(ip_to_p(ip));
                    }
                }
                (Some((pr_si, si)), Some((pr_ip, ip)), None) => {
                    if pr_si < pr_ip {
                        // s[i] ip
                        *i = Some(si_to_i(si));
                        p.push(ip_to_p(ip));
                    } else {
                        // si [i]p
                        s.push(si_to_s(si));
                        *i = Some(ip_to_i(ip));
                    }
                }
                (Some((pr_si, si)), None, Some((sip_i, pr_sip))) => {
                    if pr_si < pr_sip {
                        // s[i] sip
                        *i = Some(si_to_i(si));
                        p.extend(sip.drain(..).map(sip_to_p));
                    } else {
                        // si s[i]p
                        s.push(si_to_s(si));
                        sip_to_s_i_and_p!(sip_i);
                    }
                }
                (Some((_, si)), None, None) => {
                    // s[i]
                    *i = Some(si_to_i(si));
                }
                (None, Some((pr_ip, ip)), Some((sip_i, pr_sip))) => {
                    if pr_ip < pr_sip {
                        // sip [i]p
                        s.extend(sip.drain(..).map(sip_to_s));

                        *i = Some(ip_to_i(ip));
                    } else {
                        // s[i]p ip
                        sip_to_s_i_and_p!(sip_i);
                        p.push(ip_to_p(ip));
                    }
                }
                (None, Some((_, ip)), None) => {
                    // [i]p
                    *i = Some(ip_to_i(ip));
                }
                (None, None, Some((sip_i, _))) => {
                    // s[i]p
                    sip_to_s_i_and_p!(sip_i);
                }
                (None, None, None) => return false,
            }
            true
        }

        loop {
            let expr = if i.is_some() || si.is_some() || !sip.is_empty() {
                self._parse_expr_with_postfix_blocks::<GREEDY_MATCH>()
            } else {
                None
            };

            if let Some(expr) = expr {
                ///////// received an expression /////////

                if i.is_none() {
                    choose_i(&mut s, &mut i, &mut p, &mut si, None, &mut sip);
                }

                msy_in.push(MSYObj::Expr(expr_prev));
                msy_in.extend(s.drain(..).map(|(op, loc)| MSYObj::Op(MSYOp::S(op), loc)));
                msy_in.push(
                    i.take()
                        .map(|(op, loc)| MSYObj::Op(MSYOp::I(op), loc))
                        .unwrap(),
                );
                msy_in.extend(p.drain(..).map(|(op, loc)| MSYObj::Op(MSYOp::P(op), loc)));
                expr_prev = expr;
            } else if let Some((s_, i_, p_, loc)) = self.guard_state(|parser| {
                ///////// try for an operator /////////
                let (sy, newline_before, loc) = match parser.next()? {
                    (
                        Token {
                            content: TokenContent::Symbol(sy),
                            loc,
                        },
                        newline,
                    ) => (sy, newline, loc),
                    _ => return None,
                };

                let mut s_ = sy.as_postfix_op();
                let mut i_ = sy.as_infix_op();
                let mut p_ = sy.as_prefix_op();

                // handle linebreaks for non-greedy mode
                if !GREEDY_MATCH {
                    let newline_after = parser.peek_has_linebreak();
                    match (newline_before, newline_after) {
                        (false, false) => {}
                        (true, false) => {
                            s_ = None;
                            if p_.is_some() {
                                i_ = None;
                            }
                        }
                        (false, true) => {
                            p_ = None;
                            if s_.is_some() {
                                i_ = None;
                            }
                        }
                        (true, true) => {
                            if s_.is_some() || p_.is_some() {
                                return None;
                            }
                        }
                    }
                }
                if s_.is_none() && i_.is_none() && p_.is_none() {
                    return None;
                }

                // constrain from state
                if i.is_some() || p.len() > 0 {
                    // definitely passed infix
                    // >> only allow prefix for next expr now.
                    s_ = None;
                    i_ = None;

                    if GREEDY_MATCH {
                        if newline_before && p_.is_none() {
                            return None;
                        }
                    }
                } else {
                    // may or may not have passed infix
                    // >> still allow anything
                }
                Some((s_, i_, p_, loc))
            }) {
                ///////// received an operator /////////

                // update state
                if i.is_some() {
                    // definitely passed infix

                    if let Some(p_) = p_ {
                        p.push((p_, loc));
                    } else {
                        self.raise(ParseDiagnostic::UnexpectedOperator, loc);
                    }
                } else {
                    // may or may not have passed infix

                    match (s_, i_, p_) {
                        (Some(s_), Some(i_), Some(p_)) => {
                            // sip
                            sip.push(((s_, i_, p_), loc));
                        }
                        (Some(s_), Some(i_), None) => {
                            // si
                            drain_si_to_s(&mut s, &mut si, &mut sip);
                            si = Some(((s_, i_), loc));
                        }
                        (Some(s_), None, Some(p_)) => {
                            // sp
                            dbg!("TODO what to do with `sp` operator case, defaulting to treating it like `s`");
                            {
                                let _ = p_;
                                // as: s
                                drain_si_to_s(&mut s, &mut si, &mut sip);
                                s.push((s_, loc));
                            }
                        }
                        (Some(s_), None, None) => {
                            // s
                            drain_si_to_s(&mut s, &mut si, &mut sip);
                            s.push((s_, loc));
                        }
                        (None, Some(i_), Some(p_)) => {
                            // ip
                            choose_i(
                                &mut s,
                                &mut i,
                                &mut p,
                                &mut si,
                                Some(((i_, p_), loc)),
                                &mut sip,
                            );
                        }
                        (None, Some(i_), None) => {
                            // i
                            drain_si_to_s(&mut s, &mut si, &mut sip);
                            i = Some((i_, loc))
                        }
                        (None, None, Some(p_)) => {
                            // p
                            choose_i(&mut s, &mut i, &mut p, &mut si, None, &mut sip);
                            p.push((p_, loc))
                        }
                        (None, None, None) => {
                            // -
                            self.raise(ParseDiagnostic::UnexpectedOperator, loc);
                        }
                    }
                }
                continue;
            } else {
                // no match, break
                break;
            }
        }

        if let Some((_, loc)) = i {
            self.raise(
                ParseDiagnostic::UnexpectedOperator,
                loc.merge_some(p.last().map(HasLoc::loc)),
            );
        }
        drain_si_to_s(&mut s, &mut si, &mut sip);
        if msy_in.is_empty() && s.is_empty() {
            return Some(expr_prev);
        }
        msy_in.push(MSYObj::Expr(expr_prev));
        msy_in.extend(s.into_iter().map(|(op, loc)| MSYObj::Op(MSYOp::S(op), loc)));

        /// I don't remember how shunting yard goes so I'm reinventing it lol.
        ///
        /// msy -> "[m]odified [s]hunting [y]ard"
        fn modified_shunting_yard(msy_in: Vec<MSYObj>) -> ASTExpr {
            // how the algorithm should work:
            //   ( format is "input | value ; op_stack" )

            // a |  a ;
            // + |  _ ; (a+_)
            // b |  b ; (a+_)
            // + |  _ ; ((a+b)+_)
            // c |  c ; ((a+b)+_)
            //   |  ((a+b)+c) ;

            // - |  _ ; (-_)
            // - |  _ ; (-_) (-_)
            // a |  a ; (-_) (-_)
            // + |  _ ; ((-(-a))+_)
            // b |  b ; ((-(-a))+_)
            // + |  _ ; (((-(-a))+b)+_)
            // c |  c ; (((-(-a))+b)+_)
            //   |  (((-(-a))+b)+c) ;

            // a |  a ;
            // + |  _ ; (a+_)
            // b |  b ; (a+_)
            // + |  _ ; ((a+b)+_)
            // c |  c ; ((a+b)+_)
            // : |  (((a+b)+c):) ;

            // a |  a ;
            // + |  _ ; (a+_)
            // b |  b ; (a+_)
            // * |  _ ; (a+_) (b*_)
            // c |  c ; (a+_) (b*_)
            //   |  (b*c) ; (a+_)
            //   |  (a+(b*c)) ;

            // a |  a ;
            // ^ |  _ ; (a^_)
            // b |  b ; (a^_)
            // ^ |  _ ; (a^_) (b^_)
            // c |  c ; (a^_) (b^_)
            //   |  (b^c) ; (a^_)
            //   |  (a^(b^c)) ;

            let mut value = None;
            let mut op_stack: Vec<(OpStackEnt, u8)> = Vec::new();
            enum OpStackEnt<'src> {
                I((TI, Loc), ASTExpr<'src>),
                P((TP, Loc)),
            }
            impl<'src> OpStackEnt<'src> {
                fn apply(self, rhs: ASTExpr<'src>) -> ASTExpr<'src> {
                    match self {
                        OpStackEnt::I(op, lhs) => ASTExpr::OpInfix {
                            loc: lhs.loc().merge(rhs.loc()),
                            inner: (Box::new(lhs), Box::new(rhs)),
                            op,
                        },
                        OpStackEnt::P(op) => ASTExpr::OpPrefix {
                            loc: op.1.merge(rhs.loc()),
                            inner: Box::new(rhs),
                            op,
                        },
                    }
                }
            }

            for obj in msy_in {
                match (obj, value) {
                    (MSYObj::Expr(expr), None) => {
                        value = Some(expr);
                    }
                    (MSYObj::Op(MSYOp::I(op), loc), Some(mut v)) => {
                        // [ prev_value ; ... ]   "... <op>"
                        let precedence_self = op.precedence();

                        loop {
                            match op_stack.pop() {
                                None => break,
                                Some((popped_op, precedence_other)) => {
                                    if precedence_self > precedence_other
                                        || (precedence_self == precedence_other
                                            && op.right_associative())
                                    {
                                        // right hand side associates "a * (b * c)"
                                        // [ b ; (a^_) ]  ->  [ _ ; (a^_) (b^_) ]

                                        // put it back and break
                                        op_stack.push((popped_op, precedence_other));
                                        break;
                                    } else {
                                        // left hand side associates "(a * b) * c"
                                        // [ b ; (a+_) (-_) ]  ->  [ _ ; ((a+(-b))+_) ]

                                        // CONSUME
                                        v = popped_op.apply(v);
                                    }
                                }
                            }
                        }

                        op_stack.push((OpStackEnt::I((op, loc), v), precedence_self));

                        value = None;
                    }
                    (MSYObj::Op(MSYOp::P(op), loc), None) => {
                        // prefix, push it to the operator stack
                        op_stack.push((OpStackEnt::P((op, loc)), op.precedence()));
                        value = None;
                    }
                    (MSYObj::Op(MSYOp::S(op), loc), Some(mut v)) => {
                        // [ prev_value ; ... ]   "... <op>"
                        let precedence_self = op.precedence();

                        loop {
                            match op_stack.pop() {
                                None => break,
                                Some((popped_op, precedence_other)) => {
                                    if precedence_self > precedence_other {
                                        // right hand side associates "a * (b * c)"
                                        // [ b ; (a^_) ]  ->  [ _ ; (a^_) (b^_) ]

                                        // put it back and break
                                        op_stack.push((popped_op, precedence_other));
                                        break;
                                    } else {
                                        // left hand side associates "(a * b) * c"
                                        // [ b ; (a+_) (-_) ]  ->  [ _ ; ((a+(-b))+_) ]

                                        // CONSUME
                                        v = popped_op.apply(v);
                                    }
                                }
                            }
                        }

                        value = Some(ASTExpr::OpPostfix {
                            loc: v.loc().merge(loc),
                            inner: Box::new(v),
                            op: (op, loc),
                        });
                    }
                    _ => panic!(
                        "[{}:{}] should be unreachable, this means invalid input in `msy_in`",
                        file!(),
                        line!(),
                    ),
                }
            }

            let mut value = if let Some(value) = value {
                value
            } else {
                panic!(
                    "[{}:{}] should be unreachable, this means invalid input in `msy_in`",
                    file!(),
                    line!(),
                )
            };

            while let Some((popped_op, _)) = op_stack.pop() {
                value = popped_op.apply(value);
            }

            value
        }

        Some(modified_shunting_yard(msy_in))
    }

    fn _parse_expr_with_postfix_blocks<const GREEDY_MATCH: bool>(
        &mut self,
    ) -> Option<ASTExpr<'src>> {
        let mut current = self._parse_expr_simple()?;
        while let Some(postfix) = self.parse_postfix_block::<GREEDY_MATCH>() {
            // ASTExpr::PostfixBlock //
            current = ASTExpr::PostfixBlock {
                inner: Box::new(current),
                postfix,
            };
        }
        Some(current)
    }
    fn parse_postfix_block<const GREEDY_MATCH: bool>(
        &mut self,
    ) -> Option<(ASTPostfixBlock<'src>, Loc)> {
        Some(
            if let Some((args, loc)) = self.parse_brackets(TBracketType::Paren, GREEDY_MATCH, |p| {
                p.parse_list_commasep(
                    Self::parse_expr_greedy,
                    &ParseDiagnostic::ExpectedFunctionCallArgument,
                )
            }) {
                // ASTPostfixBlock::Call //
                (ASTPostfixBlock::Call { args }, loc)
            } else if let Some((args, loc)) =
                self.parse_brackets(TBracketType::Square, GREEDY_MATCH, |p| {
                    p.parse_list_commasep(
                        Self::parse_expr_greedy,
                        &ParseDiagnostic::ExpectedIndexCallArgument,
                    )
                })
            {
                // ASTPostfixBlock::Index //
                (ASTPostfixBlock::Index { args }, loc)
            } else if let Some(lambda) = self.parse_lambda_postfix() {
                // ASTPostfixBlock::Lambda //
                let loc = lambda.loc;
                (ASTPostfixBlock::Lambda { lambda }, loc)
            } else if let Some((block, loc)) = self.guard_state(|p| {
                let loc_start = p.next_if_eq(TokenContent::Symbol(TSymbol::Period))?;
                if !GREEDY_MATCH && p.peek_has_linebreak() {
                    p.raise(
                        ParseDiagnostic::ExpectedPostfixAfterDot,
                        loc_start.new_from_end(),
                    );
                    todo!("error expressions");
                }
                if let Some(name) = p.parse_name() {
                    // ASTPostfixBlock::PropertyAccess //
                    let loc = loc_start.merge(name.loc());
                    Some((ASTPostfixBlock::PropertyAccess { name }, loc))
                } else if let Some(x) = p.parse_brackets(TBracketType::Curly, true, |p| {
                    ASTPostfixBlock::DataStructInit {
                        entries: p
                            .parse_list_commasep(
                                |p| {
                                    let name = p.parse_name()?;

                                    let expr = p
                                        .next_if_eq(TokenContent::Symbol(TSymbol::Assign))
                                        .ok_or_else(|| {
                                            p.raise(
                                                ParseDiagnostic::ExpectedValueStructInitAssign,
                                                name.loc.new_from_end(),
                                            )
                                        })
                                        .and_then(|_| {
                                            p.parse_expr_greedy().ok_or_else(|| {
                                                p.raise(
                                                    ParseDiagnostic::ExpectedValueStructInitValue,
                                                    name.loc.new_from_end(),
                                                )
                                            })
                                        });

                                    Some((name, expr))
                                },
                                &ParseDiagnostic::ExpectedValueStructInit,
                            )
                            .into_iter()
                            .filter_map(|ent| ent.ok())
                            .collect(),
                    }
                }) {
                    // ASTPostfixBlock::DataStructInit //
                    Some(x)
                } else if let Some(x) = p.parse_brackets(TBracketType::Paren, true, |p| {
                    ASTPostfixBlock::DataTupleInit {
                        entries: p.parse_list_commasep(
                            Self::parse_expr_greedy,
                            &ParseDiagnostic::ExpectedValueTupleInit,
                        ),
                    }
                }) {
                    // ASTPostfixBlock::DataTupleInit //
                    Some(x)
                } else {
                    None
                }
            }) {
                (block, loc)
            } else {
                return None;
            },
        )
    }

    fn _parse_expr_simple(&mut self) -> Option<ASTExpr<'src>> {
        Some(if let Some((value, loc)) = self.parse_literal() {
            // ASTExpr::Literal //
            ASTExpr::Literal { loc, value }
        } else if let Some(ident) = self.parse_ident() {
            // ASTExpr::Ident //
            ASTExpr::Ident(ident)
        } else if let Some(lambda) = self.parse_lambda_expr() {
            // ASTExpr::Lambda //
            ASTExpr::Lambda(lambda)
        } else if let Some((inner, loc)) = self.parse_keyword_then_optional_expr(TKeyword::Return) {
            // ASTExpr::Return //
            ASTExpr::Return {
                loc,
                inner: inner.map(Box::new),
            }
        } else if let Some((inner, loc)) = self.parse_keyword_then_optional_expr(TKeyword::Break) {
            // ASTExpr::Break //
            ASTExpr::Break {
                loc,
                inner: inner.map(Box::new),
            }
        } else if let Some(loc) = self.next_if_eq(TokenContent::Keyword(TKeyword::Continue)) {
            // ASTExpr::Continue //
            ASTExpr::Continue { loc }
        } else if let Some(x) = self.parse_subblocked() {
            // ASTExpr::SubBlocked //
            ASTExpr::SubBlocked(x)
        } else if let Some((inner, loc)) =
            self.parse_paren_or_tuple(Self::parse_expr_greedy, &ParseDiagnostic::ExpectedExpr)
        {
            match inner {
                // ASTExpr::Parentheses //
                HelperParenOrTuple::Paren(inner) => ASTExpr::Parentheses { loc, inner },
                // ASTExpr::Array [tuple] //
                HelperParenOrTuple::Tuple(inner) => ASTExpr::Array {
                    loc,
                    inner,
                    is_tuple: true,
                },
            }
        } else if let Some((inner, loc)) = self.parse_brackets(TBracketType::Square, true, |p| {
            p.parse_list_commasep(Self::parse_expr_greedy, &ParseDiagnostic::ExpectedExpr)
        }) {
            // ASTExpr::Array [array] //
            ASTExpr::Array {
                loc,
                inner,
                is_tuple: false,
            }
        } else {
            return None;
        })
    }

    fn parse_keyword_then_optional_expr(
        &mut self,
        keyword: TKeyword,
    ) -> Option<(Option<ASTExpr<'src>>, Loc)> {
        let loc_start = self.next_if_eq(TokenContent::Keyword(keyword))?;

        let expr = self.parse_expr_greedy();
        let loc = loc_start.merge_some(loco!(expr));
        Some((expr, loc))
    }
    fn parse_literal(&mut self) -> Option<(ASTLiteral<'src>, Loc)> {
        self.guard_state(|p| {
            let Token { loc, content } = p.next()?.0;
            Some((
                match content {
                    TokenContent::Bool(v) => {
                        // bool //
                        ASTLiteral::Bool(v)
                    }

                    TokenContent::Number {
                        decimal,
                        exp,
                        radix,
                        unparsed,
                    } => {
                        if decimal || exp {
                            // float //
                            if radix == 10 {
                                ASTLiteral::Float(
                                    unparsed
                                        .parse()
                                        .expect("TODO invalid floats / error expressions"),
                                    unparsed
                                        .parse()
                                        .expect("TODO invalid floats / error expressions"),
                                )
                            } else {
                                todo!("parsing non-decimal floats")
                            }
                        } else {
                            match if unparsed.starts_with('-') {
                                // negative integer //
                                i128::from_str_radix(unparsed, radix as u32)
                                    .map(|n| ASTLiteral::NInt(n))
                            } else {
                                // integer //
                                u128::from_str_radix(unparsed, radix as u32)
                                    .map(|n| ASTLiteral::Int(n))
                            } {
                                Ok(int) => int,
                                Err(err) => {
                                    let diagnostic = match err.kind() {
                                        IntErrorKind::PosOverflow => {
                                            ParseDiagnostic::IntOverflowPos
                                        }
                                        IntErrorKind::NegOverflow => {
                                            ParseDiagnostic::IntOverflowNeg
                                        }
                                        _ => unreachable!(),
                                    };
                                    ASTLiteral::IntInvalid(unparsed, p.raise(diagnostic, loc))
                                }
                            }
                        }
                    }
                    TokenContent::Str {
                        unescaped,
                        interpolation,
                        missing_closing,
                    } => {
                        // string //
                        if interpolation.0 || interpolation.1 {
                            p.raise(ParseDiagnostic::StringIllegalInterpolation, loc);
                        }
                        if missing_closing {
                            p.raise(ParseDiagnostic::StringMissingClosing, loc);
                        }
                        ASTLiteral::String(Self::flee_from_string(unescaped))
                    }
                    _ => return None,
                },
                loc,
            ))
        })
    }

    fn parse_lambda_expr(&mut self) -> Option<ASTLambda<'src>> {
        self.guard_state(|p| {
            let loc_start = p.next_if_eq(TokenContent::Symbol(TSymbol::Lambda))?;

            let (args, loc_args) = p
                .parse_brackets(TBracketType::Paren, true, |p| {
                    p.parse_list_commasep(
                        |p| p.parse_typed_destructure(true),
                        &ParseDiagnostic::ExpectedLambdaArgument,
                    )
                })
                .unzip();
            let (ty_return, loc_ty_return) = if args.is_some() {
                p.parse_type_optional(true)
            } else {
                (None, None)
            };
            let block = p.parse_block().unwrap_or_else(|| {
                p.raise(
                    ParseDiagnostic::ExpectedLambdaBody,
                    loc_start.merge_some(merge!(loc_ty_return, loc_args)),
                );
                ASTBlock {
                    body: Vec::new(),
                    loc: merge!(loc_ty_return, loc_args)
                        .unwrap_or(loc_start)
                        .new_from_end(),

                    scope: p.new_empty_scope(),
                }
            });

            Some(ASTLambda {
                loc: loc_start.merge(block.loc()),
                args,
                ty_return,
                block,
            })
        })
    }
    fn parse_lambda_postfix(&mut self) -> Option<ASTLambda<'src>> {
        // `... { \ a: i32, b -> ... }`
        self.parse_brackets(TBracketType::Curly, true, |p| {
            p.guard_scope(|p, scope| {
                let args = p
                    .parse_brackets(TBracketType::LambdaArgs, true, |p| {
                        p.parse_list_commasep(
                            |p| p.parse_typed_destructure(true),
                            &ParseDiagnostic::ExpectedLambdaArgument,
                        )
                    })
                    .map(|(args_list, _)| args_list);
                let block = p.parse_stmt_list();
                (args, block, scope)
            })
        })
        .map(|((args, block, scope), loc)| ASTLambda {
            loc,
            args,
            ty_return: None, // must be inferred by type system
            block: ASTBlock {
                loc,
                body: block,
                scope,
            },
        })
    }

    /// Escape the contents of string literals, converting `"\n"`, `"\t"`, etc. into their actual newline, tab, etc..
    fn flee_from_string(unescaped: &'src str) -> String {
        fn next(unescaped: &mut std::str::Chars, escaped: &mut String) -> bool {
            let ch = if let Some(ch) = unescaped.next() {
                ch
            } else {
                return false;
            };

            match ch {
                '\\' => {
                    let ch = unescaped.next().unwrap(); // a string cannot end with an escape because then the terminator would have been escaped and we'd keep going.
                    let ch = match ch {
                        '0' => '\0',
                        '\'' => '\'',
                        '"' => '\"',
                        '\\' => '\\',
                        'n' => '\n',
                        'r' => '\r',
                        't' => '\t',
                        '\n' => return true, // reduces to empty string
                        'x' => {
                            let u = unescaped
                                .clone()
                                .take(2)
                                .map_while(|ch| match ch {
                                    '0' => Some(0),
                                    '1' => Some(1),
                                    '2' => Some(2),
                                    '3' => Some(3),
                                    '4' => Some(4),
                                    '5' => Some(5),
                                    '6' => Some(6),
                                    '7' => Some(7),
                                    '8' => Some(8),
                                    '9' => Some(9),
                                    'a' | 'A' => Some(10),
                                    'b' | 'B' => Some(11),
                                    'c' | 'C' => Some(12),
                                    'd' | 'D' => Some(13),
                                    'e' | 'E' => Some(14),
                                    'f' | 'F' => Some(15),
                                    _ => None,
                                })
                                .collect::<heapless::Vec<u8, 2>>();
                            if u.len() == 2 {
                                let _ = unescaped.next();
                                let _ = unescaped.next();
                                (u[0] << 4 + u[1]) as char
                            } else {
                                // failed to match, reemit `\x`
                                escaped.push('\\');
                                'x'
                            }
                        }
                        'u' => {
                            let mut u = unescaped.clone();
                            let mut output = None;
                            if u.next() == Some('{') {
                                let mut chrs = heapless::Vec::<char, 5>::new();
                                let mut hit_end = false;
                                while let Some(ch) = u.next() {
                                    if ch.is_ascii_hexdigit() {
                                        if chrs.push(ch).is_err() {
                                            break;
                                        }
                                    } else if ch == '}' {
                                        hit_end = true;
                                        break;
                                    } else {
                                        break;
                                    }
                                }
                                if hit_end {
                                    let n_consumed = chrs.len() + 2;
                                    let mut i = 0;
                                    for ch in chrs {
                                        i <<= 4;
                                        i |= match ch {
                                            '0' => 0,
                                            '1' => 1,
                                            '2' => 2,
                                            '3' => 3,
                                            '4' => 4,
                                            '5' => 5,
                                            '6' => 6,
                                            '7' => 7,
                                            '8' => 8,
                                            '9' => 9,
                                            'a' | 'A' => 10,
                                            'b' | 'B' => 11,
                                            'c' | 'C' => 12,
                                            'd' | 'D' => 13,
                                            'e' | 'E' => 14,
                                            'f' | 'F' => 15,
                                            _ => unreachable!(),
                                        }
                                    }
                                    output = char::from_u32(i).map(|ch| (ch, n_consumed));
                                }
                            }

                            if let Some((ch, n_consumed)) = output {
                                escaped.push(ch);
                                for _ in 0..n_consumed {
                                    let _ = unescaped.next();
                                }
                            } else {
                                escaped.push('\\');
                                escaped.push('u');
                            }
                            return true;
                        }
                        ch => {
                            escaped.push('\\');
                            ch
                        }
                    };
                    escaped.push(ch);
                }
                _ => escaped.push(ch),
            }

            true
        }

        let mut escaped = String::with_capacity(unescaped.len());
        let mut unescaped = unescaped.chars();
        while next(&mut unescaped, &mut escaped) {}

        escaped.shrink_to_fit();
        escaped
    }

    fn parse_type_optional(
        &mut self,
        allow_leading_newline: bool,
    ) -> (Option<Fallible<ASTType<'src>>>, Option<Loc>) {
        // `: void` as `~` shorthand
        if let Some((_, loc)) = self.next_if_peek_and(|token, had_newline| match token {
            TokenContent::Symbol(TSymbol::Xor) if allow_leading_newline || !had_newline => Some(()),
            _ => None,
        }) {
            return (Some(Ok(ASTType::Unit { loc })), Some(loc));
        }
        // main parser
        if let Some((_, loc_colon)) = self.next_if_peek_and(|token, had_newline| match token {
            TokenContent::Symbol(TSymbol::Colon) if allow_leading_newline || !had_newline => {
                Some(())
            }
            _ => None,
        }) {
            let ty_return = self.parse_type().ok_or_else(|| {
                self.raise(
                    ParseDiagnostic::ExpectedTypeAfterColon,
                    loc_colon.new_from_end(),
                )
            });

            let loc = loc_colon.merge_some(ty_return.as_ref().ok().map(HasLoc::loc));
            (Some(ty_return), Some(loc))
        } else {
            (None, None)
        }
    }
    fn parse_type(&mut self) -> Option<ASTType<'src>> {
        if let Some((loc, mutable)) = self.guard_state(|p| {
            let ampersand = p.next_if_eq(TokenContent::Symbol(TSymbol::Ampersand));
            let (loc, mutable) = if let Some(loc_amp) = ampersand {
                let mutable = p.next_if_eq(TokenContent::Keyword(TKeyword::Mut));
                (loc_amp.merge_some(mutable), mutable.is_some())
            } else {
                let loc = p.next_if_eq(TokenContent::Symbol(TSymbol::RefMut))?;
                (loc, true)
            };
            Some((loc, mutable))
        }) {
            // ASTType::Ref //
            let inner = self.parse_type().ok_or_else(|| {
                self.diagnostics
                    .raise(ParseDiagnostic::ExpectedTypeInner, loc.new_from_end())
            });
            Some(ASTType::Ref {
                loc: loc.merge_some(locr!(inner)),
                mutable,
                inner: Box::new(inner),
            })
        } else {
            self.parse_type_no_prefix()
        }
    }
    fn parse_type_no_prefix(&mut self) -> Option<ASTType<'src>> {
        let mut current = self._parse_type_no_postfix()?;
        loop {
            if let Some(ident) = self.guard_state(|p| {
                p.next_if_eq(TokenContent::Symbol(TSymbol::Period))?;
                p.parse_ident()
            }) {
                // ASTType::Access //
                current = ASTType::Access {
                    ident,
                    inner: Box::new(current),
                };
            } else if let Some(((params, named_params), loc)) =
                self.parse_brackets(TBracketType::Angle, true, |p| {
                    // ASTType::TypeParam //
                    let s = p.parse_list_commasep(
                        |p| {
                            let key = p.guard_state(|p| {
                                let key = p.parse_name()?;
                                let loc_eq = p.next_if_eq(TokenContent::Symbol(TSymbol::Assign))?;
                                Some((key, loc_eq))
                            });
                            let ty = p.parse_type();

                            match (key, ty) {
                                (None, None) => None,
                                (Some((key, loc_eq)), None) => Some((Some((key, loc_eq)), None)),
                                (key, Some(ty)) => Some((key, Some(ty))),
                            }
                        },
                        &ParseDiagnostic::ExpectedTemplateTypeEntry,
                    );
                    let mut params = Vec::with_capacity(s.len());
                    let mut named_params = Vec::new();

                    for r in s {
                        match r {
                            Ok((key, ty)) => {
                                if let Some((key, loc_eq)) = key {
                                    let ty = ty.ok_or_else(|| {
                                        p.raise(
                                            ParseDiagnostic::ExpectedTemplateTypeAfterEq,
                                            key.loc.merge(loc_eq),
                                        )
                                    });
                                    named_params.push((key, ty));
                                } else {
                                    let ty = ty.unwrap();
                                    if !named_params.is_empty() {
                                        p.raise(
                                            ParseDiagnostic::TemplateOrderOrderedFoundAfterNamed,
                                            ty.loc(),
                                        );
                                    }
                                    params.push(Ok(ty));
                                }
                            }
                            Err(e) => {
                                if named_params.is_empty() {
                                    params.push(Err(e));
                                }
                            }
                        }
                    }
                    params.shrink_to_fit();

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
        if let Some(ident) = self.parse_ident() {
            // ASTType::Ident //
            Some(ASTType::Ident(ident))
        } else if let Some(loc) = self
            .next_if_eq(TokenContent::Keyword(TKeyword::Unit))
            .or_else(|| self.next_if_eq(TokenContent::Symbol(TSymbol::Xor)))
        {
            // ASTType::Unit //
            Some(ASTType::Unit { loc })
        } else if let Some((inner, loc)) =
            self.parse_paren_or_tuple(|p| p.parse_type(), &ParseDiagnostic::ExpectedTypeInner)
        {
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
        if let Some(x) = self.parse_free_impl() {
            // ASTDeclr::FreeImpl //
            Some(ASTDeclr::FreeImpl(x))
        } else if let Some(x) = self.parse_import() {
            // ASTDeclr::Import //
            Some(ASTDeclr::Import(x))
        } else {
            self.guard_state(|p| {
                let mut annot = p.parse_annot();
                p.parse_static_declaration_givenannot(&mut annot)
            })
        }
    }
    fn parse_static_declaration_givenannot(
        &mut self,
        annot: &mut ASTAnnot<'src>,
    ) -> Option<ASTDeclr<'src>> {
        Some(if let Some(x) = self.parse_function(annot) {
            // ASTDeclr::Function //
            ASTDeclr::Function(x)
        } else if let Some(x) = self.parse_const(annot) {
            // ASTDeclr::Const //
            ASTDeclr::Const(x)
        } else if let Some(x) = self.parse_trait(annot) {
            // ASTDeclr::Trait //
            ASTDeclr::Trait(x)
        } else if let Some(x) = self.parse_data(annot) {
            // ASTDeclr::Data //
            ASTDeclr::Data(x)
        } else if let Some(x) = self.parse_type_alias(annot) {
            // ASTDeclr::TypeAlias //
            ASTDeclr::TypeAlias(x)
        } else {
            return None;
        })
    }

    fn parse_function(&mut self, annot: &mut ASTAnnot<'src>) -> Option<ASTFunction<'src>> {
        let loc_start = self.next_if_eq(TokenContent::Keyword(TKeyword::Fn))?;

        // function name
        let name = self
            .parse_name()
            .ok_or_else(|| self.raise(ParseDiagnostic::ExpectedFunctionName, loc_start));

        // template bounds
        let templates = self.parse_templates();

        // args
        let (args, loc_args) = self
            .parse_brackets(TBracketType::Paren, true, |p| {
                p.parse_list_commasep(
                    |p| p.parse_typed_destructure(true),
                    &ParseDiagnostic::ExpectedFunctionArgument,
                )
            })
            .map(|(args, loc_args)| (args, Some(loc_args)))
            .unwrap_or_else(|| {
                self.raise(
                    ParseDiagnostic::ExpectedFunctionArgumentList,
                    merge!(loco!(templates), locr!(name)).unwrap_or(loc_start),
                );
                (Vec::new(), None)
            });

        // return ty
        let (ty_return, loc_ty_return) = self.parse_type_optional(false);

        // block
        let block = self
            .parse_either(Self::parse_block, |p| {
                p.next_if_eq(TokenContent::Symbol(TSymbol::Assign))
                    .map(|loc| {
                        (
                            loc,
                            p.guard_scope(|p, scope| (p.parse_expr_greedy(), scope)),
                        )
                    }) // TODO: cascading expr greediness
            })
            .map(|block| match block {
                AorB::A(block) => block,
                AorB::B((loc_eq, (expr, scope))) => {
                    if expr.is_none() {
                        self.raise(
                            ParseDiagnostic::ExpectedFunctionBodyExprAfterEq,
                            loc_eq.new_from_end(),
                        );
                    }
                    ASTBlock {
                        loc: loc_eq.merge_some(loco!(expr)),
                        body: expr.into_iter().map(|expr| ASTStmt::Expr(expr)).collect(),
                        scope,
                    }
                }
            });

        Some(ASTFunction {
            annot: annot.take(),
            loc: loc_start.merge_some(merge!(
                loco!(block),
                loc_ty_return,
                loc_args,
                loco!(templates),
                locr!(name),
            )),
            containing_scope: self.current_scope(),
            templates,
            args,
            block,
            ty_return,
            name,
        })
    }
    fn parse_const(&mut self, annot: &mut ASTAnnot<'src>) -> Option<ASTConst<'src>> {
        let loc_start = self.next_if_eq(TokenContent::Keyword(TKeyword::Const))?;

        let name = self
            .parse_name()
            .ok_or_else(|| self.raise(ParseDiagnostic::ExpectedConstName, loc_start));
        let (ty, loc_ty) = self.parse_type_optional(true);

        let (value, loc_eq) =
            if let Some(loc_eq) = self.next_if_eq(TokenContent::Symbol(TSymbol::Assign)) {
                // TODO: cascading expr greediness
                let value = self.parse_expr_greedy().ok_or_else(|| {
                    self.raise(ParseDiagnostic::ExpectedAssignValue, loc_eq.new_from_end())
                });

                (Some(value), Some(loc_eq))
            } else {
                (None, None)
            };

        Some(ASTConst {
            loc: loc_start.merge_some(merge!(
                value
                    .as_ref()
                    .and_then(|it| it.as_ref().ok().map(|it| it.loc())),
                loc_eq,
                loc_ty,
                locr!(name)
            )),
            annot: annot.take(),
            containing_scope: self.current_scope(),
            name,
            ty,
            value,
        })
    }
    fn parse_trait_const(&mut self, annot: &mut ASTAnnot<'src>) -> Option<ASTTraitConst<'src>> {
        let loc_start = self.next_if_eq(TokenContent::Keyword(TKeyword::Const))?;

        let name = self
            .parse_name()
            .ok_or_else(|| self.raise(ParseDiagnostic::ExpectedConstName, loc_start));
        let (ty, loc_ty) = self.parse_type_optional(true);
        let ty = match ty {
            Some(ty) => ty,
            None => Err(self.raise(
                ParseDiagnostic::ExpectedTraitConstType,
                loc_start.merge_some(locr!(name)),
            )),
        };

        Some(ASTTraitConst {
            loc: loc_start.merge_some(merge!(loc_ty, locr!(name))),
            annot: annot.take(),
            containing_scope: self.current_scope(),
            name,
            ty,
        })
    }
    fn parse_trait(&mut self, annot: &mut ASTAnnot<'src>) -> Option<ASTTrait<'src>> {
        let loc_start = self.next_if_eq(TokenContent::Keyword(TKeyword::Trait))?;

        let name = self
            .parse_name()
            .ok_or_else(|| self.raise(ParseDiagnostic::ExpectedTraitName, loc_start));

        let templates = self.parse_templates();
        let (bounds, loc_bounds) = self.parse_bounds();
        let bounds = bounds;

        let (loc_body, functions, consts, types) =
            self.parse_trait_impl_contents().unwrap_or_else(|| {
                let loc = loc_start.merge_some(merge!(loc_bounds, loco!(templates), locr!(name)));
                self.raise(ParseDiagnostic::ExpectedTraitBody, loc);
                (loc, Vec::new(), Vec::new(), Vec::new())
            });

        Some(ASTTrait {
            loc: loc_start.merge(loc_body),
            annot: annot.take(),
            containing_scope: self.current_scope(),
            name,
            templates,
            bounds,
            functions,
            consts,
            types,
        })
    }
    fn parse_data(&mut self, annot: &mut ASTAnnot<'src>) -> Option<ASTData<'src>> {
        let loc_start = self.next_if_eq(TokenContent::Keyword(TKeyword::Data))?;

        let name = self
            .parse_name()
            .ok_or_else(|| self.raise(ParseDiagnostic::ExpectedDataName, loc_start));

        let templates = self.parse_templates();

        let (contents, loc_contents) = match self.parse_data_contents() {
            Some((contents, loc_contents)) => (contents, Some(loc_contents)),
            None => {
                self.raise(
                    ParseDiagnostic::ExpectedDataBody,
                    loc_start.merge_some(merge!(loco!(templates), locr!(name))),
                );
                (ASTDataContents::Unit, None)
            }
        };

        let attatched_impls = std::iter::repeat_with(|| self.parse_bound_impl())
            .map_while(|it| it)
            .collect::<Vec<_>>();

        Some(ASTData {
            loc: loc_start.merge_some(attatched_impls.last().map(HasLoc::loc).or(merge!(
                loc_contents,
                loco!(templates),
                locr!(name),
            ))),
            annot: annot.take(),
            containing_scope: self.current_scope(),
            name,
            templates,
            contents,
            attatched_impls,
        })
    }
    fn parse_data_contents(&mut self) -> Option<(ASTDataContents<'src>, Loc)> {
        fn finish_struct_and_tuple<'a, 'src, T: Default>(
            p: &mut Parser<'a, 'src>,
            loc_keyword: Option<Loc>,
            block: Option<(T, Loc)>,
            diagnostic_missingblock: &'static ParseDiagnostic,
        ) -> Option<(T, Loc)> {
            match (loc_keyword, block) {
                (Some(loc_start), Some((block, loc_block))) => {
                    Some((block, loc_start.merge(loc_block)))
                }
                (None, Some(block)) => Some(block),
                (Some(loc_start), None) => {
                    p.raise(*diagnostic_missingblock, loc_start.new_from_end());
                    Some((T::default(), loc_start))
                }
                (None, None) => None,
            }
        }

        if let Some(loc) = self.next_if_eq(TokenContent::Keyword(TKeyword::Unit)) {
            // ASTDataContents::Unit //
            Some((ASTDataContents::Unit, loc))
        } else if let Some(loc) = self.next_if_eq(TokenContent::Keyword(TKeyword::Abstract)) {
            // ASTDataContents::Abstract //
            Some((ASTDataContents::Abstract, loc))
        } else if let Some(loc_start) = self.next_if_eq(TokenContent::Keyword(TKeyword::Enum)) {
            // ASTDataContents::Enum //
            let (variants, loc_enum) = self
                .parse_brackets(TBracketType::Curly, true, |p| {
                    p.parse_list_commasep(
                        Self::parse_enum_variant,
                        &ParseDiagnostic::ExpectedDataProperty,
                    )
                    .into_iter()
                    .filter_map(Result::ok)
                    .collect()
                })
                .unwrap_or_else(|| (Vec::new(), loc_start));

            Some((
                ASTDataContents::Enum { variants },
                loc_start.merge(loc_enum),
            ))
        } else if let Some((properties, loc)) = {
            // ASTDataContents::Struct //
            let loc_keyword = self.next_if_eq(TokenContent::Keyword(TKeyword::Struct));
            let block = self.parse_brackets(TBracketType::Curly, true, |p| {
                p.parse_list_commasep(
                    Self::parse_data_property,
                    &ParseDiagnostic::ExpectedDataProperty,
                )
                .into_iter()
                .filter_map(Result::ok)
                .collect()
            });
            finish_struct_and_tuple(
                self,
                loc_keyword,
                block,
                &ParseDiagnostic::ExpectedDataStructBody,
            )
        } {
            Some((ASTDataContents::Struct { properties }, loc))
        } else if let Some((properties, loc)) = {
            // ASTDataContents::Tuple //
            let loc_keyword = self.next_if_eq(TokenContent::Keyword(TKeyword::Tuple));
            let block = self.parse_brackets(TBracketType::Paren, true, |p| {
                p.parse_list_commasep(
                    Self::parse_type,
                    &ParseDiagnostic::ExpectedDataTupleTypeEntry,
                )
            });
            finish_struct_and_tuple(
                self,
                loc_keyword,
                block,
                &ParseDiagnostic::ExpectedDataTupleBody,
            )
        } {
            Some((ASTDataContents::Tuple { properties }, loc))
        } else {
            None
        }
    }
    fn parse_enum_variant(&mut self) -> Option<ASTEnumVariant<'src>> {
        self.guard_state(|p| {
            let annot = p.parse_annot();
            let name = p.parse_name()?;
            let (contents, loc_contents) = if let Some((properties, loc)) = {
                // ASTEnumVariantType::Struct //
                p.parse_brackets(TBracketType::Curly, true, |p| {
                    p.parse_list_commasep(
                        Self::parse_data_property,
                        &ParseDiagnostic::ExpectedDataProperty,
                    )
                    .into_iter()
                    .filter_map(Result::ok)
                    .collect()
                })
            } {
                (ASTEnumVariantType::Struct { properties }, Some(loc))
            } else if let Some((properties, loc)) = {
                // ASTEnumVariantType::Tuple //
                p.parse_brackets(TBracketType::Paren, true, |p| {
                    p.parse_list_commasep(
                        Self::parse_type,
                        &ParseDiagnostic::ExpectedDataTupleTypeEntry,
                    )
                })
            } {
                (ASTEnumVariantType::Tuple { properties }, Some(loc))
            } else {
                // ASTEnumVariantType::Unit //
                (ASTEnumVariantType::Unit, None)
            };

            Some(ASTEnumVariant {
                loc: name.loc.merge_some(loc_contents),
                annot,
                name,
                contents,
            })
        })
    }
    fn parse_data_property(&mut self) -> Option<ASTDataProperty<'src>> {
        self.guard_state(|p| {
            let annot = p.parse_annot();
            let name = p.parse_name()?;

            let (ty, loc_ty) = p.parse_type_optional(true);

            Some(ASTDataProperty {
                loc: name.loc().merge_some(loc_ty),
                annot,
                name,
                ty,
            })
        })
    }
    fn parse_bound_impl(&mut self) -> Option<ASTImpl<'src>> {
        self.guard_state(|p| {
            let (target, attatched_impl) = p._parse_impl()?;
            match target {
                Some(_) => None,
                None => Some(attatched_impl),
            }
        })
    }
    fn parse_free_impl(&mut self) -> Option<ASTFreeImpl<'src>> {
        self.guard_state(|p| {
            let (target, attatched_impl) = p._parse_impl()?;
            let containing_scope = p.current_scope();
            Some(match target {
                Some(target) => ASTFreeImpl {
                    target,
                    attatched_impl,
                    containing_scope,
                },
                None => ASTFreeImpl {
                    target: Err(p.raise(ParseDiagnostic::ExpectedImplTarget, attatched_impl.loc())),
                    attatched_impl,
                    containing_scope,
                },
            })
        })
    }
    /// Parses `impl` blocks, returning `(target, impl)`.
    fn _parse_impl(&mut self) -> Option<(Option<Fallible<ASTType<'src>>>, ASTImpl<'src>)> {
        let loc_start = self.next_if_eq(TokenContent::Keyword(TKeyword::Impl))?;

        let templates = self.parse_templates();
        let target_trait = self.parse_type();

        let target = self
            .next_if_eq(TokenContent::Keyword(TKeyword::For))
            .map(|loc_for| {
                let target = self.parse_type().ok_or_else(|| {
                    self.raise(
                        ParseDiagnostic::ExpectedImplTargetAfterFor,
                        loc_start.merge_some(merge!(loco!(target_trait), loco!(templates))),
                    )
                });
                (target, loc_start.merge(loc_for))
            });

        let contents = self.parse_impl_contents().unwrap_or_else(|| {
            let loc = merge!(loco!(target), loco!(target_trait), loco!(templates))
                .unwrap_or(loc_start)
                .new_from_end();
            self.raise(ParseDiagnostic::ExpectedImplContents, loc);
            ASTImplContents {
                loc,
                functions: Vec::new(),
                consts: Vec::new(),
                types: Vec::new(),
            }
        });

        Some((
            target.map(|(target, _)| target),
            ASTImpl {
                loc: loc_start.merge(contents.loc()),
                templates,
                target_trait,
                contents,
            },
        ))
    }
    fn parse_impl_contents(&mut self) -> Option<ASTImplContents<'src>> {
        self.parse_brackets(TBracketType::Curly, true, |p| {
            let r = p.parse_list(
                |p| {
                    p.guard_state(|p| {
                        let mut annot = p.parse_annot();
                        if let Some(x) = p.parse_function(&mut annot) {
                            Some((Some(x), None, None))
                        } else if let Some(x) = p.parse_const(&mut annot) {
                            Some((None, Some(x), None))
                        } else if let Some(x) = p.parse_type_alias(&mut annot) {
                            Some((None, None, Some(x)))
                        } else {
                            None
                        }
                    })
                },
                &ParseDiagnostic::ExpectedImplContentEntry,
            );
            let mut functions = Vec::new();
            let mut consts = Vec::new();
            let mut types = Vec::new();
            for (function, const_, type_) in r {
                if let Some(function_) = function {
                    functions.push(function_);
                }
                if let Some(const_) = const_ {
                    consts.push(const_);
                }
                if let Some(type_) = type_ {
                    types.push(type_);
                }
            }

            (functions, consts, types)
        })
        .map(|((functions, consts, types), loc)| ASTImplContents {
            loc,
            functions,
            consts,
            types,
        })
    }
    fn parse_trait_impl_contents(
        &mut self,
    ) -> Option<(
        Loc,
        Vec<ASTFunction<'src>>,
        Vec<ASTTraitConst<'src>>,
        Vec<ASTTraitTypeAlias<'src>>,
    )> {
        self.parse_brackets(TBracketType::Curly, true, |p| {
            let r = p.parse_list(
                |p| {
                    p.guard_state(|p| {
                        let mut annot = p.parse_annot();
                        if let Some(x) = p.parse_function(&mut annot) {
                            Some((Some(x), None, None))
                        } else if let Some(x) = p.parse_trait_const(&mut annot) {
                            Some((None, Some(x), None))
                        } else if let Some(x) = p.parse_trait_type_alias(&mut annot) {
                            Some((None, None, Some(x)))
                        } else {
                            None
                        }
                    })
                },
                &ParseDiagnostic::ExpectedImplContentEntry,
            );
            let mut functions = Vec::new();
            let mut consts = Vec::new();
            let mut types = Vec::new();
            for (function, const_, type_) in r {
                if let Some(function_) = function {
                    functions.push(function_);
                }
                if let Some(const_) = const_ {
                    consts.push(const_);
                }
                if let Some(type_) = type_ {
                    types.push(type_);
                }
            }

            (functions, consts, types)
        })
        .map(|((functions, consts, types), loc)| (loc, functions, consts, types))
    }
    fn parse_type_alias(&mut self, annot: &mut ASTAnnot<'src>) -> Option<ASTTypeAlias<'src>> {
        let loc_start = self.next_if_eq(TokenContent::Keyword(TKeyword::Type))?;

        let name = self
            .parse_name()
            .ok_or_else(|| self.raise(ParseDiagnostic::ExpectedTypealiasName, loc_start));

        let templates = self.parse_templates();

        let (value, loc_eq) =
            if let Some(loc_eq) = self.next_if_eq(TokenContent::Symbol(TSymbol::Assign)) {
                (self.parse_type(), Some(loc_eq))
            } else {
                (None, None)
            };
        let value = value.ok_or_else(|| {
            self.raise(
                ParseDiagnostic::ExpectedTypealiasValue,
                loc_start.merge_some(merge!(loc_eq, loco!(templates), locr!(name))),
            )
        });

        Some(ASTTypeAlias {
            loc: loc_start.merge_some(merge!(locr!(value), loc_eq, loco!(templates), locr!(name))),
            annot: annot.take(),
            containing_scope: self.current_scope(),
            name,
            templates,
            value,
        })
    }
    fn parse_trait_type_alias(
        &mut self,
        annot: &mut ASTAnnot<'src>,
    ) -> Option<ASTTraitTypeAlias<'src>> {
        let loc_start = self.next_if_eq(TokenContent::Keyword(TKeyword::Type))?;

        let name = self
            .parse_name()
            .ok_or_else(|| self.raise(ParseDiagnostic::ExpectedTypealiasName, loc_start));

        let (bounds, loc_bounds) = self.parse_bounds();

        Some(ASTTraitTypeAlias {
            loc: loc_start.merge_some(merge!(loc_bounds, locr!(name))),
            annot: annot.take(),
            containing_scope: self.current_scope(),
            name,
            bounds,
        })
    }
    fn parse_import(&mut self) -> Option<ASTImport<'src>> {
        let loc_start = self.next_if_eq(TokenContent::Keyword(TKeyword::Import))?;
        let tree = self
            ._parse_import_tree()
            .ok_or_else(|| self.raise(ParseDiagnostic::ExpectedImportTreeBase, loc_start));
        let loc = loc_start.merge_some(tree.as_ref().ok().map(HasLoc::loc));
        Some(ASTImport {
            loc,
            containing_scope: self.current_scope(),
            tree,
        })
    }
    fn _parse_import_tree(&mut self) -> Option<ASTImportTree<'src>> {
        if let Some(ident) = self.parse_ident() {
            Some(
                if let Some(loc_dot) = self.next_if_eq(TokenContent::Symbol(TSymbol::Period)) {
                    if let Some((inner, loc_inner)) =
                        self.parse_brackets(TBracketType::Square, true, |p| {
                            p.parse_list_commasep(
                                Self::_parse_import_tree,
                                &ParseDiagnostic::ExpectedImportTreeInner,
                            )
                            .into_iter()
                            .filter_map(Result::ok)
                            .collect()
                        })
                    {
                        // Self::Group //
                        ASTImportTree::Group {
                            loc: ident.loc.merge(loc_inner),
                            ident,
                            inner,
                        }
                    } else if let Some(inner) = self._parse_import_tree() {
                        // Self::Name //
                        ASTImportTree::Name {
                            loc: ident.loc.merge(inner.loc()),
                            ident,
                            inner: Some(Ok(Box::new(inner))),
                        }
                    } else {
                        // Self::Name (no following) //
                        ASTImportTree::Name {
                            loc: ident.loc,
                            ident,
                            inner: Some(Err(self.raise(
                                ParseDiagnostic::ExpectedImportTreeAfterDot,
                                loc_dot.new_from_end(),
                            ))),
                        }
                    }
                } else {
                    // Self::Name (no following) //
                    ASTImportTree::Name {
                        loc: ident.loc,
                        ident,
                        inner: None,
                    }
                },
            )
        } else if let Some(loc) = self.next_if_eq(TokenContent::Symbol(TSymbol::Asterisk)) {
            // Self::ToAll //
            Some(ASTImportTree::ToAll(loc))
        } else {
            None
        }
    }

    fn parse_templates(&mut self) -> ASTTemplates<'src> {
        self.parse_brackets(TBracketType::Angle, true, |p| {
            p.parse_list_commasep(
                |p| {
                    let name = p.parse_name()?;
                    let (bounds, loc_bounds) = p.parse_bounds();

                    Some(ASTTemplateBound {
                        loc: name.loc.merge_some(loc_bounds),
                        name,
                        bounds,
                    })
                },
                &ParseDiagnostic::ExpectedBound,
            )
            .into_iter()
            .filter_map(Result::ok)
            .collect()
        })
    }
    /// NOTE: `None` no-consume rule is goverened by the `Option<Loc>` return value in this case.
    fn parse_bounds(&mut self) -> (Vec<ASTType<'src>>, Option<Loc>) {
        if let Some(loc_colon) = self.next_if_eq(TokenContent::Symbol(TSymbol::Colon)) {
            let (bounds, loc_bounds) = self.parse_list_require_sep(
                |p| p.parse_type(),
                TokenContent::Symbol(TSymbol::Or),
                &ParseDiagnostic::ExpectedBoundEntry,
                &ParseDiagnostic::ExpectedBoundUnionSep,
                loc_colon,
            );
            (bounds, Some(loc_bounds))
        } else {
            (Vec::new(), None)
        }
    }

    /// Parses a doc comment. Note that we dont use [`Self::next`], becasue that skips comments.
    fn parse_doc(&mut self) -> Option<ASTDoc> {
        self.guard_state(|p| {
            let mut contents: Option<Vec<(&'src str, Loc)>> = None;

            loop {
                match p.tokens.get(p.i).copied() {
                    Some(Token {
                        content: TokenContent::Space { .. },
                        ..
                    }) => {
                        // skip whitespace
                        p.i += 1;
                        continue;
                    }
                    Some(Token {
                        content:
                            TokenContent::Comment {
                                content,
                                documenting,
                                missing_closing,
                            },
                        loc,
                    }) => {
                        if missing_closing {
                            // we are consuming here, so this error must be reported
                            p.raise(ParseDiagnostic::CommentMissingClosing, loc);
                        }
                        if documenting {
                            contents.get_or_insert_with(Vec::new).push((content, loc));
                        } else {
                            // skip regular comments
                            p.i += 1;
                            continue;
                        }
                    }
                    _ => break,
                }
            }

            contents
        })
        .map(|contents| {
            // process contents, this process regularizes indentation and strips the '*' from multiline comments that look like this:
            /*
             *
             */

            // contents guaranteed to have at least one element, first and last always valid.
            let loc = contents
                .first()
                .unwrap()
                .1
                .merge(contents.last().unwrap().1);

            let inline_indent = contents
                .iter()
                .filter_map(|(line, _)| line.chars().enumerate().find(|(_, c)| *c != ' '))
                .filter_map(|(len, c)| if c.is_whitespace() { None } else { Some(len) })
                .min()
                .unwrap_or(0);
            let outline_indent = contents
                .iter()
                .flat_map(|(line, _)| line.split('\n').skip(1))
                .filter(|line| {
                    let trimmed = line.trim();
                    !(trimmed == "" || trimmed == "*")
                })
                .map(|line| {
                    let mut had_star = false;
                    line.chars()
                        .take_while(|it| match (it, had_star) {
                            (' ', _) => true,
                            ('*', false) => {
                                had_star = true;
                                true
                            }
                            _ => false,
                        })
                        .count()
                })
                .min()
                .unwrap_or(0);

            let mut processed_text = String::new();
            for line in contents
                .into_iter()
                .flat_map(|(subcontent, _)| {
                    let mut iter = subcontent.split('\n');
                    let first_line = iter.next().unwrap().get(inline_indent..).unwrap();
                    std::iter::once(first_line)
                        .chain(iter.map(|line| line.get(outline_indent..).unwrap()))
                })
                .map(|line| line.trim_end())
                .skip_while(|line| line.is_empty())
            {
                processed_text.push_str(line);
                processed_text.push('\n')
            }
            processed_text.drain(..processed_text.trim_end().len());

            ASTDoc {
                loc,
                processed_text,
            }
        })
    }
    fn parse_annot(&mut self) -> ASTAnnot<'src> {
        let mut doc = Vec::new();
        let mut attrs = Vec::new();
        loop {
            if let Some(value) = self.parse_doc() {
                doc.push(value);
            } else if let Some(attr) = self.parse_macro(TSymbol::MacroAttr) {
                if let Ok(attr) = attr {
                    attrs.push(attr);
                }
            } else {
                break;
            }
        }
        let is_public = self.next_if_eq(TokenContent::Keyword(TKeyword::Export));
        ASTAnnot {
            doc,
            attrs,
            is_public,
        }
    }

    fn parse_macro(
        &mut self,
        leading_symbol: TSymbol,
    ) -> Option<Fallible<ASTMacroInvocation<'src>>> {
        let loc_start = self.next_if_eq(TokenContent::Symbol(leading_symbol))?;

        Some((|| {
            let name = self.parse_name().ok_or_else(|| {
                self.diagnostics
                    .raise(ParseDiagnostic::ExpectedMacroName, loc_start.new_from_end())
            });

            let body = self._parse_macro_body().ok_or_else(|| {
                self.diagnostics.raise(
                    ParseDiagnostic::ExpectedMacroBody,
                    loc_start.merge_some(locr!(name)),
                )
            });

            Ok(ASTMacroInvocation {
                loc: loc_start,
                name: name?,
                body: body?,
            })
        })())
    }
    fn _parse_macro_body(&mut self) -> Option<ASTMacroInvocationBody<'src>> {
        Some(if let Some(name) = self.parse_name() {
            ASTMacroInvocationBody::Name { name }
        } else if let Some((exprs, loc)) = self.parse_brackets(TBracketType::Curly, true, |p| {
            p.parse_list_commasep(|p| p.parse_expr_greedy(), &ParseDiagnostic::ExpectedExpr)
        }) {
            ASTMacroInvocationBody::Expr { loc, exprs }
        } else {
            todo!("finish macro body parsing")
        })
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

                    let condition = self
                        .parse_expr_before_block()
                        .ok_or_else(|| self.raise(ParseDiagnostic::ExpectedIfCondition, loc_start));
                    let block = self.parse_block().ok_or_else(|| {
                        self.raise(
                            ParseDiagnostic::ExpectedIfBlock,
                            loc_start.merge_some(merge!(locr!(condition))),
                        )
                    });
                    let mut loc_end = merge!(locr!(block), locr!(condition)).unwrap_or(loc_start);
                    let mut elifs = Vec::new();
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

                    let block = self
                        .parse_block()
                        .ok_or_else(|| self.raise(ParseDiagnostic::ExpectedLoopBlock, loc_start));

                    Some(ASTSubBlocked::Loop {
                        loc: loc_start.merge_some(locr!(block)),
                        block,
                    })
                }
                TKeyword::While => {
                    // ASTSubBlocked::While //

                    let condition = self.parse_expr_before_block().ok_or_else(|| {
                        self.raise(ParseDiagnostic::ExpectedWhileCondition, loc_start)
                    });
                    let block = self.parse_block().ok_or_else(|| {
                        self.raise(
                            ParseDiagnostic::ExpectedWhileBlock,
                            loc_start.merge_some(locr!(condition)),
                        )
                    });

                    let mut loc_end = merge!(locr!(block), locr!(condition)).unwrap_or(loc_start);
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

                    let var = self
                        .parse_typed_destructure(true)
                        .ok_or_else(|| self.raise(ParseDiagnostic::ExpectedForVar, loc_start));
                    let keyword_in = self
                        .next_if_eq(TokenContent::Keyword(TKeyword::In))
                        .ok_or_else(|| {
                            self.raise(
                                ParseDiagnostic::ExpectedForIn,
                                loc_start.merge_some(locr!(var)),
                            )
                        });
                    let iterator = self.parse_expr_before_block().ok_or_else(|| {
                        self.raise(
                            ParseDiagnostic::ExpectedForIterator,
                            loc_start.merge_some(merge!(locr!(keyword_in), locr!(var))),
                        )
                    });
                    let block = self.parse_block().ok_or_else(|| {
                        self.raise(
                            ParseDiagnostic::ExpectedForBlock,
                            loc_start.merge_some(merge!(
                                locr!(iterator),
                                locr!(keyword_in),
                                locr!(var)
                            )),
                        )
                    });

                    let mut loc_end =
                        merge!(locr!(block), locr!(iterator), locr!(var), keyword_in.ok())
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
        elifs: &mut Vec<(Fallible<Box<ASTExpr<'src>>>, Fallible<ASTBlock<'src>>)>,
        else_block: &mut Option<Fallible<ASTBlock<'src>>>,
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
            let condition = self
                .parse_expr_before_block()
                .ok_or_else(|| self.raise(ParseDiagnostic::ExpectedIfCondition, loc_if));
            let block = self.parse_block().ok_or_else(|| {
                self.raise(
                    ParseDiagnostic::ExpectedIfBlock,
                    loc_if.merge_some(locr!(condition)),
                )
            });

            *loc_end = merge!(locr!(block), locr!(condition)).unwrap_or(loc_if);
            elifs.push((condition.map(Box::new), block));
            true
        } else {
            // matching `else ...`
            let else_block_read = self
                .parse_block()
                .ok_or_else(|| self.raise(ParseDiagnostic::ExpectedElseBlock, loc_else));
            if let Ok(else_block) = &else_block_read {
                *loc_end = else_block.loc;
            } else {
                *loc_end = loc_else;
            }
            *else_block = Some(else_block_read);
            false
        }
    }
    fn _parse_subblocked_else(&mut self, loc_end: &mut Loc) -> Option<Fallible<ASTBlock<'src>>> {
        let loc_else = self.next_if_eq(TokenContent::Keyword(TKeyword::Else))?;

        let else_block = self
            .parse_block()
            .ok_or_else(|| self.raise(ParseDiagnostic::ExpectedElseBlock, loc_else));
        if let Ok(else_block) = &else_block {
            *loc_end = else_block.loc;
        } else {
            *loc_end = loc_else;
        }
        Some(else_block)
    }

    fn parse_var_declare(&mut self) -> Option<ASTVarDeclare<'src>> {
        let loc_start = self.next_if_eq(TokenContent::Keyword(TKeyword::Let))?;

        let declr = self
            .parse_typed_destructure(false)
            .ok_or_else(|| self.raise(ParseDiagnostic::ExpectedDestructure, loc_start));

        let (initializer, loc_assign_symbol) =
            if let Some(loc_assign_symbol) = self.next_if_eq(TokenContent::Symbol(TSymbol::Assign))
            {
                let expr = self.parse_expr_non_greedy().map(Box::new).ok_or_else(|| {
                    self.raise(ParseDiagnostic::ExpectedAssignValue, loc_assign_symbol)
                });

                (Some(expr), Some(loc_assign_symbol))
            } else {
                (None, None)
            };

        Some(ASTVarDeclare {
            loc: loc_start.merge_some(merge!(
                initializer
                    .as_ref()
                    .and_then(|it| it.as_ref().ok().map(|it| it.loc())),
                loco!(loc_assign_symbol),
                locr!(declr)
            )),
            declr,
            initializer,
        })
    }

    fn parse_typed_destructure(
        &mut self,
        allow_leading_newline: bool,
    ) -> Option<ASTTypedDestructure<'src>> {
        let destructure = self.parse_destructure()?;
        let (ty, _) = self.parse_type_optional(allow_leading_newline);
        Some(ASTTypedDestructure { destructure, ty })
    }
    fn parse_destructure(&mut self) -> Option<ASTDestructure<'src>> {
        if let Some(name) = self.parse_name() {
            Some(ASTDestructure::Name(name))
        } else if let Some((inner, loc)) = self.parse_paren_or_tuple(
            |p| p.parse_destructure(),
            &ParseDiagnostic::ExpectedDestructureInner,
        ) {
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
    fn parse_paren_or_tuple<T: HasLoc + Clone, F: Fn(&mut Self) -> Option<T>>(
        &mut self,
        parse_contents: F,
        fail_diagnostic: &'static ParseDiagnostic,
    ) -> Option<(HelperParenOrTuple<T>, Loc)> {
        self.guard_state(|p| {
            p.parse_brackets(TBracketType::Paren, true, |p| {
                if p.peek(|t| t.next().is_none()) {
                    // empty, we have got a `()`, (empty tuple)
                    // Tuple //
                    HelperParenOrTuple::Tuple(Vec::new())
                } else if let Some(inner) = p.guard_state(|p| {
                    // the whole deal with this section of code is that we want to match one expression and _not_ match a
                    // comma, because if we do, that means this is actually a tuple.
                    const PARSE_COMMA: fn(&mut Parser) -> Option<Loc> =
                        |p| p.next_if_eq(TokenContent::Symbol(TSymbol::Comma));
                    let inner = match p.fail_until_parse_nonempty(
                        |p| p.parse_either(|p| parse_contents(p), PARSE_COMMA),
                        fail_diagnostic,
                    ) {
                        Err(err) => return Some(Err(err)), // invalid contents, return this to count it as a paren but mark it as invalid.
                        Ok(AorB::A(inner)) => inner,
                        Ok(AorB::B(_comma_)) => return None, // this is a tuple, skip to tuple parsing
                    };
                    let comma_after =
                        p.fail_until_parse(PARSE_COMMA, &ParseDiagnostic::ExpectedClosing);
                    if comma_after.is_some() {
                        return None; // comma detected! this is a tuple, skip to tuple parsing
                    }

                    Some(Ok(inner))
                }) {
                    // Paren //
                    HelperParenOrTuple::Paren(inner.map(Box::new))
                } else {
                    // Tuple //
                    HelperParenOrTuple::Tuple(p.parse_list_commasep(
                        |p| parse_contents(p),
                        &ParseDiagnostic::ExpectedTupleEntry,
                    ))
                }
            })
        })
    }

    fn parse_ident(&mut self) -> Option<ASTIdent<'src>> {
        self.next_if_peek_and(|token, _| {
            Some(match token {
                TokenContent::Identifier(ident) => ASTIdentValue::Name(ident),
                TokenContent::Keyword(TKeyword::SelfTy) => ASTIdentValue::SelfTy,
                TokenContent::Keyword(TKeyword::SelfVar) => ASTIdentValue::SelfVar,
                TokenContent::Keyword(TKeyword::Root) => ASTIdentValue::Root,
                TokenContent::Keyword(TKeyword::Super) => ASTIdentValue::Super,
                _ => None?,
            })
        })
        .map(|(value, loc)| ASTIdent { value, loc })
    }
    fn parse_name(&mut self) -> Option<ASTName<'src>> {
        self.next_if_peek_and(|token, _| match token {
            TokenContent::Identifier(ident) => Some(ident),
            _ => None,
        })
        .map(|(value, loc)| ASTName { value, loc })
    }
}

/// Parse a single source file into its corresponding AST (abstraxt syntax tree),
/// returning any issues found with the source code as a list of diagnostics.
pub fn parse_from_source<'src>(
    src: &'src str,
    src_id: SourceFileId,
    diagnostics: &mut Diagnostics,
    scopes: &mut Scopes<ASTScope<'src>>,
) -> ASTSourceFile<'src> {
    let tokens = super::tokenize::tokenize(src, src_id);
    let mut parser = Parser::new(&tokens, diagnostics, scopes);
    let source_file = parser.parse_source_file();
    source_file
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Testing

#[cfg(test)]
mod test {
    use crate::{
        front::{
            ast::{ASTExpr, ASTIdent, ASTIdentValue},
            parse::{ParseDiagnostic, Parser},
            source::Loc,
            tokenize::{
                tokenize, TBracketType, TInfixOperatorType, TPrefixOperatorType, TokenContent,
            },
        },
        lint::diagnostic::{Diagnostic, DiagnosticContent, Diagnostics},
        middle::statics::scopes::Scopes,
    };

    fn loc(s: usize, l: usize) -> Loc {
        Loc {
            src_id: 0, // placeholder for testing
            start: s,
            length: l,
        }
    }
    fn var(n: &'static str, loc: Loc) -> ASTExpr<'static> {
        ASTExpr::Ident(ASTIdent {
            loc,
            value: ASTIdentValue::Name(n),
        })
    }

    #[test]
    fn mismatched_brackets_automatically_close() {
        let tokens = tokenize("{ ( 3 }", 0);
        let mut diagnostics = Diagnostics::init();
        let mut scopes = Scopes::init();
        let mut p = Parser::new(&tokens, &mut diagnostics, &mut scopes);
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
                radix: 10,
                unparsed: "3"
            })))
        );
        assert_eq!(
            p.diagnostics.dbg_as_slice(),
            [Diagnostic {
                content: DiagnosticContent::Parse(ParseDiagnostic::MissingClosing {
                    expected: TBracketType::Paren
                }),
                loc: loc(6, 0),
            }]
            .as_slice()
        );
    }
    #[test]
    fn expression_operator_parsing_works_0() {
        let mut scopes = Scopes::init();
        let t = crate::front::tokenize::tokenize("a + b * * c", 0);
        let mut diagnostics = Diagnostics::init();
        let mut p = Parser::new(&t, &mut diagnostics, &mut scopes);
        assert_eq!(
            p.parse_expr_greedy(),
            // -> (a + (b * (*c)))
            Some(ASTExpr::OpInfix {
                loc: loc(0, 11),
                op: (TInfixOperatorType::Add, loc(2, 1)),
                inner: (
                    Box::new(var("a", loc(0, 1))),
                    Box::new(ASTExpr::OpInfix {
                        loc: loc(4, 7),
                        op: (TInfixOperatorType::Multiply, loc(6, 1)),
                        inner: (
                            Box::new(var("b", loc(4, 1))),
                            Box::new(ASTExpr::OpPrefix {
                                loc: loc(8, 3),
                                op: (TPrefixOperatorType::Deref, loc(8, 1)),
                                inner: Box::new(var("c", loc(10, 1))),
                            }),
                        ),
                    }),
                ),
            })
        );
    }
}
