use crate::lint::diagnostic::{DiagnosticContent, ToDiagnostic};

use super::{
    ast::{ASTSourceFile, ASTStmt},
    source::Loc,
    tokenize::{TBracketType, Token, TokenContent},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ParseDiagnostic {
    ExpectedClosingUnexpectedJunk,
    MissingClosing { expected: TBracketType },

    ExpectedStmt,
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
    /// [`ParseDiagnostic::ExpectedClosingUnexpectedJunk`] is raised.
    /// - If the closing bracket mismatches what was expected, [`ParseDiagnostic::MissingClosing`] is raised.
    fn parse_brackets<T, F: Fn(&mut Self) -> T>(
        &mut self,
        bracket_ty: TBracketType,
        allow_leading_newline: bool,
        parse_contents: F,
    ) -> Option<T> {
        // try to match opening bracket and short to None if not found.
        self.next_if_peek_and(|token, newline| match token {
            _ if newline && !allow_leading_newline => None,
            TokenContent::Symbol(s) if s.as_bracket_open() == Some(bracket_ty) => Some(()),
            _ => None,
        })?;
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
            self.diagnostics
                .push((ParseDiagnostic::ExpectedClosingUnexpectedJunk, loc))
        }

        // consume closing paren if correct, else raise diagnostic
        if self
            .next_simple_if_peek_and(|token| match token {
                TokenContent::Symbol(s) if s.as_bracket_close() == Some(bracket_ty) => Some(()),
                _ => None,
            })
            .is_none()
        {
            let loc = self.tokens[self.i - 1].loc;
            self.diagnostics.push((
                ParseDiagnostic::MissingClosing {
                    expected: bracket_ty,
                },
                Loc {
                    start: loc.start + loc.length,
                    length: 0,
                },
            ));
        }

        // pop state and return
        self.bracket_stack.pop();
        Some(value)
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
                    self.diagnostics.push((*fail_diagnostic, start.merge(end)));
                }
            } else {
                match self.next() {
                    None => {
                        if let Some((start, end)) = failing {
                            self.diagnostics.push((*fail_diagnostic, start.merge(end)));
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

    ////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Language parsers
    //

    fn parse_stmt(&mut self) -> Option<ASTStmt<'src>> {
        todo!();
    }

    fn parse_stmt_list(&mut self) -> Vec<ASTStmt<'src>> {
        self.parse_list(Self::parse_stmt, &ParseDiagnostic::ExpectedStmt)
    }

    fn parse_source_file(&mut self) -> ASTSourceFile<'src> {
        ASTSourceFile {
            body: self.parse_stmt_list(),
        }
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
            }),
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
