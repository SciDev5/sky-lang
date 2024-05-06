use std::{
    fmt::Debug,
    iter::{Enumerate, Map},
    ops::{Deref, DerefMut},
    str::CharIndices,
};

use super::{Loc, Token, TokenContent};

/// Iterator with a small state stack for rewinding to past states.
pub(super) struct Rewindable<Iter: Iterator + Clone + Debug> {
    current: Iter,
    stack: heapless::Vec<Iter, 3>,
}
impl<Iter: Iterator + Clone + Debug> Rewindable<Iter> {
    fn new(iter: Iter) -> Self {
        Self {
            current: iter,
            stack: heapless::Vec::new(),
        }
    }
}
impl<Iter: Iterator + Clone + Debug> Rewindable<Iter> {
    /// Saves the current state to the stack.
    pub fn push(&mut self) {
        self.stack.push(self.current.clone()).unwrap()
    }
    /// Discards the top stack state, keeping the current state.
    ///
    /// Returns the iterator dropped from the top of the stack.
    pub fn pop_continue(&mut self) -> Iter {
        self.stack.pop().unwrap()
    }
    /// Pops the top stack state and returns it to being the current state.
    pub fn pop_rewind(&mut self) {
        self.current = self.stack.pop().unwrap();
    }
    /// Take the next value off the top without incrementing the current.
    pub fn peek_next(&mut self) -> Option<Iter::Item> {
        self.push();
        let next = self.next();
        self.pop_rewind();
        next
    }
}
impl<Iter: Iterator + Clone + Debug> Iterator for Rewindable<Iter> {
    type Item = Iter::Item;
    fn next(&mut self) -> Option<Self::Item> {
        self.current.next()
    }
}

type TokenizeNext<'src> = fn(&mut TokenizeIter<'src>) -> Option<TokenContent<'src>>;
pub(super) struct CharIndex {
    pub char: char,
    pub byte_i: usize,
    pub char_i: usize,
}

type K<'src> =
    Rewindable<Map<Enumerate<CharIndices<'src>>, fn((usize, (usize, char))) -> CharIndex>>;
pub(super) struct TokenizeIter<'src> {
    pub(super) char_iter: K<'src>,
    src: &'src str,
    chars_len: usize,
    tokenize_next: TokenizeNext<'src>,
}
impl<'src> Deref for TokenizeIter<'src> {
    type Target = K<'src>;
    fn deref(&self) -> &Self::Target {
        &self.char_iter
    }
}
impl<'src> DerefMut for TokenizeIter<'src> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.char_iter
    }
}
impl<'src> TokenizeIter<'src> {
    pub fn new(src: &'src str, tokenize_next: TokenizeNext<'src>) -> Self {
        Self {
            char_iter: Rewindable::new(src.char_indices().enumerate().map(
                |(char_i, (byte_i, char))| CharIndex {
                    char,
                    byte_i,
                    char_i,
                },
            )),
            src,
            chars_len: src.chars().count(),
            tokenize_next,
        }
    }
    /// Increments the current iterator if the provided function returns true.
    /// Returns Some(char)  or None, if the end
    /// of the source was reached.
    pub fn next_if(&mut self, f: fn(char) -> bool) -> Option<char> {
        self.push();
        if let Some(next) = self.char_iter.next() {
            if f(next.char) {
                let _ = self.pop_continue();
                Some(next.char)
            } else {
                self.pop_rewind();
                None
            }
        } else {
            self.pop_rewind();
            None
        }
    }
    pub fn pop_continue_as_str(&mut self) -> &'src str {
        let start = self
            .pop_continue()
            .next()
            .map(|CharIndex { byte_i, .. }| byte_i)
            .unwrap_or(self.src.len());
        let end = self
            .peek_next()
            .map(|CharIndex { byte_i, .. }| byte_i)
            .unwrap_or(self.src.len());
        unsafe { self.src.get_unchecked(start..end) }
    }
    fn pop_continue_as_loc(&mut self) -> Loc {
        let start = self
            .pop_continue()
            .next()
            .map(|CharIndex { char_i, .. }| char_i)
            .unwrap_or(self.chars_len);
        let length = self
            .peek_next()
            .map(|CharIndex { char_i, .. }| char_i)
            .unwrap_or(self.chars_len)
            - start;
        Loc { start, length }
    }
}

impl<'src> Iterator for TokenizeIter<'src> {
    type Item = Token<'src>;
    fn next(&mut self) -> Option<Self::Item> {
        if self.char_iter.peek_next().is_none() {
            return None;
        }
        self.push();
        let content = if let Some(next) = (self.tokenize_next)(self) {
            next
        } else {
            // begin matching [`TokenContent::Unknown`]
            loop {
                // Step internal iterator and check for end of file.
                if self.char_iter.next().is_none() {
                    break;
                }
                self.push();
                if (self.tokenize_next)(self).is_none() {
                    // still in the unknown
                    let _ = self.pop_continue();
                    continue;
                } else {
                    // no longer in unknown, rewind to put current
                    // pointer at the end of the unknown.
                    self.pop_rewind();
                    break;
                }
            }
            TokenContent::Unknown
        };
        Some(Token {
            loc: self.pop_continue_as_loc(),
            content,
        })
    }
}
