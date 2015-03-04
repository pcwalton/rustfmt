// Copyright (c) 2014 Mozilla Foundation
// Permission is hereby granted, free of charge, to any person obtaining
// a copy of this software and associated documentation files (the
// "Software"), to deal in the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// * The above copyright notice and this permission notice shall be
// included in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
// OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
// IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
// CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
// TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
// SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

// src/token.rs

use syntax::diagnostic::SpanHandler;
use syntax::parse::lexer::{StringReader, TokenAndSpan, Reader};
use syntax::parse::token;

use self::TransformedToken::{LexerVal, BlankLine, Comment};

#[derive(Clone)]
pub enum TransformedToken {
    LexerVal(TokenAndSpan),
    BlankLine,
    Comment(String, bool, bool)
}

impl TransformedToken {
    pub fn contains_newline(&self, sh: &SpanHandler) -> bool {
        match self {
            &BlankLine => true,
            &LexerVal(ref t) => {
                if t.tok == token::Whitespace {
                    let comment_str = sh.cm.span_to_snippet(t.sp).unwrap();
                    comment_str.as_slice().contains("\n")
                } else {
                    false
                }
            },
            &Comment(ref c, _, _) => c.as_slice().contains("\n")
        }
    }
}

pub fn extract_tokens(lexer: &mut StringReader) -> Vec<TransformedToken> {
    let mut in_toknspans = Vec::new();
    loop {
        match lexer.next_token() {
            t @ TokenAndSpan{tok: token::Eof, sp: _} => {
                in_toknspans.push(LexerVal(t));
                break;
            },
            t @ _ => in_toknspans.push(LexerVal(t))
        }
    }
    in_toknspans
}
