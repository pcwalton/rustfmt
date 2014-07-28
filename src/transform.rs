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

// src/transform.rs

use syntax::diagnostic::SpanHandler;
use syntax::parse::lexer::{TokenAndSpan};
use syntax::parse::token;

use token::{Comment, LexerVal, BlankLine, TransformedToken};

pub type TransformerResult<T> = Result<T, String>;

#[allow(dead_code)]
pub fn has_blank_line<'a>(ws_str: &'a str) -> bool {
    use std::str::StrSlice;
    let newlines: Vec<(uint, uint)> = ws_str.match_indices("\n").collect();
    let newline_count = newlines.len();
    newline_count > 1
}

pub fn transform_tokens(in_toknspans: &[TransformedToken], span_handler: &SpanHandler) -> TransformerResult<Vec<TransformedToken>> {
    let mut out_tokens = Vec::new();
    let mut curr_idx = 0;
    let in_len = in_toknspans.len();
    loop {
        if curr_idx >= in_len {
            break
        }

        let current_token = &in_toknspans[curr_idx];

        match current_token {
            &LexerVal(ref current_token) => {
                match current_token {
                    t @ &TokenAndSpan { tok: token::WS, sp: _ } => {
                        let ws_str = span_handler.cm.span_to_snippet(t.sp).unwrap();
                        if has_blank_line(ws_str.as_slice()) {
                            out_tokens.push(BlankLine);
                        }
                        curr_idx += 1;
                    },
                    t @ &TokenAndSpan { tok: token::COMMENT, sp: _ } => {
                        handle_comment(in_toknspans, &mut out_tokens, &mut curr_idx, span_handler, t);
                    }
                    t => {
                        out_tokens.push(LexerVal(t.clone()));
                        curr_idx += 1;
                    }
                }
            },
            t => {
                out_tokens.push(t.clone());
                curr_idx += 1;
            }
        }
    }
    Ok(out_tokens)
}

fn handle_comment(in_toknspans: &[TransformedToken], out_tokens: &mut Vec<TransformedToken>, curr_idx: &mut uint, span_handler: &SpanHandler, t: &TokenAndSpan) {
    let curr_idx_cpy = *curr_idx;
    let comment_str = span_handler.cm.span_to_snippet(t.sp).unwrap();
    let starts_line = {
        let last_token = if curr_idx_cpy == 0 {
            &in_toknspans[0]
        } else {
            &in_toknspans[curr_idx_cpy - 1]
        };
        last_token.contains_newline(span_handler)
    };
    let ends_line = {
        let next_token = if curr_idx_cpy + 1 >= in_toknspans.len() {
            &in_toknspans[in_toknspans.len() - 1]
        } else {
            &in_toknspans[curr_idx_cpy + 1]
        };
        next_token.contains_newline(span_handler)
    };
    out_tokens.push(Comment(comment_str, starts_line, ends_line));
    *curr_idx += 1;
}
