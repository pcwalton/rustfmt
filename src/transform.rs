// Copyright (c) 2014 Patrick Walton <pcwalton@mozilla.com>
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

use syntax::parse::lexer::{TokenAndSpan};
use syntax::parse::token;

pub type TransformerResult<T> = Result<T, String>;

pub fn transform_tokens(in_toknspans: Vec<TokenAndSpan>) -> TransformerResult<Vec<TokenAndSpan>> {
    let mut out_tokens = Vec::new();
    let mut curr_idx = 0;
    let in_len = in_toknspans.len();
    loop {
        if curr_idx >= in_len {
            break
        }

        let current_token = &in_toknspans[curr_idx];

        match current_token {
            &TokenAndSpan { tok: token::WS, sp: _ } => curr_idx += 1,
            t => {
                out_tokens.push(t.clone());
                curr_idx += 1;
            }
        }
    }
    Ok(out_tokens)
}
