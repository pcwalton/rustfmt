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

// rustfmt/main.rs

#![crate_name="rustfmt"]
#![desc = "Rust code formatter"]
#![license = "MIT"]
#![feature(macro_rules)]

extern crate syntax;

use std::io;
use std::str;
use syntax::parse::lexer;
use syntax::parse;

mod rustfmt;
#[cfg(test)]
mod test;

/// The Main Function
pub fn main() {
    let source = io::stdin().read_to_end().unwrap();
    let source = str::from_utf8(source.as_slice()).unwrap();

    // nothing special
    let session = parse::new_parse_sess();
    let filemap = parse::string_to_filemap(&session, source.to_string(), "<stdin>".to_string());
    let lexer = lexer::StringReader::new(&session.span_diagnostic, filemap);
    let mut stdout = io::stdio::stdout();
    let mut formatter = rustfmt::Formatter::new(lexer, &mut stdout);

    loop {
        match formatter.next_token() {
            Ok(true) => {
                match formatter.parse_production() {
                    Err(e) => fail!(e),
                    _ => {}
                }
            },
            Ok(false) => break,
            Err(e) => fail!(e)
        }
    }
}
