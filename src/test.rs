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

// rustfmt/test.rs

use rustfmt;

use std::io::MemWriter;
use std::str;
use syntax::parse::lexer;
use syntax::parse;

fn test_rustfmt(source: &str) -> String {

    // nothing special
    let session = parse::new_parse_sess();
    let filemap = parse::string_to_filemap(&session, source.to_string(), "<stdin>".to_string());
    let lexer = lexer::StringReader::new(&session.span_diagnostic, filemap);
    let mut output = MemWriter::new();
    {
        let mut formatter = rustfmt::Formatter::new(lexer, &mut output);
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
    str::from_utf8(output.unwrap().as_slice()).unwrap().to_string()
}

#[test]
fn can_format_a_basic_function() {
    let result = test_rustfmt("fn main() {}");
    assert_eq!(result,
"fn main() {
}
".to_string());
}

#[test]
fn adds_newline_after_attributes() {
    let result = test_rustfmt("#[foo]fn main() {}");
    assert_eq!(result,
"#[foo]
fn main() {
}
".to_string());
}

#[test]
fn adds_newline_after_doc_comments() {
    let result = test_rustfmt("/// The Main function
fn main() {}");
    assert_eq!(result,
"/// The Main function
fn main() {
}
".to_string());
}

#[test]
fn adds_newline_after_multiline_doc_comment() {
    let result = test_rustfmt(
"/*! The Main function
* some neat info goes here
* ````
* bleh();
* ````
*/
fn main() {}");
    assert_eq!(result,
"/*! The Main function
* some neat info goes here
* ````
* bleh();
* ````
*/
fn main() {
}
".to_string());
}

#[test]
fn indent_regression_from_port_to_result_api() {
    let input = "#![feature(macro_rules)]
extern crate syntax;
use foo;
mod rustfmt;
#[cfg(test)]
mod test;
/// The Main Function
pub fn main() {
    foo();
}
";

    assert_eq!(input.to_string(), test_rustfmt(input));
}

#[test]
fn should_preserve_empty_blocks() {
    let input = "match foo {
    _ => {}
}
";

    assert_eq!(input.to_string(), test_rustfmt(input));
}

#[test]
fn full_regression() {
    let input = "#![feature(macro_rules)]
extern crate syntax;
use foo;
mod rustfmt;
#[cfg(test)]
mod test;
/// The Main Function
pub fn main() {
    let source = io::stdin().read_to_end().unwrap();
    let source = str::from_utf8(source.as_slice()).unwrap();
    let session = parse::new_parse_sess();
    let filemap = parse::string_to_filemap(&session, source.to_string(), foo.to_string());
    let lexer = lexer::StringReader::new(&session.span_diagnostic, filemap);
    let mut output = io::MemWriter::new();
    {
        let mut formatter = rustfmt::Formatter::new(lexer, &mut output);
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
    let output = str::from_utf8(output.unwrap().as_slice()).unwrap().to_string();
    print!(bar, output);
}
";

    assert_eq!(input.to_string(), test_rustfmt(input));
}
