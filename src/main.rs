// rustfmt/main.rs
#![crate_id="http://github.com/pcwalton/rustfmt#rustfmt:0.0.1"]
#![desc = "Rust code formatter"]
#![license = "MIT"]

extern crate syntax;

use std::io;
use std::str;
use syntax::parse::lexer;
use syntax::parse;

pub mod rustfmt;

/// The Main Function
#[main]
pub fn main() {
    let source = io::stdin().read_to_end().unwrap();
    let source = str::from_utf8(source.as_slice()).unwrap();

    // nothing special
    let session = parse::new_parse_sess();
    let filemap = parse::string_to_filemap(&session, source.to_string(), "<stdin>".to_string());
    let lexer = lexer::StringReader::new(&session.span_diagnostic, filemap);
    let mut stdout = io::stdio::stdout();
    let mut formatter = rustfmt::Formatter::new(lexer, &mut stdout);

    while formatter.next_token() {
        formatter.parse_production();
    }
}

