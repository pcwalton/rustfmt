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

