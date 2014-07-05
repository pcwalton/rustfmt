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

        while formatter.next_token() {
            formatter.parse_production();
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
