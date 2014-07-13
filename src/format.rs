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

use std::io::Writer;

use syntax::parse::lexer::{TokenAndSpan, Reader};
use syntax::parse::token::Token;
use syntax::parse::token::keywords;
use syntax::parse::token;

macro_rules! try_io(
    ($e:expr) => (match $e {
        Ok(_) => {},
        Err(err) => return Err(
                format!("Err in Formatter: {}: '{}' details: {}", err.to_string(), err.desc, err.detail))
    })
)

static TAB_WIDTH: i32 = 4;

pub type FormatterResult<T> = Result<T, String>;

enum ProductionToParse {
    MatchProduction,
    UseProduction,
    BracesProduction,
    ParenthesesProduction,
    AttributeProduction
}

struct LineToken {
    token_and_span: TokenAndSpan,
    x_pos: i32,
}

impl LineToken {
    fn new(token_and_span: TokenAndSpan) -> LineToken {
        LineToken {
            token_and_span: token_and_span,
            x_pos: 0,
        }
    }

    fn whitespace_needed_after(&self, next: &LineToken) -> bool {
        match (&self.token_and_span.tok, &next.token_and_span.tok) {
            (&token::IDENT(..), &token::IDENT(..)) => true,
            (&token::IDENT(..), &token::NOT)
                    if !token::is_any_keyword(&self.token_and_span.tok) => {
                // Macros.
                false
            }

            (&token::IDENT(..), _) if
                    token::is_keyword(keywords::If, &self.token_and_span.tok) ||
                    token::is_keyword(keywords::As, &self.token_and_span.tok) ||
                    token::is_keyword(keywords::Match, &self.token_and_span.tok) => {
                true
            }
            (_, &token::IDENT(..))
                    if token::is_keyword(keywords::If, &next.token_and_span.tok) => {
                true
            }

            (&token::COLON, _) => true,
            (&token::COMMA, _) => true,
            (&token::EQ, _) | (_, &token::EQ) => true,
            (&token::LT, _) | (_, &token::LT) => true,
            (&token::LE, _) | (_, &token::LE) => true,
            (&token::EQEQ, _) | (_, &token::EQEQ) => true,
            (&token::NE, _) | (_, &token::NE) => true,
            (&token::GE, _) | (_, &token::GE) => true,
            (&token::GT, _) | (_, &token::GT) => true,
            (&token::ANDAND, _) | (_, &token::ANDAND) => true,
            (&token::OROR, _) | (_, &token::OROR) => true,
            (&token::TILDE, _) | (_, &token::TILDE) => true,

            (&token::LPAREN, _) => false,
            (_, &token::RPAREN) => false,
            (&token::BINOP(token::AND), _) => false,

            (&token::BINOP(_), _) | (_, &token::BINOP(_)) => true,
            (&token::BINOPEQ(_), _) | (_, &token::BINOPEQ(_)) => true,

            (&token::MOD_SEP, _) | (_, &token::MOD_SEP) => false,

            (&token::RARROW, _) | (_, &token::RARROW) => true,
            (&token::FAT_ARROW, _) | (_, &token::FAT_ARROW) => true,
            (&token::LBRACE, _) | (_, &token::LBRACE) => true,
            (&token::RBRACE, _) | (_, &token::RBRACE) => true,
            _ => false,
        }
    }

    fn length(&self) -> i32 {
        token::to_string(&self.token_and_span.tok).len() as i32
    }

    fn preindentation(&self) -> i32 {
        match self.token_and_span.tok {
            token::RBRACE => -TAB_WIDTH,
            _ => 0,
        }
    }
}

struct LogicalLine {
    tokens: Vec<LineToken>,
}

impl LogicalLine {
    fn new() -> LogicalLine {
        LogicalLine {
            tokens: Vec::new(),
        }
    }

    fn layout(&mut self, mut x_pos: i32) {
        if self.tokens.len() == 0 {
            return
        }

        for i in range(0, self.tokens.len()) {
            self.tokens.get_mut(i).x_pos = x_pos;
            x_pos += self.tokens[i].length();

            if i < self.tokens.len() - 1 &&
                    self.tokens[i].whitespace_needed_after(&self.tokens[i + 1]) {
                x_pos += 1;
            }
        }
    }

    fn whitespace_after(&self, index: uint) -> i32 {
        if self.tokens.len() <= 1 || index >= self.tokens.len() - 1 {
            return 0
        }

        self.tokens[index + 1].x_pos - (self.tokens[index].x_pos +
                                            self.tokens[index].length())
    }

    fn postindentation(&self) -> i32 {
        match self.tokens.as_slice().last() {
            None => 0,
            Some(line_token) => {
                match line_token.token_and_span.tok {
                    token::LBRACE => TAB_WIDTH,
                    _ => 0,
                }
            }
        }
    }
}

pub struct Formatter<'a> {
    in_toknspans: Vec<TokenAndSpan>,
    curr_idx: uint,
    indent: i32,
    logical_line: LogicalLine,
    last_token: Token,
    second_previous_token: Token,
    newline_after_comma: bool,
    newline_after_brace: bool,
    in_attribute: bool,
    output: &'a mut Writer
}

impl<'a> Formatter<'a> {
    pub fn new<'a>(in_toknspans: Vec<TokenAndSpan>, output: &'a mut Writer) -> Formatter<'a> {
        Formatter {
            in_toknspans: in_toknspans,
            curr_idx: 0,
            indent: 0,
            logical_line: LogicalLine::new(),
            last_token: token::SEMI,
            second_previous_token: token::SEMI,
            newline_after_comma: false,
            newline_after_brace: true,
            in_attribute: false,
            output: output
        }
    }

    pub fn process(mut self) {
        loop {
            match self.next_token() {
                Ok(true) => {
                    match self.parse_production() {
                        Err(e) => fail!(e),
                        _ => {}
                    }
                },
                Ok(false) => break,
                Err(e) => fail!(e)
            }
        }
    }
    
    fn curr_tok(&'a self) -> &'a TokenAndSpan {
        &self.in_toknspans[self.curr_idx]
    }

    fn is_eof(&'a self) -> bool {
        self.in_toknspans.len() == self.curr_idx
    }

    fn token_ends_logical_line(&self, line_token: &LineToken) -> bool {
        match line_token.token_and_span.tok {
            token::SEMI => true,
            token::RBRACE => {
                match self.curr_tok() {
                    &TokenAndSpan { tok: token::COMMA, sp: _ } => {
                        false
                    }
                    _ => true
                }
            },
            token::COMMA => self.newline_after_comma,
            token::LBRACE => {
                match self.curr_tok() {
                    &TokenAndSpan { tok: token::RBRACE, sp: _ } => {
                        false
                    }
                    _ => self.newline_after_brace
                }
            },
            token::DOC_COMMENT(_) => true,
            token::RBRACKET => self.in_attribute,
            _ => false,
        }
    }

    fn token_starts_logical_line(&self, line_token: &LineToken) -> bool {
        match line_token.token_and_span.tok {
            token::RBRACE => {
                match (&self.second_previous_token, &self.last_token) {
                    // suppress newline separating braces in empty match arms
                    (&token::FAT_ARROW, &token::LBRACE) => false,
                    _ => self.newline_after_brace
                }
            },
            _ => false,
        }
    }

    fn parse_tokens_up_to(&mut self, pred: |&token::Token| -> bool) -> FormatterResult<bool> {
        while try!(self.next_token()) {
            if pred(&self.last_token) {
                return Ok(true);
            }
        }
        return Ok(false);
    }

    fn parse_productions_up_to(&mut self, pred: |&token::Token| -> bool) -> FormatterResult<bool> {
        while try!(self.next_token()) {
            if pred(&self.last_token) {
                return Ok(true);
            }
            try!(self.parse_production());
        }
        return Ok(false);
    }

    fn parse_match(&mut self) -> FormatterResult<bool> {
        // We've already parsed the keyword. Parse until we find a `{`.
        if !try!(self.parse_tokens_up_to(|token| *token == token::LBRACE)) {
            return Ok(false);
        }

        let old_newline_after_comma_setting = self.newline_after_comma;
        self.newline_after_comma = true;

        if !try!(self.parse_productions_up_to(|token| *token == token::RBRACE)) {
            return Ok(false);
        }

        self.newline_after_comma = old_newline_after_comma_setting;
        return Ok(true);
    }

    fn parse_use(&mut self) -> FormatterResult<bool> {
        let old_newline_after_brace_setting = self.newline_after_brace;
        self.newline_after_brace = false;

        // We've already parsed the keyword. Parse until we find a `{`.
        if !try!(self.parse_tokens_up_to(|token| *token == token::LBRACE || *token == token::SEMI)) {
            return Ok(false);
        }

        if self.last_token == token::LBRACE {
            let old_newline_after_comma_setting = self.newline_after_comma;
            self.newline_after_comma = false;

            if !try!(self.parse_productions_up_to(|token| *token == token::RBRACE)) {
                return Ok(false);
            }

            self.newline_after_comma = old_newline_after_comma_setting;
        }

        self.newline_after_brace = old_newline_after_brace_setting;
        return Ok(true);
    }

    fn parse_braces(&mut self) -> FormatterResult<bool> {
        let old_newline_after_comma_setting = self.newline_after_comma;
        self.newline_after_comma = true;
        // We've already parsed the '{'. Parse until we find a '}'.
        let result = try!(self.parse_productions_up_to(|token| *token == token::RBRACE));

        self.newline_after_comma = old_newline_after_comma_setting;
        return Ok(result);
    }

    fn parse_parentheses(&mut self) -> FormatterResult<bool> {
        let old_newline_after_comma_setting = self.newline_after_comma;
        self.newline_after_comma = false;

        // We've already parsed the '('. Parse until we find a ')'.
        let result = try!(self.parse_productions_up_to(|token| *token == token::RPAREN));

        self.newline_after_comma = old_newline_after_comma_setting;
        return Ok(result);
    }

    fn parse_attribute(&mut self) -> FormatterResult<bool> {
        // Parse until we find a ']'.
        self.in_attribute = true;
        let result = try!(self.parse_productions_up_to(|token| *token == token::RBRACKET));
        //try!(self.flush_line());
        return Ok(result);
    }

    pub fn parse_production(&mut self) -> FormatterResult<bool> {
        let production_to_parse;
        // TRANSFORM
        match self.last_token {
            token::IDENT(..) if token::is_keyword(keywords::Match, &self.last_token) => {
                production_to_parse = MatchProduction;
            }
            token::IDENT(..) if token::is_keyword(keywords::Use, &self.last_token) => {
                production_to_parse = UseProduction;
            }
            token::LBRACE => production_to_parse = BracesProduction,
            token::LPAREN => production_to_parse = ParenthesesProduction,
            token::POUND => production_to_parse = AttributeProduction,
            _ => return Ok(true),
        }

        match production_to_parse {
            MatchProduction => return self.parse_match(),
            UseProduction => return self.parse_use(),
            BracesProduction => return self.parse_braces(),
            ParenthesesProduction => return self.parse_parentheses(),
            AttributeProduction => return self.parse_attribute()
        }
    }

    pub fn next_token(&mut self) -> FormatterResult<bool> {
        use syntax::parse::lexer::Reader;
        loop {
            // this first half of the function is actually
            // the "after parse_production()" section

            if self.is_eof() {
                return Ok(false);
            }

            let current_line_token = LineToken::new(self.curr_tok().clone());
            // FORMAT check if the current token starts a logical line
            // (that is, the previous token would be the end..
            if self.token_starts_logical_line(&current_line_token) && self.logical_line.tokens.len() > 0 {
                // if so, we flush the tokens in the current logical
                // line to the output Writer
                try!(self.flush_line());
                continue;
            }

            // if we are starting a new line, then bump out indent
            if self.logical_line.tokens.len() == 0 {
                self.indent += current_line_token.preindentation();
            }

            // this is the actual token advancement, ie the "first-half"
            // with the subsequent call to "parse_production()" sandwiched
            // between

            // TRANSFORM
            let curr_tok_copy = self.curr_tok().clone();
            self.curr_idx += 1;
            let token_ends_logical_line = self.token_ends_logical_line(&current_line_token);
            // TRANSFORM set the previous token buffers
            self.second_previous_token = self.last_token.clone();
            self.last_token = curr_tok_copy.tok;
            self.logical_line.tokens.push(current_line_token);
            if token_ends_logical_line {
                try!(self.flush_line());
            }

            return Ok(true);
        }
    }

    fn flush_line(&mut self) -> FormatterResult<()> {

        self.in_attribute = false;
        self.logical_line.layout(self.indent);

        for _ in range(0, self.indent) {
            try_io!(self.output.write_str(" "));
        }
        for i in range(0, self.logical_line.tokens.len()) {
            let curr_tok = &self.logical_line.tokens[i].token_and_span.tok;
            try_io!(self.output.write_str(format!("{}", token::to_string(curr_tok)).as_slice()));

            // collapse empty blocks in match arms
            if (curr_tok == &token::LBRACE && i != self.logical_line.tokens.len() -1) &&
                &self.logical_line.tokens[i+1].token_and_span.tok == &token::RBRACE {
                continue;
            }
            // no whitespace after right-brackets, before comma in match arm
            if (curr_tok == &token::RBRACE && i != self.logical_line.tokens.len() -1) &&
                &self.logical_line.tokens[i+1].token_and_span.tok == &token::COMMA {
                continue;
            }
            for _ in range(0, self.logical_line.whitespace_after(i)) {
                try_io!(self.output.write_str(" "));
            }
        }
        try_io!(self.output.write_line(""));

        self.indent += self.logical_line.postindentation();
        self.logical_line = LogicalLine::new();
        Ok(())
    }
}
