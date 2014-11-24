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

use std::io::Writer;

use syntax::parse::lexer::{TokenAndSpan, Reader};
use syntax::parse::token::Token;
use syntax::parse::token::DelimToken;
use syntax::parse::token::keywords;
use syntax::parse::token;

use token::TransformedToken;
use token::TransformedToken::{Comment,LexerVal,BlankLine};

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

pub struct LineToken {
    tok: TransformedToken,
    x_pos: i32,
}

impl LineToken {
    pub fn new(tok: TransformedToken) -> LineToken {
        LineToken {
            tok: tok,
            x_pos: 0,
        }
    }
    
    pub fn is_token(&self, token: &token::Token) -> bool {
        match &self.tok {
            &LexerVal(ref t) => &t.tok == token,
            _ => false
        }
    }
    pub fn is_blank_line(&self) -> bool {
        match &self.tok {
            &BlankLine => true,
            _ => false
        }
    }

    fn whitespace_needed_after(&self, next: &LineToken) -> bool {
        let (curr_tok, _curr_comment_ends_line) = match &self.tok {
            &LexerVal(ref token_and_span) => (token_and_span.tok.clone(), false),
            &Comment(_, _, ends_line) => (token::COMMENT, ends_line),
            _ => (token::WS, false)
        };
        let (next_tok, _next_comment_ends_line) = match &next.tok {
            &LexerVal(ref token_and_span) => (token_and_span.tok.clone(), false),
            &Comment(_, _, ends_line) => (token::COMMENT, ends_line),
            _ => (token::WS, false)
        };
        match (&curr_tok, &next_tok) {
            (&Token::IDENT(..), &Token::IDENT(..)) => true,
            (&Token::IDENT(..), &Token::NOT)
                    if !token::is_any_keyword(&curr_tok) => {
                // Macros.
                false
            }

            (&Token::IDENT(..), _) if
                    token::is_keyword(keywords::If, &curr_tok) ||
                    token::is_keyword(keywords::As, &curr_tok) ||
                    token::is_keyword(keywords::Match, &curr_tok) => {
                true
            }
            (_, &Token::IDENT(..))
                    if token::is_keyword(keywords::If, &next_tok) => {
                true
            }

            (&Token::COLON, _) => true,
            (&Token::COMMA, _) => true,
            (&Token::EQ, _) | (_, &Token::EQ) => true,
            (&Token::LT, _) | (_, &Token::LT) => true,
            (&Token::LE, _) | (_, &Token::LE) => true,
            (&Token::EQEQ, _) | (_, &Token::EQEQ) => true,
            (&Token::NE, _) | (_, &Token::NE) => true,
            (&Token::GE, _) | (_, &Token::GE) => true,
            (&Token::GT, _) | (_, &Token::GT) => true,
            (&Token::ANDAND, _) | (_, &Token::ANDAND) => true,
            (&Token::OROR, _) | (_, &Token::OROR) => true,
            (&Token::TILDE, _) | (_, &Token::TILDE) => true,

            (&Token::LPAREN, _) => false,
            (_, &Token::RPAREN) => false,
            (&Token::BINOP(token::AND), _) => false,

            (&Token::BINOP(_), _) | (_, &Token::BINOP(_)) => true,
            (&Token::BINOPEQ(_), _) | (_, &Token::BINOPEQ(_)) => true,

            (&Token::MOD_SEP, _) | (_, &Token::MOD_SEP) => false,

            (&Token::RARROW, _) | (_, &Token::RARROW) => true,
            (&Token::FAT_ARROW, _) | (_, &Token::FAT_ARROW) => true,
            (&Token::LBRACE, _) | (_, &Token::LBRACE) => true,
            (&Token::RBRACE, _) | (_, &Token::RBRACE) => true,
            (&Token::SEMI, _) | (_, &Token::COMMENT) => true,
            (&Token::COMMENT, _) => true,
            _ => false,
        }
    }

    fn length(&self) -> i32 {
        match &self.tok {
            &LexerVal(ref token_and_span) =>
                token::to_string(&token_and_span.tok).len() as i32,
            _ => 0
        }
    }

    fn preindentation(&self) -> i32 {
        match &self.tok {
            &LexerVal(ref token_and_span) => {
                match &token_and_span.tok {
                    &Token::RBRACE => -TAB_WIDTH,
                    _ => 0,
                }
            },
            _ => 0
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
                match &line_token.tok {
                    &LexerVal(ref token_and_span) => {
                        match token_and_span.tok {
                            token::LBRACE => TAB_WIDTH,
                            _ => 0,
                        }
                    },
                    _ => 0
                }
            }
        }
    }
}

pub struct Formatter<'a> {
    input_tokens: &'a [TransformedToken],
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
    pub fn new<'a>(input_tokens: &'a [TransformedToken], output: &'a mut Writer) -> Formatter<'a> {
        Formatter {
            input_tokens: input_tokens,
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
                        Err(e) => panic!(e),
                        _ => {}
                    }
                },
                Ok(false) => break,
                Err(e) => panic!(e)
            }
        }
    }
    
    fn curr_tok(&'a self) -> &'a TransformedToken {
        &self.input_tokens[self.curr_idx]
    }

    fn is_eof(&'a self) -> bool {
        self.input_tokens.len() == self.curr_idx
    }

    fn token_ends_logical_line(&self, line_token: &LineToken) -> bool {
        match &line_token.tok {
            &LexerVal(ref token_and_span) => {
                match token_and_span.tok {
                    token::SEMI => {
                        match self.curr_tok() {
                            &Comment(_, starts_line, _) => starts_line,
                            _ => true
                        }
                    },
                    token::RBRACE => {
                        match self.curr_tok() {
                            &LexerVal(TokenAndSpan { tok: token::COMMA, sp: _ }) => {
                                false
                            }
                            _ => true
                        }
                    },
                    token::COMMA => self.newline_after_comma,
                    token::LBRACE => {
                        match self.curr_tok() {
                            &LexerVal(ref t) => {
                                if t.tok == token::RBRACE {
                                    false
                                } else {
                                    self.newline_after_brace
                                }
                            }
                            _ => self.newline_after_brace
                        }
                    },
                    token::DOC_COMMENT(_) => true,
                    token::RBRACKET => self.in_attribute,
                    _ => false,
                }
            },
            &BlankLine => true,
            &Comment(_, _, ends_line) => ends_line
        }
    }

    fn token_starts_logical_line(&self, line_token: &LineToken) -> bool {
        match &line_token.tok {
            &LexerVal(ref token_and_span) => {
                match token_and_span.tok {
                    token::RBRACE => {
                        match (&self.second_previous_token, &self.last_token) {
                            (&Token::FAT_ARROW, &Token::LBRACE) => false,
                            _ => self.newline_after_brace
                        }
                    },
                    _ => false
                }
            },
            &BlankLine => true,
            &Comment(_, starts_line, _) => starts_line,
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
            if self.is_eof() {
                return Ok(false);
            }

            let current_line_token = LineToken::new(self.curr_tok().clone());
            
            if self.token_starts_logical_line(&current_line_token) && self.logical_line.tokens.len() > 0 {
                try!(self.flush_line());
                continue;
            }

            if self.logical_line.tokens.len() == 0 {
                self.indent += current_line_token.preindentation();
            }

            let curr_tok_copy = self.curr_tok().clone();
            self.curr_idx += 1;
            let token_ends_logical_line = self.token_ends_logical_line(&current_line_token);
            self.second_previous_token = self.last_token.clone();
            self.last_token = match curr_tok_copy {
                LexerVal(token_and_span) => token_and_span.tok,
                BlankLine => token::WS,
                Comment(_, _, _) => token::COMMENT
            };
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

        if !self.logical_line.tokens[0].is_blank_line() {
            for _ in range(0, self.indent) {
                try_io!(self.output.write_str(" "));
            }
        }
        for i in range(0, self.logical_line.tokens.len()) {
            match &self.logical_line.tokens[i] {
                &LineToken{ tok: LexerVal(ref token_and_span), x_pos: _ } => {
                    let curr_tok = &token_and_span.tok;
                    try_io!(self.output.write_str(format!("{}", token::to_string(curr_tok)).as_slice()));

                    // collapse empty blocks in match arms
                    if (curr_tok == &Token::OpenDelim(DelimToken::Brace) && i != self.logical_line.tokens.len() - 1) &&
                        self.logical_line.tokens[i+1].is_token(&Token::CloseDelim(DelimToken::Brace)) {
                        continue;
                    }
                    // no whitespace after right-brackets, before comma in match arm
                    if (curr_tok == &Token::CloseDelim(DelimToken::Brace) && i != self.logical_line.tokens.len() - 1) &&
                        self.logical_line.tokens[i+1].is_token(&Token::Comma) {
                        continue;
                    }
                    for _ in range(0, self.logical_line.whitespace_after(i)) {
                        try_io!(self.output.write_str(" "));
                    }
                },
                &LineToken{ tok: Comment(ref comment_str, _, _), x_pos: _ } => {
                    try_io!(self.output.write_str(comment_str.as_slice()));
                    for _ in range(0, self.logical_line.whitespace_after(i)) {
                        try_io!(self.output.write_str(" "));
                    }
                }
                _ => {}
            }
        }
        try_io!(self.output.write_line(""));

        self.indent += self.logical_line.postindentation();
        self.logical_line = LogicalLine::new();
        Ok(())
    }
}
