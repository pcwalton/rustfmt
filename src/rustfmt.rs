// rustfmt/rustfmt.rs

use std::io::Writer;

use syntax::parse::lexer::{StringReader, TokenAndSpan};
use syntax::parse::token::Token;
use syntax::parse::token::keywords;
use syntax::parse::token;

static TAB_WIDTH: i32 = 4;

enum ProductionToParse {
    MatchProduction,
    UseProduction,
    BracesProduction,
    ParenthesesProduction,
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
        token::to_str(&self.token_and_span.tok).len() as i32
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
            x_pos += self.tokens.get(i).length();

            if i < self.tokens.len() - 1 &&
                    self.tokens.get(i).whitespace_needed_after(self.tokens.get(i + 1)) {
                x_pos += 1;
            }
        }
    }

    fn whitespace_after(&self, index: uint) -> i32 {
        if self.tokens.len() <= 1 || index >= self.tokens.len() - 1 {
            return 0
        }

        self.tokens.get(index + 1).x_pos - (self.tokens.get(index).x_pos +
                                            self.tokens.get(index).length())
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
    lexer: StringReader<'a>,
    indent: i32,
    logical_line: LogicalLine,
    last_token: Token,
    newline_after_comma: bool,
    newline_after_brace: bool,
    output: &'a mut Writer
}

impl<'a> Formatter<'a> {
    pub fn new<'a>(lexer: StringReader<'a>, output: &'a mut Writer) -> Formatter<'a> {
        Formatter {
            lexer: lexer,
            indent: 0,
            logical_line: LogicalLine::new(),
            last_token: token::SEMI,
            newline_after_comma: false,
            newline_after_brace: true,
            output: output
        }
    }

    fn token_ends_logical_line(&self, line_token: &LineToken) -> bool {
        match line_token.token_and_span.tok {
            token::SEMI | token::RBRACE => true,
            token::COMMA => self.newline_after_comma,
            token::LBRACE => self.newline_after_brace,
            _ => false,
        }
    }

    fn token_starts_logical_line(&self, line_token: &LineToken) -> bool {
        match line_token.token_and_span.tok {
            token::RBRACE => self.newline_after_brace,
            _ => false,
        }
    }

    fn parse_tokens_up_to(&mut self, pred: |&token::Token| -> bool) -> bool {
        while self.next_token() {
            if pred(&self.last_token) {
                return true;
            }
        }
        return false;
    }

    fn parse_productions_up_to(&mut self, pred: |&token::Token| -> bool) -> bool {
        while self.next_token() {
            if pred(&self.last_token) {
                return true;
            }
            self.parse_production();
        }
        return false;
    }

    fn parse_match(&mut self) -> bool {
        // We've already parsed the keyword. Parse until we find a `{`.
        if !self.parse_tokens_up_to(|token| *token == token::LBRACE) {
            return false;
        }

        let old_newline_after_comma_setting = self.newline_after_comma;
        self.newline_after_comma = true;

        if !self.parse_productions_up_to(|token| *token == token::RBRACE) {
            return false;
        }

        self.newline_after_comma = old_newline_after_comma_setting;
        return true;
    }

    fn parse_use(&mut self) -> bool {
        let old_newline_after_brace_setting = self.newline_after_brace;
        self.newline_after_brace = false;

        // We've already parsed the keyword. Parse until we find a `{`.
        if !self.parse_tokens_up_to(|token| *token == token::LBRACE || *token == token::SEMI) {
            return false;
        }

        if self.last_token == token::LBRACE {
            let old_newline_after_comma_setting = self.newline_after_comma;
            self.newline_after_comma = false;

            if !self.parse_productions_up_to(|token| *token == token::RBRACE) {
                return false;
            }

            self.newline_after_comma = old_newline_after_comma_setting;
        }

        self.newline_after_brace = old_newline_after_brace_setting;
        return true;
    }

    fn parse_braces(&mut self) -> bool {
        let old_newline_after_comma_setting = self.newline_after_comma;
        self.newline_after_comma = true;

        // We've already parsed the '{'. Parse until we find a '}'.
        let result = self.parse_productions_up_to(|token| *token == token::RBRACE);

        self.newline_after_comma = old_newline_after_comma_setting;
        return result;
    }

    fn parse_parentheses(&mut self) -> bool {
        let old_newline_after_comma_setting = self.newline_after_comma;
        self.newline_after_comma = false;

        // We've already parsed the '('. Parse until we find a ')'.
        let result = self.parse_productions_up_to(|token| *token == token::RPAREN);

        self.newline_after_comma = old_newline_after_comma_setting;
        return result;
    }

    pub fn parse_production(&mut self) -> bool {
        let production_to_parse;
        match self.last_token {
            token::IDENT(..) if token::is_keyword(keywords::Match, &self.last_token) => {
                production_to_parse = MatchProduction;
            }
            token::IDENT(..) if token::is_keyword(keywords::Use, &self.last_token) => {
                production_to_parse = UseProduction;
            }
            token::LBRACE => production_to_parse = BracesProduction,
            token::LPAREN => production_to_parse = ParenthesesProduction,
            _ => return true,
        }

        match production_to_parse {
            MatchProduction => return self.parse_match(),
            UseProduction => return self.parse_use(),
            BracesProduction => return self.parse_braces(),
            ParenthesesProduction => return self.parse_parentheses(),
        }
    }

    pub fn next_token(&mut self) -> bool {
        use syntax::parse::lexer::Reader;

        loop {
            if self.lexer.is_eof() {
                return false;
            }

            let last_token = self.lexer.peek();
            self.last_token = last_token.tok.clone();
            let line_token = LineToken::new(last_token);
            if self.token_starts_logical_line(&line_token) && self.logical_line.tokens.len() > 0 {
                self.flush_line();
                continue;
            }

            if self.logical_line.tokens.len() == 0 {
                self.indent += line_token.preindentation();
            }

            drop(self.lexer.next_token());
            let token_ends_logical_line = self.token_ends_logical_line(&line_token);
            self.logical_line.tokens.push(line_token);
            if token_ends_logical_line {
                self.flush_line();
            }

            return true;
        }
    }

    fn flush_line(&mut self) {
        self.logical_line.layout(self.indent);

        for _ in range(0, self.indent) {
            self.output.write_str(" ");
        }
        for i in range(0, self.logical_line.tokens.len()) {
            self.output.write_str(format!("{}", token::to_str(&self.logical_line.tokens.get(i).token_and_span.tok)).as_slice());
            for _ in range(0, self.logical_line.whitespace_after(i)) {
                self.output.write_str(" ");
            }
        }
        self.output.write_line("");

        self.indent += self.logical_line.postindentation();
        self.logical_line = LogicalLine::new();
    }
}

