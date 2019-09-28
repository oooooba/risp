use std::collections::LinkedList;

use super::super::core::env::EnvPtr;
use super::super::core::exception::{Exception, ExceptionKind, InfoKind};
use super::super::core::reserved;
use super::super::reader::{Token, TokenKind};

fn is_delim_char(c: char) -> bool {
    c.is_whitespace() || c == reserved::CHAR_COMMA || c == reserved::CHAR_SEMICOLON
}

fn is_digit(c: char) -> bool {
    c.is_numeric()
}

fn is_grouping_char(c: char) -> bool {
    c == reserved::CHAR_L_PAREN
        || c == reserved::CHAR_R_PAREN
        || c == reserved::CHAR_L_BRACKET
        || c == reserved::CHAR_R_BRACKET
        || c == reserved::CHAR_L_CURLY
        || c == reserved::CHAR_R_CURLY
}

pub struct Tokenizer {
    input: String,
    pos: usize,
    _env: EnvPtr,
}

impl Tokenizer {
    pub fn new(input: String, env: EnvPtr) -> Tokenizer {
        Tokenizer {
            input: input,
            pos: 0,
            _env: env,
        }
    }

    fn peek(&self, k: usize) -> Option<char> {
        self.input.chars().nth(self.pos + k)
    }

    fn ahead(&mut self, k: usize) {
        self.pos += k
    }

    fn sub(&self, pos: usize, len: usize) -> &str {
        &self.input.as_str()[pos..(pos + len)]
    }

    fn create_token(&self, kind: TokenKind, pos: usize, len: usize) -> Token {
        let lexeme = self.sub(pos, len).to_string();
        let info = Some(InfoKind::TokenizerInfo(pos, len));
        Token::new(lexeme, kind, info)
    }

    fn create_invalid_lexeme_exception(&self, pos: usize, len: usize) -> Exception {
        let lexeme = self.sub(pos, len).to_string();
        let info = Some(InfoKind::TokenizerInfo(pos, len));
        Exception::new(ExceptionKind::TokenizerInvalidLexemeException(lexeme), info)
    }

    fn tokenize_string(&mut self) -> Result<Token, Exception> {
        assert_eq!(self.peek(0).unwrap(), reserved::CHAR_D_QUOTE);
        let pos = self.pos;
        self.ahead(1);
        let mut s = String::new();
        let mut err = None;
        loop {
            match self.peek(0) {
                Some(reserved::CHAR_D_QUOTE) => {
                    self.ahead(1);
                    break;
                }
                Some(reserved::CHAR_BACKSLASH) => {
                    self.ahead(1);
                    match self.peek(0) {
                        Some(c) if c == reserved::CHAR_D_QUOTE || c == reserved::CHAR_BACKSLASH => {
                            s.push(c)
                        }
                        Some('n') => s.push(reserved::CHAR_NEWLINE),
                        Some(c) => {
                            if err == None {
                                err =
                                    Some(ExceptionKind::TokenizerInvalidEscapedCharacterException(
                                        c,
                                        self.pos + 1 - pos,
                                    ))
                            }
                        }
                        None => {
                            err = Some(ExceptionKind::TokenizerNonTerminatedStringException);
                            break;
                        }
                    }
                }
                Some(c) => s.push(c),
                None => {
                    err = Some(ExceptionKind::TokenizerNonTerminatedStringException);
                    break;
                }
            }
            self.ahead(1);
        }
        let len = self.pos - pos;
        if let Some(kind) = err {
            let info = Some(InfoKind::TokenizerInfo(pos, len));
            Err(Exception::new(kind, info))
        } else {
            Ok(self.create_token(TokenKind::StringToken, pos, len))
        }
    }

    fn tokenize_symbol(&mut self) -> Result<Token, Exception> {
        let pos = self.pos;
        self.ahead(1);
        while let Some(c) = self.peek(0) {
            if is_delim_char(c) || is_grouping_char(c) {
                break;
            }
            self.ahead(1);
        }
        let len = self.pos - pos;
        let kind = match self.sub(pos, len) {
            reserved::STR_TRUE => TokenKind::TrueToken,
            reserved::STR_FALSE => TokenKind::FalseToken,
            reserved::STR_NIL => TokenKind::NilToken,
            _ => TokenKind::SymbolToken,
        };
        Ok(self.create_token(kind, pos, len))
    }

    fn tokenize_keyword(&mut self) -> Result<Token, Exception> {
        assert_eq!(self.peek(0).unwrap(), reserved::CHAR_COLON);
        let pos = self.pos;
        match self.peek(1) {
            Some(c) if is_grouping_char(c) || is_delim_char(c) => {
                return Err(self.create_invalid_lexeme_exception(pos, 2))
            }
            Some(_) => (),
            None => return Err(self.create_invalid_lexeme_exception(pos, 1)),
        }

        self.ahead(1);
        while let Some(c) = self.peek(0) {
            if is_delim_char(c) || is_grouping_char(c) {
                break;
            }
            self.ahead(1);
        }
        let len = self.pos - pos;
        Ok(self.create_token(TokenKind::KeywordToken, pos, len))
    }

    fn tokenize_number(&mut self) -> Result<Token, Exception> {
        assert!(is_digit(self.peek(0).unwrap()) || self.peek(0).unwrap() == reserved::CHAR_MINUS);
        let pos = self.pos;
        self.ahead(1);
        let mut is_valid = true;
        while let Some(c) = self.peek(0) {
            if is_digit(c) {
                self.ahead(1);
            } else if is_delim_char(c) || is_grouping_char(c) {
                break;
            } else {
                is_valid = false;
                self.ahead(1);
            }
        }
        let len = self.pos - pos;
        if is_valid {
            Ok(self.create_token(TokenKind::IntegerToken, pos, len))
        } else {
            Err(self.create_invalid_lexeme_exception(pos, len))
        }
    }

    fn skip_line_comment(&mut self) {
        assert_eq!(self.peek(0).unwrap(), reserved::CHAR_SEMICOLON);
        self.ahead(1);
        while let Some(c) = self.peek(0) {
            self.ahead(1);
            if c == reserved::CHAR_NEWLINE {
                break;
            }
        }
    }

    pub fn tokenize(&mut self) -> Result<LinkedList<Token>, Exception> {
        let mut tokens = LinkedList::new();
        while let Some(c) = self.peek(0) {
            let token = if is_digit(c) {
                Some(self.tokenize_number()?)
            } else if c == reserved::CHAR_MINUS {
                match self.peek(1) {
                    Some(c) if is_digit(c) => Some(self.tokenize_number()?),
                    _ => Some(self.tokenize_symbol()?),
                }
            } else if c == reserved::CHAR_D_QUOTE {
                Some(self.tokenize_string()?)
            } else if is_grouping_char(c) {
                let pos = self.pos;
                self.ahead(1);
                let kind = match c {
                    reserved::CHAR_L_PAREN => TokenKind::LParenToken,
                    reserved::CHAR_R_PAREN => TokenKind::RParenToken,
                    reserved::CHAR_L_BRACKET => TokenKind::LBracketToken,
                    reserved::CHAR_R_BRACKET => TokenKind::RBracketToken,
                    reserved::CHAR_L_CURLY => TokenKind::LCurlyToken,
                    reserved::CHAR_R_CURLY => TokenKind::RCurlyToken,
                    _ => unreachable!(),
                };
                Some(self.create_token(kind, pos, 1))
            } else if c == reserved::CHAR_COLON {
                Some(self.tokenize_keyword()?)
            } else if c == reserved::CHAR_QUOTE {
                let pos = self.pos;
                self.ahead(1);
                Some(self.create_token(TokenKind::QuoteToken, pos, 1))
            } else if c == reserved::CHAR_BACK_QUOTE {
                let pos = self.pos;
                self.ahead(1);
                Some(self.create_token(TokenKind::BackQuoteToken, pos, 1))
            } else if c == reserved::CHAR_TILDE {
                let pos = self.pos;
                let (kind, len) = match self.peek(1) {
                    Some(c) if c == reserved::CHAR_AT => (TokenKind::TildeAtToken, 2),
                    _ => (TokenKind::TildeToken, 1),
                };
                self.ahead(len);
                Some(self.create_token(kind, pos, len))
            } else if c == reserved::CHAR_SEMICOLON {
                self.skip_line_comment();
                None
            } else if c == reserved::CHAR_SHARP {
                let pos = self.pos;
                let kind = match self.peek(1) {
                    Some(c) if c == reserved::CHAR_L_CURLY => TokenKind::SharpLCurlyToken,
                    Some(c) if c == reserved::CHAR_L_PAREN => TokenKind::SharpLParenToken,
                    _ => unreachable!(),
                };
                self.ahead(2);
                Some(self.create_token(kind, pos, 2))
            } else if is_delim_char(c) {
                self.ahead(1);
                None
            } else {
                Some(self.tokenize_symbol()?)
            };
            match token {
                Some(token) => tokens.push_back(token),
                None => (),
            }
        }
        Ok(tokens)
    }
}

#[cfg(test)]
mod tests {
    use super::super::super::core::env::Env;
    use super::*;
    use std::iter::FromIterator;

    fn s(x: &str) -> String {
        x.to_string()
    }

    fn i(p: usize, l: usize) -> Option<InfoKind> {
        Some(InfoKind::TokenizerInfo(p, l))
    }

    #[test]
    fn test_acceptance() {
        assert_eq!(
            Tokenizer::new("123 -456".to_string(), Env::create_default()).tokenize(),
            Ok(LinkedList::from_iter(vec![
                Token::new(s("123"), TokenKind::IntegerToken, i(0, 3)),
                Token::new(s("-456"), TokenKind::IntegerToken, i(4, 4)),
            ]))
        );
        assert_eq!(
            Tokenizer::new(
                r#""abc" "d\ne\\f\"g" + - -- -h"#.to_string(),
                Env::create_default()
            )
            .tokenize(),
            Ok(LinkedList::from_iter(vec![
                Token::new(s(r#""abc""#), TokenKind::StringToken, i(0, 5)),
                Token::new(s(r#""d\ne\\f\"g""#), TokenKind::StringToken, i(6, 12)),
                Token::new(s("+"), TokenKind::SymbolToken, i(19, 1)),
                Token::new(s("-"), TokenKind::SymbolToken, i(21, 1)),
                Token::new(s("--"), TokenKind::SymbolToken, i(23, 2)),
                Token::new(s("-h"), TokenKind::SymbolToken, i(26, 2)),
            ]))
        );
        assert_eq!(
            Tokenizer::new(":a".to_string(), Env::create_default()).tokenize(),
            Ok(LinkedList::from_iter(vec![Token::new(
                s(":a"),
                TokenKind::KeywordToken,
                i(0, 2)
            ),]))
        );
        assert_eq!(
            Tokenizer::new("true".to_string(), Env::create_default()).tokenize(),
            Ok(LinkedList::from_iter(vec![Token::new(
                s("true"),
                TokenKind::TrueToken,
                i(0, 4)
            ),]))
        );
    }

    #[test]
    fn test_rejection() {
        use self::ExceptionKind::*;
        assert_eq!(
            Tokenizer::new("( 1x2 )".to_string(), Env::create_default()).tokenize(),
            Err(Exception::new(
                TokenizerInvalidLexemeException("1x2".to_string()),
                Some(InfoKind::TokenizerInfo(2, 3))
            ))
        );
        assert_eq!(
            Tokenizer::new(r#"x "abc"#.to_string(), Env::create_default()).tokenize(),
            Err(Exception::new(
                TokenizerNonTerminatedStringException,
                Some(InfoKind::TokenizerInfo(2, 4))
            ))
        );
        assert_eq!(
            Tokenizer::new(r#""a\bc""#.to_string(), Env::create_default()).tokenize(),
            Err(Exception::new(
                TokenizerInvalidEscapedCharacterException('b', 4),
                Some(InfoKind::TokenizerInfo(0, 6))
            ))
        );
    }
}
