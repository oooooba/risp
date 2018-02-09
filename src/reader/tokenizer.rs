use core::value::{Value, ValuePtr, EnvPtr};
use core::exception::{Exception, ExceptionKind, InfoKind};

const CHAR_L_PAREN: char = '(';
const CHAR_R_PAREN: char = ')';
const CHAR_COMMA: char = ',';
const CHAR_MINUS: char = '-';
const CHAR_D_QUOTE: char = '"';
const CHAR_BACKSLASH: char = '\\';

fn is_delim_char(c: char) -> bool {
    c.is_whitespace() || c == CHAR_COMMA
}

fn is_grouping_char(c: char) -> bool {
    c == CHAR_L_PAREN || c == CHAR_R_PAREN
}

pub struct Tokenizer {
    input: String,
    pos: usize,
    env: EnvPtr,
}

impl Tokenizer {
    pub fn new(input: String, env: EnvPtr) -> Tokenizer {
        Tokenizer {
            input: input,
            pos: 0,
            env: env,
        }
    }

    fn peek(&self, k: usize) -> Option<char> {
        self.input.chars().nth(self.pos + k)
    }

    fn ahead(&mut self, k: usize) {
        self.pos += k
    }

    fn sub(&self, start_pos: usize, end_pos: usize) -> &str {
        &self.input.as_str()[start_pos..end_pos]
    }

    fn tokenize_string(&mut self) -> Result<ValuePtr, ExceptionKind> {
        let pos = self.pos;
        self.ahead(1);
        let mut s = String::new();
        let mut err = None;
        loop {
            match self.peek(0) {
                Some(CHAR_D_QUOTE) => {
                    self.ahead(1);
                    break;
                }
                Some(CHAR_BACKSLASH) => {
                    self.ahead(1);
                    match self.peek(0) {
                        Some(c) if c == CHAR_D_QUOTE || c == CHAR_BACKSLASH => s.push(c),
                        Some('n') => s.push('\n'),
                        Some(c) => {
                            if err == None {
                                err = Some(ExceptionKind::TokenizerInvalidEscapedCharacterException(c, self.pos + 1 - pos))
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
        if let Some(kind) = err {
            Err(kind)
        } else {
            Ok(Value::create_string(s))
        }
    }

    fn tokenize_symbol(&mut self) -> Result<ValuePtr, ExceptionKind> {
        let pos = self.pos;
        self.ahead(1);
        while let Some(c) = self.peek(0) {
            if is_delim_char(c) || is_grouping_char(c) {
                break;
            }
            self.ahead(1);
        }
        Ok(Value::create_symbol(self.sub(pos, self.pos).to_string()))
    }

    fn tokenize_number(&mut self) -> Result<ValuePtr, ExceptionKind> {
        let pos = self.pos;
        self.ahead(1);
        let mut is_valid = true;
        while let Some(c) = self.peek(0) {
            if c.is_numeric() {
                self.ahead(1);
            } else if is_delim_char(c) || is_grouping_char(c) {
                break
            } else {
                is_valid = false;
                self.ahead(1);
            }
        }
        let len = self.pos - pos;
        let s = self.sub(pos, pos + len);
        if is_valid {
            let n = s.parse::<isize>().unwrap();
            Ok(Value::create_integer(n))
        } else {
            Err(ExceptionKind::TokenizerInvalidLexemeException(s.to_string()))
        }
    }

    pub fn tokenize(&mut self) -> Result<ValuePtr, Exception> {
        let mut tokens = Vec::new();
        while let Some(c) = self.peek(0) {
            let pos = self.pos;
            let result = if c.is_numeric() {
                Some(self.tokenize_number())
            } else if c == CHAR_MINUS {
                match self.peek(1) {
                    Some(c) if c.is_numeric() => Some(self.tokenize_number()),
                    _ => Some(self.tokenize_symbol()),
                }
            } else if c == CHAR_D_QUOTE {
                Some(self.tokenize_string())
            } else if is_grouping_char(c) {
                self.ahead(1);
                match c {
                    CHAR_L_PAREN => self.env.lookup(&CHAR_L_PAREN.to_string()).map(|v| Ok(v.clone())),
                    CHAR_R_PAREN => self.env.lookup(&CHAR_R_PAREN.to_string()).map(|v| Ok(v.clone())),
                    _ => unreachable!(),
                }
            } else if is_delim_char(c) {
                self.ahead(1);
                None
            } else {
                Some(self.tokenize_symbol())
            };
            let len = self.pos - pos;
            match result {
                Some(Ok(token)) => tokens.push(token),
                Some(Err(kind)) => {
                    let info = InfoKind::TokenizerInfo(pos, len);
                    return Err(Exception::new(kind, Some(info)));
                }
                None => (),
            }
        }
        Ok(Value::create_list(tokens))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use core::value::Env;

    #[test]
    fn test_acceptance() {
        assert_eq!(Tokenizer::new("123 -456".to_string(),
                                  Env::create_global()).tokenize(),
                   Ok(Value::create_list(vec![
                       Value::create_integer(123),
                       Value::create_integer(-456),
                   ])));
        assert_eq!(Tokenizer::new(r#""abc" "d\ne\\f\"g" + - -- -h"#.to_string(),
                                  Env::create_global()).tokenize(),
                   Ok(Value::create_list(vec![
                       Value::create_string("abc".to_string()),
                       Value::create_string("d\ne\\f\"g".to_string()),
                       Value::create_symbol("+".to_string()),
                       Value::create_symbol("-".to_string()),
                       Value::create_symbol("--".to_string()),
                       Value::create_symbol("-h".to_string()),
                   ])));
    }

    #[test]
    fn test_rejection() {
        use self::ExceptionKind::*;
        assert_eq!(Tokenizer::new("( 1x2 )".to_string(),
                                  Env::create_global()).tokenize(),
                   Err(Exception::new(
                       TokenizerInvalidLexemeException("1x2".to_string()),
                       Some(InfoKind::TokenizerInfo(2, 3)))));
        assert_eq!(Tokenizer::new(r#"x "abc"#.to_string(),
                                  Env::create_global()).tokenize(),
                   Err(Exception::new(
                       TokenizerNonTerminatedStringException,
                       Some(InfoKind::TokenizerInfo(2, 4)))));
        assert_eq!(Tokenizer::new(r#""a\bc""#.to_string(),
                                  Env::create_global()).tokenize(),
                   Err(Exception::new(
                       TokenizerInvalidEscapedCharacterException('b', 4),
                       Some(InfoKind::TokenizerInfo(0, 6)))));
    }
}
