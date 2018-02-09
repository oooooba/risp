use std::collections::LinkedList;

use core::value::{Value, ValuePtr, ValueKind};
use core::exception::{Exception, ExceptionKind};

pub struct Parser {
    tokens: LinkedList<ValuePtr>,
}

impl Parser {
    pub fn new(tokens: LinkedList<ValuePtr>) -> Parser {
        Parser {
            tokens: tokens,
        }
    }

    fn peek(&self) -> Option<&ValuePtr> {
        self.tokens.front()
    }

    fn pop(&mut self) -> Option<ValuePtr> {
        self.tokens.pop_front()
    }

    fn parse_grouping(&mut self, opening_lexeme: &str, closing_lexeme: &str) -> Result<ValuePtr, Exception> {
        use self::ValueKind::*;
        assert_eq!(*self.peek().unwrap(), Value::create_keyword(opening_lexeme.to_string()));
        self.pop();
        let mut parsed = Vec::new();
        loop {
            match self.peek() {
                Some(token) => {
                    if token.kind == KeywordValue(closing_lexeme.to_string()) {
                        break
                    }
                }
                None => return Err(Exception::new(ExceptionKind::ParserUnterminatedTokensException(Value::create_keyword(closing_lexeme.to_string())), None)),
            }
            parsed.push(self.parse()?)
        }
        assert_eq!(self.peek().unwrap().kind, KeywordValue(closing_lexeme.to_string()));
        self.pop();
        Ok(Value::create_list(parsed))
    }

    pub fn parse(&mut self) -> Result<ValuePtr, Exception> {
        use self::ValueKind::*;
        let is_transformable = if let Some(token) = self.peek() {
            match token.kind {
                IntegerValue(_) => false,
                StringValue(_) => false,
                SymbolValue(_) => false,
                KeywordValue(ref keyword) => {
                    match keyword.as_str() {
                        "(" => true,
                        ")" => return Err(Exception::new(ExceptionKind::ParserUnexpectedKeywordException(token.clone()), None)),
                        _ => false,
                    }
                }
                _ => return Err(Exception::new(ExceptionKind::ParserInvalidStatusException, None)),
            }
        } else {
            return Err(Exception::new(ExceptionKind::ParserInvalidStatusException, None));
        };
        if is_transformable {
            let (opening_lexeme, closing_lexeme) = match self.peek().unwrap().kind {
                KeywordValue(ref lexeme) => {
                    match lexeme.as_str() {
                        "(" => ("(", ")"),
                        _ => unreachable!(),
                    }
                }
                _ => unreachable!(),
            };
            self.parse_grouping(opening_lexeme, closing_lexeme)
        } else {
            Ok(self.pop().unwrap())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn convert_vec_to_linked_list<T>(mut xs: Vec<T>) -> LinkedList<T> {
        let mut list = LinkedList::new();
        while let Some(x) = xs.pop() {
            list.push_front(x);
        }
        list
    }

    #[test]
    fn test_acceptance() {
        assert_eq!(Parser::new(convert_vec_to_linked_list(vec![
            Value::create_integer(123),
        ])).parse(), Ok(
            Value::create_integer(123)
        ));
        assert_eq!(Parser::new(convert_vec_to_linked_list(vec![
            Value::create_keyword("(".to_string()),
            Value::create_keyword(")".to_string()),
        ])).parse(), Ok(
            Value::create_list(vec![])
        ));
        assert_eq!(Parser::new(convert_vec_to_linked_list(vec![
            Value::create_keyword("(".to_string()),
            Value::create_integer(123),
            Value::create_string("abc".to_string()),
            Value::create_symbol("def".to_string()),
            Value::create_keyword("XYZ".to_string()),
            Value::create_keyword(")".to_string()),
        ])).parse(), Ok(
            Value::create_list(vec![
                Value::create_integer(123),
                Value::create_string("abc".to_string()),
                Value::create_symbol("def".to_string()),
                Value::create_keyword("XYZ".to_string()),
            ])
        ));
        assert_eq!(Parser::new(convert_vec_to_linked_list(vec![
            Value::create_keyword("(".to_string()),
            Value::create_keyword("(".to_string()),
            Value::create_integer(123),
            Value::create_keyword(")".to_string()),
            Value::create_string("abc".to_string()),
            Value::create_keyword("(".to_string()),
            Value::create_symbol("def".to_string()),
            Value::create_keyword("XYZ".to_string()),
            Value::create_keyword(")".to_string()),
            Value::create_keyword(")".to_string()),
        ])).parse(), Ok(
            Value::create_list(vec![
                Value::create_list(vec![
                    Value::create_integer(123),
                ]),
                Value::create_string("abc".to_string()),
                Value::create_list(vec![
                    Value::create_symbol("def".to_string()),
                    Value::create_keyword("XYZ".to_string()),
                ]),
            ])
        ));
    }

    #[test]
    fn test_rejection() {
        use core::exception::*;
        use self::ExceptionKind::*;
        assert_eq!(Parser::new(convert_vec_to_linked_list(vec![])).parse(), Err(Exception::new(ParserInvalidStatusException, None)));
        assert_eq!(Parser::new(convert_vec_to_linked_list(vec![
            Value::create_list(vec![
                Value::create_integer(123),
            ]),
        ])).parse(), Err(Exception::new(ParserInvalidStatusException, None)));
        assert_eq!(Parser::new(convert_vec_to_linked_list(vec![
            Value::create_keyword("(".to_string()),
            Value::create_keyword("(".to_string()),
            Value::create_integer(123),
            Value::create_keyword(")".to_string()),
        ])).parse(), Err(Exception::new(ParserUnterminatedTokensException(Value::create_keyword(")".to_string())), None)));
    }
}
