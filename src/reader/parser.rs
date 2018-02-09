use std::collections::LinkedList;

use core::value;
use core::value::{Value, ValueKind};
use core::exception::{Exception, ExceptionKind};

pub struct Parser {
    tokens: LinkedList<Value>,
}

impl Parser {
    pub fn new(tokens: LinkedList<Value>) -> Parser {
        Parser {
            tokens: tokens,
        }
    }

    fn peek(&self) -> Option<&Value> {
        self.tokens.front()
    }

    fn pop(&mut self) -> Option<Value> {
        self.tokens.pop_front()
    }

    fn parse_grouping(&mut self, opening_lexeme: &str, closing_lexeme: &str) -> Result<Value, Exception> {
        use self::ValueKind::*;
        assert_eq!(*self.peek().unwrap(), value::create_keyword_value(opening_lexeme.to_string()));
        self.pop();
        let mut parsed = Vec::new();
        loop {
            match self.peek() {
                Some(token) => {
                    if **token == KeywordValue(closing_lexeme.to_string()) {
                        break
                    }
                }
                None => return Err(Exception::new(ExceptionKind::ParserUnterminatedTokensException(value::create_keyword_value(closing_lexeme.to_string())), None)),
            }
            parsed.push(self.parse()?)
        }
        assert_eq!(**self.peek().unwrap(), KeywordValue(closing_lexeme.to_string()));
        self.pop();
        Ok(value::create_list_value(parsed))
    }

    pub fn parse(&mut self) -> Result<Value, Exception> {
        use self::ValueKind::*;
        let is_transformable = if let Some(token) = self.peek() {
            match **token {
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
            let (opening_lexeme, closing_lexeme) = match **self.peek().unwrap() {
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
    use core::value::*;

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
            create_integer_value(123),
        ])).parse(), Ok(
            create_integer_value(123)
        ));
        assert_eq!(Parser::new(convert_vec_to_linked_list(vec![
            create_keyword_value("(".to_string()),
            create_keyword_value(")".to_string()),
        ])).parse(), Ok(
            create_list_value(vec![])
        ));
        assert_eq!(Parser::new(convert_vec_to_linked_list(vec![
            create_keyword_value("(".to_string()),
            create_integer_value(123),
            create_string_value("abc".to_string()),
            create_symbol_value("def".to_string()),
            create_keyword_value("XYZ".to_string()),
            create_keyword_value(")".to_string()),
        ])).parse(), Ok(
            create_list_value(vec![
                create_integer_value(123),
                create_string_value("abc".to_string()),
                create_symbol_value("def".to_string()),
                create_keyword_value("XYZ".to_string()),
            ])
        ));
        assert_eq!(Parser::new(convert_vec_to_linked_list(vec![
            create_keyword_value("(".to_string()),
            create_keyword_value("(".to_string()),
            create_integer_value(123),
            create_keyword_value(")".to_string()),
            create_string_value("abc".to_string()),
            create_keyword_value("(".to_string()),
            create_symbol_value("def".to_string()),
            create_keyword_value("XYZ".to_string()),
            create_keyword_value(")".to_string()),
            create_keyword_value(")".to_string()),
        ])).parse(), Ok(
            create_list_value(vec![
                create_list_value(vec![
                    create_integer_value(123),
                ]),
                create_string_value("abc".to_string()),
                create_list_value(vec![
                    create_symbol_value("def".to_string()),
                    create_keyword_value("XYZ".to_string()),
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
            create_list_value(vec![
                create_integer_value(123),
            ]),
        ])).parse(), Err(Exception::new(ParserInvalidStatusException, None)));
        assert_eq!(Parser::new(convert_vec_to_linked_list(vec![
            create_keyword_value("(".to_string()),
            create_keyword_value("(".to_string()),
            create_integer_value(123),
            create_keyword_value(")".to_string()),
        ])).parse(), Err(Exception::new(ParserUnterminatedTokensException(create_keyword_value(")".to_string())), None)));
    }
}
