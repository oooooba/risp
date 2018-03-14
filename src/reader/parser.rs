use std::collections::LinkedList;

use core::value::{Value, ValuePtr};
use core::exception::{Exception, ExceptionKind};
use reader::{Token, TokenKind};

pub struct Parser {
    tokens: LinkedList<Token>,
}

impl Parser {
    pub fn new(tokens: LinkedList<Token>) -> Parser {
        Parser {
            tokens: tokens,
        }
    }

    pub fn pop_all(&mut self) -> Option<LinkedList<Token>> {
        if self.peek() == None {
            return None;
        }
        let mut rest_tokens = LinkedList::new();
        while let Some(token) = self.pop() {
            rest_tokens.push_back(token);
        }
        Some(rest_tokens)
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.front()
    }

    fn pop(&mut self) -> Option<Token> {
        self.tokens.pop_front()
    }

    fn parse_integer(&mut self) -> Result<ValuePtr, Exception> {
        assert_eq!(self.peek().unwrap().kind, TokenKind::IntegerToken);
        let token = self.pop().unwrap();
        let n = token.lexeme.parse::<isize>().unwrap();
        Ok(Value::create_integer(n))
    }

    fn parse_string(&mut self) -> Result<ValuePtr, Exception> {
        assert_eq!(self.peek().unwrap().kind, TokenKind::StringToken);
        let token = self.pop().unwrap();
        let len = token.lexeme.len();
        assert!(len >= 2);
        assert_eq!(token.lexeme.chars().nth(0), Some('"'));
        assert_eq!(token.lexeme.chars().nth(len - 1), Some('"'));
        let s = token.lexeme.as_str()[1..(len - 1)].to_string();
        Ok(Value::create_string(s))
    }

    fn parse_symbol(&mut self) -> Result<ValuePtr, Exception> {
        assert_eq!(self.peek().unwrap().kind, TokenKind::SymbolToken);
        let token = self.pop().unwrap();
        let len = token.lexeme.len();
        assert!(len >= 1);
        Ok(Value::create_symbol(token.lexeme))
    }

    fn parse_keyword(&mut self) -> Result<ValuePtr, Exception> {
        assert_eq!(self.peek().unwrap().kind, TokenKind::KeywordToken);
        let token = self.pop().unwrap();
        let len = token.lexeme.len();
        assert!(len >= 2);
        assert_eq!(token.lexeme.chars().nth(0), Some(':'));
        let s = token.lexeme.as_str()[1..len].to_string();
        Ok(Value::create_keyword(s))
    }

    fn parse_boolean(&mut self) -> Result<ValuePtr, Exception> {
        assert!(self.peek().unwrap().kind == TokenKind::TrueToken ||
            self.peek().unwrap().kind == TokenKind::FalseToken);
        let token = self.pop().unwrap();
        let b = token.kind == TokenKind::TrueToken;
        Ok(Value::create_boolean(b))
    }

    fn parse_sequence<F>(&mut self, begin_token: TokenKind, end_token: TokenKind, f: &F) -> Result<ValuePtr, Exception>
        where F: Fn(Vec<ValuePtr>) -> Result<ValuePtr, Exception> {
        assert_eq!(self.peek().unwrap().kind, begin_token);
        self.pop();
        let mut parsed = Vec::new();
        loop {
            match self.peek() {
                Some(token) if token.kind == end_token => break,
                Some(_) => (),
                None => return Err(Exception::new(ExceptionKind::ParserUnterminatedTokens2Exception(
                    Token::new("".to_string(), end_token, None)), None)),
            }
            parsed.push(self.parse()?);
        }
        assert_eq!(self.peek().unwrap().kind, end_token);
        self.pop();
        f(parsed)
    }

    fn parse_list(&mut self) -> Result<ValuePtr, Exception> {
        assert_eq!(self.peek().unwrap().kind, TokenKind::LParenToken);
        self.parse_sequence(TokenKind::LParenToken, TokenKind::RParenToken, &|v| {
            Ok(Value::create_list_from_vec(v))
        })
    }

    fn parse_vector(&mut self) -> Result<ValuePtr, Exception> {
        assert_eq!(self.peek().unwrap().kind, TokenKind::LBracketToken);
        self.parse_sequence(TokenKind::LBracketToken, TokenKind::RBracketToken, &|v| {
            Ok(Value::create_vector(v))
        })
    }

    fn parse_amp(&mut self) -> Result<ValuePtr, Exception> {
        assert_eq!(self.peek().unwrap().kind, TokenKind::AmpToken);
        self.pop();
        Ok(Value::create_keyword("&".to_string()))
    }

    fn parse_quote(&mut self) -> Result<ValuePtr, Exception> {
        assert_eq!(self.peek().unwrap().kind, TokenKind::QuoteToken);
        self.pop();
        let val = self.parse()?;
        Ok(Value::create_list_from_vec(vec![
            Value::create_symbol("quote".to_string()),
            val,
        ]))
    }

    pub fn parse(&mut self) -> Result<ValuePtr, Exception> {
        use self::TokenKind::*;
        match self.peek() {
            Some(&Token { kind: IntegerToken, .. }) => self.parse_integer(),
            Some(&Token { kind: StringToken, .. }) => self.parse_string(),
            Some(&Token { kind: SymbolToken, .. }) => self.parse_symbol(),
            Some(&Token { kind: KeywordToken, .. }) => self.parse_keyword(),
            Some(&Token { kind: TrueToken, .. }) => self.parse_boolean(),
            Some(&Token { kind: FalseToken, .. }) => self.parse_boolean(),
            Some(&Token { kind: LParenToken, .. }) => self.parse_list(),
            Some(&Token { kind: LBracketToken, .. }) => self.parse_vector(),
            Some(&Token { kind: AmpToken, .. }) => self.parse_amp(), // ToDo: fix
            Some(&Token { kind: QuoteToken, .. }) => self.parse_quote(),
            None => Err(Exception::new(ExceptionKind::ParserEmptyTokensException, None)),
            _ => unimplemented!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use super::TokenKind::*;
    use std::iter::FromIterator;

    fn s(x: &str) -> String { x.to_string() }

    #[test]
    fn test_acceptance() {
        assert_eq!(Parser::new(LinkedList::from_iter(vec![
            Token::new(s("123"), IntegerToken, None),
        ])).parse(), Ok(
            Value::create_integer(123)
        ));
        assert_eq!(Parser::new(LinkedList::from_iter(vec![
            Token::new(s("("), LParenToken, None),
            Token::new(s(")"), RParenToken, None),
        ])).parse(), Ok(
            Value::create_list_from_vec(vec![])
        ));
        assert_eq!(Parser::new(LinkedList::from_iter(vec![
            Token::new(s("("), LParenToken, None),
            Token::new(s("123"), IntegerToken, None),
            Token::new(s(r#""abc""#), StringToken, None),
            Token::new(s("def"), SymbolToken, None),
            Token::new(s(":XYZ"), KeywordToken, None),
            Token::new(s(")"), RParenToken, None),
        ])).parse(), Ok(
            Value::create_list_from_vec(vec![
                Value::create_integer(123),
                Value::create_string("abc".to_string()),
                Value::create_symbol("def".to_string()),
                Value::create_keyword("XYZ".to_string()),
            ])
        ));
        assert_eq!(Parser::new(LinkedList::from_iter(vec![
            Token::new(s("("), LParenToken, None),
            Token::new(s("("), LParenToken, None),
            Token::new(s("123"), IntegerToken, None),
            Token::new(s(")"), RParenToken, None),
            Token::new(s(r#""abc""#), StringToken, None),
            Token::new(s("("), LParenToken, None),
            Token::new(s("def"), SymbolToken, None),
            Token::new(s(":XYZ"), KeywordToken, None),
            Token::new(s(")"), RParenToken, None),
            Token::new(s(")"), RParenToken, None),
        ])).parse(), Ok(
            Value::create_list_from_vec(vec![
                Value::create_list_from_vec(vec![
                    Value::create_integer(123),
                ]),
                Value::create_string("abc".to_string()),
                Value::create_list_from_vec(vec![
                    Value::create_symbol("def".to_string()),
                    Value::create_keyword("XYZ".to_string()),
                ]),
            ])
        ));
        assert_eq!(Parser::new(LinkedList::from_iter(vec![
            Token::new(s("false"), FalseToken, None),
        ])).parse(), Ok(
            Value::create_boolean(false)
        ));
        assert_eq!(Parser::new(LinkedList::from_iter(vec![
            Token::new(s("["), LBracketToken, None),
            Token::new(s("123"), IntegerToken, None),
            Token::new(s(r#""abc""#), StringToken, None),
            Token::new(s("]"), RBracketToken, None),
        ])).parse(), Ok(
            Value::create_vector(vec![
                Value::create_integer(123),
                Value::create_string("abc".to_string()),
            ])
        ));
    }

    #[test]
    fn test_rejection() {
        use self::ExceptionKind::*;
        assert_eq!(Parser::new(LinkedList::from_iter(vec![])).parse(),
                   Err(Exception::new(ParserEmptyTokensException, None)));
        assert_eq!(Parser::new(LinkedList::from_iter(vec![
            Token::new(s("("), LParenToken, None),
            Token::new(s("("), LParenToken, None),
            Token::new(s("123"), IntegerToken, None),
            Token::new(s(")"), RParenToken, None),
        ])).parse(), Err(Exception::new(ParserUnterminatedTokens2Exception(
            Token::new(s(""), RParenToken, None)), None)));
    }
}
