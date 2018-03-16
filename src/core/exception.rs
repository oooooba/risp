use core::value::ValuePtr;
use core::env::EnvPtr;

use reader::Token;

#[derive(PartialEq, Debug)]
pub enum ExceptionKind {
    TokenizerInvalidLexemeException(String),
    TokenizerInvalidEscapedCharacterException(char, usize),
    TokenizerNonTerminatedStringException,
    ReaderIOException(String),
    ReaderInvalidStatusException,
    ReaderEndOfInputException,
    ParserInvalidStatusException,
    ParserUnexpectedKeywordException(ValuePtr),
    ParserUnterminatedTokensException(ValuePtr),
    ParserUnterminatedTokens2Exception(Token), // (expected)
    ParserEmptyTokensException,
    EvaluatorUndefinedSymbolException(String),
    EvaluatorTypeException(&'static str, &'static str), // (expected, actual)
    EvaluatorArityException(usize, usize), // (expected, actual)
    EvaluatorIllegalFormException(&'static str), // (form)
    EvaluatorIllegalArgumentException(String), // (message)
    Continuation(EnvPtr),
}

#[derive(PartialEq, Debug)]
pub enum InfoKind {
    TokenizerInfo(usize, usize),
}

#[derive(PartialEq, Debug)]
pub struct Exception {
    kind: ExceptionKind,
    info: Option<InfoKind>,
}

impl Exception {
    pub fn new(kind: ExceptionKind, info: Option<InfoKind>) -> Exception {
        Exception {
            kind: kind,
            info: info,
        }
    }

    pub fn is_ereader_end_of_input_exception(&self) -> bool {
        self.kind == ExceptionKind::ReaderEndOfInputException
    }

    pub fn extract_env_from_continuation(&self) -> Option<EnvPtr> {
        match self.kind {
            ExceptionKind::Continuation(ref env) => Some(env.clone()),
            _ => None,
        }
    }
}
