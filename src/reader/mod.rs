mod tokenizer;

use std::io;

use value::{Value, Exception, ExceptionKind};
use self::tokenizer::Tokenizer;

pub fn read() -> Result<Option<Value>, Exception> {
    let mut buf = String::new();
    match io::stdin().read_line(&mut buf) {
        Ok(0) => Ok(None),
        Ok(_) => {
            let mut tokenizer = Tokenizer::new(buf);
            let tokens = tokenizer.tokenize()?;
            Ok(Some(tokens))
        }
        Err(error) => {
            Err(Exception::new(ExceptionKind::ReaderIOException(error.to_string()), None))
        }
    }
}
