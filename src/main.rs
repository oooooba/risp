extern crate risp;

use std::io;
use std::io::Write;

use risp::evaluator;
use risp::reader;
use risp::core::value;

fn main() {
    let env = value::Env::create_global();
    loop {
        print!("> ");
        let _ = io::stdout().flush();
        match reader::read(env.clone()) {
            Ok(tokens) => {
                println!("read ok: {:?}", tokens);
                match evaluator::eval(tokens, env.clone()) {
                    Ok(v) => println!("eval ok: {:?}", v),
                    Err(err) => println!("eval err: {:?}", err),
                }
            }
            Err(err) => {
                println!("read err: {:?}", err);
                if err.is_ereader_end_of_input_exception() {
                    break;
                }
            }
        }
    }
}

