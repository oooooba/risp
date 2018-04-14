extern crate risp;

use std::io;
use std::io::Write;

use risp::evaluator;
use risp::reader;
use risp::core::env;

fn main() {
    let mut env = env::Env::create_default();
    loop {
        print!("> ");
        let _ = io::stdout().flush();
        match reader::read(env.clone()) {
            Ok(tokens) => {
                println!("read ok: {:?}", tokens);
                match evaluator::eval(tokens, env.clone()) {
                    Ok(v) => {
                        println!("eval ok: {:?}", v);
                        println!("eval ok: {}", v.to_string());
                    }
                    Err(exception) => match exception.extract_env_from_continuation() {
                        Some(new_env) => env = new_env,
                        None => println!("eval err: {:?}", exception),
                    }
                }
            }
            Err(err) => {
                println!("read err: {:?}", err);
                if err.is_reader_end_of_input_exception() {
                    break;
                }
            }
        }
    }
}

