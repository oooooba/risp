extern crate risp;

use std::io;
use std::io::Write;

use risp::reader;

fn main() {
    loop {
        print!("> ");
        let _ = io::stdout().flush();
        match reader::read() {
            Ok(tokens) => println!("ok: {:?}", tokens),
            Err(err) => {
                println!("err: {:?}", err);
                if err.is_ereader_end_of_input_exception() {
                    break;
                }
            }
        }
    }
}

