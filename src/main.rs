extern crate risp;

use std::io;
use std::io::Write;

use risp::reader;

fn main() {
    loop {
        print!("> ");
        let _ = io::stdout().flush();
        match reader::read() {
            Ok(None) => {
                break
            }
            Ok(Some(tokens)) => println!("ok: {:?}", tokens),
            Err(err) => println!("err: {:?}", err),
        }
    }
}

