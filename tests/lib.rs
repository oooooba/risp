extern crate risp;

use std::env;
use std::fs;
use std::fs::File;
use std::io::Read;
use std::path::PathBuf;

use risp::core::parse_and_eval;
use risp::core::value::Value;
use risp::core::env::Env;
use risp::reader::tokenize;

#[test]
fn test_examples() {
    let expected_value = Value::create_boolean(true);

    let risp_home = env::var("RISP_HOME").unwrap();
    let examples_dir: PathBuf = [&risp_home, "examples"].iter().collect();
    let paths = fs::read_dir(examples_dir).unwrap();
    for path in paths {
        let path = path.unwrap().path();
        println!("[+] testing: {:?}", path);
        let mut file = File::open(path).unwrap();
        let mut content = String::new();
        file.read_to_string(&mut content).unwrap();
        let mut env = Env::create_default();
        let mut tokens = tokenize(content, env.clone()).unwrap();
        loop {
            let (result, rest_tokens) = parse_and_eval(tokens, env.clone());
            match result {
                Ok(val) => assert_eq!(val, expected_value),
                Err(exception) => env = exception.extract_env_from_continuation().unwrap(),
            };
            match rest_tokens {
                None => break,
                Some(rest_tokens) => {
                    tokens = rest_tokens;
                }
            }
        }
    }
}
