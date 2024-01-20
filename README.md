# CMPSC 260 IR Parser (Rewrite it in Rust!)

This is an onofficial parser I created as a student in UC Santa Barbara's CMPSC 260.
This parser was written for the IR language which was used in the Winter 2023 course.

This library may still contain bugs.
Use at your own risk!!

## Installation

Add this crate as a dependency to your Cargo.toml file.

```toml
[dependencies]
# your other dependencies here
ir-parser = { git = "https://github.com/hitomi-nakayama/cmpsc-260-ir-parser.git" }
```

## Useage

This example code will parse the IR file and print the resulting `Program`.
Please replace `[MY PATH TO IR FILE]` with the path to your IR file.

```rs
use std::fs::File;
use std::io::BufReader;

fn main() {
    let ir_path = "[MY PATH TO IR FILE]";

    let ir_file = match File::open(ir_path) {
        Ok(f) => f,
        Err(e) => {
            eprintln!("Error opening file: {}", e);
            return;
        }
    };
    let buf_reader = BufReader::new(ir_file);

    // the TokenReader class turns the characters into tokens
    let mut ir_tokens = ir_parser::create_token_reader(buf_reader);

    // the parse function will parse an entire IR file and return a Program
    let program = match ir_parser::parse(&mut ir_tokens) {
        Ok(p) => p,
        Err(e) => {
            panic!("Error parsing IR: {}", e);
        }
    };

    println!("{:?}", program);
}
```
