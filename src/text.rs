use std::io::BufRead;

pub use tokenizer::TokenReader;

const SPECIAL_TOKENS: &'static [&'static str] = &["->", ",", "(", ")", "{", "}", "=", ":", "*", "[", "]"];

pub fn create_token_reader<'a, T: BufRead + 'a>(reader: T) -> TokenReader<'a> {
    TokenReader::from_buf_read(reader, SPECIAL_TOKENS)
}

pub fn str_to_tokens(s: &str) -> TokenReader {
    let reader = create_token_reader(s.as_bytes());
    reader
}
