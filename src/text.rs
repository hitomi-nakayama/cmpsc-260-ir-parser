
use std::{io::BufRead, mem};

use crate::parse_result::{ParseError, ParseResult};

pub struct SourceReader<'a> {
    line_number: usize,
    line_reader: Box<dyn BufRead + 'a>,
    next_line: Option<LineResult>
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct LineResult {
    pub line_number: usize,
    pub tokens: Vec<String>
}

impl<'a> SourceReader<'a> {
    pub fn new<T: BufRead + 'a>(line_reader: T) -> Self {
        let mut reader = SourceReader {
            line_reader: Box::new(line_reader),
            line_number: 0,
            next_line: None
        };
        reader.read_next_line();
        reader
    }

    pub fn is_empty(&self) -> bool {
        self.next_line.is_none()
    }

    pub fn read_line(&mut self) -> Option<LineResult> {
        let line = mem::take(&mut self.next_line);
        self.read_next_line();
        return line;
    }

    fn read_next_line(&mut self) {
        loop {
            self.line_number += 1;

            let mut line = String::new();

            let result = self.line_reader.read_line(&mut line).unwrap();
            if result == 0 {
                self.next_line = None;
                return;
            }
            let tokens = tokenize_line(&line);
            if tokens.len() > 0 {
                self.next_line = Some(LineResult{
                    line_number: self.line_number,
                    tokens
                });
                return;
            }
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct TokenCursor {
    line_number_: usize,
    index: usize,
    tokens: Vec<String>
}

impl TokenCursor {
    pub fn new(line_number: usize, tokens: Vec<String>) -> Self {
        TokenCursor {
            line_number_: line_number,
            index: 0,
            tokens: tokens
        }
    }

    pub fn empty(&self) -> bool {
        self.index >= self.tokens.len()
    }

    pub fn peek(&self) -> Option<&String> {
        if !(self.empty()) {
            Some(&self.tokens[self.index])
        } else {
            None
        }
    }

    pub fn line_number(&self) -> usize {
        self.line_number_
    }

    pub fn take(&mut self) -> Option<String> {
        if !(self.empty()) {
            let token = self.tokens[self.index].clone();
            self.index += 1;
            Some(token)
        } else {
            None
        }
    }

    pub fn expect(&mut self, expected: &str) -> ParseResult<()> {
        let line_number = self.line_number();
        if let Some(actual) = self.take() {
            if expected != actual {
                Err(ParseError::ExpectedFound(line_number, expected.to_string(), actual))
            } else {
                Ok(())
            }
        } else {
            Err(ParseError::Expected(line_number, expected.to_string()))
        }
    }
}

impl From<LineResult> for TokenCursor {
    fn from(line: LineResult) -> Self {
        let line_number = line.line_number;
        let tokens = line.tokens;
        TokenCursor::new(line_number, tokens)
    }
}

/** Read the next non-empty line from the reader and tokenize it. */
fn tokenize_line(input: &str) -> Vec<String> {
    let mut tokens: Vec<String> = Vec::new();

    let mut word_start: Option<usize> = None;

    for (i, c) in input.chars().enumerate() {
        if c.is_whitespace() || c == ',' || c == '(' || c == ')' {
            if let Some(start) = word_start {
                tokens.push(input[start..i].to_string());
                word_start = None;
            }
            if !(c.is_whitespace()) {
                tokens.push(c.to_string());
            }
        } else {
            if word_start.is_none() {
                word_start = Some(i);
            }
        }
    }
    if let Some(start) = word_start {
        tokens.push(input[start..input.len()].to_string());
    }

    tokens
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tokenize_line_statement() {
        let tokens = tokenize_line("div:int = $arith div i2:int i3:int");
        assert_eq!(tokens, vec!["div:int", "=", "$arith", "div", "i2:int", "i3:int"]);
    }

    #[test]
    fn tokenize_line_function_call() {
        let tokens = tokenize_line("call3:int = $call input()");
        assert_eq!(tokens, vec!["call3:int", "=", "$call", "input", "(", ")"]);
    }

    #[test]
    fn tokenize_line_function_declaration() {
        let tokens = tokenize_line("function construct(value:int, left:TreeNode*) -> TreeNode* {");
        assert_eq!(tokens, vec!["function", "construct", "(", "value:int", ",", "left:TreeNode*", ")", "->", "TreeNode*", "{"]);
    }

    #[test]
    fn tokenize_line_indented() {
        let tokens = tokenize_line("  a b c");
        assert_eq!(tokens, vec!["a", "b", "c"]);
    }

    #[test]
    fn source_reader_blank_lines() {
        let source = "a b c

d e f
";

        let line_0_expected = vec!["a", "b", "c"];
        let line_1_expected = vec!["d", "e", "f"];

        let mut reader = SourceReader::new(source.as_bytes());

        let line_0_actual = reader.read_line().unwrap().tokens;
        assert_eq!(line_0_expected, line_0_actual);

        let line_1_actual = reader.read_line().unwrap().tokens;
        assert_eq!(line_1_expected, line_1_actual);

        assert!(reader.read_line().is_none());
    }

}
