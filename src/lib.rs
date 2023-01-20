use std::io::BufRead;

pub type ParseResult<T> = Result<T, ParseError>;

#[derive(Clone, Debug)]
pub enum ParseError {
    Expected(usize, String),  // expected but did not find
    ExpectedFound(usize, String, String),  // (Expected, Found)
    VariableParseError(usize, String),
    Unexpected(usize, String)  // found but did not expect
}

pub struct IR {
}

pub struct Function {
    name: String,
    params: Vec<Variable>,
    return_type: String,
    basic_blocks: Vec<BasicBlock>
}

pub struct BasicBlock {
    name: String,
    instructions: Vec<Instruction>
}

pub struct Instruction {}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Variable {
    name: String,
    type_name: String
}

impl TryFrom<&str> for Variable {
    type Error = ();

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        if let Some(split) = value.split_once(":") {
            let (name, type_name) = split;
            Ok(Variable{name: name.to_owned(), type_name: type_name.to_owned()})
        } else {
            Err(())
        }
    }
}

pub fn parse(source_reader: &mut SourceReader) -> ParseResult<IR> {
    // parse_function_header(source_reader);

    Ok(IR{})
}

fn parse_function(source_reader: &mut SourceReader) -> ParseResult<Function> {
    let (function_name, params, return_type) = parse_function_header(source_reader)?;
    let basic_blocks = parse_basic_blocks(source_reader)?;

    Ok(Function{
        name: function_name,
        params,
        return_type,
        basic_blocks
    })
}

fn parse_function_header(source_reader: &mut SourceReader) -> ParseResult<(String, Vec<Variable>, String)> {
    let line = source_reader.read_line();
    if line.is_none() {
        return Err(ParseError::Expected(line.line_number, "function".to_string()));
    }
    let line = line.unwrap();
    let line_number = line.line_number;
    let tokens = line.tokens.iter();

    let mut i = 0;
    parse_expect(line_number, "function", &tokens[i])?;
    i += 1;
    let function_name = tokens[i].clone();
    i += 1;
    parse_expect(line_number, "(", &tokens[i])?;
    i += 1;
    let (params_num_tokens, params) = parse_function_params(line_number, &tokens[3..])?;
    i += params_num_tokens;
    parse_expect(line_number, "->", &tokens[i])?;
    i += 1;
    let return_type = tokens[i].clone();
    i += 1;
    parse_expect(line_number, "{", &tokens[i])?;
    i += 1;

    if tokens.len() > i {
        return Err(ParseError::Unexpected(line_number, tokens[i].clone()));
    }

    Ok((function_name, params, return_type))
}

fn parse_function_params(line_number: usize, tokens: &[String]) -> ParseResult<(usize, Vec<Variable>)> {
    let mut params: Vec<Variable> = Vec::new();

    if tokens.len() == 0 {
        return Err(ParseError::Expected(line_number, ")".to_string()));
    }

    if tokens[0] == ")" {
        return Ok((1, params));
    }

    let mut i = 0;
    while i < tokens.len() {
        let variable = parse_variable(line_number, &tokens[i])?;
        params.push(variable);

        i += 1;

        if i >= tokens.len() {
            return Err(ParseError::Expected(line_number, ")".to_string()));
        }

        if tokens[i] == ")" {
            return Ok((i + 1, params));
        }
        parse_expect(line_number, ",", &tokens[i])?;

        i += 1;
    }
    Err(ParseError::Expected(line_number, ")".to_string()))
}

fn parse_basic_blocks(source_reader: &mut SourceReader) -> ParseResult<Vec<BasicBlock>> {
    let mut basic_blocks: Vec<BasicBlock> = Vec::new();

    loop {
        let line = source_reader.read_line();
        let line_number = line.line_number;
        let tokens = line.tokens;

        if line.is_none() {
            return return Err(ParseError::Expected(line_number, "}".to_string()));
        }
        if tokens[0] == "}" {
            return Ok(basic_blocks);
        }

        let line = line.unwrap();
        let line_number = line.line_number;
        let tokens = line.tokens;


        if tokens[0] == "block" {
            let basic_block = parse_basic_block(source_reader
    Ok(Vec::new())
}

fn parse_variable(line_number: usize, token: &str) -> ParseResult<Variable> {
    match Variable::try_from(token) {
        Ok(variable) => Ok(variable),
        Err(error) => Err(ParseError::VariableParseError(line_number, token.to_string()))
    }
}

fn parse_expect(line_number: usize, expected: &str, actual: &str) -> ParseResult<()> {
    if expected != actual {
        Err(ParseError::ExpectedFound(line_number, expected.to_string(), actual.to_string()))
    } else {
        Ok(())
    }
}

pub struct SourceReader<'a> {
    line_number: usize,
    line_reader: Box<dyn BufRead + 'a>
}

#[derive(Clone, Debug)]
pub struct LineResult {
    is_indented: bool,
    line_number: usize,
    tokens: Vec<String>
}

impl<'a> SourceReader<'a> {
    pub fn new<T: BufRead + 'a>(line_reader: T) -> Self {
        SourceReader {
            line_reader: Box::new(line_reader),
            line_number: 0
        }
    }

    pub fn read_line(&mut self) -> Option<LineResult> {
        loop {
            self.line_number += 1;

            let mut line = String::new();

            let result = self.line_reader.read_line(&mut line).unwrap();
            if result == 0 {
                return None;
            }
            let is_indented = line.starts_with(" ") || line.starts_with("\t");
            let tokens = tokenize_line(&line);
            if tokens.len() > 0 {
                return Some(LineResult{
                    is_indented,
                    line_number: self.line_number,
                    tokens
                });
            }
        }
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
                if !(c.is_whitespace()) {
                    tokens.push(c.to_string());
                }
            }
        } else {
            if word_start.is_none() {
                word_start = Some(i);
            }
        }
    }
    if let Some(start) = word_start {
        tokens.push(input[start..input.len()].to_string());
        word_start = None;
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

    #[test]
    fn source_reader_indentation() {
        let source = "a b c
  d e f
";
        let mut reader = SourceReader::new(source.as_bytes());

        let line_1_indent = reader.read_line().unwrap().is_indented;
        assert!(!(line_1_indent));

        let line_2_indent = reader.read_line().unwrap().is_indented;
        assert!(line_2_indent);
    }

    #[test]
    fn parse_variable_0() {
        let variable = "a:int";
        let expected = Variable{name: "a".to_string(), type_name: "int".to_string()};
        let actual = parse_variable(1, variable).unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn parse_function_params_0() {
        let params = vec!["value:int", ",", "left:TreeNode*", ",", "right:TreeNode*", ")"];
        let params: Vec<String> = params.iter().map(|s| s.to_string()).collect();

        let expected = vec![
            Variable::try_from("value:int").unwrap(),
            Variable::try_from("left:TreeNode*").unwrap(),
            Variable::try_from("right:TreeNode*").unwrap()
        ];
        let expected_tokens_parsed = params.len();

        let (tokens_parsed, actual) = parse_function_params(1, &params[..]).unwrap();
        assert_eq!(expected, actual);
        assert_eq!(expected_tokens_parsed, tokens_parsed);
    }

    #[test]
    fn parse_function_params_empty() {
        let params = vec![")".to_string()];

        let expected: Vec<Variable> = vec![];
        let expected_tokens_parsed = params.len();

        let (tokens_parsed, actual) = parse_function_params(1, &params[..]).unwrap();
        assert_eq!(expected, actual);
        assert_eq!(expected_tokens_parsed, tokens_parsed);
    }

    #[test]
    fn parse_function_header_0() {
        let source = "function foo(p:int) -> int {";
        let mut reader = SourceReader::new(source.as_bytes());

        let expected_name = "foo".to_string();
        let expected_params = vec![Variable::try_from("p:int").unwrap()];
        let expected_return_type = "int".to_string();

        let actual = parse_function_header(&mut reader).unwrap();
        let (actual_name, actual_params, actual_return_type) = actual;

        assert_eq!(expected_name, actual_name);
        assert_eq!(expected_params, actual_params);
        assert_eq!(expected_return_type, actual_return_type);
    }
}
