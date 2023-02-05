use std::io::BufRead;

mod instruction;
mod parse_result;

use instruction::{Value, Instruction, Operation, Relation, Variable, TypeName};
use parse_result::{ParseResult, ParseError};

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
        return Err(ParseError::Generic("No line to consume".to_string()));
    }
    let line = line.unwrap();
    let line_number = line.line_number;
    let mut tokens = TokenCursor::from(line);

    tokens.expect("function")?;
    let function_name = tokens.take().ok_or(ParseError::Expected(line_number, "Expected a function name here.".to_string()))?;
    let params = parse_function_params(&mut tokens)?;
    tokens.expect("->")?;
    let return_type = tokens.take().ok_or(ParseError::Expected(line_number, "Expected a return type here.".to_string()))?;
    tokens.expect("{")?;
    if !(tokens.empty()) {
        return Err(ParseError::Unexpected(line_number, tokens.take().unwrap()));
    }

    Ok((function_name, params, return_type))
}

fn parse_basic_blocks(source_reader: &mut SourceReader) -> ParseResult<Vec<BasicBlock>> {
    // let mut basic_blocks: Vec<BasicBlock> = Vec::new();

    // loop {
    //     let line = source_reader.read_line();
    //     let line_number = line.line_number;
    //     let tokens = line.tokens;

    //     if line.is_none() {
    //         return return Err(ParseError::Expected(line_number, "}".to_string()));
    //     }
    //     if tokens[0] == "}" {
    //         return Ok(basic_blocks);
    //     }

    //     let line = line.unwrap();
    //     let line_number = line.line_number;
    //     let tokens = line.tokens;


    //     if tokens[0] == "block" {
    //         let basic_block = parse_basic_block(source_reader);
    //     }
    // }
    Ok(Vec::new())
}

fn parse_instruction(tokens: &mut TokenCursor) -> ParseResult<Instruction> {
    use Instruction::*;

    let first_token = tokens.peek().ok_or(ParseError::Generic("Expected an instruction here.".into()))?.as_str();
    match first_token {
        "$branch" => {
            tokens.take();
            let cond = take_variable(tokens)?;
            let true_branch = take_label(tokens)?;
            let false_branch = take_label(tokens)?;
            Ok(Branch(cond, true_branch, false_branch))
        },
        "$jump" => {
            tokens.take();
            let label = take_label(tokens)?;
            Ok(Jump(label))
        },
        "$ret" => {
            tokens.take();
            let value = take_value(tokens)?;
            Ok(Ret(value))
        },
        "$store" => {
            tokens.take();
            let dest = take_variable(tokens)?;
            let src = take_value(tokens)?;
            Ok(Store(dest, src))
        },
        _ => parse_assign_instruction(tokens)
    }
}

fn parse_assign_instruction(tokens: &mut TokenCursor) -> ParseResult<Instruction> {
    use Instruction::*;

    let lhs = take_variable(tokens)?;
    tokens.expect("=")?;

    let opcode = tokens.take().ok_or(ParseError::Generic("Expected an instruction here.".into()))?;
    match opcode.as_str() {
        "$addrof" => {
            let rhs = take_variable(tokens)?;
            Ok(AddrOf(lhs, rhs))
        },
        "$alloc" => {
            Ok(Instruction::Alloc(lhs))
        },
        "$arith" => {
            let op = tokens.take().ok_or(ParseError::Generic("Expected an arithmetic operation here.".into()))?;
            let op = op.as_str().try_into()?;

            let rhs1 = take_value(tokens)?;
            let rhs2 = take_value(tokens)?;
            Ok(Instruction::Arith(op, lhs, rhs1, rhs2))
        },
        "$cmp" => {
            let relation = tokens.take().ok_or(ParseError::Generic("Expected a comparison relation here.".into()))?;
            let relation = relation.as_str().try_into()?;

            let rhs1 = take_value(tokens)?;
            let rhs2 = take_value(tokens)?;
            Ok(Instruction::Cmp(relation, lhs, rhs1, rhs2))
        },
        "$call" => {
            let label = take_label(tokens)?;
            let args = parse_value_list(tokens)?;
            Ok(Instruction::Call(lhs, label, args))
        },
        "$copy" => {
            let rhs = take_value(tokens)?;
            Ok(Instruction::Copy(lhs, rhs))
        },
        "$gep" => {
            let rhs1 = take_variable(tokens)?;
            let rhs2 = take_value(tokens)?;
            let rhs3 = tokens.take().ok_or(ParseError::Generic("Expected a value here.".into()))?;
            Ok(Gep(lhs, rhs1, rhs2, rhs3))
        },
        "$icall" => {
            let func_ptr = take_variable(tokens)?;
            let args = parse_value_list(tokens)?;
            Ok(Instruction::ICall(lhs, func_ptr, args))
        },
        "$load" => {
            let rhs = take_variable(tokens)?;
            Ok(Instruction::Load(lhs, rhs))
        },
        "$phi" => {
            let args = parse_value_list(tokens)?;
            Ok(Instruction::Phi(lhs, args))
        }
        "$select" => {
            let cond = take_variable(tokens)?;
            let true_value = take_value(tokens)?;
            let false_value = take_value(tokens)?;
            Ok(Instruction::Select(lhs, cond, true_value, false_value))
        }
        x => Err(ParseError::Generic(format!("Unknown instruction: {x}")))
    }
}

fn parse_function_params(tokens: &mut TokenCursor) -> ParseResult<Vec<Variable>> {
    // We don't want to duplicate code, so we'll just filter a list of values
    let values = parse_value_list(tokens)?;
    let mut params: Vec<Variable> = Vec::new();
    for value in values {
        match value {
            Value::Variable(var) => params.push(var),
            _ => return Err(ParseError::Generic("Expected a variable here.".into()))
        }
    }
    Ok(params)
}

fn parse_value_list(tokens: &mut TokenCursor) -> ParseResult<Vec<Value>> {
    let line_number = tokens.line_number();
    let mut params: Vec<Value> = Vec::new();

    tokens.expect("(")?;

    if tokens.peek().ok_or(ParseError::Expected(line_number, ")".to_string()))? == ")" {
        return Ok(params);
    }

    loop {
        println!("Token: {}", tokens.peek().unwrap());
        let variable = take_value(tokens)?;
        params.push(variable);

        if tokens.peek().ok_or(ParseError::Expected(line_number, ")".to_string()))? == ")" {
            tokens.take();
            return Ok(params);
        }
        tokens.expect(",")?;
    }
}

fn take_label(tokens: &mut TokenCursor) -> ParseResult<String> {
    let label = tokens.take().ok_or(ParseError::Generic("Expected a label here.".to_string()))?;
    Ok(label)
}

fn take_value(tokens: &mut TokenCursor) -> ParseResult<Value> {
    let line_number = tokens.line_number();

    let token = tokens.take().ok_or(ParseError::Generic("Expected a value here.".to_string()))?;

    Value::try_from(token.as_str())
}

fn take_variable(tokens: &mut TokenCursor) -> ParseResult<Variable> {
    let line_number = tokens.line_number();

    let token = tokens.take().ok_or(ParseError::Generic("Expected a parameter here.".to_string()))?;
    Variable::try_from(token.as_str())
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

struct TokenCursor {
    line_number_: usize,
    index: usize,
    tokens: Vec<String>
}

impl TokenCursor {
    fn new(line_number: usize, tokens: Vec<String>) -> Self {
        TokenCursor {
            line_number_: line_number,
            index: 0,
            tokens: tokens
        }
    }

    fn empty(&self) -> bool {
        self.index >= self.tokens.len()
    }

    fn peek(&self) -> Option<&String> {
        if !(self.empty()) {
            Some(&self.tokens[self.index])
        } else {
            None
        }
    }

    fn line_number(&self) -> usize {
        self.line_number_
    }

    fn take(&mut self) -> Option<String> {
        if !(self.empty()) {
            let token = self.tokens[self.index].clone();
            self.index += 1;
            Some(token)
        } else {
            None
        }
    }

    fn expect(&mut self, expected: &str) -> ParseResult<()> {
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
    use std::rc::Rc;

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
    fn parse_function_params_0() {
        let params = vec!["(", "value:int", ",", "left:TreeNode*", ",", "right:TreeNode*", ")"];
        let params: Vec<String> = params.iter().map(|s| s.to_string()).collect();
        let mut params = TokenCursor::new(0, params);

        let expected = vec![
            Variable::try_from("value:int").unwrap(),
            Variable::try_from("left:TreeNode*").unwrap(),
            Variable::try_from("right:TreeNode*").unwrap()
        ];

        let actual = parse_function_params(&mut params).unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn parse_function_params_empty() {
        let params = vec!["(".to_owned(), ")".to_owned()];
        let mut params = TokenCursor::new(0, params);

        let expected: Vec<Variable> = vec![];

        let actual = parse_function_params(&mut params).unwrap();
        assert_eq!(expected, actual);
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

    #[test]
    fn parse_instruction_jump() {
        let instruction = "$jump if.end";
        let expected = Instruction::Jump("if.end".into());

        let mut tokens = str_to_tokens(instruction);

        let actual = parse_instruction(&mut tokens).unwrap();

        assert_eq!(expected, actual);
    }

    #[test]
    fn parse_instruction_store() {
        let instruction = "$store x:int* 0";
        let expected = Instruction::Store("x:int*".try_into().unwrap(), Value::Constant(0));

        let mut tokens = str_to_tokens(instruction);

        let actual = parse_instruction(&mut tokens).unwrap();

        assert_eq!(expected, actual);
    }

    #[test]
    fn parse_instruction_ret() {
        let instruction = "$ret i8:int";
        let expected = Instruction::Ret(
            "i8:int".try_into().unwrap(),
        );

        let mut tokens = str_to_tokens(instruction);

        let actual = parse_instruction(&mut tokens).unwrap();

        assert_eq!(expected, actual);
    }



    #[test]
    fn parse_instruction_branch() {
        let instruction = "$branch cmp1:int if.then if.end";
        let expected = Instruction::Branch(
            "cmp1:int".try_into().unwrap(),
            "if.then".to_owned(),
            "if.end".to_owned()
        );

        let mut tokens = str_to_tokens(instruction);

        let actual = parse_instruction(&mut tokens).unwrap();

        assert_eq!(expected, actual);
    }

    #[test]
    fn parse_instruction_arith_add() {
        let instruction = "inc:int = $arith add i6:int 1";
        let expected = Instruction::Arith(
            Operation::Add,
            "inc:int".try_into().unwrap(),
            "i6:int".try_into().unwrap(),
            "1".try_into().unwrap()
        );

        let mut tokens = str_to_tokens(instruction);

        let actual = parse_instruction(&mut tokens).unwrap();

        assert_eq!(expected, actual);
    }

    #[test]
    fn parse_instruction_cmp_lte() {
        let instruction = "cmp:int = $cmp lte i:int 1";
        let expected = Instruction::Cmp(
            Relation::Lte,
            "cmp:int".try_into().unwrap(),
            "i:int".try_into().unwrap(),
            "1".try_into().unwrap()
        );

        let mut tokens = str_to_tokens(instruction);

        let actual = parse_instruction(&mut tokens).unwrap();

        assert_eq!(expected, actual);
    }

    #[test]
    fn parse_instruction_phi() {
        let instruction = "a.0:int = $phi(add:int, sub:int)";
        let expected = Instruction::Phi(
            "a.0:int".try_into().unwrap(),
            vec![
                "add:int".try_into().unwrap(),
                "sub:int".try_into().unwrap()
            ]
        );

        let mut tokens = str_to_tokens(instruction);
        let actual = parse_instruction(&mut tokens).unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn parse_instruction_copy() {
        let instruction = "primality:int = $copy 0";
        let expected = Instruction::Copy(
            "primality:int".try_into().unwrap(),
            "0".try_into().unwrap()
        );

        let mut tokens = str_to_tokens(instruction);
        let actual = parse_instruction(&mut tokens).unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn parse_instruction_load() {
        let instruction = "i:foo* = $load next3:foo**";
        let expected = Instruction::Load(
            "i:foo*".try_into().unwrap(),
            "next3:foo**".try_into().unwrap()
        );

        let mut tokens = str_to_tokens(instruction);
        let actual = parse_instruction(&mut tokens).unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn parse_instruction_select() {
        let instruction = "cond:int[int]* = $select tobool:int @foo:int[int]* @bar:int[int]*";
        let expected = Instruction::Select(
            "cond:int[int]*".try_into().unwrap(),
            "tobool:int".try_into().unwrap(),
            "@foo:int[int]*".try_into().unwrap(),
            "@bar:int[int]*".try_into().unwrap()
        );

        let mut tokens = str_to_tokens(instruction);
        let actual = parse_instruction(&mut tokens).unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn parse_instruction_call() {
        let instruction = "call:TreeNode* = $call construct(1, @nullptr:TreeNode*, @nullptr:TreeNode*)";
        let expected = Instruction::Call(
            "call:TreeNode*".try_into().unwrap(),
            "construct".to_owned(),
            vec![
                Value::Constant(1),
                Value::Variable("@nullptr:TreeNode*".try_into().unwrap()),
                Value::Variable("@nullptr:TreeNode*".try_into().unwrap())
            ]
        );

        let mut tokens = str_to_tokens(instruction);

        let actual = parse_instruction(&mut tokens).unwrap();

        assert_eq!(expected, actual);
    }

    #[test]
    fn parse_instruction_icall() {
        let instruction = "call2:int = $icall i1:int[int]*(call1:int)";
        let expected = Instruction::ICall(
            "call2:int".try_into().unwrap(),
            "i1:int[int]*".try_into().unwrap(),
            vec![Value::Variable("call1:int".try_into().unwrap())]
        );

        let mut tokens = str_to_tokens(instruction);

        let actual = parse_instruction(&mut tokens).unwrap();

        assert_eq!(expected, actual);
    }

    #[test]
    fn parse_instruction_alloc() {
        let instruction = "y:int** = $alloc";
        let expected = Instruction::Alloc(
            "y:int**".try_into().unwrap()
        );

        let mut tokens = str_to_tokens(instruction);
        let actual = parse_instruction(&mut tokens).unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn parse_instruction_addr_of() {
        let instruction = "s.ptr:bar* = $addrof s:bar";
        let expected = Instruction::AddrOf(
            "s.ptr:bar*".try_into().unwrap(),
            "s:bar".try_into().unwrap()
        );

        let mut tokens = str_to_tokens(instruction);
        let actual = parse_instruction(&mut tokens).unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn parse_instruction_gep() {
        let instruction = "a:foo* = $gep s.ptr:bar* 0 a";
        let expected = Instruction::Gep(
            "a:foo*".try_into().unwrap(),
            "s.ptr:bar*".try_into().unwrap(),
            "0".try_into().unwrap(),
            "a".to_owned()
        );

        let mut tokens = str_to_tokens(instruction);
        let actual = parse_instruction(&mut tokens).unwrap();
        assert_eq!(expected, actual);
    }

    fn str_to_tokens(input: &str) -> TokenCursor {
        let mut reader = SourceReader::new(input.as_bytes());

        let line = reader.read_line().unwrap();
        let tokens = TokenCursor::from(line);

        return tokens;
    }
}
