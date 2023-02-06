use std::collections::HashMap;

use crate::instruction::TypeName;
#[allow(unused_imports)]
use crate::instruction::{BasicBlockName, Instruction, Operation, Relation,
    Value, Variable};

use crate::parse_result::{ParseError, ParseResult};
use crate::program::{BasicBlock, Function, Program};
use crate::text::{SourceReader, TokenCursor};


pub fn parse(source_reader: &mut SourceReader) -> ParseResult<Program> {
    let mut ir = Program {
        functions: HashMap::new()
    };
    while !source_reader.is_empty() {
        let function = parse_function(source_reader)?;
        ir.functions.insert(function.name.clone(), function);
    }
    Ok(ir)
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

fn parse_function_header(source_reader: &mut SourceReader) -> ParseResult<(String, Vec<Variable>, TypeName)> {
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
    let return_type = TypeName::try_from(return_type.as_str())?;

    tokens.expect("{")?;
    if !(tokens.empty()) {
        return Err(ParseError::Unexpected(line_number, tokens.take().unwrap()));
    }

    Ok((function_name, params, return_type))
}

fn parse_basic_blocks(source_reader: &mut SourceReader) -> ParseResult<HashMap<BasicBlockName, BasicBlock>> {
    let mut basic_blocks = HashMap::new();
    let mut current_block: Option<BasicBlock> = None;

    loop {
        let line = source_reader.read_line().ok_or(ParseError::Generic("No line to consume".to_string()))?;
        let is_indented = line.is_indented;
        let line_number = line.line_number;
        let mut tokens = TokenCursor::from(line);

        if is_indented {
            let instruction = parse_instruction(&mut tokens)?;
            if let Some(block) = current_block.as_mut() {
                block.instructions.push(instruction);
            } else {
                return Err(ParseError::Generic("Expected a basic block label.".into()));
            }
        } else {
            if let Some(block) = current_block {
                basic_blocks.insert(block.name.to_owned(), block);
            }
            let label = tokens.take().ok_or(ParseError::Expected(line_number, "Expected a basic block label here.".to_string()))?;
            if label == "}" {

                return Ok(basic_blocks);
            }
            let label = label.strip_suffix(":").ok_or(ParseError::Expected(line_number, "Expected a basic block label here.".to_string()))?;
            current_block = Some(BasicBlock{
                name: label.to_owned(),
                instructions: Vec::new()
            });
        }
    }
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
            Ok(Alloc(lhs))
        },
        "$arith" => {
            let op = tokens.take().ok_or(ParseError::Generic("Expected an arithmetic operation here.".into()))?;
            let op = op.as_str().try_into()?;

            let rhs1 = take_value(tokens)?;
            let rhs2 = take_value(tokens)?;
            Ok(Arith(op, lhs, rhs1, rhs2))
        },
        "$cmp" => {
            let relation = tokens.take().ok_or(ParseError::Generic("Expected a comparison relation here.".into()))?;
            let relation = relation.as_str().try_into()?;

            let rhs1 = take_value(tokens)?;
            let rhs2 = take_value(tokens)?;
            Ok(Cmp(relation, lhs, rhs1, rhs2))
        },
        "$call" => {
            let label = take_label(tokens)?;
            let args = parse_value_list(tokens)?;
            Ok(Call(lhs, label, args))
        },
        "$copy" => {
            let rhs = take_value(tokens)?;
            Ok(Copy(lhs, rhs))
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
            Ok(ICall(lhs, func_ptr, args))
        },
        "$load" => {
            let rhs = take_variable(tokens)?;
            Ok(Load(lhs, rhs))
        },
        "$phi" => {
            let args = parse_value_list(tokens)?;
            Ok(Phi(lhs, args))
        }
        "$select" => {
            let cond = take_variable(tokens)?;
            let true_value = take_value(tokens)?;
            let false_value = take_value(tokens)?;
            Ok(Select(lhs, cond, true_value, false_value))
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
        tokens.take();
        return Ok(params);
    }

    loop {
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

    let token = tokens.take().ok_or(ParseError::Expected(line_number, "Expected a value here.".to_string()))?;

    Value::try_from(token.as_str())
}

fn take_variable(tokens: &mut TokenCursor) -> ParseResult<Variable> {
    let line_number = tokens.line_number();

    let token = tokens.take().ok_or(ParseError::Expected(line_number, "Expected a parameter here.".to_string()))?;
    Variable::try_from(token.as_str())
}

#[cfg(test)]
mod tests {
    use super::*;
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
        let expected_return_type: TypeName = "int".try_into().unwrap();

        let actual = parse_function_header(&mut reader).unwrap();
        let (actual_name, actual_params, actual_return_type) = actual;

        assert_eq!(expected_name, actual_name);
        assert_eq!(expected_params, actual_params);
        assert_eq!(expected_return_type, actual_return_type);
    }

    #[test]
    fn parse_function_header_empty() {
        let source = "function main() -> int {";
        let mut reader = SourceReader::new(source.as_bytes());

        let expected_name = "main".to_owned();
        let expected_params: Vec<Variable> = Vec::new();
        let expected_return_type: TypeName = "int".try_into().unwrap();

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

    #[test]
    fn parse_basic_block_0() {
        let basic_block = "
entry:
    call:int = $call input()
    x:int = $copy call:int
    $ret 0
}";
        let expected = map![
            "entry".to_owned() => BasicBlock{
                name: "entry".to_owned(),
                instructions: vec![
                    Instruction::Call(
                        "call:int".try_into().unwrap(),
                        "input".to_owned(),
                        vec![]
                    ),
                    Instruction::Copy(
                        "x:int".try_into().unwrap(),
                        "call:int".try_into().unwrap()
                    ),
                    Instruction::Ret(
                        Value::Constant(0)
                    )
                ]
            }
        ];

        let mut reader = SourceReader::new(basic_block.as_bytes());
        let actual = parse_basic_blocks(&mut reader).unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn parse_basic_block_1() {
        let basic_blocks = "entry:
    tobool:int = $cmp neq call1:int 0
    $branch tobool:int if.then if.else

if.else:
    call3:int = $call input()
    $jump if.end
}";
        let expected = map![
            "entry".to_owned() => BasicBlock{
                name: "entry".to_owned(),
                instructions: vec![
                    Instruction::Cmp(
                        "neq".try_into().unwrap(),
                        "tobool:int".try_into().unwrap(),
                        "call1:int".try_into().unwrap(),
                        Value::Constant(0)
                    ),
                    Instruction::Branch(
                        "tobool:int".try_into().unwrap(),
                        "if.then".to_owned(),
                        "if.else".to_owned()
                    )
                ]
            },
            "if.else".to_owned() => BasicBlock{
                name: "if.else".to_owned(),
                instructions: vec![
                    Instruction::Call(
                        "call3:int".try_into().unwrap(),
                        "input".to_owned(),
                        vec![]
                    ),
                    Instruction::Jump(
                        "if.end".to_owned()
                    )
                ]
            }
        ];

        let mut reader = SourceReader::new(basic_blocks.as_bytes());
        let actual = parse_basic_blocks(&mut reader).unwrap();
        assert_eq!(expected, actual);
    }


    fn str_to_tokens(input: &str) -> TokenCursor {
        let mut reader = SourceReader::new(input.as_bytes());

        let line = reader.read_line().unwrap();
        let tokens = TokenCursor::from(line);

        return tokens;
    }
}
