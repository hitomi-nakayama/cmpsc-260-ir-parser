use std::collections::HashMap;

use crate::instruction::TypeName;
#[allow(unused_imports)]
use crate::instruction::{BasicBlockName, Instruction, Operation, Relation,
    Value, Variable};

use crate::parse_result::{ParseError, ParseResult};
use crate::program::{BasicBlock, Function, Program, Struct};
use crate::text::{TokenReader};


pub fn parse(tokens: &mut TokenReader) -> ParseResult<Program> {
    let mut structs = HashMap::new();
    let mut functions = HashMap::new();
    while !(tokens.is_empty()) {
        match tokens.peek().unwrap() {
            "struct" => {
                let struct_ = parse_struct(tokens)?;
                structs.insert(struct_.name.clone(), struct_);
            },
            "function" => {
                let function = parse_function(tokens)?;
                functions.insert(function.name.clone(), function);
            },
            _ => {
                return Err(ParseError::Expected(tokens.line_number(), "Expected a struct or function here.".to_string()))
            }
        }
    }
    Ok(Program{structs, functions})
}

fn parse_struct(tokens: &mut TokenReader) -> ParseResult<Struct> {
    let line_number = tokens.line_number();
    expect(tokens, "struct")?;
    let struct_name = tokens.take().ok_or(ParseError::Expected(line_number, "Expected a struct name here.".to_string()))?;
    expect(tokens, "{")?;

    let mut fields = HashMap::new();
    while !(is_end_of_struct(tokens)?) {
        let field_name = parse_label(tokens)?;
        let field_type = parse_type_name(tokens)?;
        fields.insert(field_name, field_type);
    }
    expect(tokens, "}")?;

    Ok(Struct{
        name: struct_name,
        fields
    })
}

fn parse_function(tokens: &mut TokenReader) -> ParseResult<Function> {
    let (function_name, params, return_type) = parse_function_header(tokens)?;
    let basic_blocks = parse_basic_blocks(tokens)?;

    Ok(Function{
        name: function_name,
        params,
        return_type,
        basic_blocks
    })
}

fn parse_function_header(tokens: &mut TokenReader) -> ParseResult<(String, Vec<Variable>, TypeName)> {
    let line_number = tokens.line_number();

    expect(tokens, "function")?;
    let function_name = tokens.take().ok_or(ParseError::Expected(line_number, "Expected a function name here.".to_string()))?;
    let params = parse_function_params(tokens)?;
    expect(tokens, "->")?;

    let return_type = tokens.take().ok_or(ParseError::Expected(line_number, "Expected a return type here.".to_string()))?;
    let return_type = TypeName::try_from(return_type.as_str())?;

    expect(tokens, "{")?;

    Ok((function_name, params, return_type))
}

fn parse_basic_blocks(tokens: &mut TokenReader) -> ParseResult<HashMap<BasicBlockName, BasicBlock>> {
    let mut basic_blocks = HashMap::new();

    loop {
        let label = parse_label(tokens)?;
        let mut block = BasicBlock {
            name: label,
            instructions: Vec::new()
        };
        while !(is_end_of_basic_block(tokens)?) {
            let instruction = parse_instruction(tokens)?;
            block.instructions.push(instruction);
        }
        basic_blocks.insert(block.name.clone(), block);
        if is_end_of_function(tokens)? {
            expect(tokens, "}")?;
            break;
        }
    }
    Ok(basic_blocks)
}

fn is_end_of_basic_block(tokens: &mut TokenReader) -> ParseResult<bool> {
    Ok(is_end_of_function(tokens)? || is_label(tokens))
}

fn is_end_of_function(tokens: &mut TokenReader) -> ParseResult<bool> {
    is_peek_close_brace(tokens, "Expected an instruction, basic block label, or '}' here.")
}

fn is_end_of_struct(tokens: &mut TokenReader) -> ParseResult<bool> {
    is_peek_close_brace(tokens, "Expected a field declaration, or '}' here.")
}

fn is_peek_close_brace(tokens: &mut TokenReader, error_msg: &str) -> ParseResult<bool> {
    let line_number = tokens.line_number();
    let next_token = tokens.peek()
        .ok_or(ParseError::Expected(line_number, error_msg.to_owned()))?;
    Ok(next_token == "}")
}

fn parse_instruction(tokens: &mut TokenReader) -> ParseResult<Instruction> {
    if is_variable(tokens) {
        parse_assign_instruction(tokens)
    } else {
        parse_non_assign_instruction(tokens)
    }
}

fn parse_assign_instruction(tokens: &mut TokenReader) -> ParseResult<Instruction> {
    use Instruction::*;

    let lhs = parse_variable(tokens)?;
    expect(tokens, "=")?;

    let opcode = tokens.take().ok_or(ParseError::Generic("Expected an instruction here.".into()))?;
    match opcode.as_str() {
        "$addrof" => {
            let rhs = parse_variable(tokens)?;
            Ok(AddrOf(lhs, rhs))
        },
        "$alloc" => {
            Ok(Alloc(lhs))
        },
        "$arith" => {
            let op = tokens.take().ok_or(ParseError::Generic("Expected an arithmetic operation here.".into()))?;
            let op = op.as_str().try_into()?;

            let rhs1 = parse_value(tokens)?;
            let rhs2 = parse_value(tokens)?;
            Ok(Arith(op, lhs, rhs1, rhs2))
        },
        "$cmp" => {
            let relation = tokens.take().ok_or(ParseError::Generic("Expected a comparison relation here.".into()))?;
            let relation = relation.as_str().try_into()?;

            let rhs1 = parse_value(tokens)?;
            let rhs2 = parse_value(tokens)?;
            Ok(Cmp(relation, lhs, rhs1, rhs2))
        },
        "$call" => {
            let label = parse_block_name(tokens)?;
            let args = parse_value_list(tokens)?;
            Ok(Call(lhs, label, args))
        },
        "$copy" => {
            let rhs = parse_value(tokens)?;
            Ok(Copy(lhs, rhs))
        },
        "$gep" => {
            let rhs1 = parse_value(tokens)?;
            let rhs2 = parse_value(tokens)?;
            let rhs3 = if is_end_of_gep(tokens)? {
                None
            } else {
                Some(tokens.take()
                    .ok_or(ParseError::Generic("Expected a field name here.".into()))?)
            };
            Ok(Gep(lhs, rhs1, rhs2, rhs3))
        },
        "$icall" => {
            let func_ptr = parse_variable(tokens)?;
            let args = parse_value_list(tokens)?;
            Ok(ICall(lhs, func_ptr, args))
        },
        "$load" => {
            let rhs = parse_variable(tokens)?;
            Ok(Load(lhs, rhs))
        },
        "$phi" => {
            let args = parse_value_list(tokens)?;
            Ok(Phi(lhs, args))
        }
        "$select" => {
            let cond = parse_value(tokens)?;
            let true_value = parse_value(tokens)?;
            let false_value = parse_value(tokens)?;
            Ok(Select(lhs, cond, true_value, false_value))
        }
        x => Err(ParseError::Generic(format!("Unknown instruction: {x}")))
    }
}

fn is_end_of_gep(tokens: &mut TokenReader) -> ParseResult<bool> {
    Ok(is_variable(tokens) || is_opcode(tokens) || tokens.peek().is_none())
}

fn parse_non_assign_instruction(tokens: &mut TokenReader) -> ParseResult<Instruction> {
    use Instruction::*;

    let first_token = tokens.take()
        .ok_or(ParseError::Expected(tokens.line_number(), "Expected an instruction here.".into()))?;
    match first_token.as_str() {
        "$branch" => {
            let cond = parse_value(tokens)?;
            let true_branch = parse_block_name(tokens)?;
            let false_branch = parse_block_name(tokens)?;
            Ok(Branch(cond, true_branch, false_branch))
        },
        "$jump" => {
            let label = parse_block_name(tokens)?;
            Ok(Jump(label))
        },
        "$ret" => {
            let value = parse_value(tokens)?;
            Ok(Ret(value))
        },
        "$store" => {
            let dest = parse_variable(tokens)?;
            let src = parse_value(tokens)?;
            Ok(Store(dest, src))
        },
        _ => Err(ParseError::ExpectedFound(tokens.line_number(), "an instruction".to_owned(), first_token.to_owned()))
    }
}

fn parse_function_params(tokens: &mut TokenReader) -> ParseResult<Vec<Variable>> {
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

fn parse_value_list(tokens: &mut TokenReader) -> ParseResult<Vec<Value>> {
    let line_number = tokens.line_number();
    let mut params: Vec<Value> = Vec::new();

    expect(tokens, "(")?;

    if tokens.peek().ok_or(ParseError::Expected(line_number, ")".to_string()))? == ")" {
        tokens.take();
        return Ok(params);
    }

    loop {
        let variable = parse_value(tokens)?;
        params.push(variable);

        if tokens.peek().ok_or(ParseError::Expected(line_number, ")".to_string()))? == ")" {
            tokens.take();
            return Ok(params);
        }
        expect(tokens, ",")?;
    }
}

fn is_opcode(tokens: &mut TokenReader) -> bool {
    tokens.peek_n(1).map_or(false, |x| x.starts_with("$"))
}

fn is_variable(tokens: &mut TokenReader) -> bool {
    tokens.peek_n(1).map_or(false, |x| x == ":")
}

fn is_label(tokens: &mut TokenReader) -> bool {
    tokens.peek_n(1).map_or(false, |x| x == ":")
        // a hack to filter out the lhs of instructions
        && tokens.peek_n(3).map_or(false, |x| x != "=")
}

fn parse_label(tokens: &mut TokenReader) -> ParseResult<BasicBlockName> {
    let err = ParseError::Generic("Expected a label here.".to_string());
    let label = tokens.take().ok_or(err.clone())?;
    expect(tokens, ":")?;
    Ok(label)
}

fn parse_value(tokens: &mut TokenReader) -> ParseResult<Value> {
    let line_number = tokens.line_number();

    let first_token = tokens.take().ok_or(ParseError::Expected(line_number, "Expected a value here.".to_string()))?;
    let next_token = tokens.peek();
    if next_token == Some(":") {
        tokens.take();
        let type_name = parse_type_name(tokens)?;
        Ok(Value::Variable(Variable::new(first_token, type_name)))
    } else {
        Ok(Value::try_from(first_token.as_str())?)
    }
}

pub fn parse_variable(tokens: &mut TokenReader) -> ParseResult<Variable> {
    let line_number = tokens.line_number();

    let token = tokens.take().ok_or(ParseError::Expected(line_number, "Expected a variable name here.".to_string()))?;
    expect(tokens, ":")?;
    let type_name = parse_type_name(tokens)?;
    Ok(Variable::new(token, type_name))
}

fn parse_type_name(tokens: &mut TokenReader) -> ParseResult<TypeName> {
    let line_number = tokens.line_number();
    let token = tokens.take().ok_or(ParseError::Expected(line_number, "Expected a type name here.".to_string()))?;
    TypeName::try_from(token.as_str())
}

fn parse_block_name(tokens: &mut TokenReader) -> ParseResult<BasicBlockName> {
    let line_number = tokens.line_number();
    let token = tokens.take().ok_or(ParseError::Expected(line_number, "Expected a block name here.".to_string()))?;
    Ok(token)
}

pub fn expect(tokens: &mut TokenReader, expected: &str) -> ParseResult<()> {
    let line_number = tokens.line_number();
    if let Some(actual) = tokens.take() {
        if expected != actual {
            Err(ParseError::ExpectedFound(line_number, expected.to_string(), actual))
        } else {
            Ok(())
        }
    } else {
        Err(ParseError::Expected(line_number, expected.to_string()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn parse_function_params_0() {
        let params = vec!["(", "value:int", ",", "left:TreeNode*", ",", "right:TreeNode*", ")"];
        let params: Vec<String> = params.iter().map(|s| s.to_string()).collect();
        let mut params = TokenReader::from_tokens(params);

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
        let mut params = TokenReader::from_tokens(params);

        let expected: Vec<Variable> = vec![];

        let actual = parse_function_params(&mut params).unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn parse_function_header_0() {
        let source = "function foo(p:int) -> int {";
        let mut reader = TokenReader::from_buf_read(source.as_bytes());

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
        let mut reader = TokenReader::from_buf_read(source.as_bytes());

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
    fn parse_function_header_no_spaces() {
        let source = "function main(x:int*)->int{";
        let mut reader = TokenReader::from_buf_read(source.as_bytes());

        let expected_name = "main".to_owned();
        let expected_params: Vec<Variable> = vec!["x:int*".try_into().unwrap()];
        let expected_return_type: TypeName = "int".try_into().unwrap();

        let actual = parse_function_header(&mut reader).unwrap();
        let (actual_name, actual_params, actual_return_type) = actual;

        assert_eq!(expected_name, actual_name);
        assert_eq!(expected_params, actual_params);
        assert_eq!(expected_return_type, actual_return_type);
    }

    #[test]
    fn parse_function_line_independent() {
        let source = "function
main()
->
int{
main.entry:
$ret 0 }";

        let mut reader = TokenReader::from_buf_read(source.as_bytes());

        let expected = Function {
            name: "main".to_owned(),
            params: vec![],
            return_type: "int".try_into().unwrap(),
            basic_blocks: map![
                "main.entry".to_owned() => BasicBlock {
                    name: "main.entry".to_owned(),
                    instructions: vec![
                        Instruction::Ret(Value::Constant(0))
                    ]
                }
            ]
        };
        let actual = parse_function(&mut reader).unwrap();
        assert_eq!(expected, actual);
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
    fn parse_instruction_branch_const_cond() {
        let instruction = "$branch 0 end end";
        let expected = Instruction::Branch(
            "0".try_into().unwrap(),
            "end".to_owned(),
            "end".to_owned()
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
            Some("a".to_owned())
        );

        let mut tokens = str_to_tokens(instruction);
        let actual = parse_instruction(&mut tokens).unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn parse_instruction_gep_no_field_name() {
        let instruction = "arrayidx:foo* = $gep call:foo* idxprom:int";
        let expected = Instruction::Gep(
            "arrayidx:foo*".try_into().unwrap(),
            "call:foo*".try_into().unwrap(),
            "idxprom:int".try_into().unwrap(),
            None
        );

        let mut tokens = str_to_tokens(instruction);
        let actual = parse_instruction(&mut tokens).unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn parse_basic_block_0() {
        let basic_block = "entry:
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

        let mut reader = TokenReader::from_buf_read(basic_block.as_bytes());
        let actual = parse_basic_blocks(&mut reader).unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn parse_basic_block_indentation_insensitive() {
        let basic_block = "entry:
$ret 0
}";
        let expected = map![
            "entry".to_owned() => BasicBlock{
                name: "entry".to_owned(),
                instructions: vec![
                    Instruction::Ret(
                        Value::Constant(0)
                    )
                ]
            }
        ];

        let mut reader = TokenReader::from_buf_read(basic_block.as_bytes());
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

        let mut reader = TokenReader::from_buf_read(basic_blocks.as_bytes());
        let actual = parse_basic_blocks(&mut reader).unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn parse_struct_0() {
        let mut tokens = str_to_tokens("struct bar {
            a: int
            b: int
        }");
        let expected = Struct{
            name: "bar".to_owned(),
            fields: map![
                "a".to_owned() => "int".try_into().unwrap(),
                "b".to_owned() => "int".try_into().unwrap()
            ]
        };

        let actual = parse_struct(&mut tokens).unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn parse_struct_no_spaces() {
        let mut tokens = str_to_tokens("struct bar {
            a:int
            b:int
        }");
        let expected = Struct{
            name: "bar".to_owned(),
            fields: map![
                "a".to_owned() => "int".try_into().unwrap(),
                "b".to_owned() => "int".try_into().unwrap()
            ]
        };

        let actual = parse_struct(&mut tokens).unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn parse_struct_and_function() {
        let mut tokens = str_to_tokens("struct bar {
            a: int
        }
        function main() -> int {
        entry:
            $ret 0
        }");
        let expected = Program{
            structs: map![
                "bar".to_owned() => Struct{
                    name: "bar".to_owned(),
                    fields: map![
                        "a".to_owned() => "int".try_into().unwrap()
                    ]
                }
            ],
            functions: map![
                "main".to_owned() => Function {
                    name: "main".to_owned(),
                    return_type: "int".try_into().unwrap(),
                    params: vec![],
                    basic_blocks: map![
                        "entry".to_owned() => BasicBlock{
                            name: "entry".to_owned(),
                            instructions: vec![
                                Instruction::Ret(
                                    Value::Constant(0)
                                )
                            ]
                        }
                    ]
                }
            ]
        };
        let actual = parse(&mut tokens).unwrap();
        assert_eq!(expected, actual);
    }

    fn str_to_tokens(input: &str) -> TokenReader {
        TokenReader::from_buf_read(input.as_bytes())
    }
}
