use std::collections::HashMap;
use std::sync::Arc;

use tokenizer::{TokenReader};

use crate::instruction::{BasicBlockName, Instruction};
use crate::{s, FunctionName};
use crate::variable::{BaseType, TypeName, Variable, Value};
use crate::parse_result::{ParseError, ParseResult};
use crate::program::{BasicBlock, Function, Program, Struct};



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
        name: Arc::new(struct_name),
        fields
    })
}

pub fn parse_function(tokens: &mut TokenReader) -> ParseResult<Function> {
    let (function_name, params, return_type) = parse_function_header(tokens)?;
    let basic_blocks = parse_basic_blocks(tokens, &function_name)?;

    Ok(Function{
        name: function_name,
        params,
        return_type,
        basic_blocks
    })
}

fn parse_function_header(tokens: &mut TokenReader) -> ParseResult<(FunctionName, Vec<Variable>, TypeName)> {
    let line_number = tokens.line_number();

    expect(tokens, "function")?;
    let function_name = tokens.take().ok_or(ParseError::Expected(line_number, "Expected a function name here.".to_string()))?;
    let params = parse_function_params(tokens)?;
    expect(tokens, "->")?;

    let return_type = parse_type_name(tokens)?;

    expect(tokens, "{")?;

    Ok((Arc::new(function_name), params, return_type))
}

fn parse_basic_blocks(tokens: &mut TokenReader, function_name: &str) -> ParseResult<HashMap<BasicBlockName, BasicBlock>> {
    let mut basic_blocks = HashMap::new();

    loop {
        let label = parse_label(tokens)?;
        let mut block = BasicBlock {
            name: label,
            function: s!(function_name).into(),
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

pub fn parse_instruction(tokens: &mut TokenReader) -> ParseResult<Instruction> {
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
                Some(Arc::new(tokens.take()
                    .ok_or(ParseError::Generic("Expected a field name here.".into()))?))
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
    parse_list(tokens, "(", ")", parse_value)
}

fn is_opcode(tokens: &mut TokenReader) -> bool {
    tokens.peek().map_or(false, |x| x.starts_with('$'))
}

fn is_variable(tokens: &mut TokenReader) -> bool {
    tokens.peek_n(1).map_or(false, |x| x == ":")
}

fn is_label(tokens: &mut TokenReader) -> bool {
    tokens.peek_n(1).map_or(false, |x| x == ":")
        // a hack to filter out variables.
        && tokens.peek_n(3).map_or(false, |x| x != "=" && x != "[" && x != "*")
}

fn parse_label(tokens: &mut TokenReader) -> ParseResult<BasicBlockName> {
    let err = ParseError::Generic("Expected a label here.".to_string());
    let label = tokens.take().ok_or(err)?;
    expect(tokens, ":")?;
    Ok(Arc::new(label))
}

pub fn parse_value(tokens: &mut TokenReader) -> ParseResult<Value> {
    let line_number = tokens.line_number();

    let minus_token = tokens.peek()
        .ok_or(ParseError::Expected(line_number, "Expected a value here.".to_string()))?;
    let is_negative_constant = if minus_token == "-" {
        tokens.take();
        true
    } else {
        false
    };

    let num_token = tokens.peek()
        .ok_or(ParseError::Expected(line_number, "Expected a value here.".to_string()))?;
    if let Ok(constant) = num_token.parse::<i32>() {
        tokens.take();
        let sign = if is_negative_constant { -1 } else { 1 };
        Ok(Value::Constant(sign * constant))
    } else {
        if is_negative_constant {
            return Err(ParseError::Expected(line_number, "Expected a negative constant here, but could not parse numerical part.".to_string()));
        }
        Ok(parse_variable(tokens)?.into())
    }
}

pub fn parse_variable(tokens: &mut TokenReader) -> ParseResult<Variable> {
    let line_number = tokens.line_number();

    let token = tokens.take()
        .ok_or(ParseError::VariableParseError(line_number, "Expected a variable name here.".to_string()))?;

    expect(tokens, ":")
        .map_err(|e| ParseError::VariableParseError(line_number, e.to_string()))?;

    let type_name = parse_type_name(tokens)
        .map_err(|e| ParseError::VariableParseError(line_number, e.to_string()))?;

    Ok(Variable::new(token.into(), type_name))
}

pub fn parse_type_name(tokens: &mut TokenReader) -> Result<TypeName, ParseError> {
    let line_number = tokens.line_number();

    let base_type = tokens.take()
        .ok_or(ParseError::TypeParseError(line_number, "Expected a type name here.".to_string()))?;

    let indirection = take_n_matching(tokens, "*");

    let (base_type, indirection) = if tokens.peek() == Some("[") {
        let args = parse_list(tokens, "[", "]", parse_type_name)
            .map_err(|e| ParseError::TypeParseError(line_number, e.to_string()))?;

        let func_ptr_indirection = take_n_matching(tokens, "*");

        let return_type = Box::new(TypeName{
            indirection_level: indirection as u8,
            base_type: BaseType::VariableType(base_type.into())
        });
        (BaseType::FunctionPointer(return_type, args), func_ptr_indirection)
    } else {
        (BaseType::VariableType(base_type.into()), indirection)
    };

    Ok(TypeName{
        indirection_level: indirection as u8,
        base_type
    })
}


fn take_n_matching(tokens: &mut TokenReader, pattern: &str) -> usize {
    let mut count = 0;
    while tokens.peek() == Some(pattern) {
        tokens.take();
        count += 1;
    }
    count
}


pub fn parse_list<T, F>(tokens: &mut TokenReader, open: &str, close: &str,
                    mut parse_fn: F) -> ParseResult<Vec<T>>
        where F: FnMut(&mut TokenReader) -> ParseResult<T> {

    let line_number = tokens.line_number();
    let mut items: Vec<T> = Vec::new();

    expect(tokens, open)?;

    let next_token = tokens.peek().ok_or(ParseError::ListError(line_number, close.to_owned()))?;
    if next_token == close {
        tokens.take();
        return Ok(items);
    }

    loop {
        let item = parse_fn(tokens)?;
        items.push(item);

        let next_token = tokens.peek().ok_or(ParseError::ListError(line_number, close.to_owned()))?;
        if next_token == close {
            tokens.take();
            return Ok(items);
        }
        expect(tokens, ",")?;
    }
}

fn parse_block_name(tokens: &mut TokenReader) -> ParseResult<BasicBlockName> {
    let line_number = tokens.line_number();
    let token = tokens.take().ok_or(ParseError::Expected(line_number, "Expected a block name here.".to_string()))?;
    Ok(Arc::new(token))
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
    use crate::instruction::{Operation, Relation};

    use crate::s;
    use crate::text::str_to_tokens;

    use super::*;

    #[test]
    fn parse_function_addrof() {
        let source = "function main() -> int {
            entry:
              x:int* = $addrof y:int
              $ret 0
            }";

        let mut tokens = str_to_tokens(source);

        let expected = Function {
            name: s!("main").into(),
            return_type: "int".try_into().unwrap(),
            params: vec![],
            basic_blocks: map![
                s!("entry").into() => BasicBlock {
                    function: s!("main").into(),
                    name: s!("entry").into(),
                    instructions: vec![
                        Instruction::AddrOf(
                            "x:int*".try_into().unwrap(),
                            "y:int".try_into().unwrap()
                        ),
                        Instruction::Ret (
                            0.into(),
                        )
                    ],
                }
            ],
        };

        let actual = parse_function(&mut tokens).unwrap();

        assert_eq!(expected, actual);
    }

    #[test]
    fn parse_function_params_0() {
        let params = vec!["(", "value", ":", "int", ",", "left", ":", "TreeNode", "*", ",", "right", ":", "TreeNode", "*", ")"];
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
        let mut reader = str_to_tokens(source);

        let expected_name = Arc::new(s!("foo"));
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
        let mut reader = str_to_tokens(source);

        let expected_name = Arc::new(s!("main"));
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
        let mut reader = str_to_tokens(source);

        let expected_name = Arc::new(s!("main"));
        let expected_params: Vec<Variable> = vec!["x:int*".try_into().unwrap()];
        let expected_return_type: TypeName = "int".try_into().unwrap();

        let actual = parse_function_header(&mut reader).unwrap();
        let (actual_name, actual_params, actual_return_type) = actual;

        assert_eq!(expected_name, actual_name);
        assert_eq!(expected_params, actual_params);
        assert_eq!(expected_return_type, actual_return_type);
    }

    #[test]
    fn parse_function_header_ptr_return() {
        let source = "function main() -> int* {";
        let mut reader = str_to_tokens(source);

        let expected_name = Arc::new(s!("main"));
        let expected_params: Vec<Variable> = vec![];
        let expected_return_type: TypeName = "int*".try_into().unwrap();

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

        let mut reader = str_to_tokens(source);

        let expected = Function {
            name: s!("main").into(),
            params: vec![],
            return_type: "int".try_into().unwrap(),
            basic_blocks: map![
                s!("main.entry").into() => BasicBlock {
                    name: s!("main.entry").into(),
                    function: s!("main").into(),
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
        let expected = Instruction::Jump(s!("if.end").into());

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
            s!("if.then").into(),
            s!("if.end").into()
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
            s!("end").into(),
            s!("end").into()
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
    fn parse_instruction_copy_function_ptr() {
        let instruction = "y:int[ int, int* ]* = $copy x:int[ int, int* ]*";
        let expected = Instruction::Copy(
            "y:int[int,int*]*".try_into().unwrap(),
            "x:int[int,int*]*".try_into().unwrap()
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
    fn parse_instruction_select_with_spaces() {
        let instruction = "cond : int [ int, int * ] * = $select tobool : int @foo : int [ int , int * ] * @bar : int [ int , int * ] *";
        let expected = Instruction::Select(
            "cond:int[int,int*]*".try_into().unwrap(),
            "tobool:int".try_into().unwrap(),
            "@foo:int[int,int*]*".try_into().unwrap(),
            "@bar:int[int,int*]*".try_into().unwrap()
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
            s!("construct").into(),
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
            Some(s!("a").into())
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
            s!("entry").into() => BasicBlock{
                function: s!("main").into(),
                name: s!("entry").into(),
                instructions: vec![
                    Instruction::Call(
                        "call:int".try_into().unwrap(),
                        s!("input").into(),
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

        let mut reader = str_to_tokens(basic_block);
        let actual = parse_basic_blocks(&mut reader, "main").unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn parse_basic_block_indentation_insensitive() {
        let basic_block = "entry:
$ret 0
}";
        let expected = map![
            s!("entry").into() => BasicBlock{
                name: s!("entry").into(),
                function: s!("main").into(),
                instructions: vec![
                    Instruction::Ret(
                        Value::Constant(0)
                    )
                ]
            }
        ];

        let mut reader = str_to_tokens(basic_block);
        let actual = parse_basic_blocks(&mut reader, "main").unwrap();
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
            s!("entry").into() => BasicBlock{
                name: s!("entry").into(),
                function: s!("main").into(),
                instructions: vec![
                    Instruction::Cmp(
                        "neq".try_into().unwrap(),
                        "tobool:int".try_into().unwrap(),
                        "call1:int".try_into().unwrap(),
                        Value::Constant(0)
                    ),
                    Instruction::Branch(
                        "tobool:int".try_into().unwrap(),
                        s!("if.then").into(),
                        s!("if.else").into()
                    )
                ]
            },
            s!("if.else").into() => BasicBlock{
                name: s!("if.else").into(),
                function: s!("main").into(),
                instructions: vec![
                    Instruction::Call(
                        "call3:int".try_into().unwrap(),
                        s!("input").into(),
                        vec![]
                    ),
                    Instruction::Jump(
                        s!("if.end").into()
                    )
                ]
            }
        ];

        let mut reader = str_to_tokens(basic_blocks);
        let actual = parse_basic_blocks(&mut reader, "main").unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn parse_struct_0() {
        let mut tokens = str_to_tokens("struct bar {
            a: int
            b: int
        }");
        let expected = Struct{
            name: s!("bar").into(),
            fields: map![
                s!("a").into() => "int".try_into().unwrap(),
                s!("b").into() => "int".try_into().unwrap()
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
            name: s!("bar").into(),
            fields: map![
                s!("a").into() => "int".try_into().unwrap(),
                s!("b").into() => "int".try_into().unwrap()
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
                s!("bar").into() => Struct{
                    name: s!("bar").into(),
                    fields: map![
                        s!("a").into() => "int".try_into().unwrap()
                    ]
                }
            ],
            functions: map![
                s!("main").into() => Function {
                    name: s!("main").into(),
                    return_type: "int".try_into().unwrap(),
                    params: vec![],
                    basic_blocks: map![
                        s!("entry").into() => BasicBlock{
                            name: s!("entry").into(),
                            function: s!("main").into(),
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

    #[test]
    fn test_is_opcode() {
        let mut tokens = str_to_tokens("$ret");
        let actual = is_opcode(&mut tokens);
        assert!(actual);
    }
}
