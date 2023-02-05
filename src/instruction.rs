pub type FunctionName = String;
pub type BasicBlockName = String;

use crate::parse_result::{ParseResult, ParseError};

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Instruction {
    Arith(Operation, Variable, Value, Value),
    Cmp(Relation, Variable, Value, Value),
    Phi(Variable, Vec<Value>),
    Copy(Variable, Value),
    Load(Variable, Variable),
    Store(Variable, Value),
    Select(Variable, Variable, Value, Value),
    Call(Variable, FunctionName, Vec<Value>),
    ICall(Variable, Variable, Vec<Value>),
    Ret(Value),
    Alloc(Variable),
    AddrOf(Variable, Variable),
    Gep(Variable, Variable, Value, VariableName),
    Jump(BasicBlockName),
    Branch(Variable, BasicBlockName, BasicBlockName),
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Operation {
    Add
}

impl TryFrom<&str> for Operation {
    type Error = ParseError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        use Operation::*;
        match value {
            "add" => Ok(Add),
            _ => Err(ParseError::Generic(format!("Unknown arithmetic operation: {value}")))
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Relation {
    Lte,  // less than or equal
    Neq
}

impl TryFrom<&str> for Relation {
    type Error = ParseError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        use Relation::*;
        match value {
            "lte" => Ok(Lte),
            "neq" => Ok(Neq),
            _ => Err(ParseError::Generic(format!("Invalid relation {}", value)))
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Value {
    Constant(i32),
    Variable(Variable),
}

impl Value {
    pub fn is_variable(&self) -> bool {
        match self {
            Value::Variable(_) => true,
            _ => false
        }
    }

    pub fn is_constant(&self) -> bool {
        match self {
            Value::Constant(_) => true,
            _ => false
        }
    }
}

impl TryFrom<&str> for Value {
    type Error = ParseError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        if let Ok(constant) = value.parse() {
            Ok(Value::Constant(constant))
        } else {
            Ok(Value::Variable(value.try_into()?))
        }
    }
}

type VariableName = String;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Variable {
    name: VariableName,
    type_name: TypeName
}

impl TryFrom<&str> for Variable {
    type Error = ParseError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        if let Some((name, type_name)) = value.split_once(":") {
            let type_name = type_name.try_into()?;
            Ok(Variable{name: name.to_owned(), type_name: type_name})
        } else {
            Err(ParseError::Generic(format!("Invalid variable name {}", value)))
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct TypeName {
    indirection_level: u8,  // a function pointer has indirection level 1
    base_type: BaseType
}

impl TypeName {
    fn parse_type(value: &str) -> Result<TypeName, ParseError> {
        let indirection = value.chars().rev().take_while(|c| *c == '*').count();
        let base_type = &value[..value.len() - indirection];

        let base_type = if let Some((return_type, rest)) = base_type.split_once("[") {
            println!("return_type: {}", return_type);
            let return_type = Box::new(return_type.try_into()?);
            println!("rest: {}", rest);
            let args = Self::get_func_arg_types(rest)?;
            BaseType::FunctionPointer(return_type, args)
        } else { // basic type
            if base_type.find(&['*', '[', ']']).is_some() {
                return Err(ParseError::Generic(format!("Invalid type name {}", base_type)));
            }
            BaseType::VariableType(base_type.to_owned())
        };

        Ok(TypeName{indirection_level: indirection as u8, base_type})
    }

    fn get_func_arg_types(s: &str) -> ParseResult<Vec<TypeName>> {
        let mut types = Vec::new();
        let mut bracket_level = 0;
        let mut word_start = 0;
        for (i, c) in s.chars().enumerate() {
            if (c == ',' || c == ']') && bracket_level == 0 {
                let type_name = &s[word_start..i];
                println!("type_name: {}", type_name);
                let type_name = type_name.try_into()?;
                types.push(type_name);
                word_start = i + 1;
            }
            if c == '[' {
                bracket_level += 1;
            }
            if c == ']' {
                if bracket_level == 0 {
                    return Ok(types)
                }
                bracket_level -= 1;
            }
        }
        Err(ParseError::Generic(format!("Invalid function pointer arguments [{}", s)))
    }
}

impl TryFrom<&str> for TypeName {
    type Error = ParseError;
    fn try_from(value: &str) -> Result<Self, Self::Error> {
        Self::parse_type(value)
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum BaseType {
    VariableType(String),
    FunctionPointer(Box<TypeName>, Vec<TypeName>)
}

#[cfg(test)]
mod tests{
    use super::*;

    #[test]
    fn test_variable_0() {
        let var = Variable::try_from("x:i32").unwrap();
        assert_eq!(var.name, "x");

        let expected_type = "i32".try_into().unwrap();
        assert_eq!(var.type_name, expected_type);
    }

    #[test]
    fn test_variable_1() {
        let var = Variable::try_from("my_var:i32*").unwrap();
        assert_eq!(var.name, "my_var");

        let expected_type = "i32*".try_into().unwrap();
        assert_eq!(var.type_name, expected_type);
    }

    #[test]
    fn test_type_name_0() {
        let type_name: TypeName = "i32".try_into().unwrap();
        assert_eq!(type_name.indirection_level, 0);
        assert_eq!(type_name.base_type, BaseType::VariableType("i32".into()));
    }

    #[test]
    fn test_type_name_1() {
        let type_name: TypeName = "i32*".try_into().unwrap();
        assert_eq!(type_name.indirection_level, 1);
        assert_eq!(type_name.base_type, BaseType::VariableType("i32".into()));
    }

    #[test]
    fn test_type_name_2() {
        let type_name: TypeName = "i32**".try_into().unwrap();
        assert_eq!(type_name.indirection_level, 2);
        assert_eq!(type_name.base_type, BaseType::VariableType("i32".into()));
    }

    #[test]
    fn test_type_name_star_inside() {
        let type_name = TypeName::try_from("i32**a");
        assert!(type_name.is_err());
    }

    #[test]
    fn test_type_name_func_ptr_int_int() {
        // in C: int (*int2int_t)(int)
        let expected = TypeName{
            indirection_level: 1,
            base_type: BaseType::FunctionPointer(
                Box::new("int".try_into().unwrap()),
                vec!["int".try_into().unwrap()]
            )
        };

        let actual = TypeName::try_from("int[int]*").unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_type_name_func_ptr_int_int_int_s() {
        // in C: int (*func_ptr)(int, int*)
        let expected = TypeName{
            indirection_level: 1,
            base_type: BaseType::FunctionPointer(
                Box::new("int".try_into().unwrap()),
                vec![
                    "int".try_into().unwrap(),
                    "int*".try_into().unwrap()
                ]
            )
        };

        let actual = TypeName::try_from("int[int,int*]*").unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_type_name_func_ptr_ptr_int_int_int_s() {
        // in C: int (**func_ptr)(int, int*)
        let expected = TypeName{
            indirection_level: 2,
            base_type: BaseType::FunctionPointer(
                Box::new("int".try_into().unwrap()),
                vec![
                    "int".try_into().unwrap(),
                    "int*".try_into().unwrap()
                ]
            )
        };

        let actual = TypeName::try_from("int[int,int*]**").unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_type_name_nested_func_ptr() {
        // in C: int (*func_ptr)(int (*)(int*))
        let expected = TypeName{
            indirection_level: 1,
            base_type: BaseType::FunctionPointer(
                Box::new("int".try_into().unwrap()),
                vec![
                    TypeName{
                        indirection_level: 1,
                        base_type: BaseType::FunctionPointer(
                            Box::new("int".try_into().unwrap()),
                            vec![
                                "int*".try_into().unwrap()
                            ]
                        )
                    }
                ]
            )
        };

        let actual = TypeName::try_from("int[int[int*]*]*").unwrap();
        assert_eq!(expected, actual);
    }


}
