type FunctionName = String;
type BasicBlockName = String;

#[path = "parse_result.rs"]
mod parse_result;
use parse_result::{ParseResult, ParseError};

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Instruction {
    Arith(Operation, Variable, Variable, Variable),
    Phi(Variable, Vec<Variable>),
    Copy(Variable, Variable),
    Load(Variable, Variable),
    Store(Variable, Variable),
    Select(Variable, Variable, Variable, Variable),
    Call(Variable, FunctionName, Vec<Variable>),
    ICall(Variable, Variable, Vec<Variable>),
    Ret(Variable),
    Alloc(Variable, Variable),
    AddrOf(Variable, Variable),
    Gep(Variable, Variable, Variable),
    Jump(BasicBlockName),
    Branch(Variable, BasicBlockName, BasicBlockName),
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Operation {
    Add
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Relation {
    Leq
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Variable {
    name: String,
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
    indirection_level: u8,
    base_type: String
}

impl TryFrom<&str> for TypeName {
    type Error = ParseError;
    fn try_from(value: &str) -> Result<Self, Self::Error> {
        if let Some((base_type, stars)) = value.split_once("*") {
            let mut indirection_level = 1;
            for c in stars.chars() {
                if c != '*' {
                    return Err(ParseError::Generic(format!("Invalid type name {}", value)));
                }
                indirection_level += 1;
            }
            Ok(TypeName{indirection_level, base_type: base_type.to_owned()})
        } else {
            Ok(TypeName{indirection_level: 0, base_type: value.to_owned()})
        }
    }
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
        assert_eq!(type_name.base_type, "i32");
    }

    #[test]
    fn test_type_name_1() {
        let type_name: TypeName = "i32*".try_into().unwrap();
        assert_eq!(type_name.indirection_level, 1);
        assert_eq!(type_name.base_type, "i32");
    }

    #[test]
    fn test_type_name_2() {
        let type_name: TypeName = "i32**".try_into().unwrap();
        assert_eq!(type_name.indirection_level, 2);
        assert_eq!(type_name.base_type, "i32");
    }

    #[test]
    fn test_type_name_star_inside() {
        let type_name = TypeName::try_from("i32**a");
        assert!(type_name.is_err());
    }
}
