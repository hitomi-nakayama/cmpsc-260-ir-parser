use std::convert::{TryFrom};
use std::fmt;
use std::sync::Arc;

use crate::{create_token_reader, parse_variable};
use crate::parse_result::ParseError;
use crate::parser::{parse_type_name, parse_value};


#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Value {
    Constant(i32),
    Variable(Variable),
}

impl Value {
    pub fn is_variable(&self) -> bool {
        matches!(self, Value::Variable(_))
    }

    pub fn is_constant(&self) -> bool {
        matches!(self, Value::Constant(_))
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Constant(c) => write!(f, "{}", c),
            Value::Variable(v) => write!(f, "{}", v)
        }
    }
}

impl From<Variable> for Value {
    fn from(var: Variable) -> Self {
        Value::Variable(var)
    }
}
impl From<i32> for Value {
    fn from(constant: i32) -> Self {
        Value::Constant(constant)
    }
}

impl TryFrom<&str> for Value {
    type Error = ParseError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        let mut tokens = create_token_reader(value.as_bytes());
        let variable = parse_value(&mut tokens)?;

        if !(tokens.is_empty()) {
            Err(ParseError::Generic(format!("Invalid value {}", value)))
        } else {
            Ok(variable)
        }
    }
}

pub type VariableName = Arc<String>;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Variable {
    pub name: VariableName,
    pub type_name: TypeName
}

impl Variable {
    pub fn new(name: VariableName, type_name: TypeName) -> Self {
        Variable{name, type_name}
    }
}

impl fmt::Display for Variable {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.name, self.type_name)
    }
}

impl TryFrom<&str> for Variable {
    type Error = ParseError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        let mut tokens = create_token_reader(value.as_bytes());
        let variable = parse_variable(&mut tokens)?;

        if !(tokens.is_empty()) {
            Err(ParseError::Generic(format!("Invalid variable {}", value)))
        } else {
            Ok(variable)
        }
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct TypeName {
    pub indirection_level: u8,  // a function pointer has indirection level 1
    pub base_type: BaseType
}

impl TypeName {
    pub fn new(indirection_level: u8, base_type: BaseType) -> Self {
        TypeName{indirection_level, base_type}
    }

    pub fn is_function_pointer(&self) -> bool {
        matches!(self.base_type, BaseType::FunctionPointer(_, _))
    }

    pub fn is_variable_type(&self) -> bool {
        matches!(self.base_type, BaseType::VariableType(_))
    }

    pub fn is_pointer(&self) -> bool {
        self.indirection_level > 0
    }
}

impl TryFrom<&str> for TypeName {
    type Error = ParseError;
    fn try_from(value: &str) -> Result<Self, Self::Error> {
        let mut tokens = create_token_reader(value.as_bytes());
        let type_name = parse_type_name(&mut tokens)?;

        if !(tokens.is_empty()) {
            Err(ParseError::Generic(format!("Invalid type name {}", value)))
        } else {
            Ok(type_name)
        }
    }
}

impl fmt::Display for TypeName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f,
            "{}{}",
            self.base_type,
            String::from_utf8(vec![b'*'; self.indirection_level as usize]).unwrap()
        )
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum BaseType {
    VariableType(Arc<String>),
    FunctionPointer(Box<TypeName>, Vec<TypeName>)
}

impl fmt::Display for BaseType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BaseType::VariableType(s) => write!(f, "{}", s),
            BaseType::FunctionPointer(return_type, args) => {
                write!(f, "{}[", return_type)?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ",")?;
                    }
                    write!(f, "{}", arg)?;
                }
                write!(f, "]")
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_variable_0() {
        let var = Variable::try_from("x:i32").unwrap();
        assert_eq!(var.name, "x".to_owned().into());

        let expected_type = "i32".try_into().unwrap();
        assert_eq!(var.type_name, expected_type);
    }

    #[test]
    fn test_variable_1() {
        let var = Variable::try_from("my_var:i32*").unwrap();
        assert_eq!(var.name, "my_var".to_owned().into());

        let expected_type = "i32*".try_into().unwrap();
        assert_eq!(var.type_name, expected_type);
    }

    #[test]
    fn test_variable_empty_function_ptr() {
        let var = Variable::try_from("f:int[]*").unwrap();
        assert_eq!(var.name, "f".to_owned().into());

        let expected_type = TypeName {
            indirection_level: 1,
            base_type: BaseType::FunctionPointer(
                TypeName {
                    indirection_level: 0,
                    base_type: BaseType::VariableType(s!("int").into())
                }.into(),
                vec![]
            )
        };
        assert_eq!(var.type_name, expected_type);
    }

    #[test]
    fn test_variable_nested_function_ptr() {
        let var = Variable::try_from("@f:int[]*").unwrap();
        assert_eq!(var.name, "@f".to_owned().into());

        let expected_type = TypeName {
            indirection_level: 1,
            base_type: BaseType::FunctionPointer(
                TypeName {
                    indirection_level: 0,
                    base_type: BaseType::VariableType(s!("int").into())
                }.into(),
                vec![]
            )
        };
        assert_eq!(var.type_name, expected_type);
    }

    #[test]
    fn test_type_name_0() {
        let type_name: TypeName = "i32".try_into().unwrap();
        assert_eq!(type_name.indirection_level, 0);
        assert_eq!(type_name.base_type, BaseType::VariableType(s!("i32").into()));
    }

    #[test]
    fn test_type_name_1() {
        let type_name: TypeName = "i32*".try_into().unwrap();
        assert_eq!(type_name.indirection_level, 1);
        assert_eq!(type_name.base_type, BaseType::VariableType(s!("i32").into()));
    }

    #[test]
    fn test_type_name_2() {
        let type_name: TypeName = "i32**".try_into().unwrap();
        assert_eq!(type_name.indirection_level, 2);
        assert_eq!(type_name.base_type, BaseType::VariableType(s!("i32").into()));
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

    #[test]
    fn test_type_name_to_string() {
        // in C: int (*func_ptr)(int (*)(int*))
        let ty = TypeName{
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
        let expected = "int[int[int*]*]*";
        let actual = ty.to_string();
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_value_0() {
        let value = Value::try_from("0").unwrap();
        assert_eq!(value, Value::Constant(0));
    }

    #[test]
    fn test_value_negative() {
        let value = Value::try_from("-1").unwrap();
        assert_eq!(value, Value::Constant(-1));
    }
}
