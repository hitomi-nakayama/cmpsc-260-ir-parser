pub type FieldName = String;
pub type FunctionName = String;
pub type BasicBlockName = String;
pub type StructName = String;

use crate::parse_result::{ParseError};
use crate::variable::{Variable, Value};

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Instruction {
    Arith(Operation, Variable, Value, Value),
    Cmp(Relation, Variable, Value, Value),
    Phi(Variable, Vec<Value>),
    Copy(Variable, Value),
    Load(Variable, Variable),
    Store(Variable, Value),
    Select(Variable, Value, Value, Value),
    Call(Variable, FunctionName, Vec<Value>),
    ICall(Variable, Variable, Vec<Value>),
    Ret(Value),
    Alloc(Variable),
    AddrOf(Variable, Variable),
    Gep(Variable, Value, Value, Option<FieldName>),
    Jump(BasicBlockName),
    Branch(Value, BasicBlockName, BasicBlockName),
}

impl Instruction {
    pub fn lhs(&self) -> Option<&Variable> {
        match self {
            Instruction::Arith(_, lhs, _, _)
            | Instruction::Cmp(_, lhs, _, _)
            | Instruction::Phi(lhs, _)
            | Instruction::Copy(lhs, _)
            | Instruction::Load(lhs, _)
            | Instruction::Select(lhs, _, _, _)
            | Instruction::Call(lhs, _, _)
            | Instruction::ICall(lhs, _, _)
            | Instruction::Alloc(lhs)
            | Instruction::AddrOf(lhs, _)
            | Instruction::Gep(lhs, _, _, _) => Some(lhs),

            Instruction::Branch(..)
            | Instruction::Jump(..)
            | Instruction::Ret(..)
            | Instruction::Store(..) => None
        }
    }
    /**
     * Return all values from rhs. Variables are promoted to Value.
     */
    pub fn rhs_values(&self) -> Vec<Value> {
        match self {
            Instruction::Branch(rhs, _, _)
            | Instruction::Copy(_, rhs)
            | Instruction::Ret(rhs)
                => vec![rhs.clone()],

            Instruction::AddrOf(_, rhs)
            | Instruction::Load(_, rhs) => vec![Value::Variable(rhs.clone())],

            Instruction::Arith(_, _, rhs0, rhs1)
            | Instruction::Cmp(_, _, rhs0, rhs1)
            | Instruction::Gep(_, rhs0, rhs1, _)
                => vec![rhs0.clone(), rhs1.clone()],

            Instruction::Store(rhs0, rhs1)
                => vec![Value::Variable(rhs0.clone()), rhs1.clone()],

            Instruction::Call(_, _, rhs)
            | Instruction::Phi(_, rhs) => rhs.clone(),

            Instruction::ICall(_, func, rhs) => {
                let mut result = rhs.clone();
                result.insert(0, Value::Variable(func.clone()));
                result
            },

            Instruction::Select(_, cond, t_v, f_v)
                => vec![cond.clone(), t_v.clone(), f_v.clone()],

            Instruction::Alloc(..)
            | Instruction::Jump(..) => Vec::new()
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum Operation {
    Add,
    Div,
    Mul,
    Sub
}

impl TryFrom<&str> for Operation {
    type Error = ParseError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        use Operation::*;
        match value {
            "add" => Ok(Add),
            "div" => Ok(Div),
            "mul" => Ok(Mul),
            "sub" => Ok(Sub),
            _ => Err(ParseError::Generic(format!("Unknown arithmetic operation: {value}")))
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum Relation {
    Eq,
    Gt,
    Lt,
    Lte,  // less than or equal
    Neq
}

impl TryFrom<&str> for Relation {
    type Error = ParseError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        use Relation::*;
        match value {
            "eq" => Ok(Eq),
            "gt" => Ok(Gt),
            "lt" => Ok(Lt),
            "lte" => Ok(Lte),
            "neq" => Ok(Neq),
            _ => Err(ParseError::Generic(format!("Invalid relation {}", value)))
        }
    }
}
