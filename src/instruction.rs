use std::fmt;
use std::sync::Arc;

use crate::create_token_reader;
use crate::parse_result::{ParseError};
use crate::parser::parse_instruction;
use crate::variable::{Variable, Value};

pub type FieldName = Arc<String>;
pub type FunctionName = Arc<String>;
pub type BasicBlockName = Arc<String>;
pub type StructName = Arc<String>;

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

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Instruction::Arith(op, lhs, op0, op1) => write!(f, "{} = $arith {} {} {}", lhs, op, op0, op1),
            Instruction::Branch(op0, op1, op2) => write!(f, "$branch {} {} {}", op0, op1, op2),
            Instruction::Jump(op0) => write!(f, "$jump {}", op0),
            Instruction::Cmp(rel, lhs, op0, op1) => write!(f, "{} = $cmp {} {} {}", lhs, rel, op0, op1),
            Instruction::Phi(lhs, args) => {
                write!(f, "{} = $phi(", lhs)?;
                write_call_args(f, args)?;
                write!(f, ")")
            }
            Instruction::Copy(lhs, rhs) => write!(f, "{} = $copy {}", lhs, rhs),
            Instruction::Load(lhs, rhs) => write!(f, "{} = $load {}", lhs, rhs),
            Instruction::Select(lhs, op0, op1, op2) => {
                write!(f, "{} = $select {} {} {}", lhs, op0, op1, op2)
            }
            Instruction::Call(lhs, func, args) => {
                write!(f, "{} = $call {}(", lhs, func)?;
                write_call_args(f, args)?;
                write!(f, ")")
            }
            Instruction::ICall(lhs, func_ptr, args) => {
                write!(f, "{} = $icall {}(", lhs, func_ptr)?;
                write_call_args(f, args)?;
                write!(f, ")")
            },
            Instruction::Alloc(lhs) => write!(f, "{} = $alloc", lhs),
            Instruction::AddrOf(lhs, rhs) => write!(f, "{} = $addrof {}", lhs, rhs),
            Instruction::Gep(lhs, op0, op1, op2) => {
                write!(f, "{} = $gep {} {}", lhs, op0, op1)?;
                if let Some(field) = op2 {
                    write!(f, " {}", field)?;
                }
                Ok(())
            },
            Instruction::Ret(retval) => write!(f, "$ret {}", retval),
            Instruction::Store(op0, op1) => write!(f, "$store {} {}", op0, op1),
        }
    }
}

fn write_call_args(f: &mut fmt::Formatter, args: &[Value]) -> fmt::Result {
    for (i, arg) in args.iter().enumerate() {
        if i > 0 {
            write!(f, ", ")?;
        }
        write!(f, "{}", arg)?;
    }
    Ok(())
}

impl TryFrom<&str> for Instruction {
    type Error = ParseError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        let mut tokens = create_token_reader(value.as_bytes());
        let instr = parse_instruction(&mut tokens)?;
        if !(tokens.is_empty()) {
            return Err(ParseError::Generic("Expected end of input.".to_owned()));
        }
        Ok(instr)
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum Operation {
    Add,
    Div,
    Mul,
    Sub
}

impl fmt::Display for Operation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Operation::*;
        match self {
            Add => write!(f, "add"),
            Div => write!(f, "div"),
            Mul => write!(f, "mul"),
            Sub => write!(f, "sub")
        }
    }
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

impl fmt::Display for Relation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Relation::*;
        match self {
            Eq => write!(f, "eq"),
            Gt => write!(f, "gt"),
            Lt => write!(f, "lt"),
            Lte => write!(f, "lte"),
            Neq => write!(f, "neq")
        }
    }
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
