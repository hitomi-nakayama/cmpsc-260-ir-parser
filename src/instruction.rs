type FunctionName = String;
type BasicBlockName = String;

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

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct TypeName {
    indirection_level: u8,
    base_type: String
}
