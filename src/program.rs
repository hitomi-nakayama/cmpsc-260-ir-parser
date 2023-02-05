use crate::instruction::{BasicBlockName, FunctionName, Instruction, TypeName, Variable};

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct IR {
    pub functions: Vec<Function>
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Function {
    pub name: FunctionName,
    pub params: Vec<Variable>,
    pub return_type: TypeName,
    pub basic_blocks: Vec<BasicBlock>
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct BasicBlock {
    pub name: BasicBlockName,
    pub instructions: Vec<Instruction>
}
