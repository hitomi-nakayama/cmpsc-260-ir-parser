use std::{collections::HashMap};

use crate::instruction::{BasicBlockName, FieldName, FunctionName, Instruction,
    StructName};
use crate::variable::{TypeName, Variable};

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Program {
    pub functions: HashMap<FunctionName, Function>,
    pub structs: HashMap<StructName, Struct>
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Struct {
    pub name: String,
    pub fields: HashMap<FieldName, TypeName>
}

pub type FunctionId = FunctionName;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Function {
    pub name: FunctionName,
    pub params: Vec<Variable>,
    pub return_type: TypeName,
    pub basic_blocks: HashMap<BasicBlockName, BasicBlock>
}

impl Function {
    pub fn entry_block(&self) -> &BasicBlock {
        let entry = self.basic_blocks.get("entry").expect("Function does not have an entry block");
        assert_eq!(entry.name, "entry");
        entry
    }

    pub fn id(&self) -> FunctionId {
        self.name.clone()
    }

    pub fn enumerate_instructions<'a>(&'a self)
            -> Box<dyn Iterator<Item=(InstructionId, &Instruction)> + 'a> {

        let iter = self.basic_blocks.values()
            .flat_map(|bb| {
                bb.enumerate_instructions()
            });
        Box::new(iter)
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct BasicBlock {
    pub name: BasicBlockName,
    pub function: FunctionId,
    pub instructions: Vec<Instruction>
}

impl BasicBlock {
    /**
     * Returns the name of the basic blocks
     * that this basic block jumps and branches to.
     */
    pub fn jumps_to(&self) -> Vec<BasicBlockName> {
        if let Some(instr) = self.instructions.last(){
            match instr {
                Instruction::Jump(target) => vec![target.to_owned()],
                Instruction::Branch(_, true_branch, false_branch) =>
                    vec![true_branch.to_owned(), false_branch.to_owned()],
                Instruction::Ret(_) => Vec::new(),
                _ => panic!("Basic block does not end in a jump, branch, or ret instruction")
            }
        } else {
            Vec::new()
        }
    }

    pub fn id(&self) -> BasicBlockId {
        BasicBlockId{
            function: self.function.clone(),
            basic_block: self.name.clone()
        }
    }

    pub fn enumerate_instructions<'a>(&'a self)
            -> Box<dyn Iterator<Item=(InstructionId, &Instruction)> + 'a> {
        let iter = self.instructions.iter()
            .enumerate()
            .map(|(i, instr)| {
                let id = InstructionId{
                    basic_block: self.id(),
                    index: i
                };
                (id, instr)
            });
        Box::new(iter)
    }
}

/**
 * A unique identifier for a basic block.
 */
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct BasicBlockId {
    pub function: FunctionId,
    pub basic_block: BasicBlockName
}

impl BasicBlockId {
    pub fn new(function: &Function, basic_block: &BasicBlock) -> BasicBlockId {
        BasicBlockId {
            function: function.name.clone(),
            basic_block: basic_block.name.clone()
        }
    }
}

/**
 * A unique identifier for an instruction.
 */
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct InstructionId {
    pub basic_block: BasicBlockId,
    pub index: usize, // index of instruction within basic block
}

impl InstructionId {
    pub fn new(function: &Function, basic_block: &BasicBlock, index: usize) -> InstructionId {
        InstructionId {
            basic_block: BasicBlockId::new(function, basic_block),
            index
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn jumps_to_jump() {
        let bb = BasicBlock {
            name: "bb0".into(),
            function: "main".to_owned(),
            instructions: vec![
                Instruction::Jump("bb1".into())
            ]
        };
        let expected: Vec<BasicBlockName> = vec!["bb1".into()];
        let actual = bb.jumps_to();
        assert_eq!(expected, actual);
    }

    #[test]
    fn jumps_to_branch() {
        let bb = BasicBlock {
            name: "bb0".into(),
            function: "main".to_owned(),
            instructions: vec![
                Instruction::Branch("x:int".try_into().unwrap(), "bb1".into(), "bb2".into())
            ]
        };
        let expected: Vec<BasicBlockName> = vec!["bb1".into(), "bb2".into()];
        let actual = bb.jumps_to();
        assert_eq!(expected, actual);
    }

    #[test]
    fn jumps_to_ret() {
        let bb = BasicBlock {
            name: "bb0".into(),
            function: "main".to_owned(),
            instructions: vec![
                Instruction::Ret("x:int".try_into().unwrap())
            ]
        };
        let expected: Vec<BasicBlockName> = Vec::new();
        let actual = bb.jumps_to();

        assert_eq!(expected, actual);
    }

    #[test]
    fn entry_block() {
        let function = Function {
            name: "f".to_owned(),
            params: Vec::new(),
            return_type: "int".try_into().unwrap(),
            basic_blocks: map![
                "entry".to_owned() => BasicBlock {
                    name: "entry".into(),
                    function: "main".to_owned(),
                    instructions: Vec::new()
                }
            ]
        };
        let expected = BasicBlock{
            name: "entry".into(),
            function: "main".to_owned(),
            instructions: Vec::new()
        };
        let actual = function.entry_block();
        assert_eq!(&expected, actual);
    }
}
