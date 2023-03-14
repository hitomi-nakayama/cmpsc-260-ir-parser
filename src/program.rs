use std::fmt;
use std::{collections::HashMap};

use crate::instruction::{BasicBlockName, FieldName, FunctionName, Instruction,
    StructName};
use crate::variable::{BaseType, TypeName, Variable};

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Program {
    pub functions: HashMap<FunctionName, Function>,
    pub structs: HashMap<StructName, Struct>
}

impl Program {
    pub fn enumerate_instructions<'a>(&'a self)
            -> Box<dyn Iterator<Item=(InstructionId, &Instruction)> + 'a> {

        let iter = self.functions.values()
            .flat_map(|f| {
                f.enumerate_instructions()
            });
        Box::new(iter)
    }
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

    pub fn basic_block_ids(&self) -> Vec<BasicBlockId> {
        self.basic_blocks.values()
            .map(|bb| bb.id())
            .collect()
    }

    pub fn get_basic_block(&self, id: &BasicBlockId) -> Option<&BasicBlock> {
        if id.function != self.id() {
            return None;
        }
        self.basic_blocks.get(&id.basic_block)
    }

    pub fn ptr_type(&self) -> TypeName {
        TypeName {
            indirection_level: 1,
            base_type: BaseType::FunctionPointer(
                Box::new(self.return_type.clone()),
                self.params.iter().map(|p| p.type_name.clone()).collect()
            )
        }
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
    pub fn jumps_to(&self) -> Vec<BasicBlockId> {
        if let Some(instr) = self.instructions.last(){
            match instr {
                Instruction::Jump(target) => vec![
                    BasicBlockId {
                        function: self.function.clone(),
                        basic_block: target.to_owned()
                    }],
                Instruction::Branch(_, true_branch, false_branch) =>
                    vec![
                        BasicBlockId {
                            function: self.function.clone(),
                            basic_block: true_branch.to_owned()
                        },
                        BasicBlockId {
                            function: self.function.clone(),
                            basic_block: false_branch.to_owned()
                        }
                    ],
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

    pub fn terminal_instruction(&self) -> Option<&Instruction> {
        self.instructions.last()
    }

    pub fn terminal_instruction_id(&self) -> Option<InstructionId> {
        if self.instructions.len() > 0 {
            Some(InstructionId{
                basic_block: self.id(),
                index: self.instructions.len() - 1
            })
        } else {
            None
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

#[derive(Debug, Hash, Clone, PartialEq, Eq)]
pub struct BasicBlockIdConversionError;

impl fmt::Display for BasicBlockIdConversionError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Invalid basic block id")
    }
}

/**
 * A unique identifier for a basic block.
 */
#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd, Ord)]
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

    pub fn from_str(s: &str, sep: char) -> Result<BasicBlockId, BasicBlockIdConversionError> {
        let parts: Vec<&str> = s.split(sep).collect();
        if parts.len() != 2 {
            return Err(BasicBlockIdConversionError);
        }
        let function = parts[0];
        let basic_block = parts[1];
        Ok(BasicBlockId{
            function: function.to_owned(),
            basic_block: basic_block.to_owned()
        })
    }
}

impl fmt::Display for BasicBlockId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}.{}", self.function, self.basic_block)
    }
}

impl TryFrom<&str> for BasicBlockId {
    type Error = BasicBlockIdConversionError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        Self::from_str(value, '.')
    }
}

#[derive(Debug, Hash, Clone, PartialEq, Eq)]
pub struct InstructionIdConversionError;

impl fmt::Display for InstructionIdConversionError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Invalid instruction id")
    }
}
/**
 * A unique identifier for an instruction.
 */
#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
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

    pub fn from_str(s: &str, sep: char) -> Result<InstructionId, InstructionIdConversionError> {
        let (bb, index) = s.rsplit_once(sep)
            .ok_or(InstructionIdConversionError)?;

        let bb = BasicBlockId::from_str(bb, sep)
            .map_err(|_| InstructionIdConversionError)?;

        let index = index.parse::<usize>()
            .map_err(|_| InstructionIdConversionError)?;

        Ok(InstructionId{
            basic_block: bb,
            index
        })
    }
}

impl fmt::Display for InstructionId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}.{}", self.basic_block, self.index)
    }
}

impl TryFrom<&str> for InstructionId {
    type Error = InstructionIdConversionError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        Self::from_str(value, '.')
    }
}

#[cfg(test)]
mod tests {
    use crate::variable::BaseType;

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
        let expected: Vec<BasicBlockId> = vec!["main.bb1".try_into().unwrap()];
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
        let expected: Vec<BasicBlockId> = vec!["main.bb1".try_into().unwrap(), "main.bb2".try_into().unwrap()];
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
        let expected: Vec<BasicBlockId> = Vec::new();
        let actual = bb.jumps_to();

        assert_eq!(expected, actual);
    }

    #[test]
    fn function_ptr_type() {
        let func = function!("f", ("a:int*", "b:int**") => "int", {
            "entry" => {
                "$ret 0"
            },
        });

        let actual = func.ptr_type();

        let expected = TypeName {
            indirection_level: 1,
            base_type: BaseType::FunctionPointer(
                Box::new("int".try_into().unwrap()),
                vec![
                    "int*".try_into().unwrap(),
                    "int**".try_into().unwrap()
                ]
            )
        };

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

    #[test]
    fn test_instruction_id_try_from_str() {
        let actual = InstructionId::try_from("main.entry.0").unwrap();

        let expected = InstructionId{
            basic_block: BasicBlockId{
                function: "main".to_owned(),
                basic_block: "entry".to_owned()
            },
            index: 0
        };
        assert_eq!(expected, actual);
    }
}
