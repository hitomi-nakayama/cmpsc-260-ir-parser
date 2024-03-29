use std::fmt;
use std::collections::HashMap;
use std::ops::Range;
use std::str::FromStr;
use std::sync::Arc;

use crate::instruction::{BasicBlockName, FieldName, FunctionName, Instruction,
    StructName};
use crate::parse_result::ParseError;
use crate::parser::parse;
use crate::{s, parse_function};
use crate::text::str_to_tokens;
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

    pub fn main(&self) -> Option<&Function> {
        self.functions.get(&Arc::new(s!("main")))
    }

    pub fn get_basic_block(&self, id: &BasicBlockId) -> Option<&BasicBlock> {
        let function = self.functions.get(&id.function)?;
        function.get_basic_block(id)
    }

    pub fn get_slice(&self, range: &InstructionRange) -> Option<&[Instruction]> {
        let function = self.functions.get(&range.basic_block.function)?;
        function.get_slice(range)
    }
}

impl FromStr for Program {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut tokens = str_to_tokens(s);
        let program = parse(&mut tokens)?;
        if tokens.is_empty() {
            Ok(program)
        } else {
            Err(ParseError::Generic(s!("Expected end of input.")))
        }
    }
}

impl TryFrom<&str> for Program {
    type Error = ParseError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        <Self as FromStr>::from_str(value)
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Struct {
    pub name: StructName,
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
        let entry = self.basic_blocks.get(&Arc::new(s!("entry"))).expect("Function does not have an entry block");
        assert_eq!(entry.name, s!("entry").into());
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

    pub fn get_slice(&self, range: &InstructionRange) -> Option<&[Instruction]> {
        if range.basic_block.function != self.id() {
            return None;
        }
        let basic_block = self.basic_blocks.get(&range.basic_block.basic_block)?;
        basic_block.get_slice(range)
    }
}

impl FromStr for Function {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut tokens = str_to_tokens(s);
        let function = parse_function(&mut tokens)?;
        if tokens.is_empty() {
            Ok(function)
        } else {
            Err(ParseError::Generic(s!("Expected end of input.")))
        }
    }
}

impl TryFrom<&str> for Function {
    type Error = ParseError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        <Self as FromStr>::from_str(value)
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
        if !(self.instructions.is_empty()) {
            Some(InstructionId{
                basic_block_id: self.id(),
                index_: self.instructions.len() - 1
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
                    basic_block_id: self.id(),
                    index_: i
                };
                (id, instr)
            });
        Box::new(iter)
    }

    pub fn get_slice(&self, range: &InstructionRange) -> Option<&[Instruction]> {
        if range.basic_block.basic_block != self.name {
            return None;
        }
        let start = range.instructions.start;
        let end = range.instructions.end;
        if start > end || end > self.instructions.len() {
            return None;
        }
        Some(&self.instructions[start..end])
    }
}

#[derive(Debug, Hash, Clone, PartialEq, Eq)]
pub struct BasicBlockIdConversionError;

impl fmt::Display for BasicBlockIdConversionError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Invalid basic block id")
    }
}

pub trait BasicBlockIdInfo {
    fn basic_block_id(&self) -> &BasicBlockId;
    fn function_id(&self) -> &FunctionId;
}

/**
 * A unique identifier for a basic block.
 */
#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd, Ord)]
pub struct BasicBlockId {
    function: FunctionId,
    basic_block: BasicBlockName
}

impl BasicBlockId {
    pub fn new(function: FunctionId, basic_block: BasicBlockName) -> BasicBlockId {
        BasicBlockId {
            function,
            basic_block
        }
    }

    pub fn from_function_basic_block(function: &Function, basic_block: &BasicBlock) -> BasicBlockId {
        BasicBlockId {
            function: function.name.clone(),
            basic_block: basic_block.name.clone()
        }
    }

    pub fn parse_sep(s: &str, sep: char) -> Result<BasicBlockId, BasicBlockIdConversionError> {
        let parts: Vec<&str> = s.split(sep).collect();
        if parts.len() != 2 {
            return Err(BasicBlockIdConversionError);
        }
        let function = parts[0];
        let basic_block = parts[1];
        Ok(BasicBlockId{
            function: s!(function).into(),
            basic_block: s!(basic_block).into()
        })
    }
}

impl BasicBlockIdInfo for BasicBlockId {
    fn basic_block_id(&self) -> &BasicBlockId {
        self
    }
    fn function_id(&self) -> &FunctionId {
        &self.function
    }
}

impl fmt::Display for BasicBlockId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}.{}", self.function, self.basic_block)
    }
}


impl FromStr for BasicBlockId {
    type Err = BasicBlockIdConversionError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Self::parse_sep(s, '.')
    }
}

impl TryFrom<&str> for BasicBlockId {
    type Error = BasicBlockIdConversionError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        Self::from_str(value)
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct InstructionRange {
    instructions: Range<usize>,
    basic_block: BasicBlockId
}

impl InstructionRange {
    pub fn new(basic_block: BasicBlockId, instructions: Range<usize>) -> InstructionRange {
        InstructionRange {
            instructions,
            basic_block
        }
    }
}

impl BasicBlockIdInfo for InstructionRange {
    fn basic_block_id(&self) -> &BasicBlockId {
        &self.basic_block
    }
    fn function_id(&self) -> &FunctionId {
        self.basic_block.function_id()
    }
}

impl From<InstructionId> for InstructionRange {
    fn from(id: InstructionId) -> Self {
        Self::new(id.basic_block_id, id.index_..id.index_ + 1)
    }
}

impl From<&InstructionId> for InstructionRange {
    fn from(id: &InstructionId) -> Self {
        id.clone().into()
    }
}

impl fmt::Display for InstructionRange {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}.{}..{}", self.basic_block, self.instructions.start, self.instructions.end)
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
    basic_block_id: BasicBlockId,
    index_: usize, // index of instruction within basic block
}

impl InstructionId {
    pub fn new(function: &Function, basic_block: &BasicBlock, index: usize) -> InstructionId {
        InstructionId {
            basic_block_id: BasicBlockId::from_function_basic_block(function, basic_block),
            index_: index
        }
    }

    pub fn parse_sep(s: &str, sep: char) -> Result<InstructionId, InstructionIdConversionError> {
        let (bb, index) = s.rsplit_once(sep)
            .ok_or(InstructionIdConversionError)?;

        let bb = BasicBlockId::parse_sep(bb, sep)
            .map_err(|_| InstructionIdConversionError)?;

        let index = index.parse::<usize>()
            .map_err(|_| InstructionIdConversionError)?;

        Ok(InstructionId{
            basic_block_id: bb,
            index_: index
        })
    }

    pub fn index(&self) -> usize {
        self.index_
    }
}

impl BasicBlockIdInfo for InstructionId {
    fn basic_block_id(&self) -> &BasicBlockId {
        &self.basic_block_id
    }
    fn function_id(&self) -> &FunctionId {
        self.basic_block_id.function_id()
    }
}

impl fmt::Display for InstructionId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}.{}", self.basic_block_id, self.index_)
    }
}

impl FromStr for InstructionId {
    type Err = InstructionIdConversionError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Self::parse_sep(s, '.')
    }
}

impl TryFrom<&str> for InstructionId {
    type Error = InstructionIdConversionError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        Self::from_str(value)
    }
}

#[cfg(test)]
mod tests {
    use crate::{variable::BaseType, s};

    use super::*;

    #[test]
    fn test_program_main() {
        let main: Function = "
        function main() -> int {
        entry:
            x:int = $copy 0
            $ret x:int
        }
        ".parse().unwrap();

        let program = Program {
            functions: map![
                s!("main").into() => main.clone()
            ],
            structs: HashMap::new(),

        };

        let actual = program.main().unwrap();

        assert_eq!(&main, actual);
    }

    #[test]
    fn jumps_to_jump() {
        let bb = BasicBlock {
            name: s!("bb0").into(),
            function: s!("main").into(),
            instructions: vec![
                Instruction::Jump(s!("bb1").into())
            ]
        };
        let expected: Vec<BasicBlockId> = vec!["main.bb1".try_into().unwrap()];
        let actual = bb.jumps_to();
        assert_eq!(expected, actual);
    }

    #[test]
    fn jumps_to_branch() {
        let bb = BasicBlock {
            name: s!("bb0").into(),
            function: s!("main").into(),
            instructions: vec![
                Instruction::Branch("x:int".try_into().unwrap(), s!("bb1").into(), s!("bb2").into())
            ]
        };
        let expected: Vec<BasicBlockId> = vec!["main.bb1".try_into().unwrap(), "main.bb2".try_into().unwrap()];
        let actual = bb.jumps_to();
        assert_eq!(expected, actual);
    }

    #[test]
    fn jumps_to_ret() {
        let bb = BasicBlock {
            name: s!("bb0").into(),
            function: s!("main").into(),
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
        let func = Function {
            name: s!("f").into(),
            params: vec![
                "a:int*".try_into().unwrap(),
                "b:int**".try_into().unwrap()
            ],
            return_type: "int".try_into().unwrap(),
            basic_blocks: map![
                s!("entry").into() => BasicBlock {
                    function: s!("f").into(),
                    name: s!("entry").into(),
                    instructions: vec![
                        Instruction::Ret("0".try_into().unwrap())
                    ]
                }

            ]
        };

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
            name: s!("f").into(),
            params: Vec::new(),
            return_type: "int".try_into().unwrap(),
            basic_blocks: map![
                s!("entry").into() => BasicBlock {
                    name: s!("entry").into(),
                    function: s!("main").into(),
                    instructions: Vec::new()
                }
            ]
        };
        let expected = BasicBlock{
            name: s!("entry").into(),
            function: s!("main").into(),
            instructions: Vec::new()
        };
        let actual = function.entry_block();
        assert_eq!(&expected, actual);
    }

    #[test]
    fn test_instruction_id_try_from_str() {
        let actual = InstructionId::try_from("main.entry.0").unwrap();

        let expected = InstructionId{
            basic_block_id: BasicBlockId{
                function: s!("main").into(),
                basic_block: s!("entry").into()
            },
            index_: 0
        };
        assert_eq!(expected, actual);
    }
}
