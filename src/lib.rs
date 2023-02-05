mod instruction;
mod parse_result;
mod parser;
mod program;
mod text;

pub use crate::instruction::{BaseType, BasicBlockName, Instruction,
    FunctionName, Operation, Relation, TypeName,
    Variable, Value};
pub use crate::parse_result::{ParseError, ParseResult};
pub use crate::parser::{parse};
pub use crate::program::{BasicBlock, Function, Program};
pub use crate::text::{SourceReader};
