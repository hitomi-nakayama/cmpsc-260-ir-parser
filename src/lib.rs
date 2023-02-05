mod instruction;
mod parse_result;
mod parser;
mod program;
mod text;

pub use crate::instruction::{BaseType, BasicBlockName, Instruction, FunctionName, Operation, Relation, Variable, Value};
pub use crate::parse_result::{ParseError, ParseResult};
pub use crate::parser::{parse};
pub use crate::program::{BasicBlock, Function, IR};
pub use crate::text::{SourceReader};
