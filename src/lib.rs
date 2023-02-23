mod instruction;

#[macro_use]
mod map_macro;

mod parse_result;
mod parser;
mod program;
mod text;

pub use crate::instruction::{BaseType, BasicBlockName, Instruction,
    FieldName, FunctionName, Operation, Relation, StructName, TypeName,
    Variable, Value};
pub use crate::parse_result::{ParseError, ParseResult};
pub use crate::parser::{parse, parse_variable};
pub use crate::program::{BasicBlock, Function, Program, Struct};
pub use crate::text::{TokenReader};
