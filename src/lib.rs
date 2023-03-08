mod instruction;

#[macro_use]
mod map_macro;

mod parse_result;
mod parser;
mod program;
mod text;
mod variable;

pub use crate::instruction::{BasicBlockName, Instruction,
    FieldName, FunctionName, Operation, Relation, StructName};
pub use crate::parse_result::{ParseError, ParseResult};
pub use crate::parser::{parse, parse_variable};
pub use crate::program::{BasicBlock, Function, Program, Struct};
pub use crate::text::{create_token_reader, TokenReader};
pub use crate::variable::{BaseType, TypeName, Variable, Value};
