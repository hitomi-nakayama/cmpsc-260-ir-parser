mod instruction;

#[macro_use]
mod function_macro;

#[macro_use]
mod map_macro;

mod parse_result;
mod parser;
mod program;
#[macro_use]
mod string_macro;
mod text;
mod variable;

pub use crate::instruction::{BasicBlockName, Instruction,
    FieldName, FunctionName, Operation, Relation, StructName};
pub use crate::parse_result::{ParseError, ParseResult};
pub use crate::parser::{expect, parse, parse_function, parse_variable, parse_list};
pub use crate::program::{BasicBlock, BasicBlockId, BasicBlockIdConversionError,
                         Function, FunctionId,
                         InstructionId, InstructionIdConversionError,
                         Program, Struct};
pub use crate::text::{create_token_reader, str_to_tokens, TokenReader};
pub use crate::variable::{BaseType, TypeName, Variable, VariableName, Value};
