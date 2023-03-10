use std::fmt;

pub type ParseResult<T> = Result<T, ParseError>;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ParseError {
    Generic(String),
    Expected(usize, String),  // expected but did not find
    ExpectedFound(usize, String, String),  // (Expected, Found)
    VariableParseError(usize, String),
    Unexpected(usize, String)  // found but did not expect
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use ParseError::*;
        match self {
            Generic(msg) => write!(f, "{}", msg),
            Expected(line, expected) => write!(f, "Expected {} on line {}", expected, line),
            ExpectedFound(line, expected, found) => write!(f, "Expected {} but found {} on line {}", expected, found, line),
            VariableParseError(line, msg) => write!(f, "Variable parse error on line {}: {}", line, msg),
            Unexpected(line, found) => write!(f, "Unexpected {} on line {}", found, line)
        }
    }
}
