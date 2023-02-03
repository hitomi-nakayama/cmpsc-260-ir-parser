pub type ParseResult<T> = Result<T, ParseError>;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ParseError {
    Generic(String),
    Expected(usize, String),  // expected but did not find
    ExpectedFound(usize, String, String),  // (Expected, Found)
    VariableParseError(usize, String),
    Unexpected(usize, String)  // found but did not expect
}
