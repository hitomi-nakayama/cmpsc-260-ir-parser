pub fn add(left: usize, right: usize) -> usize {
    left + right
}

fn tokenize_line(input: &str) -> Vec<String> {
    let mut tokens: Vec<String> = Vec::new();

    let mut word_start: Option<usize> = None;

    for (i, c) in input.chars().enumerate() {
        if c.is_whitespace() || c == ',' || c == '(' || c == ')' {
            if let Some(start) = word_start {
                tokens.push(input[start..i].to_string());
                word_start = None;
                if !(c.is_whitespace()) {
                    tokens.push(c.to_string());
                }
            }
        } else {
            if word_start.is_none() {
                word_start = Some(i);
            }
        }
    }
    if let Some(start) = word_start {
        tokens.push(input[start..input.len()].to_string());
        word_start = None;
    }

    tokens
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let result = add(2, 2);
        assert_eq!(result, 4);
    }

    #[test]
    fn tokenize_line_statement() {
        let tokens = tokenize_line("div:int = $arith div i2:int i3:int");
        assert_eq!(tokens, vec!["div:int", "=", "$arith", "div", "i2:int", "i3:int"]);
    }

    #[test]
    fn tokenize_line_function_dec() {
        let tokens = tokenize_line("function construct(value:int, left:TreeNode*) -> TreeNode* {");
        assert_eq!(tokens, vec!["function", "construct", "(", "value:int", ",", "left:TreeNode*", ")", "->", "TreeNode*", "{"]);
    }
}
