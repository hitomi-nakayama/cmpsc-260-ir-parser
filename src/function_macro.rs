#[macro_export]
macro_rules! function {
    ($func_name:expr $(,)? => $return_type:expr, $body:tt) => {
        $crate::Function {
            name: $func_name.to_string(),
            params: vec![],
            return_type: $return_type.try_into().unwrap(),
            basic_blocks: function!(@blocks $func_name, $body)
        }
    };

    ($func_name:expr, $args:tt => $return_type:expr, $body:tt) => {
        $crate::Function {
            name: $func_name.to_string(),
            params: function!(@params $args),
            return_type: $return_type.try_into().unwrap(),
            basic_blocks: function!(@blocks $func_name, $body)
        }
    };

    (@params ($($arg:expr),* $(,)?)) => {
        vec![$($arg.try_into().unwrap()),*]
    };

    (@blocks $func_name:expr, {$($block_name:expr => $block_body:tt),* $(,)?}) => {
        $crate::map![
            $(
                $block_name.to_string() => $crate::BasicBlock {
                    function: $func_name.to_string(),
                    name: $block_name.to_string(),
                    instructions: function!(@instructions $block_body)
                },
            )*
        ]
    };

    (@instructions {$($instruction:expr),* $(,)?}) => {
        vec![$($instruction.try_into().unwrap()),*]
    };
}

#[cfg(test)]
mod tests{
    use crate::program::{BasicBlock, Function};
    use crate::instruction::{Instruction, Operation};

    use crate::map;

    use super::*;

    #[test]
    fn test_function_macro() {
        let actual = function!("main" => "int", {
            "entry" => {
                "$ret 0"
            }
        });

        let expected = Function {
            name: "main".to_owned(),
            params: vec![],
            return_type: "int".try_into().unwrap(),
            basic_blocks: map![
                "entry".to_owned() => BasicBlock {
                    function: "main".to_owned(),
                    name: "entry".to_owned(),
                    instructions: vec![
                        Instruction::Ret("0".try_into().unwrap())
                    ]
                }
            ]
        };

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_function_macro_with_params() {
        let actual = function!("main", ("x:int", "y:int",) => "int", {
            "entry" => {
                "x:int = $arith add x:int y:int",
                "$ret x:int",
            },
        });

        let expected = Function {
            name: "main".to_owned(),
            params: vec![
                "x:int".try_into().unwrap(),
                "y:int".try_into().unwrap()
            ],
            return_type: "int".try_into().unwrap(),
            basic_blocks: map![
                "entry".to_owned() => BasicBlock {
                    function: "main".to_owned(),
                    name: "entry".to_owned(),
                    instructions: vec![
                        Instruction::Arith(
                            Operation::Add,
                            "x:int".try_into().unwrap(),
                            "x:int".try_into().unwrap(),
                            "y:int".try_into().unwrap()
                        ),
                        Instruction::Ret("x:int".try_into().unwrap())
                    ]
                }
            ]
        };

        assert_eq!(expected, actual);
    }
}
