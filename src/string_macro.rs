#[macro_export]
macro_rules! s {
    ($string:expr) => {
        $string.to_owned()
    }
}
