mod block_code_model;
mod jit;
pub use block_code_model::*;
pub use jit::*;

#[macro_export]
macro_rules! arg_chk {
    ($args: expr, $count: expr, $name: literal) => {
        if $args.len() != $count {
            return Err(StackAction::panic_msg(format!(
                "{} called with wrong number of arguments",
                $name
            )));
        }
    };
}

#[macro_export]
macro_rules! wl_panic {
    ($str: literal) => {
        return Err(StackAction::panic_msg($str.to_string()));
    };
}
