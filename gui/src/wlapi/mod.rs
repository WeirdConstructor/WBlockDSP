#[macro_export]
macro_rules! set_modfun {
    ($st: expr, $ref: ident, $fun: tt, $min: expr, $max: expr, $env: ident, $argc: ident, $b: block) => {
        {
            let $ref = $ref.clone();
            $st.fun(
                &stringify!($fun),
                move |$env: &mut Env, $argc: usize| $b, $min, $max, false);
        }
    }
}

#[macro_export]
macro_rules! arg_chk {
    ($args: expr, $count: expr, $name: literal) => {
        if $args.len() != $count {
            return Err(StackAction::panic_msg(format!(
                "{} called with wrong number of arguments",
                $name)));
        }
    }
}

#[macro_export]
macro_rules! wl_panic {
    ($str: literal) => {
        return Err(StackAction::panic_msg($str.to_string()));
    }
}

