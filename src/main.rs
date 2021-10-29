use wlambda::*;
mod jit;

fn main() {
    let global_env = wlambda::GlobalEnv::new_default();
    global_env.borrow_mut().add_func(
        "my_crazy_add",
        |env: &mut Env, _argc: usize| {
            Ok(VVal::Int(
                  env.arg(0).i() * 11
                + env.arg(1).i() * 13
            ))
        }, Some(2), Some(2));

    let mut ctx = wlambda::compiler::EvalContext::new(global_env);

    let res_add : VVal = ctx.eval("my_crazy_add 2 4").unwrap();
    assert_eq!(res_add.i(), 74);
}
