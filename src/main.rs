use wlambda::*;
mod jit;

use jit::*;
use core::mem;

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

    let mut jit = JIT::default();

    let ast =
        ASTNode::Assign(
            "&sig1".to_string(),
            Box::new(ASTNode::BinOp(
                ASTBinOp::Add,
                Box::new(ASTNode::If(
                    ASTIfOp::IsTrue(Box::new(ASTNode::Var("in2".to_string()))),
                    Box::new(ASTNode::Lit(66.12)),
                    Some(Box::new(ASTNode::Lit(99.12))))),
                Box::new(ASTNode::Var("in1".to_string())))));

    let af = ASTFun::new(Box::new(ast));

    let code_ptr = jit.compile(af).unwrap();
    let sig1 : Box<f64> = Box::new(1.1);
    let sig2 : Box<f64> = Box::new(1.1);
    let sig1 = Box::into_raw(sig1);
    let sig2 = Box::into_raw(sig2);

    unsafe {
        let code_fn =
            mem::transmute::<_, fn(f64, f64, *mut f64, *mut f64)>(code_ptr);
        code_fn(11.1, 0.1, sig1, sig2);
        println!("AFTER: {} : {}", *sig1, *sig2);
        code_fn(11.1, 0.5, sig1, sig2);
        println!("AFTER: {} : {}", *sig1, *sig2);

        Box::from_raw(sig1);
        Box::from_raw(sig2);
    }
}
