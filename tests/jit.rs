use wlambda::*;
use wblockdsp::*;
use std::mem;

#[macro_export]
macro_rules! assert_float_eq {
    ($a:expr, $b:expr) => {
        if ($a - $b).abs() > 0.0001 {
            panic!(r#"assertion failed: `(left == right)`
  left: `{:?}`,
 right: `{:?}`"#, $a, $b)
        }
    }
}

#[test]
fn check_jit() {
    let mut jit = JIT::default();

    let ast =
        ASTNode::Assign(
            "&sig1".to_string(),
            Box::new(ASTNode::BinOp(
                ASTBinOp::Add,
                Box::new(ASTNode::If(
                    Box::new(ASTNode::Var("in2".to_string())),
                    Box::new(ASTNode::Call(
                        "test".to_string(),
                        Box::new(ASTNode::Lit(11.0)))),
                    Some(Box::new(ASTNode::Lit(99.12))))),
                Box::new(ASTNode::Var("in1".to_string())))));

    let fun = ASTFun::new(Box::new(ast));
    let code = jit.compile(fun).unwrap();
    let ptr_b = unsafe {
        mem::transmute::<_, fn(
            f64, f64, f64, f64, f64, f64,
            *mut f64, *mut f64, *mut DSPState) -> f64>(code)
    };

    let mut s1 = 0.0;
    let mut s2 = 0.0;
    let mut state = DSPState { x: 11.0 };
    let res1 = ptr_b(1.0, 0.0, 3.0, 4.0, 5.0, 6.0, &mut s1, &mut s2, &mut state);
    assert_float_eq!(res1, 100.12);
    assert_float_eq!(state.x, 11.0);

    let res2 = ptr_b(22.0, 1.0, 3.0, 4.0, 5.0, 6.0, &mut s1, &mut s2, &mut state);
    assert_float_eq!(res2, 11.0 * 10000.0 + 1.0 + 22.0);
    assert_float_eq!(state.x, 11.0 * 22.0);
}

#[test]
fn check_jit_wlambda() {
    let global_env = wlambda::GlobalEnv::new_default();
    global_env.borrow_mut().set_module("jit", wlapi::setup_jit_module());
    let mut ctx = wlambda::compiler::EvalContext::new(global_env);
/*
!fconst1 = jit:node 10.0;
!var = jit:node "var";
!assign = jit:node $[:assign, var, fconst1];
!assign2 = jit:node $[:assign, var, 10.0];
*/

    let res_add : VVal = ctx.eval("!@import jit; jit:node 10").unwrap();
    assert_eq!(res_add.s(), "$<JIT::ASTNode:\"lit:10.0000\">");
}
