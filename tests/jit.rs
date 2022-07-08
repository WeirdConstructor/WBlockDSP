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
        mem::transmute::<_, fn(f64, f64, f64, f64, f64, f64, *mut f64, *mut f64, *mut DSPState) -> f64>(code)
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
