use std::mem;
use wblockdsp::wlapi::vv2ast_node;
use wblockdsp::*;
use wlambda::*;

#[macro_export]
macro_rules! assert_float_eq {
    ($a:expr, $b:expr) => {
        if ($a - $b).abs() > 0.0001 {
            panic!(
                r#"assertion failed: `(left == right)`
  left: `{:?}`,
 right: `{:?}`"#,
                $a, $b
            )
        }
    };
}

#[test]
fn check_jit() {
    let dsp_ctx = DSPNodeContext::new_ref();
    let jit = JIT::new(get_default_library(), dsp_ctx.clone());

    let ast = ASTNode::Assign(
        "&sig1".to_string(),
        Box::new(ASTNode::BinOp(
            ASTBinOp::Add,
            Box::new(ASTNode::If(
                Box::new(ASTNode::Var("in2".to_string())),
                Box::new(ASTNode::Call("test".to_string(), 1, vec![Box::new(ASTNode::Lit(11.0))])),
                Some(Box::new(ASTNode::Lit(99.12))),
            )),
            Box::new(ASTNode::Var("in1".to_string())),
        )),
    );

    let fun = ASTFun::new(Box::new(ast));
    let mut code = jit.compile(fun).unwrap();

    code.init(44100.0);

    unsafe {
        code.with_dsp_state(|state| {
            (*state).x = 11.0;
            (*state).y = 1.0;
        });
        code.with_node_state(1, |state: *mut TSTState| {
            (*state).l = 44.53;
        })
        .expect("node state exists");
    };
    let mut s1 = 0.0;
    let mut s2 = 0.0;
    let res = code.exec(1.0, 0.0, 3.0, 4.0, 5.0, 6.0, &mut s1, &mut s2);
    assert_float_eq!(res, 100.12);
    unsafe {
        code.with_dsp_state(|state| {
            assert_float_eq!((*state).x, 11.0);
        });
    }

    let res2 = code.exec(22.0, 1.0, 3.0, 4.0, 5.0, 6.0, &mut s1, &mut s2);
    assert_float_eq!(res2, 11.0 * 10000.0 + 1.0 + 22.0);
    unsafe {
        code.with_dsp_state(|state| {
            assert_float_eq!((*state).x, 11.0 * 22.0);
            assert_float_eq!((*state).y, 44.53);
        });
    }

    dsp_ctx.borrow_mut().free();
}

#[test]
fn check_jit_stmts() {
    let dsp_ctx = DSPNodeContext::new_ref();
    let jit = JIT::new(get_default_library(), dsp_ctx.clone());

    use wblockdsp::build::*;

    let fun = fun(stmts(&[assign("&sig1", var("in2")), assign("&sig2", var("in1"))]));

    let mut code = jit.compile(fun).unwrap();
    code.init(44100.0);

    let (s1, s2, res) = code.exec_2in_2out(1.1, 2.2);
    assert_float_eq!(res, 1.1);
    assert_float_eq!(s1, 2.2);
    assert_float_eq!(s2, 1.1);

    dsp_ctx.borrow_mut().free();
}

#[test]
fn check_jit_thread_stmts() {
    let dsp_ctx = DSPNodeContext::new_ref();
    let jit = JIT::new(get_default_library(), dsp_ctx.clone());
    use wblockdsp::build::*;

    let fun = fun(stmts(&[
        assign("&sig1", call("sin", 0, &[var("in2")])),
        assign("&sig2", op_add(literal(23.0), var("in1")))
    ]));

    let (tx, rx) = std::sync::mpsc::channel();

    let mut code = jit.compile(fun).unwrap();

    std::thread::spawn(move || {
        code.init(44100.0);
        let (s1, s2, res) = code.exec_2in_2out(1.1, 2.2);
        tx.send((s1, s2, res));
    })
    .join();

    let (s1, s2, res) = rx.recv().unwrap();
    assert_float_eq!(res, 24.1);
    assert_float_eq!(s1, 0.80849);
    assert_float_eq!(s2, 24.1);

    dsp_ctx.borrow_mut().free();
}

#[test]
fn check_jit_sin() {
    let ctx = DSPNodeContext::new_ref();
    let jit = JIT::new(get_default_library(), ctx.clone());

    let ast = ASTNode::Call("sin".to_string(), 0, vec![Box::new(ASTNode::Lit(0.5 * 3.14))]);
    let fun = ASTFun::new(Box::new(ast));
    let mut code = jit.compile(fun).unwrap();
    let mut s1 = 0.0;
    let mut s2 = 0.0;
    let res = code.exec(1.1, 2.2, 3.0, 4.0, 5.0, 6.0, &mut s1, &mut s2);
    assert_float_eq!(res, 1.0);

    ctx.borrow_mut().free();
}

#[test]
fn check_jit_wlambda() {
    let global_env = wlambda::GlobalEnv::new_default();
    global_env.borrow_mut().set_module("jit", wlapi::setup_jit_module());
    let mut ctx = wlambda::compiler::EvalContext::new(global_env);

    let ast: VVal = ctx
        .eval(
            r#"
            !@import jit;
            !n = jit:node
                $[:if,
                    $[:binop, :gt, "in1", 10],
                    1.2,
                    "in2"
                ];
            std:displayln "AAAAA" n.dump[];
            n
        "#,
        )
        .unwrap();

    let ctx = DSPNodeContext::new_ref();
    let jit = JIT::new(get_default_library(), ctx.clone());

    assert_eq!(ast.s(), "$<JIT::ASTNode:if>");

    let mut code = jit.compile(ASTFun::new(vv2ast_node(ast).unwrap())).unwrap();

    let mut s1 = 0.0;
    let mut s2 = 0.0;
    let ret = code.exec(20.21, 3.4, 3.0, 4.0, 5.0, 6.0, &mut s1, &mut s2);
    assert_float_eq!(ret, 1.2);

    let ret = code.exec(2.21, 3.4, 3.0, 4.0, 5.0, 6.0, &mut s1, &mut s2);
    assert_float_eq!(ret, 3.4);

    ctx.borrow_mut().free();
}

#[test]
fn check_jit_sin_wlambda() {
    let global_env = wlambda::GlobalEnv::new_default();
    global_env.borrow_mut().set_module("jit", wlapi::setup_jit_module());
    let mut ctx = wlambda::compiler::EvalContext::new(global_env);

    let ast: VVal = ctx
        .eval(
            r#"
            !@import jit;
            jit:node $[:call, "sin", 0, "in1"];
        "#,
        )
        .unwrap();

    let ctx = DSPNodeContext::new_ref();
    let jit = JIT::new(get_default_library(), ctx.clone());

    assert_eq!(ast.s(), "$<JIT::ASTNode:call0:sin>");

    let mut code = jit.compile(ASTFun::new(vv2ast_node(ast).unwrap())).unwrap();

    let mut s1 = 0.0;
    let mut s2 = 0.0;
    use std::time::{Duration, Instant};

    let now = Instant::now();
    let mut sum1 = 0.0;
    for _i in 0..10000000 {
        let ret = code.exec(2.21, 3.4, 3.0, 4.0, 5.0, 6.0, &mut s1, &mut s2);
        sum1 += ret;
    }
    println!("SUM JIT: {} time: {}", sum1, now.elapsed().as_millis());

    let now = Instant::now();
    let mut sum2 = 0.0;
    for _i in 0..10000000 {
        let ret = (2.21_f64).sin();
        sum2 += ret;
    }
    println!("SUM RST: {} time: {}", sum2, now.elapsed().as_millis());
    ctx.borrow_mut().free();

    assert_float_eq!(sum1, sum2);
}
