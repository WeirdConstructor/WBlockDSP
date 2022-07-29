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
    let mut jit = JIT::new(get_default_library(), dsp_ctx.clone());

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

//    let tst_ptr = Box::into_raw(Box::new(TSTState { l: 44.53, srate: 0.0 }));
//    let tst2_ptr = Box::into_raw(Box::new(DSPState { x: 46.53, y: 1.0, srate: 0.0 }));
//    let mut states: Vec<*mut std::ffi::c_void> =
//        vec![tst2_ptr as *mut std::ffi::c_void, tst_ptr as *mut std::ffi::c_void];
//    let states_ptr: *mut *mut std::ffi::c_void = states.as_mut_ptr();
//    let ptr_b = unsafe {
//        mem::transmute::<
//            _,
//            fn(
//                f64,
//                f64,
//                f64,
//                f64,
//                f64,
//                f64,
//                *mut f64,
//                *mut f64,
//                *mut DSPState,
//                *mut *mut std::ffi::c_void,
//            ) -> f64,
//        >(code)
//    };

    code.init(44100.0);

    unsafe {
        code.with_dsp_state(|state| {
            (*state).x = 11.0;
            (*state).y = 1.0;
        });
        code.with_node_state(1, |state: *mut TSTState| {
            (*state).l = 44.53;
        }).expect("node state exists");
    };
    let mut s1 = 0.0;
    let mut s2 = 0.0;
    let res1 = code.exec(1.0, 0.0, 3.0, 4.0, 5.0, 6.0, &mut s1, &mut s2);
    assert_float_eq!(res1, 100.12);
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
//
//#[test]
//fn check_jit_stmts() {
//    let mut jit = JIT::new(get_default_library(), DSPNodeContext::new_ref());
//
//    let ast = ASTNode::Stmts(vec![
//        Box::new(ASTNode::Assign("&sig1".to_string(), Box::new(ASTNode::Var("in2".to_string())))),
//        Box::new(ASTNode::Assign("&sig2".to_string(), Box::new(ASTNode::Var("in1".to_string())))),
//    ]);
//
//    let fun = ASTFun::new(Box::new(ast));
//    let code = jit.compile(fun).unwrap();
//    let ptr_b = unsafe {
//        mem::transmute::<
//            _,
//            fn(
//                f64,
//                f64,
//                f64,
//                f64,
//                f64,
//                f64,
//                *mut f64,
//                *mut f64,
//                *mut DSPState,
//                *mut *mut std::ffi::c_void,
//            ) -> f64,
//        >(code)
//    };
//
//    let mut s1 = 0.0;
//    let mut s2 = 0.0;
//    let mut state = DSPState { x: 0.0, y: 1.0, srate: 0.0 };
//    let res1 = ptr_b(
//        1.1,
//        2.2,
//        3.0,
//        4.0,
//        5.0,
//        6.0,
//        &mut s1,
//        &mut s2,
//        &mut state,
//        std::ptr::null_mut() as *mut *mut std::ffi::c_void,
//    );
//    assert_float_eq!(res1, 1.1);
//    assert_float_eq!(s1, 2.2);
//    assert_float_eq!(s2, 1.1);
//}
//
//fn run_ast(ast: Box<ASTNode>, in1: f64, in2: f64) -> (f64, f64, f64) {
//    let mut jit = JIT::new(get_default_library(), DSPNodeContext::new_ref());
//    let fun = ASTFun::new(ast);
//
//    let code = jit.compile(fun).unwrap();
//    let ptr_b = unsafe {
//        mem::transmute::<
//            _,
//            fn(
//                f64,
//                f64,
//                f64,
//                f64,
//                f64,
//                f64,
//                *mut f64,
//                *mut f64,
//                *mut DSPState,
//                *mut *mut std::ffi::c_void,
//            ) -> f64,
//        >(code)
//    };
//
//    let mut s1 = 0.0;
//    let mut s2 = 0.0;
//    let mut state = DSPState { x: 0.0, y: 1.0, srate: 0.0 };
//    let res1 = ptr_b(
//        in1,
//        in2,
//        3.0,
//        4.0,
//        5.0,
//        6.0,
//        &mut s1,
//        &mut s2,
//        &mut state,
//        std::ptr::null_mut() as *mut *mut std::ffi::c_void,
//    );
//    (s1, s2, res1)
//}
//
//#[test]
//fn check_jit_sin() {
//    let mut jit = JIT::new(get_default_library(), DSPNodeContext::new_ref());
//
//    let ast = ASTNode::Call("sin".to_string(), 0, vec![Box::new(ASTNode::Lit(0.5 * 3.14))]);
//
//    let fun = ASTFun::new(Box::new(ast));
//    let code = jit.compile(fun).unwrap();
//    let ptr_b = unsafe {
//        mem::transmute::<
//            _,
//            fn(
//                f64,
//                f64,
//                f64,
//                f64,
//                f64,
//                f64,
//                *mut f64,
//                *mut f64,
//                *mut DSPState,
//                *mut *mut std::ffi::c_void,
//            ) -> f64,
//        >(code)
//    };
//
//    let mut s1 = 0.0;
//    let mut s2 = 0.0;
//    let mut state = DSPState { x: 0.0, y: 1.0, srate: 0.0 };
//    let res1 = ptr_b(
//        1.1,
//        2.2,
//        3.0,
//        4.0,
//        5.0,
//        6.0,
//        &mut s1,
//        &mut s2,
//        &mut state,
//        std::ptr::null_mut() as *mut *mut std::ffi::c_void,
//    );
//    assert_float_eq!(res1, 1.0);
//}
//
//#[test]
//fn check_jit_wlambda() {
//    let global_env = wlambda::GlobalEnv::new_default();
//    global_env.borrow_mut().set_module("jit", wlapi::setup_jit_module());
//    let mut ctx = wlambda::compiler::EvalContext::new(global_env);
//
//    let ast: VVal = ctx
//        .eval(
//            r#"
//            !@import jit;
//            !n = jit:node $[:if,
//                $[:binop, :gt, "in1", 10],
//                1.2,
//                "in2"
//            ];
//            std:displayln "AAAAA" n.dump[];
//            n
//        "#,
//        )
//        .unwrap();
//    assert_eq!(ast.s(), "$<JIT::ASTNode:if>");
//    let ret = run_ast(vv2ast_node(ast.clone()).unwrap(), 20.21, 3.4);
//    assert_float_eq!(ret.2, 1.2);
//
//    let ret = run_ast(vv2ast_node(ast.clone()).unwrap(), 2.21, 3.4);
//    assert_float_eq!(ret.2, 3.4);
//}
