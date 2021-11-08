use wblockdsp::{BlockLanguage, BlockFun, BlockType, BlockASTNode};

use std::rc::Rc;
use std::cell::RefCell;

#[derive(Debug)]
pub struct DebugASTNode {
    pub typ:   String,
    pub lbl:   String,
    pub nodes: Vec<(String, DebugASTNodeRef)>,
}

#[derive(Debug, Clone)]
pub struct DebugASTNodeRef(Rc<RefCell<DebugASTNode>>);

impl DebugASTNodeRef {
    pub fn walk_dump(&self, output: &str) -> String {
        let mut s =
            if self.0.borrow().lbl.len() == 0 {
                if output.len() > 0 {
                    format!("{}(out:{})", self.0.borrow().typ, output)
                } else {
                    format!("{}", self.0.borrow().typ)
                }
            } else {
                if output.len() > 0 {
                    format!(
                        "{}:{}(out:{})",
                        self.0.borrow().typ, self.0.borrow().lbl,
                        output)
                } else {
                    format!("{}:{}", self.0.borrow().typ, self.0.borrow().lbl)
                }
            };

        let mut first = true;
        for (out, n) in &self.0.borrow().nodes {
            if first {
                s += "[";
            } else {
                s += ",";
            }
            first = false;

            s += &n.walk_dump(&out);
        }

        if !first {
            s += "]";
        }

        s
    }
}

impl BlockASTNode for DebugASTNodeRef {
    fn from(typ: &str, lbl: &str) -> DebugASTNodeRef {
        DebugASTNodeRef(Rc::new(RefCell::new(DebugASTNode {
            typ:    typ.to_string(),
            lbl:    lbl.to_string(),
            nodes:  vec![],
        })))
    }

    fn add_node(&self, out_port: String, node: DebugASTNodeRef) {
        self.0.borrow_mut().nodes.push((out_port, node));
    }
}

pub fn gen_code(code: &mut BlockFun) -> String {
    let mut tree = code.generate_tree::<DebugASTNodeRef>("zero").unwrap();
    tree.walk_dump("")
}

fn prepare(blocks: &[(usize, i64, i64, &str, Option<&str>)]) -> Rc<RefCell<BlockFun>> {
    let lang = Rc::new(RefCell::new(BlockLanguage::new()));
    let code = Rc::new(RefCell::new(BlockFun::new(lang.clone())));

    lang.borrow_mut().define(BlockType {
        category:       "literals".to_string(),
        name:           "zero".to_string(),
        rows:           1,
        inputs:         vec![],
        outputs:        vec![Some("".to_string())],
        area_count:     0,
        user_input:     false,
        description:    "The 0.0 value".to_string(),
        color:          1,
    });

    lang.borrow_mut().define(BlockType {
        category:       "literals".to_string(),
        name:           "number".to_string(),
        rows:           1,
        inputs:         vec![],
        outputs:        vec![Some("".to_string())],
        area_count:     0,
        user_input:     true,
        description:    "A literal value, typed in by the user.".to_string(),
        color:          1,
    });

    lang.borrow_mut().define(BlockType {
        category:       "routing".to_string(),
        name:           "->".to_string(),
        rows:           1,
        inputs:         vec![Some("".to_string())],
        outputs:        vec![Some("".to_string())],
        area_count:     0,
        user_input:     false,
        description:    "Forwards the value one block".to_string(),
        color:          6,
    });

    lang.borrow_mut().define(BlockType {
        category:       "routing".to_string(),
        name:           "->2".to_string(),
        rows:           2,
        inputs:         vec![Some("".to_string())],
        outputs:        vec![Some("".to_string()), Some("".to_string())],
        area_count:     0,
        user_input:     false,
        description:    "Forwards the value one block and sends it to multiple destinations".to_string(),
        color:          6,
    });

    lang.borrow_mut().define(BlockType {
        category:       "routing".to_string(),
        name:           "->3".to_string(),
        rows:           3,
        inputs:         vec![Some("".to_string())],
        outputs:        vec![Some("".to_string()), Some("".to_string()), Some("".to_string())],
        area_count:     0,
        user_input:     false,
        description:    "Forwards the value one block and sends it to multiple destinations".to_string(),
        color:          6,
    });

    lang.borrow_mut().define(BlockType {
        category:       "variables".to_string(),
        name:           "set".to_string(),
        rows:           1,
        inputs:         vec![Some("".to_string())],
        outputs:        vec![],
        area_count:     0,
        user_input:     true,
        description:    "Stores into a variable".to_string(),
        color:          2,
    });

    lang.borrow_mut().define(BlockType {
        category:       "variables".to_string(),
        name:           "get".to_string(),
        rows:           1,
        inputs:         vec![],
        outputs:        vec![Some("".to_string())],
        area_count:     0,
        user_input:     true,
        description:    "Loads a variable".to_string(),
        color:          12,
    });

    lang.borrow_mut().define(BlockType {
        category:       "variables".to_string(),
        name:           "if".to_string(),
        rows:           1,
        inputs:         vec![Some("".to_string())],
        outputs:        vec![Some("".to_string())],
        area_count:     2,
        user_input:     false,
        description:    "Divides the controlflow based on a true (>= 0.5) \
                         or false (< 0.5) input value.".to_string(),
        color:          0,
    });

    lang.borrow_mut().define(BlockType {
        category:       "functions".to_string(),
        name:           "sin".to_string(),
        rows:           1,
        inputs:         vec![Some("".to_string())],
        outputs:        vec![Some("".to_string())],
        area_count:     0,
        user_input:     false,
        description:    "Calculates the sine of the input".to_string(),
        color:          16,
    });

    for fun_name in &["+", "-", "*", "/"] {
        lang.borrow_mut().define(BlockType {
            category:       "arithmetics".to_string(),
            name:           fun_name.to_string(),
            rows:           2,
            inputs:
                if fun_name == &"-" || fun_name == &"/" {
                    vec![Some("a".to_string()), Some("b".to_string())]
                } else {
                    vec![Some("".to_string()), Some("".to_string())]
                },
            outputs:        vec![Some("".to_string())],
            area_count:     0,
            user_input:     false,
            description:    "A binary arithmetics operation".to_string(),
            color:          4,
        });
    }

//    code.borrow_mut().instanciate_at(0, 1, 1, "number", Some("2.32".to_string()));
//    code.borrow_mut().instanciate_at(0, 2, 3, "number", Some("1.0".to_string()));
//    code.borrow_mut().instanciate_at(0, 2, 2, "number", Some("-1.0".to_string()));
//    code.borrow_mut().instanciate_at(0, 2, 1, "number", Some("0.5".to_string()));
//    code.borrow_mut().instanciate_at(0, 3, 3, "+", None);
//    code.borrow_mut().instanciate_at(0, 4, 3, "->3", None);
//    code.borrow_mut().instanciate_at(0, 2, 6, "if", None);

    for (id, x, y, name, lbl) in blocks {
        code.borrow_mut().instanciate_at(
            *id, *x, *y, name, lbl.map(|s| s.to_string()));
    }
    code.borrow_mut().recalculate_area_sizes();
    code
}

fn gen(blocks: &[(usize, i64, i64, &str, Option<&str>)]) -> String {
    let code = prepare(blocks);
    let mut bcode = code.borrow_mut();
    gen_code(&mut bcode)
}

#[test]
fn check_simple_out() {
    assert_eq!(
        gen(&[
            (0, 2, 2, "number", Some("-1.0")),
            (0, 2, 3, "number", Some("1.0")),
            (0, 3, 3, "number", Some("0.5")),
        ]),
        "<r>[<a>[<res>[number:-1.0],<res>[number:1.0],<res>[number:0.5]]]");
}

#[test]
fn check_multi_out() {
    assert_eq!(
        gen(&[ (0, 4, 3, "->3", None) ]),
        "<r>[<a>[\
            <res>[->3:->3[zero]],\
            <res>[->3:->3[zero]],\
            <res>[->3:->3[zero]]]]");
}

#[test]
fn check_unconnected() {
    assert_eq!(
        gen(&[
            (0, 3, 3, "+", None),
            (0, 4, 3, "->", None),
        ]),
        "<r>[<a>[<res>[->:->[+:+[zero,zero]]]]]");

    assert_eq!(
        gen(&[
            (0, 3, 3, "+", None),
            (0, 2, 4, "number", Some("0.1")),
        ]),
        "<r>[<a>[<res>[+:+[zero,number:0.1]]]]");

    assert_eq!(
        gen(&[
            (0, 3, 3, "+", None),
            (0, 2, 3, "number", Some("0.1")),
        ]),
        "<r>[<a>[<res>[+:+[number:0.1,zero]]]]");
}

#[test]
fn check_set() {
    assert_eq!(
        gen(&[ (0, 2, 2, "set", Some("x"))]),
        "<r>[<a>[set:x[zero]]]");

    assert_eq!(
        gen(&[
            (0, 1, 2, "number", Some("0.6")),
            (0, 2, 2, "set", Some("x")),
        ]),
        "<r>[<a>[set:x[number:0.6]]]");

    assert_eq!(
        gen(&[
            (0, 1, 2, "number", Some("0.6")),
            (0, 2, 2, "set", Some("x")),
            (0, 0, 2, "set", Some("y")),
            (0, 3, 2, "set", Some("z")),
        ]),
        "<r>[<a>[set:y[zero],set:x[number:0.6],set:z[zero]]]");
}

#[test]
fn check_named_inputs() {
    assert_eq!(
        gen(&[
            (0, 2, 3, "number", Some("0.4")),
            (0, 3, 3, "-", None),
            (0, 4, 3, "set", Some("o")),
        ]),
        "<r>[<a>[set:o[-:-[number:0.4@a,zero@b]]]]");
}

#[test]
fn check_if() {
    assert_eq!(
        gen(&[
            (0, 3, 3, "if", None),
        ]),
        "<r>[<a>[<res>[if:if[zero,<a>,<a>]]]]");

    assert_eq!(
        gen(&[
            (0, 3, 3, "if", None),
            (1, 1, 1, "number", Some("0.3")),
        ]),
        "<r>[<a>[<res>[if:if[zero,<a>[<res>[number:0.3]],<a>]]]]");

    assert_eq!(
        gen(&[
            (0, 3, 3, "if", None),
            (2, 1, 1, "number", Some("0.4")),
        ]),
        "<r>[<a>[<res>[if:if[zero,<a>,<a>[<res>[number:0.4]]]]]]");

    assert_eq!(
        gen(&[
            (0, 3, 3, "if", None),
            (1, 1, 1, "number", Some("0.3")),
            (2, 1, 1, "number", Some("-0.2")),
        ]),
        "<r>[<a>[<res>[if:if[zero,<a>[<res>[number:0.3]],<a>[<res>[number:-0.2]]]]]]");

    assert_eq!(
        gen(&[
            (0, 3, 3, "if", None),
            (1, 1, 1, "number", Some("0.3")),
            (1, 2, 1, "set", Some("y")),
        ]),
        "<r>[<a>[<res>[if:if[zero,<a>[set:y[number:0.3]],<a>]]]]");

    assert_eq!(
        gen(&[
            (0, 1, 3, "number", Some("33.0")),
            (0, 2, 3, "sin", None),
            (0, 3, 3, "if", None),
            (0, 4, 3, "set", Some("&sig")),
            (1, 1, 1, "number", Some("0.3")),
            (1, 2, 1, "set", Some("y")),
        ]),
        "<r>[<a>[set:&sig[if:if[sin:sin[number:33.0],<a>[set:y[number:0.3]],<a>]]]]");
}
