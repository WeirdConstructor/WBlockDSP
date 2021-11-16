// Copyright (c) 2021 Weird Constructor <weirdconstructor@gmail.com>
// This file is a part of WBlockDSP. Released under GPL-3.0-or-later.
// See README.md and COPYING for details.

use tuix::*;
use tuix::widgets::*;
use wlambda::*;

mod painter;
mod rect;
mod block_code;
mod block_code_style;
mod wichtext;
mod block_code_editor;
mod wlapi;

use wichtext::*;

use block_code_style::BlockCodeStyle;

use wblockdsp::{BlockFun, BlockLanguage, BlockType, BlockASTNode, BlockUserInput};
use wblockdsp::wlapi::*;

use std::rc::Rc;
use std::cell::RefCell;

#[derive(Debug)]
pub struct ASTNode {
    pub id:    usize,
    pub typ:   String,
    pub lbl:   String,
    pub nodes: Vec<(String, String, ASTNodeRef)>,
}

#[derive(Debug, Clone)]
pub struct ASTNodeRef(Rc<RefCell<ASTNode>>);

impl ASTNodeRef {
    pub fn walk_dump(&self, input: &str, output: &str, indent: usize) -> String {
        let indent_str = "   ".repeat(indent + 1);

        let out_port =
            if output.len() > 0 { format!("(out: {})", output) }
            else { "".to_string() };
        let in_port =
            if input.len() > 0 { format!("(in: {})", input) }
            else { "".to_string() };

        let mut s = format!(
            "{}{}#{}[{}] {}{}\n",
            indent_str, self.0.borrow().id, self.0.borrow().typ,
            self.0.borrow().lbl, out_port, in_port);

        for (inp, out, n) in &self.0.borrow().nodes {
            s += &n.walk_dump(&inp, &out, indent + 1);
        }

        s
    }
}

impl BlockASTNode for ASTNodeRef {
    fn from(id: usize, typ: &str, lbl: &str) -> ASTNodeRef {
        ASTNodeRef(Rc::new(RefCell::new(ASTNode {
            id,
            typ:    typ.to_string(),
            lbl:    lbl.to_string(),
            nodes:  vec![],
        })))
    }

    fn add_node(&self, in_port: String, out_port: String, node: ASTNodeRef) {
        self.0.borrow_mut().nodes.push((in_port, out_port, node));
    }
}

fn vv2ast_node_ref(mut v: VVal) -> Option<ASTNodeRef> {
    v.with_usr_ref(|model: &mut ASTNodeRef| model.clone())
}

impl vval::VValUserData for ASTNodeRef {
    fn s(&self) -> String {
        format!("$<ASTNode; id={},typ={}>", self.0.borrow().id, self.0.borrow().typ)
    }

    fn set_key(&self, key: &VVal, val: VVal) -> Result<(), StackAction> {
        key.with_s_ref(|s| {
            match s {
                "id"  => { self.0.borrow_mut().id  = val.i() as usize; },
                "typ" => { self.0.borrow_mut().typ = val.s_raw(); },
                "lbl" => { self.0.borrow_mut().lbl = val.s_raw(); },
                "nodes" => {
                    let mut node = self.0.borrow_mut();

                    node.nodes.clear();
                    val.with_iter(|it| {
                        for (v, _key) in it {
                            if let Some(child) = vv2ast_node_ref(v.v_(2)) {
                                node.nodes.push((
                                    v.v_s_raw(0),
                                    v.v_s_raw(1),
                                    child,
                                ));
                            }
                        }
                    });
                },
                _ => { },
            }

            Ok(())
        })
    }

    fn get_key(&self, key: &str) -> Option<VVal> {
        match key {
            "id"  => Some(VVal::Int(self.0.borrow().id as i64)),
            "typ" => Some(VVal::new_str(&self.0.borrow().typ)),
            "lbl" => Some(VVal::new_str(&self.0.borrow().lbl)),
            "nodes" => {
                let v = VVal::vec();
                for (inp, out, noderef) in &self.0.borrow().nodes {
                    v.push(VVal::vec3(
                        VVal::new_str(inp),
                        VVal::new_str(out),
                        VVal::new_usr(noderef.clone())));
                }
                Some(v)
            },
            _ => None,
        }
    }

    fn call_method(&self, key: &str, env: &mut Env)
        -> Result<VVal, StackAction>
    {
        let args = env.argv_ref();

        match key {
            "dump" => {
                arg_chk!(args, 0, "ast_node_ref.dump[]");

                Ok(VVal::new_str_mv(self.walk_dump("", "", 0)))
            },
            _ => Ok(VVal::err_msg(&format!("Unknown method called: {}", key))),
        }
    }

    fn as_any(&mut self) -> &mut dyn std::any::Any { self }
    fn clone_ud(&self) -> Box<dyn vval::VValUserData> { Box::new(self.clone()) }
}


pub fn gen_code(code: &mut BlockFun) {
    let tree = code.generate_tree::<ASTNodeRef>("zero").unwrap();
    println!("{}", tree.walk_dump("", "", 0));
}

pub fn exec_queue(
    state: &mut State,
    queue: &Rc<RefCell<Vec<GUIMsg>>>,
    wichtext1: Entity,
) {
    let msgs = std::mem::replace(&mut *queue.borrow_mut(), vec![]);

    for msg in msgs {
        match msg {
            GUIMsg::SetText(wichtext_id, text) => {
                if wichtext_id == 0 {
                    state.insert_event(
                        Event::new(
                            WichTextMessage::SetText(text))
                        .target(wichtext1));
                }
            },
        }
    }
}

pub fn exec_cb(
    wl_ctx:     Rc<RefCell<EvalContext>>,
    callback:   VVal,
    args:       &[VVal]
) {
    if callback.is_none() {
        return;
    }

    match wl_ctx.borrow_mut().call(&callback, args) {
        Ok(_)  => {},
        Err(e) => { panic!("Error in callback: {:?}", e); }
    }
}

#[derive(Debug, Clone)]
pub enum GUIMsg {
    SetText(usize, String),
}

pub fn setup_bc_module(queue: Rc<RefCell<Vec<GUIMsg>>>)
    -> (wlambda::SymbolTable, Rc<RefCell<BlockLanguage>>, Rc<RefCell<BlockFun>>)
{
    let mut st = wlambda::SymbolTable::new();

    let vvlang = VValBlockLanguage::create();
    let vvfun  = VValBlockFun::create(vvlang.clone());

    set_modfun!(st, vvlang, lang, Some(0), Some(0), _env, _argc, {
        Ok(vvlang.clone())
    });

    set_modfun!(st, vvfun, fun, Some(0), Some(0), _env, _argc, {
        Ok(vvfun.clone())
    });

    set_modfun!(st, vvfun, generate_ast, Some(1), Some(1), env, _argc, {
        if let Some(fun) = vv2block_fun(env.arg(0)) {
            let tree = fun.borrow().generate_tree::<ASTNodeRef>("zero").unwrap();
            Ok(VVal::new_usr(tree))
        } else {
            Ok(VVal::None)
        }
    });

    set_modfun!(st, vvfun, new_ast_node, Some(3), Some(3), env, _argc, {
        let id  = env.arg(0).i() as usize;
        env.arg(1).with_s_ref(|typ|
            env.arg(2).with_s_ref(|lbl|
                Ok(VVal::new_usr::<ASTNodeRef>(BlockASTNode::from(id, typ, lbl)))))
    });

    set_modfun!(st, queue, wichtext_set, Some(2), Some(2), env, _argc, {
        queue.borrow_mut().push(
            GUIMsg::SetText(
                env.arg(0).i() as usize,
                env.arg(1).s_raw()));
        Ok(VVal::None)
    });

    (st, vv2block_code_language(vvlang).unwrap(), vv2block_fun(vvfun).unwrap())
}

pub fn main() {
    let queue = Rc::new(RefCell::new(vec![]));
    let (bc, lang, code) = setup_bc_module(queue.clone());

    lang.borrow_mut().define(BlockType {
        category:       "literals".to_string(),
        name:           "zero".to_string(),
        rows:           1,
        inputs:         vec![],
        outputs:        vec![Some("".to_string())],
        area_count:     0,
        user_input:     BlockUserInput::None,
        description:    "The 0.0 value".to_string(),
        color:          1,
    });

    lang.borrow_mut().define(BlockType {
        category:       "literals".to_string(),
        name:           "π".to_string(),
        rows:           1,
        inputs:         vec![],
        outputs:        vec![Some("".to_string())],
        area_count:     0,
        user_input:     BlockUserInput::None,
        description:    "The PI number".to_string(),
        color:          1,
    });

    lang.borrow_mut().define(BlockType {
        category:       "literals".to_string(),
        name:           "2π".to_string(),
        rows:           1,
        inputs:         vec![],
        outputs:        vec![Some("".to_string())],
        area_count:     0,
        user_input:     BlockUserInput::None,
        description:    "2 * PI == TAU".to_string(),
        color:          1,
    });

    lang.borrow_mut().define(BlockType {
        category:       "literals".to_string(),
        name:           "SR".to_string(),
        rows:           1,
        inputs:         vec![],
        outputs:        vec![Some("".to_string())],
        area_count:     0,
        user_input:     BlockUserInput::None,
        description:    "The sample rate".to_string(),
        color:          1,
    });

    lang.borrow_mut().define(BlockType {
        category:       "literals".to_string(),
        name:           "value".to_string(),
        rows:           1,
        inputs:         vec![],
        outputs:        vec![Some("".to_string())],
        area_count:     0,
        user_input:     BlockUserInput::Float,
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
        user_input:     BlockUserInput::None,
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
        user_input:     BlockUserInput::None,
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
        user_input:     BlockUserInput::None,
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
        user_input:     BlockUserInput::Identifier,
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
        user_input:     BlockUserInput::Identifier,
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
        user_input:     BlockUserInput::None,
        description:    "Divides the controlflow based on a true (>= 0.5) \
                         or false (< 0.5) input value.".to_string(),
        color:          0,
    });

    lang.borrow_mut().define(BlockType {
        category:       "nodes".to_string(),
        name:           "1pole".to_string(),
        rows:           2,
        inputs:         vec![Some("in".to_string()), Some("f".to_string())],
        outputs:        vec![Some("lp".to_string()), Some("hp".to_string())],
        area_count:     0,
        user_input:     BlockUserInput::None,
        description:    "Runs a simple one pole filter on the input".to_string(),
        color:          8,
    });

    lang.borrow_mut().define(BlockType {
        category:       "nodes".to_string(),
        name:           "svf".to_string(),
        rows:           3,
        inputs:         vec![Some("in".to_string()), Some("f".to_string()), Some("r".to_string())],
        outputs:        vec![Some("lp".to_string()), Some("bp".to_string()), Some("hp".to_string())],
        area_count:     0,
        user_input:     BlockUserInput::None,
        description:    "Runs a state variable filter on the input".to_string(),
        color:          8,
    });

    lang.borrow_mut().define(BlockType {
        category:       "functions".to_string(),
        name:           "sin".to_string(),
        rows:           1,
        inputs:         vec![Some("".to_string())],
        outputs:        vec![Some("".to_string())],
        area_count:     0,
        user_input:     BlockUserInput::None,
        description:    "Calculates the sine of the input".to_string(),
        color:          16,
    });

    lang.borrow_mut().define(BlockType {
        category:       "nodes".to_string(),
        name:           "delay".to_string(),
        rows:           2,
        inputs:         vec![Some("in".to_string()), Some("t".to_string())],
        outputs:        vec![Some("".to_string())],
        area_count:     0,
        user_input:     BlockUserInput::None,
        description:    "Runs a linearly interpolated delay on the input".to_string(),
        color:          8,
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
            user_input:     BlockUserInput::None,
            description:    "A binary arithmetics operation".to_string(),
            color:          4,
        });
    }

    lang.borrow_mut().define_identifier("alpha");
    lang.borrow_mut().define_identifier("beta");
    lang.borrow_mut().define_identifier("delta");
    lang.borrow_mut().define_identifier("gamma");
    lang.borrow_mut().define_identifier("&sig1");
    lang.borrow_mut().define_identifier("&sig2");

    let _ = code.borrow_mut().instanciate_at(0, 1, 1, "value", Some("2.32".to_string()));
    let _ = code.borrow_mut().instanciate_at(0, 2, 3, "value", Some("1.0".to_string()));
    let _ = code.borrow_mut().instanciate_at(0, 2, 2, "value", Some("-1.0".to_string()));
    let _ = code.borrow_mut().instanciate_at(0, 2, 1, "value", Some("0.5".to_string()));
    let _ = code.borrow_mut().instanciate_at(0, 3, 3, "+", None);
    let _ = code.borrow_mut().instanciate_at(0, 4, 3, "->3", None);
    let _ = code.borrow_mut().instanciate_at(0, 2, 6, "if", None);

    let global_env = wlambda::GlobalEnv::new_default();
    global_env.borrow_mut().set_module("bc", bc);

    let wl_ctx     = wlambda::EvalContext::new(global_env.clone());
    let wl_ctx     = Rc::new(RefCell::new(wl_ctx));

    match wl_ctx.borrow_mut().eval_file("wllib/main.wl") {
        Ok(_) => { },
        Err(e) => { panic!("Error in main.wl:\n{}", e); }
    }

    let init_fun =
        wl_ctx.borrow_mut().get_global_var("init")
           .expect("global 'init' function in main.wl defined");

    match wl_ctx.borrow_mut().call(&init_fun, &[]) {
        Ok(_) => {},
        Err(e) => { panic!("Error in main.wl 'init':\n{}", e); }
    }

    let vv_wt_click =
        wl_ctx.borrow_mut()
            .get_global_var("wichtext_click")
            .unwrap_or(VVal::None);

    let vv_bf_change =
        wl_ctx.borrow_mut()
            .get_global_var("block_fun_change")
            .unwrap_or(VVal::None);

    let vv_categories =
        wl_ctx.borrow_mut()
            .get_global_var("menu_categories")
            .unwrap_or(VVal::None);

    let mut categories : Vec<String> = vec![];
    vv_categories.with_iter(|it| {
        for (v, _key) in it {
            categories.push(v.s_raw());
        }
    });

    let app =
        Application::new(
            WindowDescription::new()
                .with_title("WBlockDSP GUI")
                .with_inner_size(900, 760),
            |state, window| {
                let style = BlockCodeStyle::new_default();

                let row = Row::new().build(state, window.entity(), |builder| builder);

                let wt_ref = Rc::new(RefCell::new(Entity::null()));

                let editor =
                    block_code_editor::BlockCodeEditor::new(
                        style.clone(),
                        categories.clone(),
                        lang.clone(),
                        code.clone())
                    .on_change({
                        let wl_ctx = wl_ctx.clone();
                        let queue  = queue.clone();
                        let wt_ref = wt_ref.clone();

                        move |state, ent, _fun| {
                            exec_cb(wl_ctx.clone(), vv_bf_change.clone(), &[]);
                            exec_queue(state, &queue, *wt_ref.borrow());
                        }
                    })
                    .build(state, row, |builder| builder);

                fn set_text(state: &mut State, wt: Entity, title_clr: usize) {
                    let mut text =
                        "foo[c2:Blabla[foo]]bar]and\n\
                         next line!\n     Some [c4:Other Rich]text format!!!\n\
                         and here a [ac10:Clicky Text!] [ac9:There Too!]\n\
                         |>    [c12:????][c17:Colors!] :-)".to_string();


                    text = format!("[c{}f30:Here is a Title]\n{}", title_clr, text);

                    text += "\n\nWichText also has Graphs:\n        ";
                    text += "[aw200h100gG1:Graph 1] [f20:=>] [c4C11w80h50gG2:Sin]\n";

                    text += "\nSet Title Colors:\n";
                    for clr in 1..=18 {
                        text += &format!("   * [ac{}:TitleClr={}]\n", clr, clr);
                    }

                    text += "[Lm:Middle ]And some active values: [c14f9ah20vF: F ][aw40h40vVOL:Volume][vDLY:Delay]\n";
                    text += "[Lt:Top    ]And some active values: [c14f9ah20vF: F ][aw40h40vVOL:Volume][vDLY:Delay]\n";
                    text += "[R:][Lb:Bottom ]And some active values: [c14f9ah20vF: F ][aw40h40vVOL:Volume][avDLY:Delay]\n";
                    text += "[c6aw100h100vBig:Big Knob\n";
                    text += "[R:]Lorem ipsum dolor sit amet, consetetur sadipscing[f9:]elitr, sed diam nonumy eirmod tempor invidunt ut labore [c4C11w30h20gG2:Sin] et[f20:]dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, [avDLY:Delay]no sea takimata sanctus est Lorem ipsum dolor sit amet.";

                    for l in 0..10 {
                        text += &format!("\nOn Line {}, with some clicky comand: [ac{}:{}]!",
                                l, l, l);
                    }

                    let mut sin_data = vec![];
                    for i in 0..128 {
                        sin_data.push(
                            (((i as f32 / 128.0) * std::f32::consts::TAU).sin() + 1.0) * 0.5);
                    }

                    state.insert_event(
                        Event::new(
                            WichTextMessage::SetDataSource(
                                "G2".to_string(),
                                Rc::new(sin_data)))
                        .target(wt));

                    state.insert_event(
                        Event::new(
                            WichTextMessage::SetDataSource(
                                "G1".to_string(),
                                Rc::new(vec![
                                    0.0_f32,
                                    1.0_f32,
                                    0.5_f32,
                                    0.2_f32,
                                    0.1_f32,
                                    0.5_f32,
                                    0.6_f32,
                                    0.8_f32,
                                    0.9_f32,
                                ])))
                        .target(wt));
                    state.insert_event(
                        Event::new(
                            WichTextMessage::SetText(text))
                        .target(wt));

                }

                let wt =
                    WichText::new(style)
                        .on_click({
                            let wl_ctx = wl_ctx.clone();
                            let queue  = queue.clone();

                            move |_wid, state, ent, line, frag, cmd| {
                                exec_cb(
                                    wl_ctx.clone(),
                                    vv_wt_click.clone(),
                                    &[
                                        VVal::new_str(cmd),
                                        VVal::Int(line as i64),
                                        VVal::Int(frag as i64),
                                    ]);
                                exec_queue(state, &queue, ent);
                            }
                        })
                        .build(state, row, |builder| { builder });

                (*wt_ref.borrow_mut()) = wt;

                set_text(state, wt, 0);

                exec_queue(state, &queue, wt);

                let values = Rc::new(RefCell::new(std::collections::HashMap::new()));
                state.insert_event(
                    Event::new(
                        WichTextMessage::SetValueSource(values))
                    .target(wt));


//                state.insert_event(
//                    Event::new(BlockCodeMessage::SetCode(code))
//                    .target(bc));
            });
    app.run();
}
