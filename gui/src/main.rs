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

use wichtext::*;

use block_code_style::BlockCodeStyle;

use wblockdsp::{BlockFun, BlockLanguage, BlockType, BlockASTNode};
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
    pub fn walk_dump(&self, input: &str, output: &str, indent: usize) {
        let indent_str = "   ".repeat(indent + 1);

        let out_port =
            if output.len() > 0 { format!("(out: {})", output) }
            else { "".to_string() };
        let in_port =
            if input.len() > 0 { format!("(in: {})", input) }
            else { "".to_string() };

        println!(
            "{}{}#{}[{}] {}{}",
            indent_str, self.0.borrow().id, self.0.borrow().typ,
            self.0.borrow().lbl, out_port, in_port);

        for (inp, out, n) in &self.0.borrow().nodes {
            n.walk_dump(&inp, &out, indent + 1);
        }
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

pub fn gen_code(code: &mut BlockFun) {
    let tree = code.generate_tree::<ASTNodeRef>("zero").unwrap();
    tree.walk_dump("", "", 0);
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

pub fn main() {
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
        category:       "nodes".to_string(),
        name:           "1pole".to_string(),
        rows:           2,
        inputs:         vec![Some("in".to_string()), Some("f".to_string())],
        outputs:        vec![Some("lp".to_string()), Some("hp".to_string())],
        area_count:     0,
        user_input:     false,
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
        user_input:     false,
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
        user_input:     false,
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
        user_input:     false,
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
            user_input:     false,
            description:    "A binary arithmetics operation".to_string(),
            color:          4,
        });
    }

    let _ = code.borrow_mut().instanciate_at(0, 1, 1, "number", Some("2.32".to_string()));
    let _ = code.borrow_mut().instanciate_at(0, 2, 3, "number", Some("1.0".to_string()));
    let _ = code.borrow_mut().instanciate_at(0, 2, 2, "number", Some("-1.0".to_string()));
    let _ = code.borrow_mut().instanciate_at(0, 2, 1, "number", Some("0.5".to_string()));
    let _ = code.borrow_mut().instanciate_at(0, 3, 3, "+", None);
    let _ = code.borrow_mut().instanciate_at(0, 4, 3, "->3", None);
    let _ = code.borrow_mut().instanciate_at(0, 2, 6, "if", None);

    // TODO: Subroutines:
    //  - make predefined subroutines with nargs=0..4 and one output.
    //      - inside subroutines you can use arg1, arg2, arg3, ... to refer
    //        to the subroutine arguments
    //      - inside subroutine you specify the return value the same way
    //        you do with "if".
    //      - there are no scoped variables!
    //  - you can add a name to the subroutine.
    //  - when cloning a subroutine, a "call" is created.
    //    the label will contain the subroutine name.

    // let vizia_st = setup_vizia_module(gui_rec.clone());
    // global_env.borrow_mut().set_module("vizia", vizia_st);

    let global_env = wlambda::GlobalEnv::new_default();
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

    let app =
        Application::new(
            WindowDescription::new()
                .with_title("WBlockDSP GUI")
                .with_inner_size(900, 760),
            |state, window| {
                let style = BlockCodeStyle::new_default();

                let row = Row::new().build(state, window.entity(), |builder| builder);

                let editor =
                    block_code_editor::BlockCodeEditor::new(
                        style.clone(), lang.clone(), code.clone())
                    .build(state, row, |builder| builder);

//                let col = Column::new().build(state, row, |builder| builder);
//                let pop = Popup::new().build(state, window.entity(), |builder| {
//                    builder
//                        .set_width(Pixels(100.0))
//                        .set_height(Pixels(430.0))
//                });
//
//                let pop_col = Column::new().build(state, pop, |builder| builder);
//
//                let current_pos =
//                    Rc::new(RefCell::new(BlockPos::Cell { id: 0, x: 0, y: 0 }));
//
//                spawn_button(state, pop_col, pop, "+", current_pos.clone(), {
//                    let code = code.clone();
//                        move |_state, pos| {
//                        let (id, x, y) = pos.pos();
//                        let _ = code.borrow_mut()
//                            .instanciate_at(id, x, y, "+", None);
//                        code.borrow_mut()
//                            .recalculate_area_sizes();
//
//                        gen_code(&mut code.borrow_mut());
//                    }});
//                spawn_button(state, pop_col, pop, "-", current_pos.clone(), {
//                    let code = code.clone();
//                        move |_state, pos| {
//                        let (id, x, y) = pos.pos();
//                        let _ = code.borrow_mut()
//                            .instanciate_at(id, x, y, "-", None);
//                        code.borrow_mut()
//                            .recalculate_area_sizes();
//
//                        gen_code(&mut code.borrow_mut());
//                    }});
//                spawn_button(state, pop_col, pop, "*", current_pos.clone(), {
//                    let code = code.clone();
//                        move |_state, pos| {
//                        let (id, x, y) = pos.pos();
//                        let _ = code.borrow_mut()
//                            .instanciate_at(id, x, y, "*", None);
//                        code.borrow_mut()
//                            .recalculate_area_sizes();
//
//                        gen_code(&mut code.borrow_mut());
//                    }});
//                spawn_button(state, pop_col, pop, "/", current_pos.clone(), {
//                    let code = code.clone();
//                        move |_state, pos| {
//                        let (id, x, y) = pos.pos();
//                        let _ = code.borrow_mut()
//                            .instanciate_at(id, x, y, "/", None);
//                        code.borrow_mut()
//                            .recalculate_area_sizes();
//
//                        gen_code(&mut code.borrow_mut());
//                    }});
//                spawn_button(state, pop_col, pop, "if", current_pos.clone(), {
//                    let code = code.clone();
//                        move |_state, pos| {
//                        let (id, x, y) = pos.pos();
//                        let _ = code.borrow_mut()
//                            .instanciate_at(id, x, y, "if", None);
//                        code.borrow_mut()
//                            .recalculate_area_sizes();
//
//                        gen_code(&mut code.borrow_mut());
//                    }});
//                spawn_button(state, pop_col, pop, "->", current_pos.clone(), {
//                    let code = code.clone();
//                        move |_state, pos| {
//                        let (id, x, y) = pos.pos();
//                        let _ = code.borrow_mut()
//                            .instanciate_at(id, x, y, "->", None);
//                        code.borrow_mut()
//                            .recalculate_area_sizes();
//
//                        gen_code(&mut code.borrow_mut());
//                    }});
//                spawn_button(state, pop_col, pop, "->2", current_pos.clone(), {
//                    let code = code.clone();
//                        move |_state, pos| {
//                        let (id, x, y) = pos.pos();
//                        let _ = code.borrow_mut()
//                            .instanciate_at(id, x, y, "->2", None);
//                        code.borrow_mut()
//                            .recalculate_area_sizes();
//
//                        gen_code(&mut code.borrow_mut());
//                    }});
//                spawn_button(state, pop_col, pop, "->3", current_pos.clone(), {
//                    let code = code.clone();
//                        move |_state, pos| {
//                        let (id, x, y) = pos.pos();
//                        let _ = code.borrow_mut()
//                            .instanciate_at(id, x, y, "->3", None);
//                        code.borrow_mut()
//                            .recalculate_area_sizes();
//
//                        gen_code(&mut code.borrow_mut());
//                    }});
//                spawn_button(state, pop_col, pop, "set: x", current_pos.clone(), {
//                    let code = code.clone();
//                        move |_state, pos| {
//                        let (id, x, y) = pos.pos();
//                        let _ = code.borrow_mut()
//                            .instanciate_at(id, x, y, "set", Some("x".to_string()));
//                        code.borrow_mut()
//                            .recalculate_area_sizes();
//
//                        gen_code(&mut code.borrow_mut());
//                    }});
//                spawn_button(state, pop_col, pop, "get: x", current_pos.clone(), {
//                    let code = code.clone();
//                        move |_state, pos| {
//                        let (id, x, y) = pos.pos();
//                        let _ = code.borrow_mut()
//                            .instanciate_at(id, x, y, "get", Some("x".to_string()));
//                        code.borrow_mut()
//                            .recalculate_area_sizes();
//
//                        gen_code(&mut code.borrow_mut());
//                    }});
//                spawn_button(state, pop_col, pop, "sin", current_pos.clone(), {
//                    let code = code.clone();
//                        move |_state, pos| {
//                        let (id, x, y) = pos.pos();
//                        let _ = code.borrow_mut()
//                            .instanciate_at(id, x, y, "sin", None);
//                        code.borrow_mut()
//                            .recalculate_area_sizes();
//
//                        gen_code(&mut code.borrow_mut());
//                    }});
//                spawn_button(state, pop_col, pop, "1pole", current_pos.clone(), {
//                    let code = code.clone();
//                        move |_state, pos| {
//                        let (id, x, y) = pos.pos();
//                        let _ = code.borrow_mut()
//                            .instanciate_at(id, x, y, "1pole", None);
//                        code.borrow_mut()
//                            .recalculate_area_sizes();
//
//                        gen_code(&mut code.borrow_mut());
//                    }});
//                spawn_button(state, pop_col, pop, "svf", current_pos.clone(), {
//                    let code = code.clone();
//                        move |_state, pos| {
//                        let (id, x, y) = pos.pos();
//                        let _ = code.borrow_mut()
//                            .instanciate_at(id, x, y, "svf", None);
//                        code.borrow_mut()
//                            .recalculate_area_sizes();
//
//                        gen_code(&mut code.borrow_mut());
//                    }});
//                spawn_button(state, pop_col, pop, "delay", current_pos.clone(), {
//                    let code = code.clone();
//                        move |_state, pos| {
//                        let (id, x, y) = pos.pos();
//                        let _ = code.borrow_mut()
//                            .instanciate_at(id, x, y, "delay", None);
//                        code.borrow_mut()
//                            .recalculate_area_sizes();
//
//                        gen_code(&mut code.borrow_mut());
//                    }});
//
//                pop_col.set_background_color(state, Color::white());
//                pop.set_background_color(state, Color::white());

//                let bc =
//                    BlockCode::new(style.clone())
//                        .on_click({
//                            let code = code.clone();
//                            move |_, state, _e, pos, btn| {
//                                (*current_pos.borrow_mut()) = pos;
//
//                                if let BlockPos::Block { row, col, .. } = pos {
//                                    let (id, x, y) = pos.pos();
//
//                                    if btn == MouseButton::Right {
//                                        println!("PORT CLICK {:?}", pos);
//                                        code.borrow_mut()
//                                            .shift_port(id, x, y, row, col == 1);
//                                    } else {
//                                        if col == 1 {
//                                            let _ = code.borrow_mut()
//                                                .split_block_chain_after(
//                                                    id, x, y, Some("->"));
//                                        } else {
//                                            let _ = code.borrow_mut()
//                                                .split_block_chain_after(
//                                                    id, x - 1, y, None);
//                                        }
//                                    }
//
//                                    code.borrow_mut()
//                                        .recalculate_area_sizes();
//
//                                    gen_code(&mut code.borrow_mut());
//                                } else {
//                                    println!("CLICK {:?}", pos);
//                                    state.insert_event(
//                                        Event::new(PopupEvent::OpenAtCursor)
//                                        .target(pop)
//                                        .origin(Entity::root()));
//                                }
//                            }
//                        })
//                        .on_drag({
//                            let code = code.clone();
//                            move |_, _state, _e, pos, pos2, btn| {
//                                let (id, x, y)    = pos.pos();
//                                let (id2, x2, y2) = pos2.pos();
//
//                                println!("P1={:?} P2={:?}", pos, pos2);
//
//                                if let BlockPos::Cell { .. } = pos {
//                                    if let BlockPos::Block { .. } = pos2 {
//                                        let _ = code.borrow_mut()
//                                            .clone_block_from_to(
//                                                id2, x2, y2, id, x, y);
//                                        code.borrow_mut()
//                                            .recalculate_area_sizes();
//
//                                        gen_code(&mut code.borrow_mut());
//                                    }
//                                } else {
//                                    if btn == MouseButton::Right {
//                                        let _ = code.borrow_mut()
//                                            .move_block_from_to(
//                                                id, x, y, id2, x2, y2);
//                                    } else {
//                                        if pos.pos() == pos2.pos() {
//                                            let _ = code.borrow_mut()
//                                                .remove_at(id, x, y);
//                                        } else {
//                                            let _ = code.borrow_mut()
//                                                .move_block_chain_from_to(
//                                                    id, x, y, id2, x2, y2);
//                                        }
//                                    }
//
//                                    code.borrow_mut()
//                                        .recalculate_area_sizes();
//
//                                    gen_code(&mut code.borrow_mut());
//                                }
//                            }
//                        })
//                        .build(state, col, |builder| { builder });

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
                    text += "[R:]Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore [c4C11w30h20gG2:Sin] et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, [avDLY:Delay]no sea takimata sanctus est Lorem ipsum dolor sit amet.";

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
                        .on_click(|_wid, state, ent, _line, _frag, cmd| {
                            println!("CLICK: [{}]", cmd);
                            let parts : Vec<&str> = cmd.split("=").collect();

                            if parts.len() == 2 {
                                if parts[0] == "TitleClr" {
                                    set_text(
                                        state, ent,
                                        parts[1].parse::<usize>().unwrap_or(0));
                                }
                            }
                        })
                        .build(state, row, |builder| { builder });

                set_text(state, wt, 0);

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
