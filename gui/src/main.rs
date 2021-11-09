use tuix::*;
use tuix::widgets::*;

mod painter;
mod rect;
mod block_code;

use block_code::*;

use wblockdsp::{BlockFun, BlockLanguage, BlockType, BlockASTNode};

use std::rc::Rc;
use std::cell::RefCell;

const STYLE: &str = r#"
    popup {
        background-color: #d2d2d2;
    }
    popup button {
        height: 30px;
        child-space: 1s;
        color: black;
        background-color: #d2d2d2;
    }
    popup button:hover {
        background-color: #e2e2e2;
    }
    popup button:active {
        background-color: #c2c2c2;
    }
"#;

fn spawn_button<F: 'static + Fn(&mut State, BlockPos)>(
    state: &mut State,
    parent: Entity,
    popup: Entity,
    lbl: &str,
    cur_pos: Rc<RefCell<BlockPos>>,
    cb: F)
{
    Button::with_label(lbl)
        .on_release(move |_, state, _| {
            (cb)(state, *cur_pos.borrow());

            state.insert_event(
                Event::new(PopupEvent::Close)
                .target(popup)
                .origin(Entity::root()));
        })
        .build(state, parent, |builder| builder);
}

#[derive(Debug)]
pub struct ASTNode {
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
            "{}{}[{}] {}{}",
            indent_str, self.0.borrow().typ, self.0.borrow().lbl,
            out_port, in_port);

        for (inp, out, n) in &self.0.borrow().nodes {
            n.walk_dump(&inp, &out, indent + 1);
        }
    }
}

impl BlockASTNode for ASTNodeRef {
    fn from(typ: &str, lbl: &str) -> ASTNodeRef {
        ASTNodeRef(Rc::new(RefCell::new(ASTNode {
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
    let mut tree = code.generate_tree::<ASTNodeRef>("zero").unwrap();

    tree.walk_dump("", "", 0);
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

    code.borrow_mut().instanciate_at(0, 1, 1, "number", Some("2.32".to_string()));
    code.borrow_mut().instanciate_at(0, 2, 3, "number", Some("1.0".to_string()));
    code.borrow_mut().instanciate_at(0, 2, 2, "number", Some("-1.0".to_string()));
    code.borrow_mut().instanciate_at(0, 2, 1, "number", Some("0.5".to_string()));
    code.borrow_mut().instanciate_at(0, 3, 3, "+", None);
    code.borrow_mut().instanciate_at(0, 4, 3, "->3", None);
    code.borrow_mut().instanciate_at(0, 2, 6, "if", None);

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

    let app =
        Application::new(
            WindowDescription::new()
                .with_title("WBlockDSP GUI")
                .with_inner_size(900, 760),
            |state, window| {
                let style = BlockCodeStyle::new_default();

                let col = Column::new().build(state, window.entity(), |builder| builder);

                let pop = Popup::new().build(state, window.entity(), |builder| {
                    builder
                        .set_width(Pixels(100.0))
                        .set_height(Pixels(430.0))
                });

                let pop_col = Column::new().build(state, pop, |builder| builder);

                let current_pos = Rc::new(RefCell::new(BlockPos::Cell { id: 0, x: 0, y: 0 }));

                spawn_button(state, pop_col, pop, "+", current_pos.clone(), {
                    let code = code.clone();
                        move |state, pos| {
                        let (id, x, y) = pos.pos();
                        code.borrow_mut()
                            .instanciate_at(id, x, y, "+", None);
                        code.borrow_mut()
                            .recalculate_area_sizes();

                        gen_code(&mut code.borrow_mut());
                    }});
                spawn_button(state, pop_col, pop, "-", current_pos.clone(), {
                    let code = code.clone();
                        move |state, pos| {
                        let (id, x, y) = pos.pos();
                        code.borrow_mut()
                            .instanciate_at(id, x, y, "-", None);
                        code.borrow_mut()
                            .recalculate_area_sizes();

                        gen_code(&mut code.borrow_mut());
                    }});
                spawn_button(state, pop_col, pop, "*", current_pos.clone(), {
                    let code = code.clone();
                        move |state, pos| {
                        let (id, x, y) = pos.pos();
                        code.borrow_mut()
                            .instanciate_at(id, x, y, "*", None);
                        code.borrow_mut()
                            .recalculate_area_sizes();

                        gen_code(&mut code.borrow_mut());
                    }});
                spawn_button(state, pop_col, pop, "/", current_pos.clone(), {
                    let code = code.clone();
                        move |state, pos| {
                        let (id, x, y) = pos.pos();
                        code.borrow_mut()
                            .instanciate_at(id, x, y, "/", None);
                        code.borrow_mut()
                            .recalculate_area_sizes();

                        gen_code(&mut code.borrow_mut());
                    }});
                spawn_button(state, pop_col, pop, "if", current_pos.clone(), {
                    let code = code.clone();
                        move |state, pos| {
                        let (id, x, y) = pos.pos();
                        code.borrow_mut()
                            .instanciate_at(id, x, y, "if", None);
                        code.borrow_mut()
                            .recalculate_area_sizes();

                        gen_code(&mut code.borrow_mut());
                    }});
                spawn_button(state, pop_col, pop, "->", current_pos.clone(), {
                    let code = code.clone();
                        move |state, pos| {
                        let (id, x, y) = pos.pos();
                        code.borrow_mut()
                            .instanciate_at(id, x, y, "->", None);
                        code.borrow_mut()
                            .recalculate_area_sizes();

                        gen_code(&mut code.borrow_mut());
                    }});
                spawn_button(state, pop_col, pop, "->2", current_pos.clone(), {
                    let code = code.clone();
                        move |state, pos| {
                        let (id, x, y) = pos.pos();
                        code.borrow_mut()
                            .instanciate_at(id, x, y, "->2", None);
                        code.borrow_mut()
                            .recalculate_area_sizes();

                        gen_code(&mut code.borrow_mut());
                    }});
                spawn_button(state, pop_col, pop, "->3", current_pos.clone(), {
                    let code = code.clone();
                        move |state, pos| {
                        let (id, x, y) = pos.pos();
                        code.borrow_mut()
                            .instanciate_at(id, x, y, "->3", None);
                        code.borrow_mut()
                            .recalculate_area_sizes();

                        gen_code(&mut code.borrow_mut());
                    }});
                spawn_button(state, pop_col, pop, "set: x", current_pos.clone(), {
                    let code = code.clone();
                        move |state, pos| {
                        let (id, x, y) = pos.pos();
                        code.borrow_mut()
                            .instanciate_at(id, x, y, "set", Some("x".to_string()));
                        code.borrow_mut()
                            .recalculate_area_sizes();

                        gen_code(&mut code.borrow_mut());
                    }});
                spawn_button(state, pop_col, pop, "get: x", current_pos.clone(), {
                    let code = code.clone();
                        move |state, pos| {
                        let (id, x, y) = pos.pos();
                        code.borrow_mut()
                            .instanciate_at(id, x, y, "get", Some("x".to_string()));
                        code.borrow_mut()
                            .recalculate_area_sizes();

                        gen_code(&mut code.borrow_mut());
                    }});
                spawn_button(state, pop_col, pop, "sin", current_pos.clone(), {
                    let code = code.clone();
                        move |state, pos| {
                        let (id, x, y) = pos.pos();
                        code.borrow_mut()
                            .instanciate_at(id, x, y, "sin", None);
                        code.borrow_mut()
                            .recalculate_area_sizes();

                        gen_code(&mut code.borrow_mut());
                    }});
                spawn_button(state, pop_col, pop, "1pole", current_pos.clone(), {
                    let code = code.clone();
                        move |state, pos| {
                        let (id, x, y) = pos.pos();
                        code.borrow_mut()
                            .instanciate_at(id, x, y, "1pole", None);
                        code.borrow_mut()
                            .recalculate_area_sizes();

                        gen_code(&mut code.borrow_mut());
                    }});
                spawn_button(state, pop_col, pop, "svf", current_pos.clone(), {
                    let code = code.clone();
                        move |state, pos| {
                        let (id, x, y) = pos.pos();
                        code.borrow_mut()
                            .instanciate_at(id, x, y, "svf", None);
                        code.borrow_mut()
                            .recalculate_area_sizes();

                        gen_code(&mut code.borrow_mut());
                    }});
                spawn_button(state, pop_col, pop, "delay", current_pos.clone(), {
                    let code = code.clone();
                        move |state, pos| {
                        let (id, x, y) = pos.pos();
                        code.borrow_mut()
                            .instanciate_at(id, x, y, "delay", None);
                        code.borrow_mut()
                            .recalculate_area_sizes();

                        gen_code(&mut code.borrow_mut());
                    }});


//                    Button::with_label("?")
//                        .on_release({
//                            let pop     = pop;
//                            let code    = code.clone();
//                            let cur_pos = current_pos.clone();
//
//                            move |_, state, _| {
//                                if let BlockPos::Cell { id, x, y } = *cur_pos.borrow() {
//                                    println!("CLICK {:?}", (id, x, y));
//
//                                }
//
//                                state.insert_event(
//                                    Event::new(PopupEvent::Close)
//                                    .target(pop)
//                                    .origin(Entity::root()));
//                            }
//                        })
//                        .build(state, pop_col, |builder| builder);

                pop_col.set_background_color(state, Color::white());
                pop.set_background_color(state, Color::white());

                let bc =
                    BlockCode::new(style)
                        .on_click({
                            let code = code.clone();
                            move |_, state, e, pos, btn| {
                                (*current_pos.borrow_mut()) = pos;

                                if let BlockPos::Block { row, col, .. } = pos {
                                    let (id, x, y) = pos.pos();

                                    if btn == MouseButton::Right {
                                        println!("PORT CLICK {:?}", pos);
                                        code.borrow_mut()
                                            .shift_port(id, x, y, row, col == 1);
                                    } else {
                                        if col == 1 {
                                            code.borrow_mut()
                                                .split_block_chain_after(
                                                    id, x, y, Some("->"));
                                        } else {
                                            code.borrow_mut()
                                                .split_block_chain_after(
                                                    id, x - 1, y, None);
                                        }
                                    }

                                    code.borrow_mut()
                                        .recalculate_area_sizes();

                                    gen_code(&mut code.borrow_mut());
                                } else {
                                    println!("CLICK {:?}", pos);
                                    state.insert_event(
                                        Event::new(PopupEvent::OpenAtCursor)
                                        .target(pop)
                                        .origin(Entity::root()));
                                }
                            }
                        })
                        .on_drag({
                            let code = code.clone();
                            move |_, state, e, pos, pos2, btn| {
                                let (id, x, y)    = pos.pos();
                                let (id2, x2, y2) = pos2.pos();

                                println!("P1={:?} P2={:?}", pos, pos2);

                                if let BlockPos::Cell { .. } = pos {
                                    if let BlockPos::Block { .. } = pos2 {
                                        code.borrow_mut()
                                            .clone_block_from_to(
                                                id2, x2, y2, id, x, y);
                                        code.borrow_mut()
                                            .recalculate_area_sizes();

                                        gen_code(&mut code.borrow_mut());
                                    }
                                } else {
                                    if btn == MouseButton::Right {
                                        code.borrow_mut()
                                            .move_block_from_to(
                                                id, x, y, id2, x2, y2);
                                    } else {
                                        if pos.pos() == pos2.pos() {
                                            code.borrow_mut()
                                                .remove_at(id, x, y);
                                        } else {
                                            code.borrow_mut()
                                                .move_block_chain_from_to(
                                                    id, x, y, id2, x2, y2);
                                        }
                                    }

                                    code.borrow_mut()
                                        .recalculate_area_sizes();

                                    gen_code(&mut code.borrow_mut());
                                }
                            }
                        })
                        .build(state, col, |builder| { builder });

                state.add_theme(STYLE);

                state.insert_event(
                    Event::new(BlockCodeMessage::SetCode(code))
                    .target(bc));
            });
    app.run();
}
