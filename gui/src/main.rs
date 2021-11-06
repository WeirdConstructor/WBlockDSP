use tuix::*;
use tuix::widgets::*;

mod painter;
mod rect;
mod block_code;

use block_code::*;

use wblockdsp::{BlockFun, BlockLanguage, BlockType};

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

pub fn main() {
    let lang = Rc::new(RefCell::new(BlockLanguage::new()));
    let code = Rc::new(RefCell::new(BlockFun::new(lang.clone())));

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

    code.borrow_mut().instanciate_at(0, 1, 1, "number", Some("2.32".to_string()));
    code.borrow_mut().instanciate_at(0, 3, 3, "+", None);
    code.borrow_mut().instanciate_at(0, 4, 3, "->3", None);

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
                        .set_height(Pixels(400.0))
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
                    }});
                spawn_button(state, pop_col, pop, "-", current_pos.clone(), {
                    let code = code.clone();
                        move |state, pos| {
                        let (id, x, y) = pos.pos();
                        code.borrow_mut()
                            .instanciate_at(id, x, y, "-", None);
                        code.borrow_mut()
                            .recalculate_area_sizes();
                    }});
                spawn_button(state, pop_col, pop, "*", current_pos.clone(), {
                    let code = code.clone();
                        move |state, pos| {
                        let (id, x, y) = pos.pos();
                        code.borrow_mut()
                            .instanciate_at(id, x, y, "*", None);
                        code.borrow_mut()
                            .recalculate_area_sizes();
                    }});
                spawn_button(state, pop_col, pop, "/", current_pos.clone(), {
                    let code = code.clone();
                        move |state, pos| {
                        let (id, x, y) = pos.pos();
                        code.borrow_mut()
                            .instanciate_at(id, x, y, "/", None);
                        code.borrow_mut()
                            .recalculate_area_sizes();
                    }});
                spawn_button(state, pop_col, pop, "if", current_pos.clone(), {
                    let code = code.clone();
                        move |state, pos| {
                        let (id, x, y) = pos.pos();
                        code.borrow_mut()
                            .instanciate_at(id, x, y, "if", None);
                        code.borrow_mut()
                            .recalculate_area_sizes();
                    }});
                spawn_button(state, pop_col, pop, "->", current_pos.clone(), {
                    let code = code.clone();
                        move |state, pos| {
                        let (id, x, y) = pos.pos();
                        code.borrow_mut()
                            .instanciate_at(id, x, y, "->", None);
                        code.borrow_mut()
                            .recalculate_area_sizes();
                    }});
                spawn_button(state, pop_col, pop, "->2", current_pos.clone(), {
                    let code = code.clone();
                        move |state, pos| {
                        let (id, x, y) = pos.pos();
                        code.borrow_mut()
                            .instanciate_at(id, x, y, "->2", None);
                        code.borrow_mut()
                            .recalculate_area_sizes();
                    }});
                spawn_button(state, pop_col, pop, "->3", current_pos.clone(), {
                    let code = code.clone();
                        move |state, pos| {
                        let (id, x, y) = pos.pos();
                        code.borrow_mut()
                            .instanciate_at(id, x, y, "->3", None);
                        code.borrow_mut()
                            .recalculate_area_sizes();
                    }});
                spawn_button(state, pop_col, pop, "set: x", current_pos.clone(), {
                    let code = code.clone();
                        move |state, pos| {
                        let (id, x, y) = pos.pos();
                        code.borrow_mut()
                            .instanciate_at(id, x, y, "set", Some("x".to_string()));
                        code.borrow_mut()
                            .recalculate_area_sizes();
                    }});
                spawn_button(state, pop_col, pop, "get: x", current_pos.clone(), {
                    let code = code.clone();
                        move |state, pos| {
                        let (id, x, y) = pos.pos();
                        code.borrow_mut()
                            .instanciate_at(id, x, y, "get", Some("x".to_string()));
                        code.borrow_mut()
                            .recalculate_area_sizes();
                    }});
                spawn_button(state, pop_col, pop, "sin", current_pos.clone(), {
                    let code = code.clone();
                        move |state, pos| {
                        let (id, x, y) = pos.pos();
                        code.borrow_mut()
                            .instanciate_at(id, x, y, "sin", None);
                        code.borrow_mut()
                            .recalculate_area_sizes();
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
                                    if btn == MouseButton::Right {
                                        println!("PORT CLICK {:?}", pos);
                                        let (id, x, y) = pos.pos();
                                        code.borrow_mut()
                                            .shift_port(id, x, y, row, col == 1);
                                    }
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

                                if let BlockPos::Cell { .. } = pos {
                                    if let BlockPos::Block { .. } = pos2 {
                                        code.borrow_mut()
                                            .clone_block_from_to(id2, x2, y2, id, x, y);
                                        code.borrow_mut()
                                            .recalculate_area_sizes();
                                    }
                                } else {
                                    code.borrow_mut()
                                        .move_block_from_to(id, x, y, id2, x2, y2);
                                    code.borrow_mut()
                                        .recalculate_area_sizes();
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
