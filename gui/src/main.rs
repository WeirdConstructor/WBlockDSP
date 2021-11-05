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

pub fn main() {
    let lang = Rc::new(RefCell::new(BlockLanguage::new()));
    let code = Rc::new(RefCell::new(BlockFun::new(lang.clone())));

    lang.borrow_mut().define(BlockType {
        category:       "literals".to_string(),
        name:           "number".to_string(),
        rows:           1,
        inputs:         vec![],
        outputs:        vec![Some(">".to_string())],
        area_count:     0,
        user_input:     true,
        description:    "A literal value, typed in by the user.".to_string(),
    });

    lang.borrow_mut().define(BlockType {
        category:       "routing".to_string(),
        name:           "->".to_string(),
        rows:           1,
        inputs:         vec![Some("".to_string())],
        outputs:        vec![Some(">".to_string())],
        area_count:     0,
        user_input:     false,
        description:    "Forwards the value one block".to_string(),
    });

    lang.borrow_mut().define(BlockType {
        category:       "routing".to_string(),
        name:           "->2".to_string(),
        rows:           2,
        inputs:         vec![Some("".to_string())],
        outputs:        vec![Some(">".to_string()), Some(">".to_string())],
        area_count:     0,
        user_input:     false,
        description:    "Forwards the value one block and sends it to multiple destinations".to_string(),
    });

    lang.borrow_mut().define(BlockType {
        category:       "routing".to_string(),
        name:           "->3".to_string(),
        rows:           3,
        inputs:         vec![Some("".to_string())],
        outputs:        vec![Some(">".to_string()), Some(">".to_string()), Some(">".to_string())],
        area_count:     0,
        user_input:     false,
        description:    "Forwards the value one block and sends it to multiple destinations".to_string(),
    });

    lang.borrow_mut().define(BlockType {
        category:       "variables".to_string(),
        name:           "set".to_string(),
        rows:           1,
        inputs:         vec![Some(">".to_string())],
        outputs:        vec![],
        area_count:     0,
        user_input:     true,
        description:    "Stores into a variable".to_string(),
    });

    lang.borrow_mut().define(BlockType {
        category:       "variables".to_string(),
        name:           "get".to_string(),
        rows:           1,
        inputs:         vec![],
        outputs:        vec![Some(">".to_string())],
        area_count:     0,
        user_input:     true,
        description:    "Loads a variable".to_string(),
    });

    for fun_name in &["+", "-", "*", "/"] {
        lang.borrow_mut().define(BlockType {
            category:       "arithmetics".to_string(),
            name:           fun_name.to_string(),
            rows:           1,
            inputs:         vec![Some("".to_string()), Some("".to_string())],
            outputs:        vec![Some(">".to_string())],
            area_count:     0,
            user_input:     false,
            description:    "A binary arithmetics operation".to_string(),
        });
    }

    code.borrow_mut().instanciate_at(0, 1, 1, "number", Some("2.32".to_string()));

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
                        .set_height(Pixels(100.0))
                });

                let pop_col = Column::new().build(state, pop, |builder| builder);

                let current_pos = Rc::new(RefCell::new(BlockPos::Cell { id: 0, x: 0, y: 0 }));
                let btn1 =
                    Button::with_label("?")
                        .on_release({
                            let pop     = pop;
                            let code    = code.clone();
                            let cur_pos = current_pos.clone();

                            move |_, state, _| {
                                if let BlockPos::Cell { id, x, y } = *cur_pos.borrow() {
                                    println!("CLICK {:?}", (id, x, y));

                                    code.borrow_mut()
                                        .instanciate_at(
                                            id, x, y, "number",
                                            Some("33".to_string()));
                                }

                                state.insert_event(
                                    Event::new(PopupEvent::Close)
                                    .target(pop)
                                    .origin(Entity::root()));
                            }
                        })
                        .build(state, pop_col, |builder| builder);

                pop_col.set_background_color(state, Color::white());
                pop.set_background_color(state, Color::white());

                let bc =
                    BlockCode::new(style)
                        .on_click(move |_, state, e, pos| {
                            println!("CLICK {:?}", pos);
                            (*current_pos.borrow_mut()) = pos;

                            state.insert_event(
                                Event::new(PopupEvent::OpenAtCursor)
                                .target(pop)
                                .origin(Entity::root()));
                        })
                        .build(state, col, |builder| { builder });

                state.add_theme(STYLE);

                state.insert_event(
                    Event::new(BlockCodeMessage::SetCode(code))
                    .target(bc));
            });
    app.run();
}
