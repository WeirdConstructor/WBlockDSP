use tuix::*;
use tuix::widgets::*;

mod painter;
mod rect;
mod block_code;

use block_code::*;

use wblockdsp::{BlockFun, BlockLanguage, BlockType};

use std::rc::Rc;
use std::cell::RefCell;

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

    code.borrow_mut().instanciate_at(0, 1, 1, "number", Some("2.32".to_string()));

    let app =
        Application::new(
            WindowDescription::new()
                .with_title("WBlockDSP GUI")
                .with_inner_size(900, 760),
            |state, window| {
                let style = BlockCodeStyle::new_default();
                let bc = BlockCode::new(style).build(state, window.entity(), |builder| {
                    builder
                });

                state.insert_event(Event::new(BlockCodeMessage::SetCode(code)).target(bc));
            });
    app.run();
}
