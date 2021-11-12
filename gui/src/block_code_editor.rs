// Copyright (c) 2021 Weird Constructor <weirdconstructor@gmail.com>
// This file is a part of WBlockDSP. Released under GPL-3.0-or-later.
// See README.md and COPYING for details.

use crate::block_code_style::*;
use crate::block_code::*;

use tuix::*;
use tuix::widgets::*;

use wblockdsp::{BlockFun, BlockLanguage, BlockType, BlockASTNode};
use wblockdsp::wlapi::*;

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

//fn spawn_button<F: 'static + Fn(&mut State, BlockPos)>(
//    state: &mut State,
//    parent: Entity,
//    popup: Entity,
//    lbl: &str,
//    cur_pos: Rc<RefCell<BlockPos>>,
//    cb: F)
//{
//    Button::with_label(lbl)
//        .on_release(move |_, state, _| {
//            (cb)(state, *cur_pos.borrow());
//
//            state.insert_event(
//                Event::new(PopupEvent::Close)
//                .target(popup)
//                .origin(Entity::root()));
//        })
//        .build(state, parent, |builder| builder);
//}
//
//
pub struct BlockCodeEditor {
    lang:        Rc<RefCell<BlockLanguage>>,
    code:        Rc<RefCell<BlockFun>>,
    current_pos: Rc<RefCell<BlockPos>>,
    style:       BlockCodeStyle,

    on_change:   Option<Box<dyn Fn(&mut Self, &mut State, Entity, Rc<RefCell<BlockFun>>)>>,
}

impl BlockCodeEditor {
    pub fn new(style: BlockCodeStyle, lang: Rc<RefCell<BlockLanguage>>, code: Rc<RefCell<BlockFun>>) -> Self {
        Self {
            style,
            lang,
            code,
            on_change:   None,
            current_pos: Rc::new(RefCell::new(
                BlockPos::Cell { id: 0, x: 0, y: 0 })),
        }
    }

    pub fn on_change<F>(mut self, on_change: F) -> Self
    where
        F: 'static + Fn(&mut Self, &mut State, Entity, Rc<RefCell<BlockFun>>),
    {
        self.on_change = Some(Box::new(on_change));

        self
    }
}

impl Widget for BlockCodeEditor {
    type Ret  = Entity;
    type Data = ();

    fn widget_name(&self) -> String {
        "block-code-editor".to_string()
    }

    fn on_build(&mut self, state: &mut State, entity: Entity) -> Self::Ret {
        state.add_theme(STYLE);

        entity.set_element(state, "block_code_editor");

        let pop = Popup::new().build(state, Entity::root(), |builder| {
            builder
                .set_width(Pixels(100.0))
                .set_height(Pixels(430.0))
        });

        let add_block_item =
            Rc::new({
                let code        = self.code.clone();
                let current_pos = self.current_pos.clone();

                move |state: &mut State, typ: &str| {
                    let (id, x, y) = current_pos.borrow().pos();

                    let _ = code.borrow_mut()
                        .instanciate_at(id, x, y, typ, None);

                    println!("DOOOO {:?}", current_pos.borrow());

                    code.borrow_mut()
                        .recalculate_area_sizes();

                    state.insert_event(
                        Event::new(PopupEvent::Close)
                        .target(pop)
                        .origin(Entity::root()));
                }
            });

        Button::with_label("-")
            .on_release({ let abi = add_block_item.clone();
                move |_, state, _| { (*add_block_item)(state, "-"); } })
            .build(state, pop, |builder| builder);

        let pop_col = Column::new().build(state, pop, |builder| builder);

        let bc =
            BlockCode::new(self.style.clone())
                .on_click({
                    let code        = self.code.clone();
                    let current_pos = self.current_pos.clone();

                    move |_, state, _e, pos, btn| {
                        (*current_pos.borrow_mut()) = pos;

                        if let BlockPos::Block { row, col, .. } = pos {
                            let (id, x, y) = pos.pos();

                            if btn == MouseButton::Right {
                                println!("PORT CLICK {:?}", pos);
                                code.borrow_mut()
                                    .shift_port(id, x, y, row, col == 1);
                            } else {
                                if col == 1 {
                                    let _ = code.borrow_mut()
                                        .split_block_chain_after(
                                            id, x, y, Some("->"));
                                } else {
                                    let _ = code.borrow_mut()
                                        .split_block_chain_after(
                                            id, x - 1, y, None);
                                }
                            }

                            code.borrow_mut()
                                .recalculate_area_sizes();

//                            gen_code(&mut code.borrow_mut());
//                            if let Some(cb) = editor.on_change.take() {
//                                (*cb)(editor, state, entity, code.clone());
//                                editor.on_change = Some(cb);
//                            }
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
                    let code = self.code.clone();

                    move |_, _state, _e, pos, pos2, btn| {
                        let (id, x, y)    = pos.pos();
                        let (id2, x2, y2) = pos2.pos();

                        println!("P1={:?} P2={:?}", pos, pos2);

                        if let BlockPos::Cell { .. } = pos {
                            if let BlockPos::Block { .. } = pos2 {
                                let _ = code.borrow_mut()
                                    .clone_block_from_to(
                                        id2, x2, y2, id, x, y);
                                code.borrow_mut()
                                    .recalculate_area_sizes();

//                                if let Some(cb) = editor.on_change.take() {
//                                    (*cb)(editor, state, entity, code.clone());
//                                    editor.on_change = Some(cb);
//                                }
                            }
                        } else {
                            if btn == MouseButton::Right {
                                let _ = code.borrow_mut()
                                    .move_block_from_to(
                                        id, x, y, id2, x2, y2);
                            } else {
                                if pos.pos() == pos2.pos() {
                                    let _ = code.borrow_mut()
                                        .remove_at(id, x, y);
                                } else {
                                    let _ = code.borrow_mut()
                                        .move_block_chain_from_to(
                                            id, x, y, id2, x2, y2);
                                }
                            }

                            code.borrow_mut()
                                .recalculate_area_sizes();

//                            if let Some(cb) = editor.on_change.take() {
//                                (*cb)(editor, state, entity, code.clone());
//                                editor.on_change = Some(cb);
//                            }
                        }
                    }
                })
                .build(state, entity, |builder| { builder });

        state.insert_event(
            Event::new(BlockCodeMessage::SetCode(self.code.clone()))
            .target(bc));

        entity
    }

    fn on_event(&mut self, state: &mut State, entity: Entity, event: &mut Event) {
//        if let Some(grid_msg) = event.message.downcast::<BlockCodeMessage>() {
//            match grid_msg {
//                BlockCodeMessage::SetCode(code) => {
//                    self.code = code.clone();
//                    state.insert_event(
//                        Event::new(WindowEvent::Redraw)
//                        .target(Entity::root()));
//                },
//            }
//        }
//
    }
}
