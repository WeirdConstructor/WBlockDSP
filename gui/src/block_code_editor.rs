// Copyright (c) 2021 Weird Constructor <weirdconstructor@gmail.com>
// This file is a part of WBlockDSP. Released under GPL-3.0-or-later.
// See README.md and COPYING for details.

use crate::block_code_style::*;
use crate::block_code::*;

use tuix::*;
use tuix::widgets::*;

use wblockdsp::{
    BlockFun,
    BlockLanguage,
    BlockType,
    BlockASTNode,
    BlockUserInput,
};
use wblockdsp::wlapi::*;

use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;

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

    .block_selector {
        width: 500px;
        height: 200px;
    }

    .block_input {
        width: 100px;
        height: 30px;
    }

    .block_vars {
        width: 100px;
        height: 300px;
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

#[derive(Clone)]
pub enum BlockCodeEditorMessage {
    SetCode(Rc<RefCell<BlockFun>>),
}


pub struct BlockCodeEditor {
    lang:        Rc<RefCell<BlockLanguage>>,
    code:        Rc<RefCell<BlockFun>>,
    current_pos: Rc<RefCell<BlockPos>>,
    style:       BlockCodeStyle,

    category_order: Vec<String>,

    bc_entity:  Entity,

    on_query_input: Rc<dyn Fn(&mut State, Entity, &str, Rc<dyn Fn(&mut State, String)>)>,
    on_change:      Rc<dyn Fn(&mut State, Entity, Rc<RefCell<BlockFun>>)>,
}

impl BlockCodeEditor {
    pub fn new(
        style: BlockCodeStyle,
        category_order: Vec<String>,
        lang: Rc<RefCell<BlockLanguage>>,
        code: Rc<RefCell<BlockFun>>
    ) -> Self
    {
        Self {
            style,
            category_order,
            lang,
            code,
            bc_entity:      Entity::null(),
            on_change:      Rc::new(|_, _, _| {}),
            on_query_input: Rc::new(|state, _, _, cb| { cb(state, "1.2".to_string()) }),
            current_pos: Rc::new(RefCell::new(
                BlockPos::Cell { id: 0, x: 0, y: 0 })),
        }
    }

    pub fn on_query_input<F>(mut self, on_query_input: F) -> Self
    where
        F: 'static + Fn(&mut State, Entity, &str, Rc<dyn Fn(&mut State, String)>),
    {
        self.on_query_input = Rc::new(on_query_input);

        self
    }

    pub fn on_change<F>(mut self, on_change: F) -> Self
    where
        F: 'static + Fn(&mut State, Entity, Rc<RefCell<BlockFun>>),
    {
        self.on_change = Rc::new(on_change);

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

        let var_pop = Popup::new().build(state, Entity::root(), |builder| {
            builder.class("block_vars")
        });

        let input_pop = Popup::new().build(state, Entity::root(), |builder| {
            builder.class("block_input")
        });

        let inp_col = Column::new().build(state, input_pop, |builder| builder);

        let txt_submit_cb : Rc<RefCell<Box<dyn Fn(&mut State, String) -> bool>>> =
            Rc::new(RefCell::new(Box::new(|state, input| { true })));

        let inp_tx = Textbox::new("")
            .on_submit({
                let txt_submit_cb = txt_submit_cb.clone();
                move |txt, state, ent| {
                    if (*txt_submit_cb.borrow())(state, txt.text.to_string()) {
                        println!("CLOSE");
                        state.insert_event(
                            Event::new(PopupEvent::Close)
                            .target(ent)
                            .origin(Entity::root()));
                    } else {
                        println!("???");
                    }
                }
            })
            .build(state, inp_col, |builder| builder);


        let pop = Popup::new().build(state, Entity::root(), |builder| {
            builder.class("block_selector")
        });

        let pop_row = Row::new().build(state, pop, |builder| builder);

        let add_block_item =
            Rc::new({
                let code        = self.code.clone();
                let current_pos = self.current_pos.clone();
                let on_change   = self.on_change.clone();

                move |state: &mut State, typ: &str, val: Option<String>| {
                    let (id, x, y) = current_pos.borrow().pos();

                    let _ = code.borrow_mut()
                        .instanciate_at(id, x, y, typ, val);

                    println!("DOOOO {:?}", current_pos.borrow());

                    code.borrow_mut()
                        .recalculate_area_sizes();

                    state.insert_event(
                        Event::new(PopupEvent::Close)
                        .target(pop)
                        .origin(Entity::root()));

                    (*on_change)(state, entity, code.clone());
                }
            });

        let query_variable : Rc<RefCell<dyn FnMut(&mut State, String)>> = {
                let mut col =
                    Column::new().build(
                        state, var_pop, |builder| builder);
                let lang = self.lang.clone();
                let abi  = add_block_item.clone();

                Rc::new(RefCell::new(move |state: &mut State, typ: String| {
                    state.remove(col);
                    col =
                        Column::new().build(
                            state, var_pop, |builder| builder);

                    for v in lang.borrow().list_identifiers().into_iter() {
                        Button::with_label(&v)
                            .on_release({
                                let abi = abi.clone();
                                let typ = typ.clone();
                                let var = v.clone();

                                move |wid, state, ent| {
                                    state.insert_event(
                                        Event::new(PopupEvent::Close)
                                        .target(var_pop)
                                        .origin(Entity::root()));

                                    (*abi)(state, &typ, Some(v.clone()));
                                }
                            }).build(state, col, |builder| builder);
                    }

                    state.insert_event(
                        Event::new(PopupEvent::OpenAtCursor)
                        .target(var_pop)
                        .origin(Entity::root()));
                }))
            };


        let mut cat_cols : HashMap<String, Entity> = HashMap::new();

        for cat in &self.category_order {
            let col = Column::new().build(state, pop_row, |builder| builder);
            cat_cols.insert(
                cat.to_string(),
                col);
        }

        let mut types = self.lang.borrow().get_type_list();
        types.sort_by(|a, b| a.1.cmp(&b.1));

        for (cat, typ, user_input) in types.into_iter() {
            if let Some(col) = cat_cols.get(&cat) {
                Button::with_label(&typ)
                    .on_release({
                        let query = self.on_query_input.clone();
                        let abi   = add_block_item.clone();
                        let typ   = typ.to_string();
                        let qry   = query_variable.clone();
                        let txt_submit_cb = txt_submit_cb.clone();

                        state.focused = entity;

                        move |wid, state, ent| {
                            match user_input {
                                BlockUserInput::Identifier => {
                                    state.insert_event(
                                        Event::new(PopupEvent::Close)
                                        .target(pop)
                                        .origin(Entity::root()));
                                    (*qry.borrow_mut())(state, typ.clone());
                                },
                                BlockUserInput::Float => {
                                    (*txt_submit_cb.borrow_mut()) = {
                                        let abi = abi.clone();
                                        let typ = typ.clone();
                                        Box::new(move |state, txt| {
                                            if let Ok(_) = txt.parse::<f32>() {
                                                (*abi)(state, &typ, Some(txt));
                                                true
                                            } else {
                                                false
                                            }
                                        })
                                    };

                                    state.insert_event(
                                        Event::new(PopupEvent::Close)
                                        .target(pop)
                                        .origin(Entity::root()));
                                    state.insert_event(
                                        Event::new(TextboxEvent::SetValue("".to_string()))
                                        .target(inp_tx)
                                        .origin(Entity::root()));
                                    state.insert_event(
                                        Event::new(TextboxEvent::BeginEdit)
                                        .target(inp_tx)
                                        .origin(Entity::root()));
                                    state.insert_event(
                                        Event::new(PopupEvent::OpenAtCursor)
                                        .target(input_pop)
                                        .origin(Entity::root()));
                                },
                                BlockUserInput::ClientDecision => {
                                    let abi = abi.clone();
                                    let typ = typ.clone();

                                    (*query)(state, ent, &typ, Rc::new({
                                        let typ = typ.clone();
                                        move |state, value| {
                                            (*abi)(state, &typ, Some(value));
                                        }
                                    }));
                                },
                                _ | BlockUserInput::None => {
                                    (*abi)(state, &typ, None);
                                },
                            }
                        } })
                    .build(state, *col, |builder| builder);
            }
        }


        let bc =
            BlockCode::new(self.style.clone())
                .on_click({
                    let code        = self.code.clone();
                    let current_pos = self.current_pos.clone();
                    let on_change   = self.on_change.clone();

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

                            (*on_change)(state, entity, code.clone());
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
                    let code      = self.code.clone();
                    let on_change = self.on_change.clone();

                    move |_, state, _e, pos, pos2, btn| {
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

                                (*on_change)(state, entity, code.clone());
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

                            (*on_change)(state, entity, code.clone());
                        }
                    }
                })
                .build(state, entity, |builder| { builder });

        self.bc_entity = bc;

        state.insert_event(
            Event::new(BlockCodeMessage::SetCode(self.code.clone()))
            .target(bc));

        entity
    }

    fn on_event(&mut self, state: &mut State, entity: Entity, event: &mut Event) {
        if let Some(grid_msg) = event.message.downcast::<BlockCodeEditorMessage>() {
            match grid_msg {
                BlockCodeEditorMessage::SetCode(code) => {
                    self.code = code.clone();
                    state.insert_event(
                        Event::new(BlockCodeMessage::SetCode(self.code.clone()))
                        .target(self.bc_entity));
                },
            }
        }
    }
}
