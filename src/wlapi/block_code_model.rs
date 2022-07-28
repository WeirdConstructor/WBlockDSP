// Copyright (c) 2021 Weird Constructor <weirdconstructor@gmail.com>
// This file is a part of HexoSynth. Released under GPL-3.0-or-later.
// See README.md and COPYING for details.

use crate::arg_chk;
use crate::{BlockCodeView, BlockFun, BlockLanguage, BlockType, BlockUserInput};
use wlambda::*;

use std::cell::RefCell;
use std::rc::Rc;

#[derive(Clone)]
pub struct VValBlockLanguage {
    lang: Rc<RefCell<BlockLanguage>>,
}

impl VValBlockLanguage {
    pub fn create() -> VVal {
        VVal::new_usr(VValBlockLanguage { lang: Rc::new(RefCell::new(BlockLanguage::new())) })
    }
}

impl VValUserData for VValBlockLanguage {
    fn s(&self) -> String {
        format!("$<BlockDSP::Language>")
    }

    fn call_method(&self, key: &str, env: &mut Env) -> Result<VVal, StackAction> {
        let args = env.argv_ref();

        match key {
            "get_type_list" => {
                arg_chk!(args, 0, "block_code_language.get_type_list[]");
                Ok(VVal::None)
            }
            "define" => {
                arg_chk!(args, 1, "block_code_language.define[block_type]");

                let mut bt = BlockType::default();

                bt.category = env.arg(0).v_s_rawk("category");
                bt.name = env.arg(0).v_s_rawk("name");
                bt.description = env.arg(0).v_s_rawk("description");
                bt.rows = env.arg(0).v_ik("rows") as usize;
                bt.area_count = env.arg(0).v_ik("area_count") as usize;
                bt.color = env.arg(0).v_ik("color") as usize;
                bt.inputs = vec![];
                bt.outputs = vec![];
                env.arg(0).v_k("user_input").with_s_ref(|s| {
                    bt.user_input = match &s[..] {
                        "integer" => BlockUserInput::Integer,
                        "float" => BlockUserInput::Float,
                        "identifier" => BlockUserInput::Identifier,
                        _ => BlockUserInput::None,
                    }
                });

                env.arg(0).v_k("inputs").with_iter(|it| {
                    for (i, _) in it {
                        if i.is_some() {
                            bt.inputs.push(Some(i.s_raw()));
                        } else {
                            bt.inputs.push(None);
                        }
                    }
                });

                env.arg(0).v_k("outputs").with_iter(|it| {
                    for (i, _) in it {
                        if i.is_some() {
                            bt.outputs.push(Some(i.s_raw()));
                        } else {
                            bt.outputs.push(None);
                        }
                    }
                });

                println!("DEFINE {:?}", bt);

                self.lang.borrow_mut().define(bt);

                Ok(VVal::None)
            }
            _ => Ok(VVal::err_msg(&format!("Unknown method called: {}", key))),
        }
    }

    fn as_any(&mut self) -> &mut dyn std::any::Any {
        self
    }
    fn clone_ud(&self) -> Box<dyn vval::VValUserData> {
        Box::new(self.clone())
    }
}

pub fn vv2block_code_language(mut v: VVal) -> Option<Rc<RefCell<BlockLanguage>>> {
    v.with_usr_ref(|model: &mut VValBlockLanguage| model.lang.clone())
}

#[derive(Clone)]
pub struct VValBlockFun {
    code: Rc<RefCell<BlockFun>>,
}

impl VValBlockFun {
    pub fn create(lang: VVal) -> VVal {
        if let Some(lang) = vv2block_code_language(lang.clone()) {
            VVal::new_usr(VValBlockFun { code: Rc::new(RefCell::new(BlockFun::new(lang))) })
        } else {
            VVal::err_msg(&format!("Not a $<BlockDSP:Language>: {}", lang.s()))
        }
    }
}

impl VValUserData for VValBlockFun {
    fn s(&self) -> String {
        format!("$<BlockDSP::Code>")
    }

    fn call_method(&self, key: &str, env: &mut Env) -> Result<VVal, StackAction> {
        let args = env.argv_ref();

        match key {
            "instanciate_at" => {
                arg_chk!(args, 4, "block_code.instanciate_at[area_id, $i(x, y), typ, user_input]");

                let id = env.arg(0).i() as usize;
                let x = env.arg(1).v_i(0);
                let y = env.arg(1).v_i(1);
                let input = env.arg(3);

                let input = if input.is_some() { Some(input.s_raw()) } else { None };

                println!("INSTANCIATE {}", env.arg(1).s());

                env.arg(2).with_s_ref(|typ| {
                    self.code.borrow_mut().instanciate_at(id, x, y, &typ, input).unwrap()
                });

                Ok(VVal::None)
            }
            _ => Ok(VVal::err_msg(&format!("Unknown method called: {}", key))),
        }
    }

    fn as_any(&mut self) -> &mut dyn std::any::Any {
        self
    }
    fn clone_ud(&self) -> Box<dyn vval::VValUserData> {
        Box::new(self.clone())
    }
}

pub fn vv2block_fun(mut v: VVal) -> Option<Rc<RefCell<BlockFun>>> {
    v.with_usr_ref(|model: &mut VValBlockFun| {
        let r: Rc<RefCell<BlockFun>> = model.code.clone();
        r
    })
}

pub fn vv2block_code_model(mut v: VVal) -> Option<Rc<RefCell<dyn BlockCodeView>>> {
    v.with_usr_ref(|model: &mut VValBlockFun| {
        let r: Rc<RefCell<dyn BlockCodeView>> = model.code.clone();
        r
    })
}
