// Copyright (c) 2021 Weird Constructor <weirdconstructor@gmail.com>
// This file is a part of WBlockDSP. Released under GPL-3.0-or-later.
// See README.md and COPYING for details.

use crate::jit::ASTNode;

use wlambda::*;
use crate::arg_chk;

impl VValUserData for Box<ASTNode> {
    fn s(&self) -> String { format!("$<JITASTNode:{:?}>", self.to_string()) }

    fn call_method(&self, key: &str, env: &mut Env)
        -> Result<VVal, StackAction>
    {
        let args = env.argv_ref();

        match key {
            "type" => {
                arg_chk!(args, 0, "jit_ast_node.type[]");
                Ok(VVal::new_str(self.typ_str()))
            },
            "dump" => {
                arg_chk!(args, 0, "jit_ast_node.dump[]");
                Ok(VVal::new_str_mv(self.dump(0)))
            },
            _ => Ok(VVal::err_msg(&format!("Unknown method called: {}", key))),
        }
    }

    fn as_any(&mut self) -> &mut dyn std::any::Any { self }
    fn clone_ud(&self) -> Box<dyn vval::VValUserData> { Box::new(self.clone()) }
}

pub fn vv2ast_node(v: VVal) -> Option<Box<ASTNode>> {
    if v.iter_over_vvals() {
        v.v_(0).with_s_ref(|s| {
            match s {
                "assign" => {
                    Some(Box::new(ASTNode::Assign(
                        v.v_s_raw(1),
                        vv2ast_node(v.v_(2))?)))
                },
                "binop" => {
                    None
                },
                _ => { None },
            }
        })
    } else {
        if v.is_int() || v.is_float() {
            Some(Box::new(ASTNode::Lit(v.f())))
        } else {
            Some(Box::new(ASTNode::Var(v.s_raw())))
        }
    }
}
