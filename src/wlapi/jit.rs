// Copyright (c) 2021 Weird Constructor <weirdconstructor@gmail.com>
// This file is a part of WBlockDSP. Released under GPL-3.0-or-later.
// See README.md and COPYING for details.

use crate::ast::{ASTBinOp, ASTNode};

use crate::arg_chk;
use wlambda::*;

impl VValUserData for Box<ASTNode> {
    fn s(&self) -> String {
        format!("$<JIT::ASTNode:{}>", self.to_string())
    }

    fn call_method(&self, key: &str, env: &mut Env) -> Result<VVal, StackAction> {
        let args = env.argv_ref();

        match key {
            "type" => {
                arg_chk!(args, 0, "jit_ast_node.type[]");
                Ok(VVal::new_str(self.typ_str()))
            }
            "dump" => {
                arg_chk!(args, 0, "jit_ast_node.dump[]");
                Ok(VVal::new_str_mv(self.dump(0)))
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

#[derive(Debug, Clone)]
pub enum ASTError {
    UnknownForm(String),
    UnknownBinOp(String),
    BadUserValue(String),
}

pub fn vv2ast_node(mut v: VVal) -> Result<Box<ASTNode>, ASTError> {
    if let VVal::Usr(_) = v {
        if let Some(node) = v.with_usr_ref(|node: &mut Box<ASTNode>| node.clone()) {
            Ok(node)
        } else {
            Err(ASTError::BadUserValue(v.s()))
        }
    } else if v.iter_over_vvals() {
        v.v_(0).with_s_ref(|s| match s {
            "assign" => Ok(Box::new(ASTNode::Assign(v.v_s_raw(1), vv2ast_node(v.v_(2))?))),
            "stmts" => {
                let mut stmts = vec![];
                for i in 1..v.len() {
                    stmts.push(vv2ast_node(v.v_(i))?);
                }
                Ok(Box::new(ASTNode::Stmts(stmts)))
            }
            "if" => {
                if v.len() == 4 {
                    Ok(Box::new(ASTNode::If(
                        vv2ast_node(v.v_(1))?,
                        vv2ast_node(v.v_(2))?,
                        Some(vv2ast_node(v.v_(3))?),
                    )))
                } else {
                    Ok(Box::new(ASTNode::If(vv2ast_node(v.v_(1))?, vv2ast_node(v.v_(2))?, None)))
                }
            }
            "call" => {
                if v.len() == 4 {
                    if v.v_(3).is_vec() {
                        let mut args = vec![];
                        v.v_(3).with_iter(|iter| {
                            for (v, _) in iter {
                                args.push(vv2ast_node(v)?);
                            }
                            Ok(())
                        })?;
                        Ok(Box::new(ASTNode::Call(v.v_s_raw(1), v.v_i(2) as usize, args)))
                    } else {
                        Ok(Box::new(ASTNode::Call(
                            v.v_s_raw(1),
                            v.v_i(2) as usize,
                            vec![vv2ast_node(v.v_(3))?],
                        )))
                    }
                } else if v.len() == 3 {
                    Ok(Box::new(ASTNode::Call(v.v_s_raw(1), 0, vec![vv2ast_node(v.v_(2))?])))
                } else {
                    Ok(Box::new(ASTNode::Call(v.v_s_raw(1), 0, vec![Box::new(ASTNode::Lit(0.0))])))
                }
            }
            "binop" => {
                let op = match &v.v_s_raw(1)[..] {
                    "add" => ASTBinOp::Add,
                    "sub" => ASTBinOp::Sub,
                    "mul" => ASTBinOp::Mul,
                    "div" => ASTBinOp::Div,
                    "eq" => ASTBinOp::Eq,
                    "ne" => ASTBinOp::Ne,
                    "lt" => ASTBinOp::Lt,
                    "le" => ASTBinOp::Le,
                    "gt" => ASTBinOp::Gt,
                    "ge" => ASTBinOp::Ge,
                    _ => return Err(ASTError::UnknownBinOp(v.s())),
                };
                let a = vv2ast_node(v.v_(2))?;
                let b = vv2ast_node(v.v_(3))?;

                Ok(Box::new(ASTNode::BinOp(op, a, b)))
            }
            _ => Err(ASTError::UnknownForm(v.s())),
        })
    } else {
        if v.is_int() || v.is_float() {
            Ok(Box::new(ASTNode::Lit(v.f())))
        } else {
            Ok(Box::new(ASTNode::Var(v.s_raw())))
        }
    }
}

pub fn setup_jit_module() -> wlambda::SymbolTable {
    let mut st = wlambda::SymbolTable::new();

    st.fun(
        "node",
        move |env: &mut Env, _argc: usize| match vv2ast_node(env.arg(0)) {
            Ok(node) => Ok(VVal::new_usr(node)),
            Err(e) => {
                return Err(StackAction::panic_msg(format!(
                    "Invalid $<JIT::ASTNode> value: {:?}",
                    e
                )));
            }
        },
        Some(1),
        Some(1),
        false,
    );
    st
}
