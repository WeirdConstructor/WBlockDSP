use cranelift::prelude::types::F64;
use cranelift::prelude::InstBuilder;
use cranelift::prelude::*;
use cranelift_codegen::ir::immediates::Offset32;
use cranelift_codegen::settings::{self, Configurable};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::default_libcall_names;
use cranelift_module::{DataContext, FuncId, Linkage, Module};
use std::collections::HashMap;
use std::mem;
use std::slice;

#[derive(Debug, Clone, Copy)]
pub enum ASTBinOp {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
}

#[derive(Debug, Clone)]
pub struct ASTFun {
    params: Vec<String>,
    locals: Vec<String>,
    ast: Box<ASTNode>,
}

impl ASTFun {
    pub fn new(ast: Box<ASTNode>) -> Self {
        Self {
            params: vec![
                "in1".to_string(),
                "in2".to_string(),
                "alpha".to_string(),
                "beta".to_string(),
                "delta".to_string(),
                "gamma".to_string(),
                "&sig1".to_string(),
                "&sig2".to_string(),
                "&state".to_string(),
                "&fstate".to_string(),
            ],
            locals: vec![], // vec!["x".to_string(), "y".to_string()],
            ast,
        }
    }
}

#[derive(Debug, Clone)]
pub enum ASTNode {
    Lit(f64),
    Var(String),
    Assign(String, Box<ASTNode>),
    BinOp(ASTBinOp, Box<ASTNode>, Box<ASTNode>),
    If(Box<ASTNode>, Box<ASTNode>, Option<Box<ASTNode>>),
    Call(String, usize, Box<ASTNode>),
    Stmts(Vec<Box<ASTNode>>),
}

impl ASTNode {
    pub fn to_string(&self) -> String {
        match self {
            ASTNode::Lit(v) => format!("lit:{:6.4}", v),
            ASTNode::Var(v) => format!("var:{}", v),
            ASTNode::Assign(v, _) => format!("assign:{}", v),
            ASTNode::BinOp(op, _, _) => format!("binop:{:?}", op),
            ASTNode::If(_, _, _) => format!("if"),
            ASTNode::Call(fun, fs, _) => format!("call{}:{}", fs, fun),
            ASTNode::Stmts(stmts) => format!("stmts:{}", stmts.len()),
        }
    }

    pub fn typ_str(&self) -> &str {
        match self {
            ASTNode::Lit(v) => "lit",
            ASTNode::Var(v) => "var",
            ASTNode::Assign(v, _) => "assign",
            ASTNode::BinOp(op, _, _) => "binop",
            ASTNode::If(_, _, _) => "if",
            ASTNode::Call(fun, _, _) => "call",
            ASTNode::Stmts(stmts) => "stmts",
        }
    }

    pub fn dump(&self, indent: usize) -> String {
        let indent_str = "   ".repeat(indent + 1);
        let mut s = indent_str + &self.to_string() + "\n";

        match self {
            ASTNode::Lit(_) => (),
            ASTNode::Var(_) => (),
            ASTNode::Assign(_, e) => {
                s += &e.dump(indent + 1);
            }
            ASTNode::BinOp(_, a, b) => {
                s += &a.dump(indent + 1);
                s += &b.dump(indent + 1);
            }
            ASTNode::If(c, a, b) => {
                s += &c.dump(indent + 1);
                s += &a.dump(indent + 1);
                if let Some(n) = b {
                    s += &n.dump(indent + 1);
                }
            }
            ASTNode::Call(_, _, a) => {
                s += &a.dump(indent + 1);
            }
            ASTNode::Stmts(stmts) => {
                for n in stmts {
                    s += &n.dump(indent + 1);
                }
            }
        }

        s
    }
}

/// The basic JIT class.
pub struct JIT {
    /// The function builder context, which is reused across multiple
    /// FunctionBuilder instances.
    builder_context: FunctionBuilderContext,

    /// The main Cranelift context, which holds the state for codegen. Cranelift
    /// separates this from `Module` to allow for parallel compilation, with a
    /// context per thread, though this isn't in the simple demo here.
    ctx: codegen::Context,

    /// The data context, which is to data objects what `ctx` is to functions.
    data_ctx: DataContext,

    /// The module, with the jit backend, which manages the JIT'd
    /// functions.
    module: JITModule,
}

impl Default for JIT {
    fn default() -> Self {
        let mut flag_builder = settings::builder();
        flag_builder.set("use_colocated_libcalls", "false").unwrap();
        // FIXME set back to true once the x64 backend supports it.
        flag_builder.set("is_pic", "false").unwrap();
        let isa_builder = cranelift_native::builder().unwrap_or_else(|msg| {
            panic!("host machine is not supported: {}", msg);
        });
        let isa = isa_builder
            .finish(settings::Flags::new(flag_builder))
            .unwrap();
        let mut builder = JITBuilder::with_isa(isa, default_libcall_names());
        builder.symbol("test", test as *const u8);
        builder.symbol("sin", std::primitive::f64::sin as *const u8);

        let module = JITModule::new(builder);
        Self {
            builder_context: FunctionBuilderContext::new(),
            ctx: module.make_context(),
            data_ctx: DataContext::new(),
            module,
        }
    }
}

impl JIT {
    /// Compile a string in the toy language into machine code.
    pub fn compile(&mut self, prog: ASTFun) -> Result<*const u8, String> {
        let ptr_type = self.module.target_config().pointer_type();

        for param_name in &prog.params {
            if param_name.chars().next() == Some('&') {
                self.ctx.func.signature.params.push(AbiParam::new(ptr_type));
            } else {
                self.ctx.func.signature.params.push(AbiParam::new(F64));
            };
        }

        let id = self
            .module
            .declare_function("dsp", Linkage::Export, &self.ctx.func.signature)
            .map_err(|e| e.to_string())?;

        self.ctx.func.name = ExternalName::user(0, id.as_u32());

        // Then, translate the AST nodes into Cranelift IR.
        self.translate(prog)?;

        self.module
            .define_function(id, &mut self.ctx)
            .map_err(|e| e.to_string())?;

        self.module.clear_context(&mut self.ctx);
        self.module.finalize_definitions();

        let code = self.module.get_finalized_function(id);

        Ok(code)
    }

    //    /// Create a zero-initialized data section.
    //    pub fn create_data(&mut self, name: &str, contents: Vec<u8>) -> Result<&[u8], String> {
    //        // The steps here are analogous to `compile`, except that data is much
    //        // simpler than functions.
    //        self.data_ctx.define(contents.into_boxed_slice());
    //        let id = self
    //            .module
    //            .declare_data(name, Linkage::Export, true, false)
    //            .map_err(|e| e.to_string())?;
    //
    //        self.module
    //            .define_data(id, &self.data_ctx)
    //            .map_err(|e| e.to_string())?;
    //        self.data_ctx.clear();
    //        self.module.finalize_definitions();
    //        let buffer = self.module.get_finalized_data(id);
    //        // TODO: Can we move the unsafe into cranelift?
    //        Ok(unsafe { slice::from_raw_parts(buffer.0, buffer.1) })
    //    }

    // Translate from toy-language AST nodes into Cranelift IR.
    fn translate(&mut self, fun: ASTFun) -> Result<(), String> {
        let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_context);

        let mut trans = FunctionTranslator::new(builder, &mut self.module);
        trans.register_functions();

        let ret = trans.translate(fun);
        ret

        // Now translate the statements of the function body.
        //        let mut trans = FunctionTranslator {
        //            int,
        //            builder,
        //            variables,
        //            module: &mut self.module,
        //        };

        //        trans.translate_ast(fun.ast);

        //        // Set up the return variable of the function. Above, we declared a
        //        // variable to hold the return value. Here, we just do a use of that
        //        // variable.
        //        let return_variable = trans.variables.get(&the_return).unwrap();
        //        let return_value = trans.builder.use_var(*return_variable);
    }

    //    pub fn translate_ast_node(&mut self, builder: FunctionBuilder<'a>,
}

struct FunctionTranslator<'a> {
    builder: FunctionBuilder<'a>,
    variables: HashMap<String, Variable>,
    var_index: usize,
    module: &'a mut JITModule,
    func: Option<FuncId>,
    func2: Option<FuncId>,
    ptr_w: u32,
}

pub struct DSPState {
    pub x: f64,
    pub y: f64,
}

pub struct TSTState {
    pub l: f64,
}

pub fn test(x: f64, state: *mut DSPState, mystate: *mut std::ffi::c_void) -> f64 {
    unsafe {
        let p = mystate as *mut TSTState;
        (*state).x = x * 22.0;
        (*state).y = (*p).l;
    };
    x * 10000.0 + 1.0
}

impl<'a> FunctionTranslator<'a> {
    pub fn new(builder: FunctionBuilder<'a>, module: &'a mut JITModule) -> Self {
        Self {
            var_index: 0,
            variables: HashMap::new(),
            builder,
            module,
            func: None,
            func2: None,
            ptr_w: 8,
        }
    }

    pub fn register_functions(&mut self) {
        // TODO: manage these imports and signature stuff properly!
        //       also need some compiler error handling for this at some
        //       point!
        // (see also https://zmedley.com/calling-rust.html)
        let ptr_type = self.module.target_config().pointer_type();

        let mut sig = self.module.make_signature();
        sig.params.push(AbiParam::new(F64));
        sig.params.push(AbiParam::new(ptr_type));
        sig.params.push(AbiParam::new(ptr_type));
        sig.returns.push(AbiParam::new(F64));

        let mut sig2 = self.module.make_signature();
        sig2.params.push(AbiParam::new(F64));
        sig2.returns.push(AbiParam::new(F64));

        self.func = Some(
            self.module
                .declare_function("test", cranelift_module::Linkage::Import, &sig)
                .map_err(|e| e.to_string())
                .unwrap(),
        );
        self.func2 = Some(
            self.module
                .declare_function("sin", cranelift_module::Linkage::Import, &sig2)
                .map_err(|e| e.to_string())
                .unwrap(),
        );
    }

    /// Declare a single variable declaration.
    fn declare_variable(&mut self, typ: types::Type, name: &str) -> Variable {
        let var = Variable::new(self.var_index);
        println!("DECLARE {} = {}", name, self.var_index);

        if !self.variables.contains_key(name) {
            self.variables.insert(name.into(), var);
            self.builder.declare_var(var, typ);
            self.var_index += 1;
        }

        var
    }

    fn translate(&mut self, fun: ASTFun) -> Result<(), String> {
        let ptr_type = self.module.target_config().pointer_type();
        self.ptr_w = ptr_type.bytes();

        let entry_block = self.builder.create_block();
        self.builder
            .append_block_params_for_function_params(entry_block);
        self.builder.switch_to_block(entry_block);
        self.builder.seal_block(entry_block);

        self.variables.clear();

        // declare and define parameters:
        for (i, param_name) in fun.params.iter().enumerate() {
            let val = self.builder.block_params(entry_block)[i];
            let var = if param_name.chars().next() == Some('&') {
                self.declare_variable(ptr_type, param_name)
            } else {
                self.declare_variable(F64, param_name)
            };
            println!("DEF VAR: {}", param_name);
            self.builder.def_var(var, val);
        }

        // declare and define local variables:
        for (i, local_name) in fun.locals.iter().enumerate() {
            let zero = self.builder.ins().f64const(0.0);
            let var = self.declare_variable(F64, local_name);
            self.builder.def_var(var, zero);
        }

        self.compile(&fun.ast);

        self.builder.ins().return_(&[]);
        self.builder.finalize();

        println!("{}", self.builder.func.display());

        Ok(())
    }

    fn compile(&mut self, ast: &Box<ASTNode>) -> Value {
        let ptr_type = self.module.target_config().pointer_type();

        match ast.as_ref() {
            ASTNode::Lit(v) => self.builder.ins().f64const(*v),
            ASTNode::Var(name) => {
                let variable = self.variables.get(name).unwrap();

                if name.chars().next() == Some('&') {
                    let ptr = self.builder.use_var(*variable);
                    self.builder.ins().load(F64, MemFlags::new(), ptr, 0)
                } else {
                    self.builder.use_var(*variable)
                }
            }
            ASTNode::Assign(name, ast) => {
                let value = self.compile(ast);
                let variable = self.variables.get(name).unwrap();

                if name.chars().next() == Some('&') {
                    let ptr = self.builder.use_var(*variable);
                    self.builder.ins().store(MemFlags::new(), value, ptr, 0);
                } else {
                    self.builder.def_var(*variable, value);
                }

                value
            }
            ASTNode::BinOp(ASTBinOp::Add, a, b) => {
                let value_a = self.compile(a);
                let value_b = self.compile(b);
                self.builder.ins().fadd(value_a, value_b)
            }
            ASTNode::Call(name, fstate_index, arg) => {
                let value_arg = self.compile(arg);
                if name == "test" {
                    let ptr_type = self.module.target_config().pointer_type();
                    let state_var = self.variables.get("&state").unwrap();
                    let ptr = self.builder.use_var(*state_var);

                    let fstate_var = self.variables.get("&fstate").unwrap();
                    let fptr = self.builder.use_var(*fstate_var);
                    let func_state = self.builder.ins().load(
                        ptr_type,
                        MemFlags::new(),
                        fptr,
                        Offset32::new(*fstate_index as i32 * self.ptr_w as i32),
                    );

                    let local_callee = self
                        .module
                        .declare_func_in_func(self.func.unwrap(), &mut self.builder.func);
                    let call = self
                        .builder
                        .ins()
                        .call(local_callee, &[value_arg, ptr, func_state]);
                    self.builder.inst_results(call)[0]
                } else {
                    let local_callee = self
                        .module
                        .declare_func_in_func(self.func2.unwrap(), &mut self.builder.func);
                    let call = self.builder.ins().call(local_callee, &[value_arg]);
                    self.builder.inst_results(call)[0]
                }
            }
            ASTNode::If(cond, then, els) => {
                let res = self.compile(cond);
                let cmpv = self.builder.ins().f64const(0.5);
                let condition_value =
                    self.builder
                        .ins()
                        .fcmp(FloatCC::GreaterThanOrEqual, res, cmpv);

                let then_block = self.builder.create_block();
                let else_block = self.builder.create_block();
                let merge_block = self.builder.create_block();

                // If-else constructs in the toy language have a return value.
                // In traditional SSA form, this would produce a PHI between
                // the then and else bodies. Cranelift uses block parameters,
                // so set up a parameter in the merge block, and we'll pass
                // the return values to it from the branches.
                self.builder.append_block_param(merge_block, F64);

                // Test the if condition and conditionally branch.
                self.builder.ins().brz(condition_value, else_block, &[]);
                // Fall through to then block.
                self.builder.ins().jump(then_block, &[]);

                self.builder.switch_to_block(then_block);
                self.builder.seal_block(then_block);
                let then_return = self.compile(then);

                // Jump to the merge block, passing it the block return value.
                self.builder.ins().jump(merge_block, &[then_return]);

                self.builder.switch_to_block(else_block);
                self.builder.seal_block(else_block);
                let else_return = if let Some(els) = els {
                    self.compile(els)
                } else {
                    self.builder.ins().f64const(0.0)
                };

                // Jump to the merge block, passing it the block return value.
                self.builder.ins().jump(merge_block, &[else_return]);

                // Switch to the merge block for subsequent statements.
                self.builder.switch_to_block(merge_block);

                // We've now seen all the predecessors of the merge block.
                self.builder.seal_block(merge_block);

                // Read the value of the if-else by reading the merge block
                // parameter.
                let phi = self.builder.block_params(merge_block)[0];

                phi
            }
            ASTNode::Stmts(stmts) => {
                let mut value = None;
                for ast in stmts {
                    value = Some(self.compile(ast));
                }
                if let Some(value) = value {
                    value
                } else {
                    self.builder.ins().f64const(0.0)
                }
            }
            _ => self.builder.ins().f64const(42.23),
            //    ASTNode::Lit(f64),
            //    ASTNode::Var(String),
            //    ASTNode::Assign(String),
            //    ASTNode::BinOp(ASTBinOp, Box<ASTNode>, Box<ASTNode>),
            //    ASTNode::If(Box<ASTNode>, Box<ASTNode>, Option<Box<ASTNode>>),
            //    ASTNode::Stmts(Vec<Box<ASTNode>>),
        }
    }
}

//
//impl<'a> FunctionTranslator<'a> {
//
//
//
//
//    fn translate_expr(&mut self, expr: Expr) -> Value {
//        match expr {
//            Expr::Literal(literal) => {
//                let imm: i32 = literal.parse().unwrap();
//                self.builder.ins().iconst(self.int, i64::from(imm))
//            }
//
//            Expr::Add(lhs, rhs) => {
//                let lhs = self.translate_expr(*lhs);
//                let rhs = self.translate_expr(*rhs);
//                self.builder.ins().iadd(lhs, rhs)
//            }
//
//            Expr::Sub(lhs, rhs) => {
//                let lhs = self.translate_expr(*lhs);
//                let rhs = self.translate_expr(*rhs);
//                self.builder.ins().isub(lhs, rhs)
//            }
//
//            Expr::Mul(lhs, rhs) => {
//                let lhs = self.translate_expr(*lhs);
//                let rhs = self.translate_expr(*rhs);
//                self.builder.ins().imul(lhs, rhs)
//            }
//
//            Expr::Div(lhs, rhs) => {
//                let lhs = self.translate_expr(*lhs);
//                let rhs = self.translate_expr(*rhs);
//                self.builder.ins().udiv(lhs, rhs)
//            }
//
//            Expr::Eq(lhs, rhs) => self.translate_icmp(IntCC::Equal, *lhs, *rhs),
//            Expr::Ne(lhs, rhs) => self.translate_icmp(IntCC::NotEqual, *lhs, *rhs),
//            Expr::Lt(lhs, rhs) => self.translate_icmp(IntCC::SignedLessThan, *lhs, *rhs),
//            Expr::Le(lhs, rhs) => self.translate_icmp(IntCC::SignedLessThanOrEqual, *lhs, *rhs),
//            Expr::Gt(lhs, rhs) => self.translate_icmp(IntCC::SignedGreaterThan, *lhs, *rhs),
//            Expr::Ge(lhs, rhs) => self.translate_icmp(IntCC::SignedGreaterThanOrEqual, *lhs, *rhs),
////            Expr::Call(name, args) => self.translate_call(name, args),
////            Expr::GlobalDataAddr(name) => self.translate_global_data_addr(name),
//            Expr::Identifier(name) => {
//                // `use_var` is used to read the value of a variable.
//                let variable = self.variables.get(&name).expect("variable not defined");
//                self.builder.use_var(*variable)
//            }
//            Expr::Assign(name, expr) => self.translate_assign(name, *expr),
//            Expr::IfElse(condition, then_body, else_body) => {
//                self.translate_if_else(*condition, then_body, else_body)
//            }
////            Expr::WhileLoop(condition, loop_body) => {
////                self.translate_while_loop(*condition, loop_body)
////            }
//        }
//    }
//
//    fn translate_assign(&mut self, name: String, expr: Expr) -> Value {
//        // `def_var` is used to write the value of a variable. Note that
//        // variables can have multiple definitions. Cranelift will
//        // convert them into SSA form for itself automatically.
//        let new_value = self.translate_expr(expr);
//        let variable = self.variables.get(&name).unwrap();
//        self.builder.def_var(*variable, new_value);
//        new_value
//    }
//
//    fn translate_icmp(&mut self, cmp: IntCC, lhs: Expr, rhs: Expr) -> Value {
//        let lhs = self.translate_expr(lhs);
//        let rhs = self.translate_expr(rhs);
//        let c = self.builder.ins().icmp(cmp, lhs, rhs);
//        self.builder.ins().bint(self.int, c)
//    }
//
//    fn translate_if_else(
//        &mut self,
//        condition: Expr,
//        then_body: Vec<Expr>,
//        else_body: Vec<Expr>,
//    ) -> Value {
//        let condition_value = self.translate_expr(condition);
//
//        let then_block = self.builder.create_block();
//        let else_block = self.builder.create_block();
//        let merge_block = self.builder.create_block();
//
//        // If-else constructs in the toy language have a return value.
//        // In traditional SSA form, this would produce a PHI between
//        // the then and else bodies. Cranelift uses block parameters,
//        // so set up a parameter in the merge block, and we'll pass
//        // the return values to it from the branches.
//        self.builder.append_block_param(merge_block, self.int);
//
//        // Test the if condition and conditionally branch.
//        self.builder.ins().brz(condition_value, else_block, &[]);
//        // Fall through to then block.
//        self.builder.ins().jump(then_block, &[]);
//
//        self.builder.switch_to_block(then_block);
//        self.builder.seal_block(then_block);
//        let mut then_return = self.builder.ins().iconst(self.int, 0);
//        for expr in then_body {
//            then_return = self.translate_expr(expr);
//        }
//
//        // Jump to the merge block, passing it the block return value.
//        self.builder.ins().jump(merge_block, &[then_return]);
//
//        self.builder.switch_to_block(else_block);
//        self.builder.seal_block(else_block);
//        let mut else_return = self.builder.ins().iconst(self.int, 0);
//        for expr in else_body {
//            else_return = self.translate_expr(expr);
//        }
//
//        // Jump to the merge block, passing it the block return value.
//        self.builder.ins().jump(merge_block, &[else_return]);
//
//        // Switch to the merge block for subsequent statements.
//        self.builder.switch_to_block(merge_block);
//
//        // We've now seen all the predecessors of the merge block.
//        self.builder.seal_block(merge_block);
//
//        // Read the value of the if-else by reading the merge block
//        // parameter.
//        let phi = self.builder.block_params(merge_block)[0];
//
//        phi
//    }
//
//    fn translate_while_loop(&mut self, condition: Expr, loop_body: Vec<Expr>) -> Value {
//        let header_block = self.builder.create_block();
//        let body_block = self.builder.create_block();
//        let exit_block = self.builder.create_block();
//
//        self.builder.ins().jump(header_block, &[]);
//        self.builder.switch_to_block(header_block);
//
//        let condition_value = self.translate_expr(condition);
//        self.builder.ins().brz(condition_value, exit_block, &[]);
//        self.builder.ins().jump(body_block, &[]);
//
//        self.builder.switch_to_block(body_block);
//        self.builder.seal_block(body_block);
//
//        for expr in loop_body {
//            self.translate_expr(expr);
//        }
//        self.builder.ins().jump(header_block, &[]);
//
//        self.builder.switch_to_block(exit_block);
//
//        // We've reached the bottom of the loop, so there will be no
//        // more backedges to the header to exits to the bottom.
//        self.builder.seal_block(header_block);
//        self.builder.seal_block(exit_block);
//
//        // Just return 0 for now.
//        self.builder.ins().iconst(self.int, 0)
//    }
//
//    fn translate_call(&mut self, name: String, args: Vec<Expr>) -> Value {
//        let mut sig = self.module.make_signature();
//
//        // Add a parameter for each argument.
//        for _arg in &args {
//            sig.params.push(AbiParam::new(self.int));
//        }
//
//        // For simplicity for now, just make all calls return a single I64.
//        sig.returns.push(AbiParam::new(self.int));
//
//        // TODO: Streamline the API here?
//        let callee = self
//            .module
//            .declare_function(&name, Linkage::Import, &sig)
//            .expect("problem declaring function");
//        let local_callee = self
//            .module
//            .declare_func_in_func(callee, &mut self.builder.func);
//
//        let mut arg_values = Vec::new();
//        for arg in args {
//            arg_values.push(self.translate_expr(arg))
//        }
//        let call = self.builder.ins().call(local_callee, &arg_values);
//        self.builder.inst_results(call)[0]
//    }
//
//    fn translate_global_data_addr(&mut self, name: String) -> Value {
//        let sym = self
//            .module
//            .declare_data(&name, Linkage::Export, true, false)
//            .expect("problem declaring data object");
//        let local_id = self
//            .module
//            .declare_data_in_func(sym, &mut self.builder.func);
//
//        let pointer = self.module.target_config().pointer_type();
//        self.builder.ins().symbol_value(pointer, local_id)
//    }
//}
