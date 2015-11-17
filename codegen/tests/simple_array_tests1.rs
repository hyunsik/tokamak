#![feature(libc)]
extern crate libc;
extern crate llvm;

#[macro_use]
extern crate util;

use std::mem;
use std::ptr;

use llvm::*;
use llvm::Attribute::*;

pub struct Test {
	id: i32
}

#[test]
pub fn test() {
  let ctx = Context::new();
  let module = Module::new("simple", &ctx);
  
  // add(*mut i64, *const i64, *const i64, len: isize) -> void  
  let param_tys = vec![
    Type::pointer_ty(Type::get::<i64>(&ctx)),
  	Type::pointer_ty(Type::get::<i64>(&ctx)), 
  	Type::pointer_ty(Type::get::<i64>(&ctx)),
  	Type::get::<i64>(&ctx)
 	];
  let func_ty = Type::function_ty(Type::void_ty(&ctx), &param_tys);
  
  let func = module.add_function("map", func_ty);
  
  let res    = &func[0];  
  let vec1   = &func[1];
  let vec2   = &func[2];
  let rownum = &func[3];
  
  let entry     = func.append("entry");
  let loop_head = func.append("loop-head");
  let loop_cond = func.append("loop-cond");
  let loop_body = func.append("loop-body");
  let loop_end  = func.append("loop-end");
  
  let builder = Builder::new(&ctx);
  builder.position_at_end(entry);
  builder.create_br(loop_head);
  
  builder.position_at_end(loop_head);
  // i = 0
  let idx = builder.create_alloca(Type::get::<u64>(&ctx));
  builder.create_store(0i64.compile(&ctx), idx);
  builder.create_br(loop_cond);
  
  {
	  builder.position_at_end(loop_cond);
	  let idx_value = builder.create_load(idx);
	  let cond = builder.create_cmp(idx_value, 4i64.compile(&ctx), Predicate::LessThan);
	  builder.create_cond_br(cond, loop_body, Some(loop_end));
  }
  
  {
	  builder.position_at_end(loop_body);
	  let idx_value = builder.create_load(idx);
	  let idx_list_ptr = &vec![idx_value];
	  let lhs_ptr = builder.create_gep(vec1, idx_list_ptr);
	  let lhs_value = builder.create_load(lhs_ptr);
	  
	  let rhs_ptr = builder.create_gep(vec2, idx_list_ptr);
	  let rhs_value = builder.create_load(rhs_ptr);
	  
	  let lr_add = builder.create_add(lhs_value, rhs_value);
	  
	  let res_ptr = builder.create_gep(res, idx_list_ptr);
	  //builder.create_store(lr_add, res_ptr);
	  
	  let idx_add = builder.create_add(idx_value, 1i64.compile(&ctx));
	  builder.create_store(idx_add, idx);
	  builder.create_br(loop_cond);
  }
  
  builder.position_at_end(loop_end);
  builder.create_ret_void();
  
  println!("--------------");
  module.dump();
  println!("--------------");
  module.verify().unwrap();
  
  let lhs_vec = vec![1,2,3,4];
  let rhs_vec = vec![2,5,2,1];
  let mut res = vec![0,0,0,0];
  
  let ee = JitEngine::new(&module, JitOptions {opt_level: 0}).unwrap();
  ee.with_function(func, |map: extern fn((*mut i64, *const i64, *const i64, isize))| {      
      map((res.as_mut_ptr(), lhs_vec.as_ptr(), rhs_vec.as_ptr(), 4));
  });
}