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
  let loop_body = func.append("loop-body");
  let loop_tail = func.append("loop-tail");
  
  let zero = 0i64.compile(&ctx);
  
  let builder = Builder::new(&ctx);
  builder.position_at_end(entry);
  {
  	let cond = builder.create_cmp(zero, rownum, Predicate::LessThan);
  	builder.create_cond_br(cond, loop_body, Some(loop_tail));
  }
  
  {
	  builder.position_at_end(loop_body);
	  let mut phi = builder.create_phi(Type::get::<i64>(&ctx), "loop-cond");
	  phi.add_incoming(zero, entry);
	  let increment = builder.create_add(phi, 1i64.compile(&ctx));
	  phi.add_incoming(increment, loop_body);
	  
	  //let phi_as_val: &Value = ;
	  let idx_ptr: &Vec<&Value> = &vec![&*phi];
	  let lhs_ptr = builder.create_gep(vec1, idx_ptr);
	  let rhs_ptr = builder.create_gep(vec2, idx_ptr);
	  let res_ptr = builder.create_gep(res,  idx_ptr); 
	  
	  let lhs_value = builder.create_load(lhs_ptr);
	  let rhs_value = builder.create_load(rhs_ptr);
	  let add = builder.create_add(lhs_value, rhs_value);
	  builder.create_store(add, res_ptr);
	  
	  let cond = builder.create_cmp(phi, rownum, Predicate::LessThan);
	  builder.create_cond_br(cond, loop_body, Some(loop_tail));
  }
  
  builder.position_at_end(loop_tail);
  builder.create_ret_void();
  
  println!("--------------");
  module.dump();
  println!("--------------");
  module.verify().unwrap();
  
  let lhs_vec = vec![1,2,3,4];
  let rhs_vec = vec![2,5,2,1];
  let mut res = vec![0,0,0,0];
  
  let ee = JitEngine::new(&module, JitOptions {opt_level: 0}).unwrap();
  
  let map: fn(*mut i64, *const i64, *const i64, i64);
  map = match unsafe { ee.get_function_raw(func) } {
  	Some(f) => unsafe { mem::transmute(f)},
  	_       => panic!("SS")
  };
  
  map(res.as_mut_ptr(), lhs_vec.as_ptr(), rhs_vec.as_ptr(), 4i64);
  
  let expected = vec![3,7,5,5];
  assert_eq!(expected, res);
}