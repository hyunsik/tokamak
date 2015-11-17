#![feature(libc)]

extern crate libc;
extern crate llvm;

#[macro_use]
extern crate util;

use llvm::*;
use llvm::Attribute::*;

#[test]
pub fn test() {
  let ctx = Context::new();
  let module = Module::new("simple", &ctx);
  let func = module.add_function("fib", Type::get::<fn(u64) -> u64>(&ctx));
  func.add_attributes(&[NoUnwind, ReadNone]);
  let value = &func[0];
  
  let entry    = func.append("entry");
  let then_bb  = func.append("then_block");
  let else_bb  = func.append("else_block");
  let merge_bb = func.append("merge_bb");
  
  let builder = Builder::new(&ctx);
  builder.position_at_end(entry);
  
  let local = builder.create_alloca(Type::get::<u64>(&ctx));
  
  let local1 = builder.create_alloca(Type::get::<u64>(&ctx));
  builder.create_store(8u64.compile(&ctx), local1);
  let local2 = builder.create_alloca(Type::get::<u64>(&ctx));
  builder.create_store(16u64.compile(&ctx), local2);
  
  let cond = builder.create_cmp(value, 5u64.compile(&ctx), Predicate::GreaterThan);
  builder.create_cond_br(cond, then_bb, Some(else_bb));
  
  builder.position_at_end(then_bb);
  let eight = builder.create_load(local1);
  builder.create_br(merge_bb);
  
  builder.position_at_end(else_bb);
  let sixteen = builder.create_load(local2);
  builder.create_br(merge_bb);
  
  builder.position_at_end(merge_bb);
  let mut phi = builder.create_phi(Type::get::<u64>(&ctx), "cond");
  phi.add_incoming(eight, then_bb);
  phi.add_incoming(sixteen, else_bb);
  builder.create_ret(phi);
  
  module.verify().unwrap();
  
  let ee = JitEngine::new(&module, JitOptions {opt_level: 0}).unwrap();
  ee.with_function(func, |fib: extern fn(u64) -> u64| {
      for i in 0..10 {
          println!("fib {} = {}", i, fib(i))
      }
  });
}