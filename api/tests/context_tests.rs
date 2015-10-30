#[macro_use] extern crate log;
extern crate env_logger;

extern crate api;
extern crate common;

use api::TokamakContext;

#[test]
pub fn test_data_source() {
  env_logger::init().unwrap();
  
  let ctx = TokamakContext::new().ok().unwrap();
  
  assert!(ctx.get_type("i32").is_ok());
  assert!(ctx.get_type("f32").is_ok());
  
  assert_eq!(2, ctx.all_types().len());
} 