extern crate api;
extern crate common;

use api::TokamakContext;

#[test]
pub fn test_data_source() {
  let ctx = TokamakContext::new().ok().unwrap();
  
  assert!(ctx.get_type("int4").is_ok());
  assert!(ctx.get_type("float4").is_ok());
  
  assert_eq!(2, ctx.all_types().len());
} 