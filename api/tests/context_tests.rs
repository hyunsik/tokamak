extern crate api;
extern crate common;

use api::TokamakContext;
use common::types::TypeId;

#[test]
pub fn test_types() {
  let ctx = TokamakContext::new().ok().unwrap();
  
  let ty_id = TypeId {base: "int4".to_string()};
  assert!(ctx.get_type(&ty_id).is_some());
} 