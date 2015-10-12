extern crate api;
extern crate common;

use api::TokamakContext;
use api::df::{DataSet,RandomGenerator};
use common::types::TypeId;

#[test]
pub fn test_data_source() {
  let ctx = TokamakContext::new().ok().unwrap();
  
  let rnd: Box<DataSet> = RandomGenerator::new(&ctx, vec!["int4", "int4"]).ok().unwrap();
}