extern crate api;
extern crate common;

use api::TokamakContext;
use api::df::{DataFrame,RndGenerator};
use common::types::TypeId;

#[test]
pub fn test_data_source() {
  let ctx = TokamakContext::new().ok().unwrap();
  
  let dataset = RndGenerator::new(vec!["int4", "int4"]);
  let df = ctx.from(dataset);
  assert_eq!("from", df.kind());
  
  let selected = df.select(vec![]);
  assert_eq!("select", selected.kind());
  //let rnd: Box<DataSet> = RandomGenerator::new(&ctx, vec!["int4", "int4"]).ok().unwrap();
}