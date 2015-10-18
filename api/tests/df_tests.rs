extern crate api;
extern crate common;

extern crate rustc_serialize;

use rustc_serialize::Decodable;

use api::TokamakContext;
use api::df::{DataFrame,RandomGenerator};
use common::types::TypeId;

#[test]
pub fn test_data_source() {
  let ctx = TokamakContext::new().ok().unwrap();
  
  let df = ctx.from(RandomGenerator(vec!["int4", "int4"]));
  assert_eq!("from", df.kind());
  
  let selected = df.select(vec![]);
  assert_eq!("select", selected.kind());
  //let rnd: Box<DataSet> = RandomGenerator::new(&ctx, vec!["int4", "int4"]).ok().unwrap();
}

#[test]
pub fn test_head() {
  let ctx = TokamakContext::new().ok().unwrap();
  let df = ctx.from(RandomGenerator(vec!["int4", "float4"]));
  println!("{}", df.head().ok().unwrap());
}

struct Record<D: Decodable> {
  _phantom: ::std::marker::PhantomData<D>,
}

pub struct Xxx;
impl Xxx {
  fn decode<'a, D: Decodable>(&'a mut self) -> Option<Record<D>> {
    None
  }
}

#[test]
pub fn test_tuple() {
  let mut x = Xxx;
  
  let tuple: Option<Record<(i32, i32, String)>> = x.decode();
}