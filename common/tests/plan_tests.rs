#![allow(dead_code)]
#![allow(unused_variables)]

/*
extern crate common;

use std::fmt::{Display, Formatter, Result};
use common::plan::*;

struct VisitOrder
{
  pub order: Vec<String>
}

impl<'v> Visitor<'v> for VisitOrder 
{
  fn visit_from(&mut self, ds: &DataSet) 
  {
    self.order.push("from".to_string());
  }

  fn visit_head(&mut self, child: &Plan, rownum: usize) 
  {
    walk_plan(self, child);
    self.order.push("head".to_string());
  }
  
  fn visit_tail(&mut self, child: &Plan, rownum: usize) 
  {
    walk_plan(self, child);
    self.order.push("tail".to_string());
  }
}

struct DummyDataSet;

impl DataSet for DummyDataSet 
{
  fn name(&self) -> &str {
    "dummy"  
  }
}
  
impl Display for DummyDataSet 
{
  fn fmt(&self, f: &mut Formatter) -> Result 
  {
    write!(f, "dummy")
  }
}

fn create_test_plan() -> Plan {
  Plan::Tail(Box::new(Plan::Head( Box::new(Plan::From(Box::new(DummyDataSet))), 2)), 3)
}

#[test]
fn test_visitor() {
  let mut visitor = VisitOrder {order: Vec::new()};
  walk_plan(&mut visitor, &create_test_plan());
  assert_eq!(vec!["from", "head", "tail"], visitor.order);
}

*/