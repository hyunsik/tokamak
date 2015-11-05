use std::cell::RefCell;
use std::rc::Rc;
use std::collections::HashMap;
use std::fmt;

use algebra::{Operator, Visitor, walk_op};
use common::dataset::DataSet;
use common::err::Result;
use common::plugin::{PluginManager, TypeRegistry, FuncRegistry};
use common::session::Session;

use node::*;

pub struct LogicalPlanner;

impl LogicalPlanner
{
  pub fn new() -> LogicalPlanner 
  {
    LogicalPlanner
  }
  
  pub fn build(&self, 
  	type_registry: &TypeRegistry,
  	func_registry: &FuncRegistry, 
  	session: &Session, 
  	algebra: &Operator) -> Result<LogicalPlan>
  {
    let mut builder = PlanBuilder::new();
    walk_op(self, &mut builder, algebra); 
    
    builder.build()
  } 
}

pub struct PlanBuilder {
  stack: Vec<PlanNode>,
  seq  : u32
}

impl PlanBuilder 
{
  pub fn new() -> PlanBuilder 
  {
    PlanBuilder {stack: Vec::new(), seq: 0}
  }
  
  pub fn push(&mut self, node: PlanNode) {
  	self.stack.push(node);
  }
  
  pub fn seq(&mut self) -> u32 {
  	let next = self.seq;
  	self.seq = self.seq + 1;
  	
  	next
  }
  
  pub fn build(&mut self) -> Result<LogicalPlan> 
  {
  	debug_assert!(self.stack.len() == 1, "empty or remain nodes in the stack");
  	
  	Ok(LogicalPlan {root: self.stack.pop().unwrap()})
  }
}

impl<'v> Visitor<'v, PlanBuilder> for LogicalPlanner {
  fn visit_dataset(&self, builder: &mut PlanBuilder, dataset: &'v DataSet) {
  	let scan = PlanNode {
  		id: builder.seq(),
  		decl: NodeDecl::Relation(dataset.clone())
  	};
  	
  	builder.push(scan);
  }
}

#[derive(Clone)]
pub struct LogicalPlan {
  root: PlanNode
}

impl LogicalPlan 
{
	pub fn root(&self) -> &PlanNode {
		&self.root
	}
}