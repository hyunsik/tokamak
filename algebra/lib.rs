//!
//! Plan
//!

use std::fmt;
use std::result::Result;

pub enum AlgebraError 
{
  EmptyStack,
  MismatchedStackType,
  NotConsumedStackItem
}

#[derive(Clone)]
pub struct DataSetDecl;

#[derive(Clone)]
pub enum JoinType
{
  INNER,
  LeftOuter,
  RightOuter,
  FullOuter
}

#[derive(Clone)]
pub enum Operator 
{
  DataSet   (Box<DataSetDecl>),
  Project   (Box<Operator>, Vec<Operator>),                // child, exprs
  Filter    (Box<Operator>, Vec<Operator>),                // child, bool exprs in a CNF form
  Join      (JoinType, Box<Operator>, Box<Operator>, Vec<Operator>), // join type, left, right, join condition
  Aggregate (Box<Operator>, Vec<Operator>, Vec<Operator>), // child, keys, exprs    
  Head      (Box<Operator>, usize),                        // child, row number to fetch
  Tail      (Box<Operator>, usize),                        // child, row number to fetch
}

pub struct AlgebraBuilder 
{
  stack: Vec<Operator>
}

impl AlgebraBuilder
{
  #[inline]
  pub fn new() -> AlgebraBuilder 
  {
    AlgebraBuilder {
      stack: Vec::new()
    }
  }
  
  #[inline]
  pub fn build(mut self) -> Result<Operator, AlgebraError>
  {
    match self.stack.len() {
      0 => { Err(AlgebraError::EmptyStack) },
      1 => { Ok(self.stack.pop().unwrap()) },
      _ => { Err(AlgebraError::NotConsumedStackItem) }
    }
  }
  
  #[inline]
  pub fn push(&mut self, op: Operator) -> &mut AlgebraBuilder
  {
    self.stack.push(op);
    self
  }
  
  pub fn dataset(&mut self, dataset: Box<DataSetDecl>) -> &mut AlgebraBuilder 
  {
    self.push(Operator::DataSet(dataset));
    self    
  } 
  
  
  #[inline]
  pub fn filter(&mut self, op: Operator) -> &mut AlgebraBuilder 
  {
    self.push(op)
  }
  
  pub fn join(
      &mut self, 
      join_type: JoinType,  
      cond: Vec<Operator>) -> &mut AlgebraBuilder 
  {
    debug_assert!(self.stack.len() > 1);
    let left = self.stack.pop().unwrap();
    let right = self.stack.pop().unwrap();
    self.push(Operator::Join(join_type, Box::new(left), Box::new(right), cond));
    self
  }
  
  pub fn join_with(
      &mut self, 
      join_type: JoinType, 
      right: Operator, 
      cond: Vec<Operator>) -> &mut AlgebraBuilder
  {
    debug_assert!(self.stack.len() > 0);
    let left = self.stack.pop().unwrap();
    self.push(Operator::Join(join_type, Box::new(left), Box::new(right), cond));
    self
  }
}

impl fmt::Display for AlgebraBuilder {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
      write!(f, "stack={}", self.stack.len())
    }
}

/// Visitor for Expr Tree
#[allow(unused_variables)]
pub trait Visitor<'v, T>: Sized {
 
  fn visit_dataset(
      &self, 
      &mut T, 
      dataset: &'v DataSetDecl) {}
 
  fn visit_project(
      &self, 
      context: &mut T, 
      child: &'v Operator, 
      exprs: &Vec<Operator>) {
      
    walk_op(self, context, child);
  }
  
  fn visit_filter(
      &self, 
      context: 
      &mut T, 
      child: &'v Operator, 
      filter: &Vec<Operator>) {
      
    walk_op(self, context, child);
  }

  fn visit_join(
      &self, 
      context: 
      &mut T,
      join_type: &JoinType,
      left: &'v Operator,
      right: &'v Operator, 
      cond: &Vec<Operator>) {
    
    walk_op(self, context, left);  
    walk_op(self, context, right);
  }        

  fn visit_aggregate(
      &self, 
      context: 
      &mut T, 
      child: &'v Operator, 
      keys: &Vec<Operator>,
      exprs:&Vec<Operator>) {
      
    walk_op(self, context, child);
  }      
      
  
  fn visit_head(
      &self, 
      context: &mut T, 
      child: &'v Operator, 
      fetch_row: usize) {
        
    walk_op(self, context, child);
  }
  
  fn visit_tail(
      &self, 
      context: &mut T, 
      child: &'v Operator, 
      fetch_row: usize) {
        
    walk_op(self, context, child);
  }
}

/// Walker for Expr Tree
pub fn walk_op<'v, T, V>(v: &V, ctx: &mut T, op: &'v Operator) 
    where V: Visitor<'v, T> {
  match *op {
    Operator::DataSet  (ref ds)                        => { v.visit_dataset(ctx, &**ds)},
    Operator::Project  (ref child,ref exprs)           => { v.visit_project(ctx, &**child, exprs) },
    Operator::Filter   (ref child,ref filters)         => { v.visit_filter(ctx, &**child, filters) },
    Operator::Aggregate(ref child,ref keys, ref exprs) => { v.visit_aggregate(ctx, &**child, keys, exprs) },
    Operator::Join     (ref join_type, ref left, ref right, ref cond) => {  v.visit_join(ctx, join_type, &**left, &**right, cond) },
    Operator::Head     (ref child,num)                 => { v.visit_head(ctx, &**child,num) },
    Operator::Tail     (ref child,num)                => { v.visit_tail(ctx, &**child,num) },
  }
}