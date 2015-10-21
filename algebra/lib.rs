//!
//! Plan
//!

extern crate common;

use std::fmt::{Display, Formatter, Result};

use common::err::{Void, void_ok};

pub struct DataSetDecl;

pub enum Operator {
  DataSet   (Box<DataSetDecl>),
  Project   (Box<Operator>, Vec<Operator>),                // child, exprs
  Aggregate (Box<Operator>, Vec<Operator>, Vec<Operator>), // child, keys, exprs
  Filter    (Box<Operator>, Vec<Operator>),                // child, bool exprs in a CNF form
  Head      (Box<Operator>, usize),                        // child, row number to fetch
  Tail      (Box<Operator>, usize),                        // child, row number to fetch
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
pub fn walk_op<'v, T, V: Visitor<'v, T>>(v: &V, ctx: &mut T, op: &'v Operator) {
  match *op {
    Operator::DataSet  (ref ds)                        => { v.visit_dataset(ctx, &**ds)},
    Operator::Project  (ref child,ref exprs)           => { v.visit_project(ctx, &**child, exprs) },
    Operator::Filter   (ref child,ref filters)         => { v.visit_filter(ctx, &**child, filters) },
    Operator::Aggregate(ref child,ref keys, ref exprs) => { v.visit_aggregate(ctx, &**child, keys, exprs) },
    Operator::Head     (ref child,num)                 => { v.visit_head(ctx, &**child,num) },
    Operator::Tail      (ref child,num)                => { v.visit_tail(ctx, &**child,num) },
  }
}