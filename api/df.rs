use uuid::Uuid;

use common::err::{Error, TResult};
use common::types::Type;

// let ctx = TokamakContext::new();
// ctx.from(RandomGenerator).select(...);

use super::TokamakContext;

pub struct DataFrame<'a> {
  pub ctx : &'a TokamakContext,
  pub kind: Kind<'a>
}

pub enum Kind<'a> {
  From(DataSource),
  Select(Box<DataFrame<'a>>, Vec<Expr>)
}

pub enum Expr {
  Plus,
  Field(String)
}

impl<'a> DataFrame<'a> {
  pub fn kind(&self) -> &'static str {
    match self.kind {
      Kind::From  (_) => "from",
      Kind::Select(_,_) => "select"
    } 
  }
  
  pub fn select(self, exprs: Vec<Expr>) -> DataFrame<'a> {
    DataFrame {ctx: self.ctx, kind: Kind::Select(Box::new(self), exprs)}
  }
  
  fn count(&self) -> TResult<usize> {
    Ok(0)
  } 
}

pub struct DataSource {
  src_type : String,
  schema   : Vec<String>
}

pub struct RndGenerator;

impl RndGenerator 
{
  pub fn new(types: Vec<&str>) -> DataSource
  {
    DataSource {
      src_type: "random".to_string(),
      schema  : types.into_iter().map(|s| s.to_string()).collect::<Vec<String>>()
    }
  }
}

fn typestr_to_schema(ctx: &TokamakContext, types: Vec<&str>) -> Vec<Box<Type>>
{
  types.iter()
    .map( |s| ctx.get_type(s).unwrap().clone_box() )
    .collect::<Vec<Box<Type>>>()
}

/*
pub struct DataSet {
  rnd: RandomGenerator;
}

impl DataSet
{
  fn name(&self) -> &str {
    &self.name
  }
  
  fn schema(&self) -> &Vec<Box<Type>>
  {
    &self.types
  }
  
  fn from(self) -> DataFrame<'a> {
    DataFrame::Dataset(&self as DataSet)
  }
}

pub trait Expr {
  fn name(&self) -> &str;
}

impl<'a> DataFrame<'a> {
//  // transform
//  fn select(&mut self, Vec<Box<Expr>>) -> &mut DataFrame;
//  // transform
//  fn filter(&mut self, Box<Expr>) -> &mut DataFrame;
  
  
  
  // action
  fn count(&mut self) -> TResult<usize> {
    Err(Error::InternalError)
  }
}
*/