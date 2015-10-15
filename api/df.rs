use uuid::Uuid;

use common::err::{Error, TResult};
use common::types::Type;
use common::plan::{CustomDataSource, DataSet, Expr, Plan};

// let ctx = TokamakContext::new();
// ctx.from(RandomGenerator).select(...);

use super::TokamakContext;

pub struct DataFrame<'a> {
  pub ctx : &'a TokamakContext,
  pub plan: Plan
}

impl<'a> DataFrame<'a> {
  pub fn kind(&self) -> &'static str {
    match self.plan {
      Plan::From  (_) => "from",
      Plan::Select(_,_) => "select"
    } 
  }
  
  pub fn select(self, exprs: Vec<Expr>) -> DataFrame<'a> {
    DataFrame {ctx: self.ctx, plan: Plan::Select(Box::new(self.plan), exprs)}
  }
  
  fn count(&self) -> TResult<usize> {
    Ok(0)
  }
  
  fn head(&self) -> TResult<Box<DataSet>> {
    Err(Error::InternalError)
  }
  
  fn head_with(&self, num: usize) -> TResult<Box<DataSet>> {
    Err(Error::InternalError)
  }
  
  fn tail_with(&self, num: usize) -> TResult<Box<DataSet>> {
    Err(Error::InternalError)
  }
}


pub fn RandomGenerator(types: Vec<&str>) -> Box<DataSet>
{
  Box::new(CustomDataSource::new(
    &Uuid::new_v4().to_hyphenated_string(),  
    "random",
    types,
    Vec::new()
  ))
}

fn typestr_to_schema(ctx: &TokamakContext, types: Vec<&str>) -> Vec<Box<Type>>
{
  types.iter()
    .map( |s| ctx.get_type(s).unwrap().clone_box() )
    .collect::<Vec<Box<Type>>>()
}