use uuid::Uuid;

use common::err::{Error, TResult};
use common::types::Type;

use super::TokamakContext;

pub trait DataSet<'a> 
{
  fn name(&self) -> &str;
  
  fn schema(&self) -> &Vec<Box<&'a Type>>;
  
  //fn from(&self) -> Box<DataFrame<'a>>;
}

pub struct RandomGenerator<'a>
{
  ctx  : &'a TokamakContext,
  name : String,
  types: Vec<Box<&'a Type>>
}

impl<'a> RandomGenerator<'a>
{
  pub fn new(ctx: &'a TokamakContext, types: Vec<&str>) -> TResult<Box<RandomGenerator<'a>>>
  {
    Ok(Box::new(RandomGenerator { 
      ctx  : ctx,
      name : Uuid::new_v4().to_hyphenated_string(),
      types: types.iter()
               .map( |s| Box::new(ctx.get_type(s).unwrap()))
               .collect::<Vec<Box<&Type>>>()
    }))
  }
}

impl<'a> DataSet<'a> for RandomGenerator<'a>
{
  fn name(&self) -> &str {
    &self.name
  }
  
  fn schema(&self) -> &Vec<Box<&'a Type>>
  {
    &self.types
  }
  /*
  pub fn from(&self) -> Box<DataFrame<'a>> {
    Box::new(
      TableDF {
      ctx: self.ctx,
      dataset: self
    })
  }*/
}

pub struct TableDF<'a>
{
  ctx: &'a TokamakContext,
  dataset: &'a DataSet<'a>
}

impl<'a> DataFrame<'a> for TableDF<'a> {
  fn count(&mut self) -> TResult<usize>
  {
    Ok(0)
  }
}

pub trait Expr {
  fn name(&self) -> &str;
}

pub trait DataFrame<'a> {
//  // transform
//  fn select(&mut self, Vec<Box<Expr>>) -> &mut DataFrame;
//  // transform
//  fn filter(&mut self, Box<Expr>) -> &mut DataFrame;
  
  // action
  fn count(&mut self) -> TResult<usize>;
}