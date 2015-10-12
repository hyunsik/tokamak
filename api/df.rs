use uuid::Uuid;

use common::err::{Error, TResult};
use common::types::Type;

use super::TokamakContext;

pub trait DataSet 
{
  fn name(&self) -> &str;
  
  fn schema(&self) -> &Vec<Box<Type>>;
  
  fn from(&self) -> Option<Box<DataFrame>>;
}

pub struct RandomGenerator<'a>
{
  ctx  : &'a TokamakContext,
  name : String,
  types: Vec<Box<Type>>
}

impl<'a> RandomGenerator<'a>
{
  pub fn new(ctx: &'a TokamakContext, types: Vec<&str>) -> TResult<Box<RandomGenerator<'a>>>
  {
    Ok(Box::new(RandomGenerator { 
      ctx  : ctx,
      name : Uuid::new_v4().to_hyphenated_string(),
      types: types.iter()
               .map( |s| ctx.get_type(s).unwrap().clone_box() )
               .collect::<Vec<Box<Type>>>()
    }))
  }
}

impl<'a> DataSet for RandomGenerator<'a>
{
  fn name(&self) -> &str {
    &self.name
  }
  
  fn schema(&self) -> &Vec<Box<Type>>
  {
    &self.types
  }
  
  fn from(&self) -> Option<Box<DataFrame>> {
    //let x: Box<DataFrame> = Box::new(TableDF {dataset: self});
    None
  }
}

pub struct TableDF<'a>
{
  //ctx: &'a TokamakContext,
  dataset: &'a DataSet
}

impl<'a> DataFrame for TableDF<'a> {
  fn count(&mut self) -> TResult<usize>
  {
    Ok(0)
  }
}

pub trait Expr {
  fn name(&self) -> &str;
}

pub trait DataFrame {
//  // transform
//  fn select(&mut self, Vec<Box<Expr>>) -> &mut DataFrame;
//  // transform
//  fn filter(&mut self, Box<Expr>) -> &mut DataFrame;
  
  // action
  fn count(&mut self) -> TResult<usize>;
}