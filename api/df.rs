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
  From(Box<DataSource>),
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

pub trait DataSource {
  fn name(&self) -> &str;
  
  //fn schema(&self) -> &Vec<Box<Type>>;
}

pub struct CustomSource {
  src_type : String,
  schema   : Vec<String>,
  props    : Vec<(String, String)>
}

impl DataSource for CustomSource {
  fn name(&self) -> &str {
    "aaa"
  }
}

pub fn RandomGenerator(types: Vec<&str>) -> Box<DataSource>
{
  Box::new(CustomSource {
    src_type: "random".to_string(),
    schema  : types.into_iter().map(|s| s.to_string()).collect::<Vec<String>>(),
    props   : Vec::new()
  })
}

fn typestr_to_schema(ctx: &TokamakContext, types: Vec<&str>) -> Vec<Box<Type>>
{
  types.iter()
    .map( |s| ctx.get_type(s).unwrap().clone_box() )
    .collect::<Vec<Box<Type>>>()
}