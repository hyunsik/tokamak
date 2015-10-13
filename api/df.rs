use uuid::Uuid;

use common::err::{Error, TResult};
use common::types::Type;

// let ctx = TokamakContext::new();
// ctx.from(RandomGenerator).select(...);

use super::TokamakContext;

pub struct DataFrame<'a> {
  pub ctx : &'a TokamakContext,
  pub decl: DataFrameDecl<'a>
}

pub enum DataFrameDecl<'a> {
  From(Box<DataSet>),
  Select(Box<DataFrame<'a>>, Vec<Expr>)
}

pub enum Expr {
  Plus,
  Field(String)
}

impl<'a> DataFrame<'a> {
  pub fn kind(&self) -> &'static str {
    match self.decl {
      DataFrameDecl::From  (_) => "from",
      DataFrameDecl::Select(_,_) => "select"
    } 
  }
  
  pub fn select(self, exprs: Vec<Expr>) -> DataFrame<'a> {
    DataFrame {ctx: self.ctx, decl: DataFrameDecl::Select(Box::new(self), exprs)}
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

pub trait DataSet {
  fn name(&self) -> &str;
  
  //fn schema(&self) -> &Vec<Box<Type>>;
}

pub struct CustomDataSource {
  name     : String,
  src_type : String,
  schema   : Vec<String>,
  props    : Vec<(String, String)>
}

impl DataSet for CustomDataSource {
  fn name(&self) -> &str {
    &self.name
  }
}

pub fn RandomGenerator(types: Vec<&str>) -> Box<DataSet>
{
  Box::new(CustomDataSource {
    name    :  Uuid::new_v4().to_hyphenated_string(),  
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