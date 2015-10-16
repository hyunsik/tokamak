use std::fmt::{Display, Formatter, Result};

pub enum Plan {
  From (Box<DataSet>),
  Select(Box<Plan>, Vec<Expr>), // params: child, filter in a CNF form
  Aggregate(Box<Plan>, Vec<String>, Vec<String>), // params: child, keys, expressions,
  Head(Box<Plan>, usize), // child, row number to fetch
  Tail(Box<Plan>, usize), // child, row number to fetch
}

pub trait DataSet: Display {
  fn name(&self) -> &str;
  
  //fn schema(&self) -> &Vec<Box<Type>>;
}

pub struct CustomDataSource {
  name     : String,
  src_type : String,
  schema   : Vec<String>,
  props    : Vec<(String, String)>
}

impl CustomDataSource {
  pub fn new( 
    name: &str, 
    src_type: &str, 
    schema: Vec<&str>, 
    props: Vec<(&str, &str)>) -> CustomDataSource {
      
    CustomDataSource {
      name    : name.to_string(),
      src_type: src_type.to_string(),
      schema  : schema.iter()
                  .map(|s| s.to_string())
                  .collect::<Vec<String>>(),
      props   : props.iter()
                  .map(|p| (p.0.to_string(), p.1.to_string()))
                  .collect::<Vec<(String, String)>>() 
    }
  }
}

impl DataSet for CustomDataSource {
  fn name(&self) -> &str { &self.name } 
}

impl Display for CustomDataSource {
  fn fmt(&self, f: &mut Formatter) -> Result {
    write!(f, "name={},type={}", self.name, self.src_type)
  }
}

pub enum Expr {
  Plus,
  Field(String)
}