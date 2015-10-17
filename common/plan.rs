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


/// Visitor for Expr Tree
pub trait Visitor<'v>: Sized {
  fn visit_from(&mut self, dataset: &'v DataSet) {}
  
  fn visit_select(&mut self, child: &'v Plan, filter: &Vec<Expr>) {
    walk_plan(self, child);
  }
  
  fn visit_head(&mut self, child: &'v Plan, fetch_row: usize) {
    walk_plan(self, child);
  }
  
  fn visit_tail(&mut self, child: &'v Plan, fetch_row: usize) {
    walk_plan(self, child);
  }
}

/// Walker for Expr Tree
pub fn walk_plan<'v, V: Visitor<'v>>(visitor: &mut V, plan: &'v Plan) {
  match *plan {
    Plan::From(ref ds)                 => { visitor.visit_from(&**ds)}, 
    Plan::Select(ref child,ref filter) => { visitor.visit_select(&**child, filter) },
    Plan::Head  (ref child,num)        => { visitor.visit_head(&**child,num) },
    Plan::Tail  (ref child,num)        => { visitor.visit_tail(&**child,num) },
    _                                  => {},
  }
}