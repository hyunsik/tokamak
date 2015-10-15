pub enum Plan {
  From (Box<DataSet>),
  Select(Box<Plan>, Vec<Expr>) // params: a child, a filter in a CNF form 
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

pub enum Expr {
  Plus,
  Field(String)
}