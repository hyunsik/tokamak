use task::TaskSource;

pub struct Driver;

impl Driver
{
  pub fn update_source(&self, source: TaskSource) {}
}

pub struct DriverFactory 
{
  pub input_driver: bool,
  pub output_driver: bool, 
  source_ids: Vec<String>,
  factory: Vec<Box<DriverFactory>>
}

pub struct DriverContext;

impl DriverFactory {
  pub fn create_driver(&self, ctx: &DriverContext) -> Driver
  {
    Driver
  }
}