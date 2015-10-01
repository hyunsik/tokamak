

pub struct InputSourceProvider;

impl InputSourceProvider {
  
  fn create(types: Rc<Vec<Box<Type>>>) -> Box<InputSource> {
    Box::new(
      RandomTableGenerator {
        types: types.clone(),
        page_builder: Box::new(DefaultPageBuilder)        
      }
    ) as Box<InputSource>
  }
}