extern crate itertools;

extern crate common;
extern crate rand;
extern crate sql;

use std::rc::Rc;

use common::input::InputSource;
use common::session::Session;
use common::types::Ty;

mod random;
pub use random::RandomTableGenerator;

pub type InputSourceFactory = Rc<Fn(&Session, &Vec<Ty>, usize) -> Box<InputSource>>;

pub fn get_factory(kind: &str) -> InputSourceFactory {
	match kind {
		"random" => {Rc::new(RandomTableGenerator::new)},
		_        => panic!("unknown storage: {}", kind)
			
	}
}