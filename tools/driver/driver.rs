#[macro_use] extern crate log;
extern crate env_logger;
extern crate driver;

use std::env;
use std::process;

pub fn main() {
  env_logger::init().unwrap();

  let exit_code = driver::driver(
    std::env::args().collect(),
    env::current_dir().unwrap()
  );

  process::exit(exit_code);
}