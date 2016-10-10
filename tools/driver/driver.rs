#[macro_use] extern crate log;
extern crate env_logger;
extern crate driver;

use std::env;
use std::io;
use std::process;

pub fn main() {
  env_logger::init().unwrap();

  let exit_code = driver::run_driver(
    std::env::args().collect(),
    env::current_dir().unwrap(),
    Box::new(io::stdout()), Box::new(io::stderr())
  );

  process::exit(exit_code);
}