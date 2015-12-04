#![feature(process, path, env)]
use std::process::Command;
use std::env::current_dir;

fn main() {
	
  assert!(
  	Command::new("clang++")
  		.args(&["tests/test-ir.cc", "-S", "-emit-llvm", "-O2", "-o", "tests/test-ir.ll"])
    	.status()
    	.unwrap()
    	.success()
 	);

}