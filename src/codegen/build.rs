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
   
  assert!(
  	Command::new("clang++")
  		.args(&["tests/test-ir.cc", "-O2", "-c", "-emit-llvm", "-o", "tests/test-ir.bc"])
    	.status()
    	.unwrap()
    	.success()
 	);
   
  assert!(
  	Command::new("rustc")
  		.args(&["test-ir-rs.rs", "--crate-type", "lib", "--emit", "llvm-ir", "-O"])
    	.status()
    	.unwrap()
    	.success()
 	);
   
  assert!(
  	Command::new("rustc")
  		.args(&["test-ir-rs.rs", "--crate-type", "lib", "--emit", "llvm-bc", "-O"])
    	.status()
    	.unwrap()
    	.success()
 	);
   
  assert!(
  	Command::new("llvm-link")
  		.args(&["test-ir-rs.bc", "tests/test-ir.bc", "-o=tests/test-module.bc"])
    	.status()
    	.unwrap()
    	.success()
 	);

}