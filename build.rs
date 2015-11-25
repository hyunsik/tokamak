#![feature(process, path, env)]
use std::process::Command;
use std::env::current_dir;

fn main() {
	
  assert!(
  	Command::new("clang++")
  		.args(&["llvm-ir/llvm-ir.cc", "-S", "-emit-llvm", "-O2", "-o", "llvm-ir/llvm-ir.ll"])
    	.status()
    	.unwrap()
    	.success()
 	);

}