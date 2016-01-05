#![feature(process, path, env)]
use std::process::Command;
use std::env::current_dir;

fn main() {
	
  assert!(
    Command::new("mkdir")
      .args(&["-p", "target/test-ir"])
      .status()
      .unwrap()
      .success()
  );
  
  assert!(
  	Command::new("clang++")
  		.args(&["test-ir/test-ir.cc", "-S", "-emit-llvm", "-O2", "-o", "target/test-ir/test-ir-cpp.ll"])
    	.status()
    	.unwrap()
    	.success()
 	);
 
  assert!(
  	Command::new("llvm-as")
  		.args(&["target/test-ir/test-ir-cpp.ll", "-o=target/test-ir/test-ir-cpp.bc"])
    	.status()
    	.unwrap()
    	.success()
 	);
   
  /*
  assert!(
  	Command::new("rustc")
  		.args(&["test-ir/test-ir.rs", "--crate-type", "lib", "--emit", "llvm-ir", "-O", "-o", "target/test-ir/test-ir-rs.ll"])
    	.status()
    	.unwrap()
    	.success()
 	);
   
  assert!(
  	Command::new("llvm-as")
  		.args(&["target/test-ir/test-ir-rs.ll", "-o=target/test-ir/test-ir-rs.bc"])
    	.status()
    	.unwrap()
    	.success()
 	);*/
   
  assert!(
  	Command::new("llvm-link")
  		.args(&["target/test-ir/test-ir-cpp.bc", "-o=target/test-ir/test-module.bc"])
    	.status()
    	.unwrap()
    	.success()
 	);

}