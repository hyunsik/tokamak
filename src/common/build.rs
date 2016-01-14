use std::process::Command;
use std::env::current_dir;

fn main() {
	
  assert!(
    Command::new("mkdir")
      .args(&["-p", "target/ir"])
      .status()
      .unwrap()
      .success()
  );
   
  // assert!(
  // 	Command::new("rustc")
  // 		.args(&["rows_ir.rs", "--crate-type", "dylib", "--emit", "llvm-ir", "-O", "-o", "target/ir/rows_ir.ll"])
  //   	.status()
  //   	.unwrap()
  //   	.success()
 	// );
  
  assert!(
  	Command::new("clang++")
  		.args(&["rows_ir.cc", "-std=c++11", "-S", "-emit-llvm", "-O2", "-o", "target/ir/rows_ir.ll"])
    	.status()
    	.unwrap()
    	.success()
 	);
  
  assert!(
  	Command::new("llvm-as")
  		.args(&["target/ir/rows_ir.ll", "-o=target/ir/rows_ir.bc"])
    	.status()
    	.unwrap()
    	.success()
 	); 
   
  assert!(
  	Command::new("llvm-link")
  		.args(&["target/ir/rows_ir.bc", "-o=target/ir/common.bc"])
    	.status()
    	.unwrap()
    	.success()
 	);

}
