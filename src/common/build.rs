extern crate gcc;

use std::process::Command;

fn main() {

  assert!(
    Command::new("mkdir")
      .args(&["-p", "target/ir"])
      .status()
      .unwrap()
      .success()
  );

  // Emitting LLVM IR via Rustc
  //
  // assert!(
  // 	Command::new("rustc")
  // 		.args(&["rows_ir.rs", "--crate-type", "dylib", "--emit", "llvm-ir", "-O", "-o", "target/ir/rows_ir.ll"])
  //   	.status()
  //   	.unwrap()
  //   	.success()
 	// );

  assert!(
  	Command::new("clang++")
  		.args(&["page_ir.cc", "-std=c++11", "-S", "-emit-llvm", "-O2", "-o", "target/ir/page_ir.ll"])
    	.status()
    	.unwrap()
    	.success()
 	);

  assert!(
  	Command::new("llvm-as")
  		.args(&["target/ir/page_ir.ll", "-o=target/ir/page_ir.bc"])
    	.status()
    	.unwrap()
    	.success()
 	);

  assert!(
  	Command::new("llvm-link")
  		.args(&["target/ir/page_ir.bc", "-o=target/ir/common.bc"])
    	.status()
    	.unwrap()
    	.success()
 	);

   gcc::Config::new()
        .cpp(true)
        .file("page_ir.cc")
        .compile("libpage.a");
}
