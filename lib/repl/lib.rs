extern crate rl_sys; // libreadline

use rl_sys::readline;

pub struct ReplOption {
}

pub struct ReplEnv {
}

pub struct Input;

pub struct Output;

impl ReplEnv {
  pub fn input(&self, line: &str) -> &Input {
    unimplemented!()
  }

  pub fn output(&mut self) -> &mut Output {
    unimplemented!()
  }

  pub fn handle_input(&self, c: &CompilerInstance, input: &Input) -> bool {
    unimplemented!()
  }
}

pub struct CompilerInstance;

pub fn runRepl(compiler: &CompilerInstance, env: &ReplEnv) {
  loop {
    match readline::readline(&format!("\x1b[33mtkm [{}]> \x1b[0m", 0)) {
      Ok(Some(line)) => {
        println!("{}", line);
      }
      Ok(None) => break, // eof
      Err(msg) => {
        println!("{}", msg);
      }
    }
  }
}