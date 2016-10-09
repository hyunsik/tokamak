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
  let line: String = "".to_string();
  loop {
    let input = env.input(&line);

    if !env.handle_input(compiler, input) {
      break;
    }
  }
}