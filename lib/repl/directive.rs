use super::Repl;

pub trait Directive {
  fn command(&self) -> &'static str;
  fn help(&self) -> &'static str;
  fn execute(&self, env: &Repl, args: &[&str]);
}

pub struct Help;

impl Directive for Help {
  fn command(&self) -> &'static str {
    "help"
  }

  fn help(&self) -> &'static str {
    "help command"
  }

  fn execute(&self, env: &Repl, args: &[&str]) {
  }
}