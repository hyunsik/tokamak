
#![feature(set_stdio)]
#![feature(question_mark)]

extern crate env_logger;
extern crate getopts;
extern crate syntax;
extern crate typed_arena;
#[macro_use]
extern crate log;
extern crate llvm;
extern crate rustc_serialize;

#[macro_use] pub mod macros;
pub mod backend;
pub mod config;
pub mod cstore;
pub mod driver;
pub mod filesearch;
pub mod middle;
pub mod pretty;
pub mod search_paths;
pub mod session;
pub mod targets;
//pub mod ty;

use syntax::diagnostics;
use syntax::errors;
use syntax::errors::emitter::Emitter;

use driver::CompileController;
use config::{Input, ErrorOutputType};
use session::{CompileResult, Session, early_error, early_warn};
use pretty::{PpMode, UserIdentifiedItem};

use std::env;
use std::io::{self, Read};
use std::path::PathBuf;
use std::process;

#[inline]
fn abort_msg(err_count: usize) -> String {
    match err_count {
        0 => "aborting with no errors (maybe a bug?)".to_owned(),
        1 => "aborting due to previous error".to_owned(),
        e => format!("aborting due to {} previous errors", e),
    }
}

pub fn run(args: Vec<String>) -> isize {
  let (result, session) = run_compiler(&args, &mut RustcDefaultCalls);

  if let Err(err_count) = result {
    if err_count > 0 {
      match session {
        Some(sess) => sess.fatal(&abort_msg(err_count)),
        None => {
          let mut emitter = errors::emitter::BasicEmitter::stderr(errors::ColorConfig::Auto);
          emitter.emit(None, &abort_msg(err_count), None, errors::Level::Fatal);
          exit_on_err();
        }
      }
    }
  }
  0
}

// Parse args and run the compiler. This is the primary entry point for rustc.
// See comments on CompilerCalls below for details about the callbacks argument.
pub fn run_compiler<'a>(args: &[String],
                        callbacks: &mut CompilerCalls<'a>)
                        -> (CompileResult, Option<Session>) {

  macro_rules! do_or_return {($expr: expr, $sess: expr) => {
    match $expr {
        Compilation::Stop => return (Ok(()), $sess),
        Compilation::Continue => {}
    }
  }}

  let matches = match handle_options(args.to_vec()) {
        Some(matches) => matches,
        None => return (Ok(()), None),
  };

  let sopts = config::build_session_options(&matches);

  if sopts.debugging_opts.debug_llvm {
    unsafe { llvm::LLVMSetDebug(1); }
  }

  let descriptions = diagnostics_registry();

  do_or_return!(callbacks.early_callback(&matches,
                                         &sopts,
                                         &descriptions,
                                         sopts.error_format),
                                         None);

  let (input, input_file_path) = match make_input(&matches.free) {
    Some((input, input_file_path)) => callbacks.some_input(input, input_file_path),
    None => match callbacks.no_input(&matches, &sopts, &descriptions) {
      Some((input, input_file_path)) => (input, input_file_path),
      None => return (Ok(()), None),
    },
  };

  (Ok(()), None)
}

// Extract input (string or file and optional path) from matches.
fn make_input(free_matches: &[String]) -> Option<(Input, Option<PathBuf>)> {
    if free_matches.len() == 1 {
        let ifile = &free_matches[0][..];
        if ifile == "-" {
            let mut src = String::new();
            io::stdin().read_to_string(&mut src).unwrap();
            Some((Input::Str { name: driver::anon_src(), input: src },
                  None))
        } else {
            Some((Input::File(PathBuf::from(ifile)),
                  Some(PathBuf::from(ifile))))
        }
    } else {
        None
    }
}

// Whether to stop or continue compilation.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Compilation {
    Stop,
    Continue,
}

impl Compilation {
    pub fn and_then<F: FnOnce() -> Compilation>(self, next: F) -> Compilation {
        match self {
            Compilation::Stop => Compilation::Stop,
            Compilation::Continue => next(),
        }
    }
}

// A trait for customising the compilation process. Offers a number of hooks for
// executing custom code or customising input.
pub trait CompilerCalls<'a> {
    // Hook for a callback early in the process of handling arguments. This will
    // be called straight after options have been parsed but before anything
    // else (e.g., selecting input and output).
    fn early_callback(&mut self,
                      _: &getopts::Matches,
                      _: &config::Options,
                      _: &diagnostics::registry::Registry,
                      _: ErrorOutputType)
                      -> Compilation {
        Compilation::Continue
    }

    // Hook for a callback late in the process of handling arguments. This will
    // be called just before actual compilation starts (and before build_controller
    // is called), after all arguments etc. have been completely handled.
    fn late_callback(&mut self,
                     _: &getopts::Matches,
                     _: &Session,
                     _: &Input,
                     _: &Option<PathBuf>,
                     _: &Option<PathBuf>)
                     -> Compilation {
        Compilation::Continue
    }

    // Called after we extract the input from the arguments. Gives the implementer
    // an opportunity to change the inputs or to add some custom input handling.
    // The default behaviour is to simply pass through the inputs.
    fn some_input(&mut self,
                  input: Input,
                  input_path: Option<PathBuf>)
                  -> (Input, Option<PathBuf>) {
        (input, input_path)
    }

    // Called after we extract the input from the arguments if there is no valid
    // input. Gives the implementer an opportunity to supply alternate input (by
    // returning a Some value) or to add custom behaviour for this error such as
    // emitting error messages. Returning None will cause compilation to stop
    // at this point.
    fn no_input(&mut self,
                _: &getopts::Matches,
                _: &config::Options,
                _: &diagnostics::registry::Registry)
                -> Option<(Input, Option<PathBuf>)> {
        None
    }

    // Parse pretty printing information from the arguments. The implementer can
    // choose to ignore this (the default will return None) which will skip pretty
    // printing. If you do want to pretty print, it is recommended to use the
    // implementation of this method from RustcDefaultCalls.
    // FIXME, this is a terrible bit of API. Parsing of pretty printing stuff
    // should be done as part of the framework and the implementor should customise
    // handling of it. However, that is not possible atm because pretty printing
    // essentially goes off and takes another path through the compiler which
    // means the session is either moved or not depending on what parse_pretty
    // returns (we could fix this by cloning, but it's another hack). The proper
    // solution is to handle pretty printing as if it were a compiler extension,
    // extending CompileController to make this work (see for example the treatment
    // of save-analysis in RustcDefaultCalls::build_controller).
    fn parse_pretty(&mut self,
                    _sess: &Session,
                    _matches: &getopts::Matches)
                    -> Option<(PpMode, Option<UserIdentifiedItem>)> {
        None
    }

    // Create a CompilController struct for controlling the behaviour of
    // compilation.
    fn build_controller(&mut self, &Session) -> CompileController<'a>;
}

/// Returns a version string such as "0.12.0-dev".
pub fn release_str() -> Option<&'static str> {
    option_env!("CFG_RELEASE")
}

/// Returns the full SHA1 hash of HEAD of the Git repo from which rustc was built.
pub fn commit_hash_str() -> Option<&'static str> {
    option_env!("CFG_VER_HASH")
}

/// Returns the "commit date" of HEAD of the Git repo from which rustc was built as a static string.
pub fn commit_date_str() -> Option<&'static str> {
    option_env!("CFG_VER_DATE")
}

/// Prints version information
pub fn version(binary: &str, matches: &getopts::Matches) {
    let verbose = matches.opt_present("verbose");

    println!("{} {}",
             binary,
             option_env!("CFG_VERSION").unwrap_or("unknown version"));
    if verbose {
        fn unw(x: Option<&str>) -> &str {
            x.unwrap_or("unknown")
        }
        println!("binary: {}", binary);
        println!("commit-hash: {}", unw(commit_hash_str()));
        println!("commit-date: {}", unw(commit_date_str()));
        println!("host: {}", config::host_triple());
        println!("release: {}", unw(release_str()));
    }
}

fn usage(verbose: bool, include_unstable_options: bool) {
  // TODO
}

/// Process command line options. Emits messages as appropriate. If compilation
/// should continue, returns a getopts::Matches object parsed from args,
/// otherwise returns None.
///
/// The compiler's handling of options is a little complication as it ties into
/// our stability story, and it's even *more* complicated by historical
/// accidents. The current intention of each compiler option is to have one of
/// three modes:
///
/// 1. An option is stable and can be used everywhere.
/// 2. An option is unstable, but was historically allowed on the stable
///    channel.
/// 3. An option is unstable, and can only be used on nightly.
///
/// Like unstable library and language features, however, unstable options have
/// always required a form of "opt in" to indicate that you're using them. This
/// provides the easy ability to scan a code base to check to see if anything
/// unstable is being used. Currently, this "opt in" is the `-Z` "zed" flag.
///
/// All options behind `-Z` are considered unstable by default. Other top-level
/// options can also be considered unstable, and they were unlocked through the
/// `-Z unstable-options` flag. Note that `-Z` remains to be the root of
/// instability in both cases, though.
///
/// So with all that in mind, the comments below have some more detail about the
/// contortions done here to get things to work out correctly.
pub fn handle_options(mut args: Vec<String>) -> Option<getopts::Matches> {
  // Throw away the first argument, the name of the binary
  let args = &args[1..];

  if args.is_empty() {
    // user did not write `-v` nor `-Z unstable-options`, so do not
    // include that extra information.
    usage(false, false);
    return None;
  }

  let opts = config::compiler_optgroups();
  let matches = match opts.parse(&args[..]) {
      Ok(m) => m,
      Err(f) => early_error(ErrorOutputType::default(), &f.to_string()),
  };

  if matches.opt_present("version") {
    version("tc", &matches);
    return None;
  }

  Some(matches)
}

#[derive(Copy, Clone)]
pub struct RustcDefaultCalls;

impl RustcDefaultCalls {
  pub fn test() {}
}

impl<'a> CompilerCalls<'a> for RustcDefaultCalls {
  // Create a CompilController struct for controlling the behaviour of
  // compilation.
  fn build_controller(&mut self, sess: &Session) -> CompileController<'a> {
    CompileController::basic()
  }
}

fn exit_on_err() -> ! {
    // Panic so the process returns a failure code, but don't pollute the
    // output with some unnecessary panic messages, we've already
    // printed everything that we needed to.
    io::set_panic(Box::new(io::sink()));
    panic!();
}

pub fn diagnostics_registry() -> diagnostics::registry::Registry {
    use syntax::diagnostics::registry::Registry;

    let mut all_errors = Vec::new();
    // TODO - add diagnostic extensions.
    Registry::new(&all_errors)
}

pub fn main() {
    let result = run(env::args().collect());
    process::exit(result as i32);
}