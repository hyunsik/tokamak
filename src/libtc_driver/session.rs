use config;

use filesearch;
use middle::cstore::CrateStore;
use syntax::ast::{self, NodeId, NodeIdAssigner, Name};
use syntax::codemap::{self, Span, MultiSpan};
use syntax::errors::{self, DiagnosticBuilder, Handler};
use syntax::errors::emitter::{Emitter, BasicEmitter, EmitterWriter};
use syntax::diagnostics;
use syntax::parse;
use syntax::parse::ParseSess;
use targets::Target;
use ty::tls;

use std::cell::{Cell, RefCell};
use std::collections::{HashMap, HashSet};
use std::env;
use std::path::PathBuf;
use std::rc::Rc;
use std::fmt;

// Represents the data associated with a compilation
// session for a single crate.
pub struct Session {
  pub target: config::Config,
  pub host: Target,
  pub opts: config::Options,
  pub cstore: Rc<for<'a> CrateStore<'a>>,
  pub parse_sess: ParseSess,
  // For a library crate, this is always none
  pub entry_fn: RefCell<Option<(NodeId, Span)>>,
  pub entry_type: Cell<Option<config::EntryFnType>>,
  pub plugin_registrar_fn: Cell<Option<ast::NodeId>>,
  pub default_sysroot: Option<PathBuf>,
  // The name of the root source file of the crate, in the local file system.
  // The path is always expected to be absolute. `None` means that there is no
  // source file.
  pub local_crate_source_file: Option<PathBuf>,
  pub working_dir: PathBuf,
  pub plugin_llvm_passes: RefCell<Vec<String>>,
  /// The maximum recursion limit for potentially infinitely recursive
  /// operations such as auto-dereference and monomorphization.
  pub recursion_limit: Cell<usize>,

  next_node_id: Cell<ast::NodeId>,
}

impl Session {
  pub fn struct_span_warn<'a, S: Into<MultiSpan>>(&'a self,
                                                    sp: S,
                                                    msg: &str)
                                                    -> DiagnosticBuilder<'a>  {
      self.diagnostic().struct_span_warn(sp, msg)
  }
  pub fn struct_span_warn_with_code<'a, S: Into<MultiSpan>>(&'a self,
                                                              sp: S,
                                                              msg: &str,
                                                              code: &str)
                                                              -> DiagnosticBuilder<'a>  {
    self.diagnostic().struct_span_warn_with_code(sp, msg, code)
  }
  pub fn struct_warn<'a>(&'a self, msg: &str) -> DiagnosticBuilder<'a>  {
    self.diagnostic().struct_warn(msg)
  }

  pub fn fatal(&self, msg: &str) -> ! {
    panic!(self.diagnostic().fatal(msg))
  }

  pub fn span_err_or_warn<S: Into<MultiSpan>>(&self, is_warning: bool, sp: S, msg: &str) {
    if is_warning {
      self.span_warn(sp, msg);
    } else {
      self.span_err(sp, msg);
    }
  }

  pub fn span_err<S: Into<MultiSpan>>(&self, sp: S, msg: &str) {
    match split_msg_into_multilines(msg) {
      Some(msg) => self.diagnostic().span_err(sp, &msg),
      None => self.diagnostic().span_err(sp, msg)
    }
  }

  pub fn err(&self, msg: &str) {
    self.diagnostic().err(msg)
  }

  pub fn err_count(&self) -> usize {
    self.diagnostic().err_count()
  }

  pub fn has_errors(&self) -> bool {
    self.diagnostic().has_errors()
  }

  pub fn abort_if_errors(&self) {
    self.diagnostic().abort_if_errors();
  }

  pub fn track_errors<F, T>(&self, f: F) -> Result<T, usize>
        where F: FnOnce() -> T {
    let old_count = self.err_count();
    let result = f();
    let errors = self.err_count() - old_count;
    if errors == 0 {
      Ok(result)
    } else {
      Err(errors)
    }
  }

  pub fn span_warn<S: Into<MultiSpan>>(&self, sp: S, msg: &str) {
    self.diagnostic().span_warn(sp, msg)
  }

  pub fn diagnostic<'a>(&'a self) -> &'a errors::Handler {
    &self.parse_sess.span_diagnostic
  }

  pub fn codemap<'a>(&'a self) -> &'a codemap::CodeMap {
        self.parse_sess.codemap()
  }

  pub fn verbose(&self) -> bool { self.opts.debugging_opts.verbose }
  pub fn time_passes(&self) -> bool { self.opts.debugging_opts.time_passes }
}

fn split_msg_into_multilines(msg: &str) -> Option<String> {
    // Conditions for enabling multi-line errors:
    if !msg.contains("mismatched types") &&
        !msg.contains("type mismatch resolving") &&
        !msg.contains("if and else have incompatible types") &&
        !msg.contains("if may be missing an else clause") &&
        !msg.contains("match arms have incompatible types") &&
        !msg.contains("structure constructor specifies a structure of type") &&
        !msg.contains("has an incompatible type for trait") {
            return None
    }
    let first = msg.match_indices("expected").filter(|s| {
        s.0 > 0 && (msg.char_at_reverse(s.0) == ' ' ||
                    msg.char_at_reverse(s.0) == '(')
    }).map(|(a, b)| (a - 1, a + b.len()));
    let second = msg.match_indices("found").filter(|s| {
        msg.char_at_reverse(s.0) == ' '
    }).map(|(a, b)| (a - 1, a + b.len()));

    let mut new_msg = String::new();
    let mut head = 0;

    // Insert `\n` before expected and found.
    for (pos1, pos2) in first.zip(second) {
        new_msg = new_msg +
        // A `(` may be preceded by a space and it should be trimmed
                  msg[head..pos1.0].trim_right() + // prefix
                  "\n" +                           // insert before first
                  &msg[pos1.0..pos1.1] +           // insert what first matched
                  &msg[pos1.1..pos2.0] +           // between matches
                  "\n   " +                        // insert before second
        //           123
        // `expected` is 3 char longer than `found`. To align the types,
        // `found` gets 3 spaces prepended.
                  &msg[pos2.0..pos2.1];            // insert what second matched

        head = pos2.1;
    }

    let mut tail = &msg[head..];
    let third = tail.find("(values differ")
                   .or(tail.find("(lifetime"))
                   .or(tail.find("(cyclic type of infinite size"));
    // Insert `\n` before any remaining messages which match.
    if let Some(pos) = third {
        // The end of the message may just be wrapped in `()` without
        // `expected`/`found`.  Push this also to a new line and add the
        // final tail after.
        new_msg = new_msg +
        // `(` is usually preceded by a space and should be trimmed.
                  tail[..pos].trim_right() + // prefix
                  "\n" +                     // insert before paren
                  &tail[pos..];              // append the tail

        tail = "";
    }

    new_msg.push_str(tail);
    return Some(new_msg);
}

pub fn build_session(sopts: config::Options,
                     local_crate_source_file: Option<PathBuf>,
                     registry: diagnostics::registry::Registry,
                     cstore: Rc<for<'a> CrateStore<'a>>)
                     -> Session {
  let treat_err_as_bug = sopts.treat_err_as_bug;

  let codemap = Rc::new(codemap::CodeMap::new());
  let emitter: Box<Emitter> = match sopts.error_format {
    config::ErrorOutputType::HumanReadable(color_config) => {
      Box::new(EmitterWriter::stderr(color_config, Some(registry), codemap.clone()))
    }
    config::ErrorOutputType::Json => {
      //Box::new(JsonEmitter::stderr(Some(registry), codemap.clone()))
      unimplemented!()
    }
  };

  let diagnostic_handler =
    errors::Handler::with_emitter(true, treat_err_as_bug, emitter);

  build_session_(sopts, local_crate_source_file, diagnostic_handler, codemap, cstore)
}

pub fn build_session_(sopts: config::Options,
                      local_crate_source_file: Option<PathBuf>,
                      span_diagnostic: errors::Handler,
                      codemap: Rc<codemap::CodeMap>,
                      cstore: Rc<for<'a> CrateStore<'a>>)
                      -> Session {
  let host = match Target::search(config::host_triple()) {
    Ok(t) => t,
    Err(e) => {
      panic!(span_diagnostic.fatal(&format!("Error loading host specification: {}", e)));
    }
  };
  let target_cfg = config::build_target_config(&sopts, &span_diagnostic);
  let p_s = parse::ParseSess::with_span_handler(span_diagnostic, codemap);
  let default_sysroot = match sopts.maybe_sysroot {
    Some(_) => None,
    None => Some(filesearch::get_or_default_sysroot())
  };

  // Make the path absolute, if necessary
  let local_crate_source_file = local_crate_source_file.map(|path|
    if path.is_absolute() {
      path.clone()
    } else {
      env::current_dir().unwrap().join(&path)
    }
  );

  let sess = Session {
        target: target_cfg,
        host: host,
        opts: sopts,
        cstore: cstore,
        parse_sess: p_s,
        // For a library crate, this is always none
        entry_fn: RefCell::new(None),
        entry_type: Cell::new(None),
        plugin_registrar_fn: Cell::new(None),
        default_sysroot: default_sysroot,
        local_crate_source_file: local_crate_source_file,
        working_dir: env::current_dir().unwrap(),
        plugin_llvm_passes: RefCell::new(Vec::new()),
        recursion_limit: Cell::new(64),
        next_node_id: Cell::new(1),
    };

    sess
}

pub fn early_error(output: config::ErrorOutputType, msg: &str) -> ! {
    let mut emitter: Box<Emitter> = match output {
        config::ErrorOutputType::HumanReadable(color_config) => {
            Box::new(BasicEmitter::stderr(color_config))
        }
        config::ErrorOutputType::Json => panic!("JsonEmitter is not supported yet")
    };
    emitter.emit(None, msg, None, errors::Level::Fatal);
    panic!(errors::FatalError);
}

pub fn early_warn(output: config::ErrorOutputType, msg: &str) {
    let mut emitter: Box<Emitter> = match output {
        config::ErrorOutputType::HumanReadable(color_config) => {
            Box::new(BasicEmitter::stderr(color_config))
        }
        config::ErrorOutputType::Json => panic!("JsonEmitter is not supported yet")
    };
    emitter.emit(None, msg, None, errors::Level::Warning);
}

// Err(0) means compilation was stopped, but no errors were found.
// This would be better as a dedicated enum, but using try! is so convenient.
pub type CompileResult = Result<(), usize>;

pub fn compile_result_from_err_count(err_count: usize) -> CompileResult {
    if err_count == 0 {
        Ok(())
    } else {
        Err(err_count)
    }
}

#[cold]
#[inline(never)]
pub fn bug_fmt(file: &'static str, line: u32, args: fmt::Arguments) -> ! {
    // this wrapper mostly exists so I don't have to write a fully
    // qualified path of None::<Span> inside the bug!() macro defintion
    opt_span_bug_fmt(file, line, None::<Span>, args);
}

fn opt_span_bug_fmt<S: Into<MultiSpan>>(file: &'static str,
                                          line: u32,
                                          span: Option<S>,
                                          args: fmt::Arguments) -> ! {
    tls::with_opt(move |tcx| {
        let msg = format!("{}:{}: {}", file, line, args);
        match (tcx, span) {
            (Some(tcx), Some(span)) => tcx.sess.diagnostic().span_bug(span, &msg),
            (Some(tcx), None) => tcx.sess.diagnostic().bug(&msg),
            (None, _) => panic!(msg)
        }
    });
    unreachable!();
}