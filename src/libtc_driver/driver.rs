use config::{self, Input};
use metadata::cstore::CStore;
use session::{Session, CompileResult, compile_result_from_err_count};
use util::common::time;



use syntax::ast;
use syntax::parse::{self, PResult, token};

use super::Compilation;

pub fn compile_input(sess: &Session,
                     cstore: &CStore,
                     cfg: ast::CrateConfig,
                     input: &Input,
                     addl_plugins: Option<Vec<String>>,
                     control: &CompileController) -> CompileResult {

  macro_rules! controller_entry_point {
      ($point: ident, $tsess: expr, $make_state: expr, $phase_result: expr) => {{
          let state = $make_state;
          let phase_result: &CompileResult = &$phase_result;
          if phase_result.is_ok() || control.$point.run_callback_on_error {
              (control.$point.callback)(state);
          }

          if control.$point.stop == Compilation::Stop {
              return compile_result_from_err_count($tsess.err_count());
          }
      }}
  }

  let krate = match phase_1_parse_input(sess, cfg, input) {
    Ok(krate) => krate,
    Err(mut parse_error) => {
      parse_error.emit();
      return Err(1);
    }
  };

  controller_entry_point!(after_parse,
                          sess,
                          CompileState::state_after_parse(input, sess, &krate),
                          Ok(()));
  unimplemented!()
}

/// The name used for source code that doesn't originate in a file
/// (e.g. source from stdin or a string)
pub fn anon_src() -> String {
    "<anon>".to_string()
}

/// CompileController is used to customise compilation, it allows compilation to
/// be stopped and/or to call arbitrary code at various points in compilation.
/// It also allows for various flags to be set to influence what information gets
/// collected during compilation.
///
/// This is a somewhat higher level controller than a Session - the Session
/// controls what happens in each phase, whereas the CompileController controls
/// whether a phase is run at all and whether other code (from outside the
/// the compiler) is run between phases.
///
/// Note that if compilation is set to stop and a callback is provided for a
/// given entry point, the callback is called before compilation is stopped.
///
/// Expect more entry points to be added in the future.
pub struct CompileController<'a> {
  pub after_parse: PhaseController<'a>,
  pub after_expand: PhaseController<'a>,
  pub after_write_deps: PhaseController<'a>,
  pub after_analysis: PhaseController<'a>,
  pub after_llvm: PhaseController<'a>,
}

impl<'a> CompileController<'a> {
    pub fn basic() -> CompileController<'a> {
        CompileController {
            after_parse: PhaseController::basic(),
            after_expand: PhaseController::basic(),
            after_write_deps: PhaseController::basic(),
            after_analysis: PhaseController::basic(),
            after_llvm: PhaseController::basic(),
        }
    }
}

pub struct PhaseController<'a> {
    pub stop: Compilation,
    // If true then the compiler will try to run the callback even if the phase
    // ends with an error. Note that this is not always possible.
    pub run_callback_on_error: bool,
    pub callback: Box<Fn(CompileState) -> () + 'a>,
}

impl<'a> PhaseController<'a> {
    pub fn basic() -> PhaseController<'a> {
        PhaseController {
            stop: Compilation::Continue,
            run_callback_on_error: false,
            callback: Box::new(|_| {}),
        }
    }
}

/// State that is passed to a callback. What state is available depends on when
/// during compilation the callback is made. See the various constructor methods
/// (`state_*`) in the impl to see which data is provided for any given entry point.
pub struct CompileState<'a> {
  pub input: &'a Input,
  pub session: &'a Session,
  pub cfg: Option<&'a ast::CrateConfig>,
  pub krate: Option<&'a ast::Crate>,
  pub crate_name: Option<&'a str>,
}

impl<'a> CompileState<'a> {
    fn empty(input: &'a Input,
             session: &'a Session)
             -> CompileState<'a> {
        CompileState {
            input: input,
            session: session,
            cfg: None,
            krate: None,
            crate_name: None,
        }
    }

    fn state_after_parse(input: &'a Input,
                         session: &'a Session,
                         krate: &'a ast::Crate)
                         -> CompileState<'a> {
        CompileState { krate: Some(krate), ..CompileState::empty(input, session) }
    }
}

/// Input to AST
pub fn phase_1_parse_input<'a>(sess: &'a Session,
                               cfg: ast::CrateConfig,
                               input: &Input)
                               -> PResult<'a, ast::Crate> {

 let krate = time(sess.time_passes(), "parsing", || {
    match *input {
      Input::File(ref file) => {
        parse::parse_crate_from_file(file, cfg.clone(), &sess.parse_sess)
      }
      Input::Str { ref input, ref name } => {
        parse::parse_crate_from_source_str(name.clone(),
                                            input.clone(),
                                            cfg.clone(),
                                            &sess.parse_sess)
      }
    }
 })?;


 Ok(krate)
}

// For continuing compilation after a parsed crate has been
// modified

/// Run the "early phases" of the compiler: initial `cfg` processing,
/// loading compiler plugins (including those from `addl_plugins`),
/// syntax expansion, secondary `cfg` expansion, synthesis of a test
/// harness if one is to be provided and injection of a dependency on the
/// standard library and prelude.
///
/// Returns `None` if we're aborting after handling -W help.
pub fn phase_2_configure_and_expand(sess: &Session,
                                    cstore: &CStore,
                                    mut krate: ast::Crate,
                                    crate_name: &str,
                                    addl_plugins: Option<Vec<String>>)
                                    -> Result<ast::Crate, usize> {
  unimplemented!()
}

/*
/// Run the resolution, typechecking, region checking and other
/// miscellaneous analysis passes on the crate. Return various
/// structures carrying the results of the analysis.
pub fn phase_3_run_analysis_passes<'tcx, F, R>(sess: &'tcx Session,
                                               cstore: &CStore,
                                               hir_map: hir_map::Map<'tcx>,
                                               arenas: &'tcx ty::CtxtArenas<'tcx>,
                                               name: &str,
                                               make_glob_map: resolve::MakeGlobMap,
                                               f: F)
                                               -> Result<R, usize> {
  unimplemented!()
}
*/
