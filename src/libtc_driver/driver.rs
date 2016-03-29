use super::Compilation;

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

pub struct PhaseController<'a> {
    pub stop: Compilation,
    // If true then the compiler will try to run the callback even if the phase
    // ends with an error. Note that this is not always possible.
    pub run_callback_on_error: bool,
    pub callback: Box<Fn(CompileState) -> () + 'a>,
}

/// State that is passed to a callback. What state is available depends on when
/// during compilation the callback is made. See the various constructor methods
/// (`state_*`) in the impl to see which data is provided for any given entry point.
pub struct CompileState;