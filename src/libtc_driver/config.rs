//! Contains infrastructure for configuring the compiler, including parsing
//! command line options.

pub use self::DebugInfoLevel::*;
pub use self::Passes::*;

use cstore;
use search_paths::SearchPaths;
use session::{early_error, early_warn, Session};

use syntax::ast::{self, IntTy, UintTy};
use syntax::attr::{self, AttrMetaMethods};
use syntax::errors::{ColorConfig, Handler};
use syntax::feature_gate::UnstableFeatures;
use syntax::parse::token::InternedString;
use targets::Target;

use getopts;
use std::path::PathBuf;
use std::collections::HashMap;

pub struct Config {
  pub target: Target,
  pub int_type: IntTy,
  pub uint_type: UintTy,
}

#[derive(Clone, Copy, PartialEq)]
pub enum OptLevel {
    No, // -O0
    Less, // -O1
    Default, // -O2
    Aggressive // -O3
}

#[derive(Clone, Copy, PartialEq)]
pub enum DebugInfoLevel {
    NoDebugInfo,
    LimitedDebugInfo,
    FullDebugInfo,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ErrorOutputType {
    HumanReadable(ColorConfig),
    Json,
}

impl Default for ErrorOutputType {
    fn default() -> ErrorOutputType {
        ErrorOutputType::HumanReadable(ColorConfig::Auto)
    }
}

#[derive(Clone)]
pub struct Options {
  pub optimize: OptLevel,
  pub debug_assertions: bool,
  pub debuginfo: DebugInfoLevel,
  // This was mutable for rustpkg, which updates search paths based on the
  // parsed code. It remains mutable in case its replacements wants to use
  // this.
  pub search_paths: SearchPaths,
  pub libs: Vec<(String, cstore::NativeLibraryKind)>,
  pub maybe_sysroot: Option<PathBuf>,
  pub target_triple: String,
  // User-specified cfg meta items. The compiler itself will add additional
  // items to the crate config, and during parsing the entire crate config
  // will be added to the crate AST node.  This should not be used for
  // anything except building the full crate config prior to parsing.
  pub cfg: ast::CrateConfig,
  pub test: bool,
  pub parse_only: bool,
  pub no_trans: bool,
  pub error_format: ErrorOutputType,
  pub treat_err_as_bug: bool,
  pub mir_opt_level: usize,

  /// if true, build up the dep-graph
  pub build_dep_graph: bool,

  /// if true, -Z dump-dep-graph was passed to dump out the dep-graph
  pub dump_dep_graph: bool,

  pub no_analysis: bool,
  pub debugging_opts: DebuggingOptions,
  pub prints: Vec<PrintRequest>,
  pub cg: CodegenOptions,
  pub externs: HashMap<String, Vec<String>>,
  pub crate_name: Option<String>,

  /// Indicates how the compiler should treat unstable features
  pub unstable_features: UnstableFeatures
}

#[derive(Clone, PartialEq, Eq)]
pub enum PrintRequest {
    FileNames,
    Sysroot,
    CrateName,
    Cfg,
    TargetList,
}

pub enum Input {
    /// Load source from file
    File(PathBuf),
    Str {
        /// String that is shown in place of a filename
        name: String,
        /// Anonymous source string
        input: String,
    },
}

impl Input {
    pub fn filestem(&self) -> String {
        match *self {
            Input::File(ref ifile) => ifile.file_stem().unwrap()
                                           .to_str().unwrap().to_string(),
            Input::Str { .. } => "rust_out".to_string(),
        }
    }
}

pub fn host_triple() -> &'static str {
    // Get the host triple out of the build environment. This ensures that our
    // idea of the host triple is the same as for the set of libraries we've
    // actually built.  We can't just take LLVM's host triple because they
    // normalize all ix86 architectures to i386.
    //
    // Instead of grabbing the host triple (for the current host), we grab (at
    // compile time) the target triple that this rustc is built with and
    // calling that (at runtime) the host triple.
    (option_env!("CFG_COMPILER_HOST_TRIPLE")).
        expect("CFG_COMPILER_HOST_TRIPLE")
}

/// Some reasonable defaults
pub fn basic_options() -> Options {
    Options {
        optimize: OptLevel::No,
        debuginfo: NoDebugInfo,
        search_paths: SearchPaths::new(),
        maybe_sysroot: None,
        target_triple: host_triple().to_string(),
        cfg: Vec::new(),
        test: false,
        parse_only: false,
        no_trans: false,
        treat_err_as_bug: false,
        mir_opt_level: 1,
        build_dep_graph: false,
        dump_dep_graph: false,
        no_analysis: false,
        debugging_opts: basic_debugging_options(),
        prints: Vec::new(),
        cg: basic_codegen_options(),
        error_format: ErrorOutputType::default(),
        externs: HashMap::new(),
        crate_name: None,
        libs: Vec::new(),
        unstable_features: UnstableFeatures::Disallow,
        debug_assertions: true,
    }
}

// The type of entry function, so
// users can have their own entry
// functions that don't start a
// scheduler
#[derive(Copy, Clone, PartialEq)]
pub enum EntryFnType {
    EntryMain,
    EntryStart,
    EntryNone,
}

#[derive(Clone)]
pub enum Passes {
    SomePasses(Vec<String>),
    AllPasses,
}

impl Passes {
    pub fn is_empty(&self) -> bool {
        match *self {
            SomePasses(ref v) => v.is_empty(),
            AllPasses => false,
        }
    }
}

/// Declare a macro that will define all CodegenOptions/DebuggingOptions fields and parsers all
/// at once. The goal of this macro is to define an interface that can be
/// programmatically used by the option parser in order to initialize the struct
/// without hardcoding field names all over the place.
///
/// The goal is to invoke this macro once with the correct fields, and then this
/// macro generates all necessary code. The main gotcha of this macro is the
/// cgsetters module which is a bunch of generated code to parse an option into
/// its respective field in the struct. There are a few hand-written parsers for
/// parsing specific types of values in this module.
macro_rules! options {
    ($struct_name:ident, $setter_name:ident, $defaultfn:ident,
     $buildfn:ident, $prefix:expr, $outputname:expr,
     $stat:ident, $mod_desc:ident, $mod_set:ident,
     $($opt:ident : $t:ty = ($init:expr, $parse:ident, $desc:expr)),* ,) =>
(
    #[derive(Clone)]
    pub struct $struct_name { $(pub $opt: $t),* }

    pub fn $defaultfn() -> $struct_name {
        $struct_name { $($opt: $init),* }
    }

    pub fn $buildfn(matches: &getopts::Matches, error_format: ErrorOutputType) -> $struct_name
    {
        let mut op = $defaultfn();
        for option in matches.opt_strs($prefix) {
            let mut iter = option.splitn(2, '=');
            let key = iter.next().unwrap();
            let value = iter.next();
            let option_to_lookup = key.replace("-", "_");
            let mut found = false;
            for &(candidate, setter, opt_type_desc, _) in $stat {
                if option_to_lookup != candidate { continue }
                if !setter(&mut op, value) {
                    match (value, opt_type_desc) {
                        (Some(..), None) => {
                            early_error(error_format, &format!("{} option `{}` takes no \
                                                              value", $outputname, key))
                        }
                        (None, Some(type_desc)) => {
                            early_error(error_format, &format!("{0} option `{1}` requires \
                                                              {2} ({3} {1}=<value>)",
                                                             $outputname, key,
                                                             type_desc, $prefix))
                        }
                        (Some(value), Some(type_desc)) => {
                            early_error(error_format, &format!("incorrect value `{}` for {} \
                                                              option `{}` - {} was expected",
                                                             value, $outputname,
                                                             key, type_desc))
                        }
                        (None, None) => unreachable!()
                    }
                }
                found = true;
                break;
            }
            if !found {
                early_error(error_format, &format!("unknown {} option: `{}`",
                                                 $outputname, key));
            }
        }
        return op;
    }

    pub type $setter_name = fn(&mut $struct_name, v: Option<&str>) -> bool;
    pub const $stat: &'static [(&'static str, $setter_name,
                                     Option<&'static str>, &'static str)] =
        &[ $( (stringify!($opt), $mod_set::$opt, $mod_desc::$parse, $desc) ),* ];

    #[allow(non_upper_case_globals, dead_code)]
    mod $mod_desc {
        pub const parse_bool: Option<&'static str> = None;
        pub const parse_opt_bool: Option<&'static str> =
            Some("one of: `y`, `yes`, `on`, `n`, `no`, or `off`");
        pub const parse_string: Option<&'static str> = Some("a string");
        pub const parse_opt_string: Option<&'static str> = Some("a string");
        pub const parse_list: Option<&'static str> = Some("a space-separated list of strings");
        pub const parse_opt_list: Option<&'static str> = Some("a space-separated list of strings");
        pub const parse_uint: Option<&'static str> = Some("a number");
        pub const parse_passes: Option<&'static str> =
            Some("a space-separated list of passes, or `all`");
        pub const parse_opt_uint: Option<&'static str> =
            Some("a number");
    }

    #[allow(dead_code)]
    mod $mod_set {
        use super::{$struct_name, Passes, SomePasses, AllPasses};

        $(
            pub fn $opt(cg: &mut $struct_name, v: Option<&str>) -> bool {
                $parse(&mut cg.$opt, v)
            }
        )*

        fn parse_bool(slot: &mut bool, v: Option<&str>) -> bool {
            match v {
                Some(..) => false,
                None => { *slot = true; true }
            }
        }

        fn parse_opt_bool(slot: &mut Option<bool>, v: Option<&str>) -> bool {
            match v {
                Some(s) => {
                    match s {
                        "n" | "no" | "off" => {
                            *slot = Some(false);
                        }
                        "y" | "yes" | "on" => {
                            *slot = Some(true);
                        }
                        _ => { return false; }
                    }

                    true
                },
                None => { *slot = Some(true); true }
            }
        }

        fn parse_opt_string(slot: &mut Option<String>, v: Option<&str>) -> bool {
            match v {
                Some(s) => { *slot = Some(s.to_string()); true },
                None => false,
            }
        }

        fn parse_string(slot: &mut String, v: Option<&str>) -> bool {
            match v {
                Some(s) => { *slot = s.to_string(); true },
                None => false,
            }
        }

        fn parse_list(slot: &mut Vec<String>, v: Option<&str>)
                      -> bool {
            match v {
                Some(s) => {
                    for s in s.split_whitespace() {
                        slot.push(s.to_string());
                    }
                    true
                },
                None => false,
            }
        }

        fn parse_opt_list(slot: &mut Option<Vec<String>>, v: Option<&str>)
                      -> bool {
            match v {
                Some(s) => {
                    let v = s.split_whitespace().map(|s| s.to_string()).collect();
                    *slot = Some(v);
                    true
                },
                None => false,
            }
        }

        fn parse_uint(slot: &mut usize, v: Option<&str>) -> bool {
            match v.and_then(|s| s.parse().ok()) {
                Some(i) => { *slot = i; true },
                None => false
            }
        }

        fn parse_opt_uint(slot: &mut Option<usize>, v: Option<&str>) -> bool {
            match v {
                Some(s) => { *slot = s.parse().ok(); slot.is_some() }
                None => { *slot = None; true }
            }
        }

        fn parse_passes(slot: &mut Passes, v: Option<&str>) -> bool {
            match v {
                Some("all") => {
                    *slot = AllPasses;
                    true
                }
                v => {
                    let mut passes = vec!();
                    if parse_list(&mut passes, v) {
                        *slot = SomePasses(passes);
                        true
                    } else {
                        false
                    }
                }
            }
        }
    }
) }

options! {CodegenOptions, CodegenSetter, basic_codegen_options,
         build_codegen_options, "C", "codegen",
         CG_OPTIONS, cg_type_desc, cgsetters,
    ar: Option<String> = (None, parse_opt_string,
        "tool to assemble archives with"),
    linker: Option<String> = (None, parse_opt_string,
        "system linker to link outputs with"),
    link_args: Option<Vec<String>> = (None, parse_opt_list,
        "extra arguments to pass to the linker (space separated)"),
    link_dead_code: bool = (false, parse_bool,
        "don't let linker strip dead code (turning it on can be used for code coverage)"),
    lto: bool = (false, parse_bool,
        "perform LLVM link-time optimizations"),
    target_cpu: Option<String> = (None, parse_opt_string,
        "select target processor (llc -mcpu=help for details)"),
    target_feature: String = ("".to_string(), parse_string,
        "target specific attributes (llc -mattr=help for details)"),
    passes: Vec<String> = (Vec::new(), parse_list,
        "a list of extra LLVM passes to run (space separated)"),
    llvm_args: Vec<String> = (Vec::new(), parse_list,
        "a list of arguments to pass to llvm (space separated)"),
    save_temps: bool = (false, parse_bool,
        "save all temporary output files during compilation"),
    rpath: bool = (false, parse_bool,
        "set rpath values in libs/exes"),
    no_prepopulate_passes: bool = (false, parse_bool,
        "don't pre-populate the pass manager with a list of passes"),
    no_vectorize_loops: bool = (false, parse_bool,
        "don't run the loop vectorization optimization passes"),
    no_vectorize_slp: bool = (false, parse_bool,
        "don't run LLVM's SLP vectorization pass"),
    soft_float: bool = (false, parse_bool,
        "generate software floating point library calls"),
    prefer_dynamic: bool = (false, parse_bool,
        "prefer dynamic linking to static linking"),
    no_integrated_as: bool = (false, parse_bool,
        "use an external assembler rather than LLVM's integrated one"),
    no_redzone: Option<bool> = (None, parse_opt_bool,
        "disable the use of the redzone"),
    relocation_model: Option<String> = (None, parse_opt_string,
         "choose the relocation model to use (llc -relocation-model for details)"),
    code_model: Option<String> = (None, parse_opt_string,
         "choose the code model to use (llc -code-model for details)"),
    metadata: Vec<String> = (Vec::new(), parse_list,
         "metadata to mangle symbol names with"),
    extra_filename: String = ("".to_string(), parse_string,
         "extra data to put in each output filename"),
    codegen_units: usize = (1, parse_uint,
        "divide crate into N units to optimize in parallel"),
    remark: Passes = (SomePasses(Vec::new()), parse_passes,
        "print remarks for these optimization passes (space separated, or \"all\")"),
    no_stack_check: bool = (false, parse_bool,
        "disable checks for stack exhaustion (a memory-safety hazard!)"),
    debuginfo: Option<usize> = (None, parse_opt_uint,
        "debug info emission level, 0 = no debug info, 1 = line tables only, \
         2 = full debug info with variable and type information"),
    opt_level: Option<usize> = (None, parse_opt_uint,
        "optimize with possible levels 0-3"),
    debug_assertions: Option<bool> = (None, parse_opt_bool,
        "explicitly enable the cfg(debug_assertions) directive"),
    inline_threshold: Option<usize> = (None, parse_opt_uint,
        "set the inlining threshold for"),
}

options! {DebuggingOptions, DebuggingSetter, basic_debugging_options,
         build_debugging_options, "Z", "debugging",
         DB_OPTIONS, db_type_desc, dbsetters,
    verbose: bool = (false, parse_bool,
        "in general, enable more debug printouts"),
    time_passes: bool = (false, parse_bool,
        "measure time of each rustc pass"),
    count_llvm_insns: bool = (false, parse_bool,
        "count where LLVM instrs originate"),
    time_llvm_passes: bool = (false, parse_bool,
        "measure time of each LLVM pass"),
    input_stats: bool = (false, parse_bool,
        "gather statistics about the input"),
    trans_stats: bool = (false, parse_bool,
        "gather trans statistics"),
    asm_comments: bool = (false, parse_bool,
        "generate comments into the assembly (may change behavior)"),
    no_verify: bool = (false, parse_bool,
        "skip LLVM verification"),
    borrowck_stats: bool = (false, parse_bool,
        "gather borrowck statistics"),
    no_landing_pads: bool = (false, parse_bool,
        "omit landing pads for unwinding"),
    debug_llvm: bool = (false, parse_bool,
        "enable debug output from LLVM"),
    count_type_sizes: bool = (false, parse_bool,
        "count the sizes of aggregate types"),
    meta_stats: bool = (false, parse_bool,
        "gather metadata statistics"),
    print_link_args: bool = (false, parse_bool,
        "print the arguments passed to the linker"),
    gc: bool = (false, parse_bool,
        "garbage collect shared data (experimental)"),
    print_llvm_passes: bool = (false, parse_bool,
        "prints the llvm optimization passes being run"),
    ast_json: bool = (false, parse_bool,
        "print the AST as JSON and halt"),
    ast_json_noexpand: bool = (false, parse_bool,
        "print the pre-expansion AST as JSON and halt"),
    ls: bool = (false, parse_bool,
        "list the symbols defined by a library crate"),
    save_analysis: bool = (false, parse_bool,
        "write syntax and type analysis information in addition to normal output"),
    print_move_fragments: bool = (false, parse_bool,
        "print out move-fragment data for every fn"),
    flowgraph_print_loans: bool = (false, parse_bool,
        "include loan analysis data in --unpretty flowgraph output"),
    flowgraph_print_moves: bool = (false, parse_bool,
        "include move analysis data in --unpretty flowgraph output"),
    flowgraph_print_assigns: bool = (false, parse_bool,
        "include assignment analysis data in --unpretty flowgraph output"),
    flowgraph_print_all: bool = (false, parse_bool,
        "include all dataflow analysis data in --unpretty flowgraph output"),
    print_region_graph: bool = (false, parse_bool,
         "prints region inference graph. \
          Use with RUST_REGION_GRAPH=help for more info"),
    parse_only: bool = (false, parse_bool,
          "parse only; do not compile, assemble, or link"),
    no_trans: bool = (false, parse_bool,
          "run all passes except translation; no output"),
    treat_err_as_bug: bool = (false, parse_bool,
          "treat all errors that occur as bugs"),
    incr_comp: bool = (false, parse_bool,
          "enable incremental compilation (experimental)"),
    dump_dep_graph: bool = (false, parse_bool,
          "dump the dependency graph to $RUST_DEP_GRAPH (default: /tmp/dep_graph.gv)"),
    no_analysis: bool = (false, parse_bool,
          "parse and expand the source, but run no analysis"),
    extra_plugins: Vec<String> = (Vec::new(), parse_list,
        "load extra plugins"),
    unstable_options: bool = (false, parse_bool,
          "adds unstable command line options to rustc interface"),
    print_enum_sizes: bool = (false, parse_bool,
          "print the size of enums and their variants"),
    force_overflow_checks: Option<bool> = (None, parse_opt_bool,
          "force overflow checks on or off"),
    force_dropflag_checks: Option<bool> = (None, parse_opt_bool,
          "force drop flag checks on or off"),
    trace_macros: bool = (false, parse_bool,
          "for every macro invocation, print its name and arguments"),
    enable_nonzeroing_move_hints: bool = (false, parse_bool,
          "force nonzeroing move optimization on"),
    keep_mtwt_tables: bool = (false, parse_bool,
          "don't clear the resolution tables after analysis"),
    keep_ast: bool = (false, parse_bool,
          "keep the AST after lowering it to HIR"),
    show_span: Option<String> = (None, parse_opt_string,
          "show spans for compiler debugging (expr|pat|ty)"),
    print_trans_items: Option<String> = (None, parse_opt_string,
          "print the result of the translation item collection pass"),
    mir_opt_level: Option<usize> = (None, parse_opt_uint,
          "set the MIR optimization level (0-3)"),
    dump_mir: Option<String> = (None, parse_opt_string,
          "dump MIR state at various points in translation"),
    orbit: bool = (false, parse_bool,
          "get MIR where it belongs - everywhere; most importantly, in orbit"),
}

pub fn default_configuration(sess: &Session) -> ast::CrateConfig {
    use syntax::parse::token::intern_and_get_ident as intern;

    let end = &sess.target.target.target_endian;
    let arch = &sess.target.target.arch;
    let wordsz = &sess.target.target.target_pointer_width;
    let os = &sess.target.target.target_os;
    let env = &sess.target.target.target_env;
    let vendor = &sess.target.target.target_vendor;

    let fam = if let Some(ref fam) = sess.target.target.options.target_family {
        intern(fam)
    } else if sess.target.target.options.is_like_windows {
        InternedString::new("windows")
    } else {
        InternedString::new("unix")
    };

    let mk = attr::mk_name_value_item_str;
    let mut ret = vec![ // Target bindings.
        mk(InternedString::new("target_os"), intern(os)),
        mk(InternedString::new("target_family"), fam.clone()),
        mk(InternedString::new("target_arch"), intern(arch)),
        mk(InternedString::new("target_endian"), intern(end)),
        mk(InternedString::new("target_pointer_width"), intern(wordsz)),
        mk(InternedString::new("target_env"), intern(env)),
        mk(InternedString::new("target_vendor"), intern(vendor)),
    ];
    match &fam[..] {
        "windows" | "unix" => ret.push(attr::mk_word_item(fam)),
        _ => (),
    }
    if sess.target.target.options.has_elf_tls {
        ret.push(attr::mk_word_item(InternedString::new("target_thread_local")));
    }
    if sess.opts.debug_assertions {
        ret.push(attr::mk_word_item(InternedString::new("debug_assertions")));
    }
    return ret;
}

pub fn append_configuration(cfg: &mut ast::CrateConfig,
                            name: InternedString) {
    if !cfg.iter().any(|mi| mi.name() == name) {
        cfg.push(attr::mk_word_item(name))
    }
}

pub fn build_configuration(sess: &Session) -> ast::CrateConfig {
    // Combine the configuration requested by the session (command line) with
    // some default and generated configuration items
    let default_cfg = default_configuration(sess);
    let mut user_cfg = sess.opts.cfg.clone();
    // If the user wants a test runner, then add the test cfg
    if sess.opts.test {
        append_configuration(&mut user_cfg, InternedString::new("test"))
    }
    let mut v = user_cfg.into_iter().collect::<Vec<_>>();
    v.extend_from_slice(&default_cfg[..]);
    v
}

pub fn build_target_config(opts: &Options, sp: &Handler) -> Config {
  let target = match Target::search(&opts.target_triple) {
    Ok(t) => t,
    Err(e) => {
      panic!(sp.fatal(&format!("Error loading target specification: {}", e)));
    }
  };

  let (int_type, uint_type) = match &target.target_pointer_width[..] {
    "32" => (ast::IntTy::I32, ast::UintTy::U32),
    "64" => (ast::IntTy::I64, ast::UintTy::U64),
    w    => panic!(sp.fatal(&format!("target specification was invalid: \
                                          unrecognized target-pointer-width {}", w))),
  };

  Config {
    target: target,
    int_type: int_type,
    uint_type: uint_type,
  }
}

/// Returns the "short" subset of the rustc command line options,
/// including metadata for each option, such as whether the option is
/// part of the stable long-term interface for rustc.
pub fn compiler_short_optgroups() -> getopts::Options {
  let mut opts = getopts::Options::new();
  opts.optflag("V", "version", "Print version info and exit");
  opts.optflag("v", "verbose", "Use verbose output");

  opts
}

/// Returns all rustc command line options, including metadata for
/// each option, such as whether the option is part of the stable
/// long-term interface for rustc.
pub fn compiler_optgroups() -> getopts::Options {
  compiler_short_optgroups()
}

pub fn build_session_options(matches: &getopts::Matches) -> Options {
  let color = match matches.opt_str("color").as_ref().map(|s| &s[..]) {
      Some("auto")   => ColorConfig::Auto,
      Some("always") => ColorConfig::Always,
      Some("never")  => ColorConfig::Never,
      None => ColorConfig::Auto,

      Some(arg) => {
          early_error(ErrorOutputType::default(), &format!("argument for --color must be auto, \
                                                            always or never (instead was `{}`)",
                                                            arg))
      }
  };

  // We need the opts_present check because the driver will send us Matches
  // with only stable options if no unstable options are used. Since error-format
  // is unstable, it will not be present. We have to use opts_present not
  // opt_present because the latter will panic.
  let error_format = if matches.opts_present(&["error-format".to_owned()]) {
    match matches.opt_str("error-format").as_ref().map(|s| &s[..]) {
      Some("human")   => ErrorOutputType::HumanReadable(color),
      Some("json") => ErrorOutputType::Json,

      None => ErrorOutputType::HumanReadable(color),

      Some(arg) => {
          early_error(ErrorOutputType::HumanReadable(color),
                      &format!("argument for --error-format must be human or json (instead \
                                was `{}`)",
                                arg))
      }
    }
  } else {
    ErrorOutputType::HumanReadable(color)
  };

  let debugging_opts = build_debugging_options(matches, error_format);
  let mut cg = build_codegen_options(matches, error_format);

  if cg.codegen_units < 1 {
    early_error(error_format, "Value for codegen units must be a positive nonzero integer");
  }

  let cg = cg;
  let target = matches.opt_str("target").unwrap_or(host_triple().to_string());
  let opt_level = {
     if matches.opt_present("O") {
       if cg.opt_level.is_some() {
          early_error(error_format, "-O and -C opt-level both provided");
       }
       OptLevel::Default
     } else {
       match cg.opt_level {
         None => OptLevel::No,
         Some(0) => OptLevel::No,
         Some(1) => OptLevel::Less,
         Some(2) => OptLevel::Default,
         Some(3) => OptLevel::Aggressive,
         Some(arg) => {
            early_error(error_format, &format!("optimization level needs to be \
                                                between 0-3 (instead was `{}`)",
                                                arg));
         }
      }
    }
  };
  let debug_assertions = cg.debug_assertions.unwrap_or(opt_level == OptLevel::No);
  let debuginfo = if matches.opt_present("g") {
    if cg.debuginfo.is_some() {
        early_error(error_format, "-g and -C debuginfo both provided");
    }
    FullDebugInfo
  } else {
    match cg.debuginfo {
      None | Some(0) => NoDebugInfo,
      Some(1) => LimitedDebugInfo,
      Some(2) => FullDebugInfo,
      Some(arg) => {
        early_error(error_format, &format!("debug info level needs to be between \
                                            0-2 (instead was `{}`)",
                                            arg));
      }
    }
  };

  let mut search_paths = SearchPaths::new();
  for s in &matches.opt_strs("L") {
    search_paths.add_path(&s[..], error_format);
  }

  let libs = matches.opt_strs("l").into_iter().map(|s| {
    let mut parts = s.splitn(2, '=');
    let kind = parts.next().unwrap();
    let (name, kind) = match (parts.next(), kind) {
        (None, name) |
        (Some(name), "dylib") => (name, cstore::NativeUnknown),
        (Some(name), "framework") => (name, cstore::NativeFramework),
        (Some(name), "static") => (name, cstore::NativeStatic),
        (_, s) => {
          early_error(error_format, &format!("unknown library kind `{}`, expected \
                                              one of dylib, framework, or static",
                                              s));
        }
    };
    (name.to_string(), kind)
  }).collect();

  let test = matches.opt_present("test");

  Options {
        optimize: opt_level,
        debuginfo: debuginfo,
        search_paths: search_paths,
        maybe_sysroot: None,
        target_triple: target,
        cfg: Vec::new(),
        test: test,
        parse_only: false,
        no_trans: false,
        treat_err_as_bug: false,
        mir_opt_level: 1,
        build_dep_graph: false,
        dump_dep_graph: false,
        no_analysis: false,
        debugging_opts: basic_debugging_options(),
        prints: Vec::new(),
        cg: cg,
        error_format: error_format,
        externs: HashMap::new(),
        crate_name: None,
        libs: libs,
        unstable_features: UnstableFeatures::Disallow,
        debug_assertions: debug_assertions
    }
}
