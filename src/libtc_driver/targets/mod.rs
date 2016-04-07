
use rustc_serialize::json::Json;

use std::default::Default;
use std::io::prelude::*;
use syntax::abi::Abi;

mod linux_base;

macro_rules! supported_targets {
    ( $(($triple:expr, $module:ident)),+ ) => (
        $(mod $module;)*

        /// List of supported targets
        pub const TARGETS: &'static [&'static str] = &[$($triple),*];

        // this would use a match if stringify! were allowed in pattern position
        fn load_specific(target: &str) -> Option<Target> {
            let target = target.replace("-", "_");
            if false { }
            $(
                else if target == stringify!($module) {
                    let t = $module::target();
                    debug!("Got builtin target: {:?}", t);
                    return Some(t);
                }
            )*

            None
        }
    )
}

supported_targets! {
  ("x86_64-unknown-linux-gnu", x86_64_unknown_linux_gnu)
}

/// Everything `rustc` knows about how to compile for a specific target.
///
/// Every field here must be specified, and has no default value.
#[derive(Clone, Debug)]
pub struct Target {
    /// Target triple to pass to LLVM.
    pub llvm_target: String,
    /// String to use as the `target_endian` `cfg` variable.
    pub target_endian: String,
    /// String to use as the `target_pointer_width` `cfg` variable.
    pub target_pointer_width: String,
    /// OS name to use for conditional compilation.
    pub target_os: String,
    /// Environment name to use for conditional compilation.
    pub target_env: String,
    /// Vendor name to use for conditional compilation.
    pub target_vendor: String,
    /// Architecture to use for ABI considerations. Valid options: "x86",
    /// "x86_64", "arm", "aarch64", "mips", "powerpc", and "powerpc64".
    pub arch: String,
    /// Optional settings with defaults.
    pub options: TargetOptions,
}

/// Optional aspects of a target specification.
///
/// This has an implementation of `Default`, see each field for what the default is. In general,
/// these try to take "minimal defaults" that don't assume anything about the runtime they run in.
#[derive(Clone, Debug)]
pub struct TargetOptions {
    /// [Data layout](http://llvm.org/docs/LangRef.html#data-layout) to pass to LLVM.
    pub data_layout: Option<String>,
    /// Linker to invoke. Defaults to "cc".
    pub linker: String,
    /// Archive utility to use when managing archives. Defaults to "ar".
    pub ar: String,

    /// Linker arguments that are unconditionally passed *before* any
    /// user-defined libraries.
    pub pre_link_args: Vec<String>,
    /// Objects to link before all others, always found within the
    /// sysroot folder.
    pub pre_link_objects_exe: Vec<String>, // ... when linking an executable
    pub pre_link_objects_dll: Vec<String>, // ... when linking a dylib
    /// Linker arguments that are unconditionally passed after any
    /// user-defined but before post_link_objects.  Standard platform
    /// libraries that should be always be linked to, usually go here.
    pub late_link_args: Vec<String>,
    /// Objects to link after all others, always found within the
    /// sysroot folder.
    pub post_link_objects: Vec<String>,
    /// Linker arguments that are unconditionally passed *after* any
    /// user-defined libraries.
    pub post_link_args: Vec<String>,

    /// Default CPU to pass to LLVM. Corresponds to `llc -mcpu=$cpu`. Defaults
    /// to "default".
    pub cpu: String,
    /// Default target features to pass to LLVM. These features will *always* be
    /// passed, and cannot be disabled even via `-C`. Corresponds to `llc
    /// -mattr=$features`.
    pub features: String,
    /// Whether dynamic linking is available on this target. Defaults to false.
    pub dynamic_linking: bool,
    /// Whether executables are available on this target. iOS, for example, only allows static
    /// libraries. Defaults to false.
    pub executables: bool,
    /// Relocation model to use in object file. Corresponds to `llc
    /// -relocation-model=$relocation_model`. Defaults to "pic".
    pub relocation_model: String,
    /// Code model to use. Corresponds to `llc -code-model=$code_model`. Defaults to "default".
    pub code_model: String,
    /// Do not emit code that uses the "red zone", if the ABI has one. Defaults to false.
    pub disable_redzone: bool,
    /// Eliminate frame pointers from stack frames if possible. Defaults to true.
    pub eliminate_frame_pointer: bool,
    /// Emit each function in its own section. Defaults to true.
    pub function_sections: bool,
    /// String to prepend to the name of every dynamic library. Defaults to "lib".
    pub dll_prefix: String,
    /// String to append to the name of every dynamic library. Defaults to ".so".
    pub dll_suffix: String,
    /// String to append to the name of every executable.
    pub exe_suffix: String,
    /// String to prepend to the name of every static library. Defaults to "lib".
    pub staticlib_prefix: String,
    /// String to append to the name of every static library. Defaults to ".a".
    pub staticlib_suffix: String,
    /// OS family to use for conditional compilation. Valid options: "unix", "windows".
    pub target_family: Option<String>,
    /// Whether the target toolchain is like OSX's. Only useful for compiling against iOS/OS X, in
    /// particular running dsymutil and some other stuff like `-dead_strip`. Defaults to false.
    pub is_like_osx: bool,
    /// Whether the target toolchain is like Solaris's.
    /// Only useful for compiling against Illumos/Solaris,
    /// as they have a different set of linker flags. Defaults to false.
    pub is_like_solaris: bool,
    /// Whether the target toolchain is like Windows'. Only useful for compiling against Windows,
    /// only really used for figuring out how to find libraries, since Windows uses its own
    /// library naming convention. Defaults to false.
    pub is_like_windows: bool,
    pub is_like_msvc: bool,
    /// Whether the target toolchain is like Android's. Only useful for compiling against Android.
    /// Defaults to false.
    pub is_like_android: bool,
    /// Whether the linker support GNU-like arguments such as -O. Defaults to false.
    pub linker_is_gnu: bool,
    /// Whether the linker support rpaths or not. Defaults to false.
    pub has_rpath: bool,
    /// Whether to disable linking to compiler-rt. Defaults to false, as LLVM
    /// will emit references to the functions that compiler-rt provides.
    pub no_compiler_rt: bool,
    /// Whether to disable linking to the default libraries, typically corresponds
    /// to `-nodefaultlibs`. Defaults to true.
    pub no_default_libraries: bool,
    /// Dynamically linked executables can be compiled as position independent
    /// if the default relocation model of position independent code is not
    /// changed. This is a requirement to take advantage of ASLR, as otherwise
    /// the functions in the executable are not randomized and can be used
    /// during an exploit of a vulnerability in any code.
    pub position_independent_executables: bool,
    /// Format that archives should be emitted in. This affects whether we use
    /// LLVM to assemble an archive or fall back to the system linker, and
    /// currently only "gnu" is used to fall into LLVM. Unknown strings cause
    /// the system linker to be used.
    pub archive_format: String,
    /// Is asm!() allowed? Defaults to true.
    pub allow_asm: bool,
    /// Whether the target uses a custom unwind resumption routine.
    /// By default LLVM lowers `resume` instructions into calls to `_Unwind_Resume`
    /// defined in libgcc.  If this option is enabled, the target must provide
    /// `eh_unwind_resume` lang item.
    pub custom_unwind_resume: bool,

    /// Default crate for allocation symbols to link against
    pub lib_allocation_crate: String,
    pub exe_allocation_crate: String,

    /// Flag indicating whether ELF TLS (e.g. #[thread_local]) is available for
    /// this target.
    pub has_elf_tls: bool,
    // This is mainly for easy compatibility with emscripten.
    // If we give emcc .o files that are actually .bc files it
    // will 'just work'.
    pub obj_is_bitcode: bool,
}

impl Default for TargetOptions {
    /// Create a set of "sane defaults" for any target. This is still
    /// incomplete, and if used for compilation, will certainly not work.
    fn default() -> TargetOptions {
        TargetOptions {
            data_layout: None,
            linker: option_env!("CFG_DEFAULT_LINKER").unwrap_or("cc").to_string(),
            ar: option_env!("CFG_DEFAULT_AR").unwrap_or("ar").to_string(),
            pre_link_args: Vec::new(),
            post_link_args: Vec::new(),
            cpu: "generic".to_string(),
            features: "".to_string(),
            dynamic_linking: false,
            executables: false,
            relocation_model: "pic".to_string(),
            code_model: "default".to_string(),
            disable_redzone: false,
            eliminate_frame_pointer: true,
            function_sections: true,
            dll_prefix: "lib".to_string(),
            dll_suffix: ".so".to_string(),
            exe_suffix: "".to_string(),
            staticlib_prefix: "lib".to_string(),
            staticlib_suffix: ".a".to_string(),
            target_family: None,
            is_like_osx: false,
            is_like_solaris: false,
            is_like_windows: false,
            is_like_android: false,
            is_like_msvc: false,
            linker_is_gnu: false,
            has_rpath: false,
            no_compiler_rt: false,
            no_default_libraries: true,
            position_independent_executables: false,
            pre_link_objects_exe: Vec::new(),
            pre_link_objects_dll: Vec::new(),
            post_link_objects: Vec::new(),
            late_link_args: Vec::new(),
            archive_format: "gnu".to_string(),
            custom_unwind_resume: false,
            lib_allocation_crate: "alloc_system".to_string(),
            exe_allocation_crate: "alloc_system".to_string(),
            allow_asm: true,
            has_elf_tls: false,
            obj_is_bitcode: false,
        }
    }
}

impl Target {
    /// Given a function ABI, turn "System" into the correct ABI for this target.
    pub fn adjust_abi(&self, abi: Abi) -> Abi {
        match abi {
            Abi::System => {
                if self.options.is_like_windows && self.arch == "x86" {
                    Abi::Stdcall
                } else {
                    Abi::C
                }
            },
            abi => abi
        }
    }

    /// Load a target descriptor from a JSON object.
    pub fn from_json(obj: Json) -> Target {
        // this is 1. ugly, 2. error prone.

        let get_req_field = |name: &str| {
            match obj.find(name)
                     .map(|s| s.as_string())
                     .and_then(|os| os.map(|s| s.to_string())) {
                Some(val) => val,
                None => {
                    panic!("Field {} in target specification is required", name)
                }
            }
        };

        let get_opt_field = |name: &str, default: &str| {
            obj.find(name).and_then(|s| s.as_string())
               .map(|s| s.to_string())
               .unwrap_or(default.to_string())
        };

        let mut base = Target {
            llvm_target: get_req_field("llvm-target"),
            target_endian: get_req_field("target-endian"),
            target_pointer_width: get_req_field("target-pointer-width"),
            arch: get_req_field("arch"),
            target_os: get_req_field("os"),
            target_env: get_opt_field("env", ""),
            target_vendor: get_opt_field("vendor", "unknown"),
            options: Default::default(),
        };

        macro_rules! key {
            ($key_name:ident) => ( {
                let name = (stringify!($key_name)).replace("_", "-");
                obj.find(&name[..]).map(|o| o.as_string()
                                    .map(|s| base.options.$key_name = s.to_string()));
            } );
            ($key_name:ident, bool) => ( {
                let name = (stringify!($key_name)).replace("_", "-");
                obj.find(&name[..])
                    .map(|o| o.as_boolean()
                         .map(|s| base.options.$key_name = s));
            } );
            ($key_name:ident, list) => ( {
                let name = (stringify!($key_name)).replace("_", "-");
                obj.find(&name[..]).map(|o| o.as_array()
                    .map(|v| base.options.$key_name = v.iter()
                        .map(|a| a.as_string().unwrap().to_string()).collect()
                        )
                    );
            } );
            ($key_name:ident, optional) => ( {
                let name = (stringify!($key_name)).replace("_", "-");
                if let Some(o) = obj.find(&name[..]) {
                    base.options.$key_name = o
                        .as_string()
                        .map(|s| s.to_string() );
                }
            } );
        }

        key!(cpu);
        key!(ar);
        key!(linker);
        key!(relocation_model);
        key!(code_model);
        key!(dll_prefix);
        key!(dll_suffix);
        key!(exe_suffix);
        key!(staticlib_prefix);
        key!(staticlib_suffix);
        key!(features);
        key!(data_layout, optional);
        key!(dynamic_linking, bool);
        key!(executables, bool);
        key!(disable_redzone, bool);
        key!(eliminate_frame_pointer, bool);
        key!(function_sections, bool);
        key!(target_family, optional);
        key!(is_like_osx, bool);
        key!(is_like_windows, bool);
        key!(linker_is_gnu, bool);
        key!(has_rpath, bool);
        key!(no_compiler_rt, bool);
        key!(no_default_libraries, bool);
        key!(pre_link_args, list);
        key!(post_link_args, list);
        key!(archive_format);
        key!(allow_asm, bool);
        key!(custom_unwind_resume, bool);

        base
    }

    /// Search RUST_TARGET_PATH for a JSON file specifying the given target
    /// triple. Note that it could also just be a bare filename already, so also
    /// check for that. If one of the hardcoded targets we know about, just
    /// return it directly.
    ///
    /// The error string could come from any of the APIs called, including
    /// filesystem access and JSON decoding.
    pub fn search(target: &str) -> Result<Target, String> {
        use std::env;
        use std::ffi::OsString;
        use std::fs::File;
        use std::path::{Path, PathBuf};

        fn load_file(path: &Path) -> Result<Target, String> {
            let mut f = File::open(path).map_err(|e| e.to_string())?;
            let mut contents = Vec::new();
            f.read_to_end(&mut contents).map_err(|e| e.to_string())?;
            let obj = Json::from_reader(&mut &contents[..])
                           .map_err(|e| e.to_string())?;
            Ok(Target::from_json(obj))
        }

        if let Some(t) = load_specific(target) {
            return Ok(t)
        }

        let path = Path::new(target);

        if path.is_file() {
            return load_file(&path);
        }

        let path = {
            let mut target = target.to_string();
            target.push_str(".json");
            PathBuf::from(target)
        };

        let target_path = env::var_os("RUST_TARGET_PATH")
                              .unwrap_or(OsString::new());

        // FIXME 16351: add a sane default search path?

        for dir in env::split_paths(&target_path) {
            let p =  dir.join(&path);
            if p.is_file() {
                return load_file(&p);
            }
        }

        Err(format!("Could not find specification for target {:?}", target))
    }
}

fn maybe_jemalloc() -> String {
    if cfg!(feature = "jemalloc") {
        "alloc_jemalloc".to_string()
    } else {
        "alloc_system".to_string()
    }
}