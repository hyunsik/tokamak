pub use self::NativeLibraryKind::{NativeStatic, NativeFramework, NativeUnknown};

enum_from_u32! {
    #[derive(Copy, Clone, PartialEq)]
    pub enum NativeLibraryKind {
        NativeStatic,    // native static library (.a archive)
        NativeFramework, // OSX-specific
        NativeUnknown,   // default way to specify a dynamic library
    }
}