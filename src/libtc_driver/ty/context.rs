use session::Session;
use syntax::attr;
use typed_arena::Arena;

/// Internal storage
pub struct CtxtArenas {
  stability: Arena<attr::Stability>,
}

/// The data structure to keep track of all the information that typechecker
/// generates so that so that it can be reused and doesn't have to be redone
/// later on.
pub struct TyCtxt<'tcx> {
    /// The arenas that types etc are allocated from.
    arenas: &'tcx CtxtArenas,
    pub sess: &'tcx Session,
}

pub mod tls {
    use ty::TyCtxt;

    use std::cell::Cell;
    use std::fmt;
    use syntax::codemap;

    /// Marker type used for the scoped TLS slot.
    /// The type context cannot be used directly because the scoped TLS
    /// in libstd doesn't allow types generic over lifetimes.
    struct ThreadLocalTyCx;

    thread_local! {
        static TLS_TCX: Cell<Option<*const ThreadLocalTyCx>> = Cell::new(None)
    }

    pub fn with<F: FnOnce(&TyCtxt) -> R, R>(f: F) -> R {
        TLS_TCX.with(|tcx| {
            let tcx = tcx.get().unwrap();
            f(unsafe { &*(tcx as *const TyCtxt) })
        })
    }

    pub fn with_opt<F: FnOnce(Option<&TyCtxt>) -> R, R>(f: F) -> R {
        if TLS_TCX.with(|tcx| tcx.get().is_some()) {
            with(|v| f(Some(v)))
        } else {
            f(None)
        }
    }
}