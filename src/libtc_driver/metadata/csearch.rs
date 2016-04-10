use metadata::cstore;
use middle::cstore::{CrateStore};

impl<'tcx> CrateStore<'tcx> for cstore::CStore {
}
