use common::err::Error;


enum RequestType {
  READ,
  WRITE
}

// RequestRange -> ScanRange or WriteRange

struct RequestContext {
  name: str
}

struct IoMgr {
  num_disks: u8,
  num_threads_per_disk: u8,
  min_buffer_sz: usize,
  max_buffer_sz: usize
}

impl IoMgr {

  fn register_context(ctx: &mut RequestContext) -> Result<bool, Error> {
    Ok(true)
  }

  fn unregister_context(ctx: &mut RequestContext) -> Result<bool, Error> {
    Ok(true)
  }

  fn cancel_context(ctx: &mut RequestContext) {}

  /// Adds the scan ranges to the queue. This call is non-blocking.
  /// The caller must not deallocate the scan range pointers before
  /// unregister_context.
  ///
  /// If immediately is true, the range are immediately put
  /// on the read queue (i.e., the caller should not/cannot call GetNextRange
  /// for these ranges, as in the case for columnar formats.
  fn add_scan_ranges(ctx: &mut RequestContext, immediately: bool) -> Result<bool, Error> {
    Ok(true)
  }

  fn get_next_range(ctx: &mut RequestContext) -> Result<bool, Error> {
    Ok(true)
  }

  fn read(ctx: &mut RequestContext) -> Result<bool, Error> {
    Ok(true)
  }
}
