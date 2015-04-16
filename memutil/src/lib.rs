mod alloc {

struct LimitedAllocator {
  capacity: usize
}

}

mod arena {

struct Arena {
  init_size: usize,
  increase_size: usize
}

}
