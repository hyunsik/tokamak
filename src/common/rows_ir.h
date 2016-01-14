#include <stdint.h>
#include <stddef.h>
#include <stdio.h>

enum MiniPageType {
  RAW = 0,
  RLE = 1
};

struct MiniPage {
  // MiniPageType type; - can be used for debugging
  void   *ptr;
  // byte size
  size_t size;
};

struct Page {
  MiniPage* mpages;
  // the number of minipages
  size_t    mpage_num;
  // the number of values stored in each minipage.
  // All minipages share the same val_cnt.
  size_t    value_cnt;
  // Does this page own the minipages?
  bool      owned;
};

extern "C" void write_i8_raw(MiniPage* page, uint32_t idx, int8_t val) {
  reinterpret_cast<int8_t*>(page->ptr)[idx] = val;
}

extern "C" void write_i16_raw(MiniPage* page, uint32_t idx, int16_t val) {
  reinterpret_cast<int16_t*>(page->ptr)[idx] = val;
}

extern "C" void write_i32_raw(MiniPage* page, uint32_t idx, int32_t val) {
  reinterpret_cast<int32_t*>(page->ptr)[idx] = val;
}

extern "C" void write_i64_raw(MiniPage* page, uint32_t idx, int64_t val) {
  reinterpret_cast<int64_t*>(page->ptr)[idx] = val;
}

extern "C" void write_f32_raw(MiniPage* page, uint32_t idx, float val) {
  reinterpret_cast<float*>(page->ptr)[idx] = val;
}

extern "C" void write_f64_raw(MiniPage* page, uint32_t idx, double val) {
  reinterpret_cast<double*>(page->ptr)[idx] = val;
}

void dummy(Page* v1, MiniPage* v2) {}