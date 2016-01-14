#include <stdint.h>
#include <stddef.h>
#include <stdio.h>

enum MiniPageType {
  RAW = 0,
  RLE = 1
};

struct MiniPage {
  void* ptr;
  size_t size;
};

struct Page {
  MiniPage* mpages;
  // the number of mini pages  
  size_t num;
  // The number of values stored in each minipage.
  // All minipages share the same value count;
  size_t value_cnt;
  // Does this page own the mini pages?
  bool   owned;
};

extern "C" void write_i8(MiniPage* page, uint32_t idx, int8_t val) {
  reinterpret_cast<int8_t*>(page->ptr)[idx] = val;
}

extern "C" void write_i32(MiniPage* page, uint32_t idx, int32_t val) {
  reinterpret_cast<int32_t*>(page->ptr)[idx] = val;
}

extern "C" int32_t test(MiniPage* page, int32_t x) {
  return x;
}

void dummy(Page* v1, MiniPage* v2) {}
