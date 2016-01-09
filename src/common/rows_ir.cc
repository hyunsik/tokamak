#include <stdint.h>
#include<stddef.h>
#include <stdio.h>

struct MiniPage {
  void *ptr;
  size_t len;
};

struct Page {
  MiniPage* pages;
  size_t num;
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
