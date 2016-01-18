#ifndef PAGE_IR_H_
#define PAGE_IR_H_

#include <stdio.h>
#include <stdint.h>
#include <stddef.h>

#include "page_ir_macro.h"

enum EncType {
  RAW = 0,
  RLE = 1
};

struct Chunk {
  // MiniPageType type; - can be used for debugging
  void   *ptr;
  // byte size
  size_t size;
};

struct Page {
  void* ptr;
  size_t size;

  Chunk* chunks;
  // the number of minipages
  size_t chunk_num;
  // the number of values stored in each minipage.
  // All minipages share the same val_cnt.
  size_t value_cnt;
  // Does this page own the minipages?
  bool   owned;
};

extern "C" Chunk* get_chunk(Page* page, size_t idx) {
  return &page->chunks[idx];
}

WRITE_RAW_VAL(i8, int8_t);
WRITE_RAW_VAL(i16, int16_t);
WRITE_RAW_VAL(i32, int32_t);
WRITE_RAW_VAL(i64, int64_t);
WRITE_RAW_VAL(f32, float);
WRITE_RAW_VAL(f64, double);

READ_RAW_VAL(i8, int8_t);
READ_RAW_VAL(i16, int16_t);
READ_RAW_VAL(i32, int32_t);
READ_RAW_VAL(i64, int64_t);
READ_RAW_VAL(f32, float);
READ_RAW_VAL(f64, double);

void dummy(Page* v1, Chunk* v2) {}

#endif // PAGE_IR_H_