#ifndef PAGE_IR_H_
#define PAGE_IR_H_

#include <stdio.h>
#include <stdint.h>
#include <stddef.h>
#include <memory>
#include <cstdlib>

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

struct Run {
  uint8_t *length;
  void * value;
};

struct RLEChunk {
  uint16_t run_num;
  Run* runs;
};

struct Page {
  void* ptr;
  size_t size;

  void* chunks;
  // the number of minipages
  size_t chunk_num;
  // the number of values stored in each minipage.
  // All minipages share the same val_cnt.
  size_t value_cnt;
  // Does this page own the minipages?
  bool   owned;
};

GET_CHUNK(raw, Chunk);
GET_CHUNK(rle, RLEChunk);

struct RawChunkWriter {
  Chunk* chunk;
  size_t idx;
};

struct RawChunkReader {
  Chunk* chunk;
  size_t idx;
};

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

// RLE chunk interface
READ_RLE_VAL(i8, int8_t);
READ_RLE_VAL(i16, int16_t);
READ_RLE_VAL(i32, int32_t);
READ_RLE_VAL(i64, int64_t);
READ_RLE_VAL(f32, float);
READ_RLE_VAL(f64, double);

// TODO
// Chunk rle_to_raw(RLEChunk* chunk) {}

// TODO
// RLEChunk raw_to_rle(Chunk* chunk) {}

// TODO
// Chunk filter_with_pos_return_chunk(RLEChunk* chunk, vector<size_t> pos) {}

// TODO: take a generated function as the parameter to filter input tuples out
// vector<size_t> filter_with_pred_return_pos(RLEChunk* chunk, Function predicate) {}

// TODO: take a generated function as the parameter to filter input tuples out
// type aggregate(RLEChunk* chunk, Function predicate, Function agg_func) {}

// TODO: take a generated function as the parameter to filter input tuples out
// Chunk sort(RLEChunk* chunk, Function predicate) {}

void dummy(Page* v1, Chunk* v2) {}

#endif // PAGE_IR_H_
