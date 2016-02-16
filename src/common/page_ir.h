#ifndef PAGE_IR_H_
#define PAGE_IR_H_

#include <stdio.h>
#include <stdint.h>
#include <stddef.h>
#include <memory>

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

// Simple Run-length chunk layout
//
// +----------------------+---------------------------+------------------------------+
// | # of runs (4 ~ 1024) | lengths of runs (1 ~ 256) |     fixed-length values      |
// |        2 byte        |    # of runs * 1 byte     | # of runs * type length byte |
// +----------------------+---------------------------+------------------------------+
//
// Full data scan is very popular in OLAP workloads. So, only the values in each run-length chunk are mostly expected to be read sequentially.
// In addition, to retrieve a single value from a run-length chunk, the lengths are expected to be read sequentially.
// The structure of run-length chunk is suitable to maximize the locality in memory space in both cases.
struct RLEChunk {
  int16_t run_num;
  int8_t  *lengths;
  void    *values;
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

// for RLE chunk
READ_RLE_VAL(i8, int8_t);
READ_RLE_VAL(i16, int16_t);
READ_RLE_VAL(i32, int32_t);
READ_RLE_VAL(i64, int64_t);
READ_RLE_VAL(f32, float);
READ_RLE_VAL(f64, double);


// for test
// TODO: should be removed after implementing write functions for variable-length chunks
extern "C" RLEChunk random_rle_chunk() {
  std::unique_ptr<RLEChunk> chunk(new RLEChunk);
  std::allocator<int8_t> length_alloc;
  std::allocator<int32_t> value_alloc;

  chunk->run_num = 10;
  chunk->lengths = length_alloc.allocate(chunk->run_num);
  chunk->values = value_alloc.allocate(chunk->run_num);
  for (int i = 0; i < chunk->run_num; i++) {
    chunk->lengths[i] = i + 1;
    reinterpret_cast<int32_t *>(chunk->values)[i] = i * 10;
  }
  return *chunk;
}

void dummy(Page* v1, Chunk* v2) {}

#endif // PAGE_IR_H_
