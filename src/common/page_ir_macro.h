#ifndef PAGE_IR_MACRO_H_
#define PAGE_IR_MACRO_H_

#define GET_CHUNK(suffix, type) \
  extern "C" type get_##suffix##_chunk(Page* page, size_t idx) { \
    return reinterpret_cast<type *>(page->chunks)[idx]; \
  } \

#define WRITE_RAW_VAL(suffix, type) \
  extern "C" void write_##suffix##_raw(Chunk* page, size_t idx, type val) { \
    reinterpret_cast<type *>(page->ptr)[idx] = val; \
  } \

#define READ_RAW_VAL(suffix, type) \
  extern "C" type read_##suffix##_raw(Chunk* page, size_t idx) { \
    return reinterpret_cast<type *>(page->ptr)[idx]; \
  } \

#define READ_RLE_VAL(suffix, type) \
  extern "C" type read_##suffix##_rle(RLEChunk* chunk, size_t idx) { \
    int16_t n = chunk->run_num; \
    int16_t i; \
    size_t r = 0; \
    for (i = 0; i < n; i++) { \
      Run run = chunk->runs[i]; \
      if (r <= idx && idx < r + *(run.length)) { \
        return *(reinterpret_cast<type *>(run.value)); \
      } \
      r += *(run.length); \
    } \
    fprintf(stderr, "%zu is greater than # of rows (%zu) in a chunk", idx, r); \
    std::abort(); \
  } \

#endif // PAGE_IR_MACRO_H_
