#ifndef PAGE_IR_MACRO_H_
#define PAGE_IR_MACRO_H_

#define WRITE_RAW_VAL(suffix, type) \
  extern "C" void write_##suffix##_raw(Chunk* page, size_t idx, type val) { \
    reinterpret_cast<type *>(page->ptr)[idx] = val; \
  } \

#define READ_RAW_VAL(suffix, type) \
  extern "C" type read_##suffix##_raw(Chunk* page, size_t idx) { \
    return reinterpret_cast<type *>(page->ptr)[idx]; \
  } \

#endif // PAGE_IR_MACRO_H_