#ifndef FFI_TEST_H
#define FFI_TEST_H

#include <limits.h>
#include <stdint.h>

typedef int ffi_int_t;

static const ffi_int_t ffi_int_a = 0;
static const ffi_int_t ffi_int_b = 1;
static const ffi_int_t ffi_int_c = 2;

enum ffi_test_enum {
        FFI_TEST_SIMPLE_ENUM_MIN = INT_MIN,

        FFI_TEST_SIMPLE_ENUM_A = 0,
        FFI_TEST_SIMPLE_ENUM_B,
        FFI_TEST_SIMPLE_ENUM_C,

        FFI_TEST_SIMPLE_ENUM_MAX = INT_MAX,
};

#define FFI_TEST_BITSET_A (1 << 0)
#define FFI_TEST_BITSET_B (1 << 1)
#define FFI_TEST_BITSET_C (1 << 2)

struct ffi_test_struct_packed {
        int8_t a;
        int8_t b;
        int8_t c;
};

struct ffi_test_struct_padding {
        int64_t a;
        int8_t b;
        int16_t c;
};

struct ffi_test_struct_arrays {
        int8_t a[2];
        float b[3];
};

struct ffi_test_struct_nested {
        struct ffi_test_struct_packed s;
        struct ffi_test_struct_padding s2[2];
        struct ffi_test_struct_packed *ps;
};

union ffi_test_union {
        const char *a;
        uint8_t b;
        int32_t c[4];
};

void ffi_test_void(void);
int ffi_test_add2_int(int, int);
void ffi_test_add2_int_ptr(int, int, int *);
char *ffi_test_concat(const char *, const char *);
enum ffi_test_enum ffi_test_enum_max(enum ffi_test_enum, enum ffi_test_enum);
void ffi_test_struct_packed_x2(struct ffi_test_struct_packed *);
void ffi_test_struct_padding_x2(struct ffi_test_struct_padding *);
void ffi_test_struct_arrays_x2(struct ffi_test_struct_arrays *);
void ffi_test_struct_nested_x2(struct ffi_test_struct_nested *);
int ffi_test_call(int (*)(const char *, int), const char *, int);
unsigned int ffi_test_call_enum_in(unsigned int (*)(enum ffi_test_enum),
                                   enum ffi_test_enum);

#endif
