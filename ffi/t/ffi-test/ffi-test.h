#ifndef FFI_TEST_H
#define FFI_TEST_H

#include <limits.h>
#include <stdint.h>

enum ffi_test_enum {
        FFI_TEST_SIMPLE_ENUM_MIN = INT_MIN,

        FFI_TEST_SIMPLE_ENUM_0 = 0,
        FFI_TEST_SIMPLE_ENUM_1,
        FFI_TEST_SIMPLE_ENUM_2,

        FFI_TEST_SIMPLE_ENUM_MAX = INT_MAX,
};

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

void ffi_test_void(void);
int ffi_test_add2_int(int, int);
void ffi_test_add2_int_ptr(int, int, int *);
char *ffi_test_concat(const char *, const char *);
enum ffi_test_enum ffi_test_enum_max(enum ffi_test_enum, enum ffi_test_enum);
void ffi_test_struct_packed_x2(struct ffi_test_struct_packed *);
void ffi_test_struct_padding_x2(struct ffi_test_struct_padding *);
void ffi_test_struct_arrays_x2(struct ffi_test_struct_arrays *);
void ffi_test_struct_nested_x2(struct ffi_test_struct_nested *);

#endif
