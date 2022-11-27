#ifndef FFI_TEST_H
#define FFI_TEST_H

#include <limits.h>

enum ffi_test_enum {
        FFI_TEST_SIMPLE_ENUM_MIN = INT_MIN,

        FFI_TEST_SIMPLE_ENUM_0 = 0,
        FFI_TEST_SIMPLE_ENUM_1,
        FFI_TEST_SIMPLE_ENUM_2,

        FFI_TEST_SIMPLE_ENUM_MAX = INT_MAX,
};

void ffi_test_void(void);
int ffi_test_add2_int(int, int);
void ffi_test_add2_int_ptr(int, int, int *);
char *ffi_test_concat(const char *, const char *);
enum ffi_test_enum ffi_test_enum_max(enum ffi_test_enum, enum ffi_test_enum);

#endif
