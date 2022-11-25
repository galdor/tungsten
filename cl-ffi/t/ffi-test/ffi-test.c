#include "ffi-test.h"

void
ffi_test_void(void) {
}

int
ffi_test_add2_int(int a, int b) {
        return a + b;
}

void
ffi_test_add2_int_ptr(int a, int b, int *c) {
        *c = a + b;
}
