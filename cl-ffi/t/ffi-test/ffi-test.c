#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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

char *
ffi_test_concat(const char *a, const char *b) {
        size_t asz, bsz, absz;
        char *ab;

        asz = strlen(a);
        bsz = strlen(b);

        absz = asz + bsz + 1;

        ab = malloc(absz);
        if (ab == NULL) {
                fprintf(stderr, "cannot allocate %zu bytes\n", absz);
                abort();
        }

        memcpy(ab, a, asz);
        memcpy(ab + asz, b, bsz);
        ab[absz - 1] = '\0';

        return ab;
}

enum ffi_test_enum
ffi_test_enum_max(enum ffi_test_enum a, enum ffi_test_enum b) {
        if (a >= b) {
                return a;
        } else {
                return b;
        }
}
