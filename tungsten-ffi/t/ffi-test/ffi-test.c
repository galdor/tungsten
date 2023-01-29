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

void
ffi_test_struct_packed_x2(struct ffi_test_struct_packed *s) {
        s->a *= 2;
        s->b *= 2;
        s->c *= 2;
}

void
ffi_test_struct_padding_x2(struct ffi_test_struct_padding *s) {
        s->a *= 2;
        s->b *= 2;
        s->c *= 2;
}

void
ffi_test_struct_arrays_x2(struct ffi_test_struct_arrays *s) {
        s->a[0] *= 2;
        s->a[1] *= 2;

        s->b[0] *= 2;
        s->b[1] *= 2;
        s->b[2] *= 2;
}

void
ffi_test_struct_nested_x2(struct ffi_test_struct_nested *s) {
        ffi_test_struct_packed_x2(&s->s);

        ffi_test_struct_padding_x2(&s->s2[0]);
        ffi_test_struct_padding_x2(&s->s2[1]);

        ffi_test_struct_packed_x2(s->ps);
}

int
ffi_test_call(int (*fn)(const char *, int), const char *s, int n) {
        return fn(s, n);
}

unsigned int
ffi_test_call_enum_in(unsigned int (*fn)(enum ffi_test_enum),
                      enum ffi_test_enum e) {
        return fn(e);
}
