#include "core.h"
#include "string8slice.h"
#include <string.h>
#include <stdio.h>

string8slice string8slice_from_parts(u8 *data, u64 length) {
    string8slice s;
    s.data = data;
    s.length = length;
    return s;
}

string8slice string8slice_sub(string8slice s, u64 start, u64 count) {
    if (start >= s.length) return (string8slice){0};
    if (start + count > s.length) count = s.length - start;

    string8slice out;
    out.data = s.data + start;
    out.length = count;
    return out;
}

bool string8slice_equals(string8slice a, string8slice b) {
    if (a.length != b.length) return false;
    return memcmp(a.data, b.data, a.length) == 0;
}

bool string8slice_equals_cstr(string8slice s, const char *cstr) {
    u64 len = strlen(cstr);
    if (s.length != len) return false;
    return memcmp(s.data, cstr, len) == 0;
}

char *string8slice_to_cstr_temp(Arena *arena, string8slice s) {
    char *out = arenaPushArray(arena, char, s.length + 1);
    memcpy(out, s.data, s.length);
    out[s.length] = 0;
    return out;
}


void string8slice_print(string8slice s) {
    printf("%.*s\n", (int)s.length, s.data);
}
