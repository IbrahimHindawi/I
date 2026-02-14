#pragma once

#include <stdint.h>
#include <stdbool.h>
#include "Array_u8.h"
#include "core.h"
#include "saha.h"
#include "string8.h"

typedef struct string8slice string8slice;
struct string8slice {
    u8 *data;
    u64 length;
};

string8slice string8slice_from_parts(u8 *data, u64 length);
string8slice string8slice_sub(string8slice s, u64 start, u64 count);

bool string8slice_equals(string8slice a, string8slice b);
bool string8slice_equals_cstr(string8slice s, const char *cstr);

char *string8slice_to_cstr_temp(memops_arena *arena, string8slice s);

// Array_string8slice string8slice_split(memops_arena *arena, string8slice s, char sep);
// Array_string8slice string8slice_split_from_string8(memops_arena *arena, string8 s, char sep);

void string8slice_print(string8slice s);

