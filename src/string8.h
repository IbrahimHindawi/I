#pragma once

#include <stdint.h>
#include <stdbool.h>
#include "Array_u8.h"
#include "core.h"
#include "saha.h"

typedef uint8_t u8;
typedef uint64_t u64;

typedef struct string8 string8;
struct string8 {
    u8 *data;
    u64 length;
    u64 capacity;
};

string8 string8_reserve(Arena *arena, u64 capacity);
string8 string8_from_cstr(Arena *arena, const char *cstr);
string8 string8_copy_from_slice(Arena *arena, u8 *data, u64 length);
char *string8_to_cstr_temp(Arena *arena, string8 string);

void string8_append_byte(Arena *arena, string8 *s, u8 byte);
void string8_append_bytes(Arena *arena, string8 *s, const u8 *src, u64 count);
void string8_append_cstr(Arena *arena, string8 *s, const char *cstr);

void string8_clear(string8 *s);

string8 string8_read_file(Arena *arena, const char *filename);

bool string8_equals(const string8 *a, const string8 *b);
bool string8_equals_cstr(const string8 *a, const char *cstr);

void string8_print(const string8 *s);
