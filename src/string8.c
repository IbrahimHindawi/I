#include "string8.h"
#include <string.h>
#include <stdio.h>

static void string8_terminate(string8 *s) {
    s->data[s->length] = 0;
}

string8 string8_reserve(Arena *arena, u64 capacity) {
    string8 s;
    s.data = arenaPushArray(arena, u8, capacity);
    s.length = 0;
    s.capacity = capacity;
    s.data[0] = 0;
    return s;
}

string8 string8_from_cstr(Arena *arena, const char *cstr) {
    u64 len = strlen(cstr);
    string8 s = string8_reserve(arena, len + 1);
    memcpy(s.data, cstr, len + 1);
    s.length = len;
    return s;
}

string8 string8_copy_from_slice(Arena *arena, u8 *data, u64 length) {
    string8 s = string8_reserve(arena, length + 1);
    memcpy(s.data, data, length);
    s.data[length] = 0;
    s.length = length;
    return s;
}

char *string8_to_cstr_temp(Arena *arena, string8 s) {
    char *out = arenaPushArray(arena, char, s.length + 1);
    memcpy(out, s.data, s.length);
    out[s.length] = 0;
    return out;
}

static void string8_grow(Arena *arena, string8 *s, u64 min_capacity) {
    Array_u8 tmp;
    tmp.data = s->data;
    tmp.length = s->length + 1;
    tmp.border = s->capacity;

    while (tmp.border < min_capacity) {
        Array_u8_resize(arena, &tmp);
    }

    s->data = tmp.data;
    s->capacity = tmp.border;
}

void string8_append_byte(Arena *arena, string8 *s, u8 byte) {
    if (s->length + 1 >= s->capacity)
        string8_grow(arena, s, s->length + 2);

    s->data[s->length] = byte;
    s->length++;
    string8_terminate(s);
}

void string8_append_bytes(Arena *arena, string8 *s, const u8 *src, u64 count) {
    if (s->length + count >= s->capacity)
        string8_grow(arena, s, s->length + count + 1);

    memcpy(s->data + s->length, src, count);
    s->length += count;
    string8_terminate(s);
}

void string8_append_cstr(Arena *arena, string8 *s, const char *cstr) {
    string8_append_bytes(arena, s, (const u8*)cstr, strlen(cstr));
}

void string8_clear(string8 *s) {
    s->length = 0;
    s->data[0] = 0;
}

string8 string8_read_file(Arena *arena, const char *filename) {
    FILE *f = fopen(filename, "rb");
    if (!f) return (string8){0};

    fseek(f, 0, SEEK_END);
    long size = ftell(f);
    fseek(f, 0, SEEK_SET);

    if (size <= 0) {
        fclose(f);
        return (string8){0};
    }

    u8 *data = arenaPushArray(arena, u8, size + 1);
    fread(data, 1, size, f);
    fclose(f);

    data[size] = 0;

    string8 s;
    s.data = data;
    s.length = size;
    s.capacity = size + 1;
    return s;
}

bool string8_equals(const string8 *a, const string8 *b) {
    if (a->length != b->length) return false;
    return memcmp(a->data, b->data, a->length) == 0;
}

bool string8_equals_cstr(const string8 *a, const char *cstr) {
    u64 len = strlen(cstr);
    if (a->length != len) return false;
    return memcmp(a->data, cstr, len) == 0;
}

void string8_print(const string8 *s) {
    printf("%.*s\n", (int)s->length, s->data);
}
