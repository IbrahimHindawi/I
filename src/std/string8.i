cinclude "stdio.h"
cinclude "string.h"

import "memops.i"
import "Vec.i"

FILE: struct = { external; }

string8: struct = {
    data: *u8;
    length: u64;
    capacity: u64;
}

string8slice: struct = {
    data: *u8;
    length: u64;
}

string8_reserve: proc(arena: *memops_arena, capacity: u64)->string8 = {
    s: string8 = {};
    if (capacity == 0) {
        return s;
    }
    s.data = cast(memops_arena_push_zero(arena, capacity + 1, alignof(u8)), *u8);
    if (s.data == null) {
        printf("I runtime: string8 reserve allocation failure\n");
        exit(1);
    }
    s.capacity = capacity;
    return s;
}

string8_grow: proc(arena: *memops_arena, s: *string8, min_capacity: u64)->void = {
    if (s[0].capacity >= min_capacity) {
        return;
    }

    new_capacity: u64 = s[0].capacity;
    if (new_capacity == 0) {
        new_capacity = 8;
    }
    while (new_capacity < min_capacity) {
        new_capacity *= 2;
    }

    old_capacity: u64 = s[0].capacity;
    old_data: *u8 = s[0].data;
    s[0].data = cast(memops_arena_realloc_(arena, new_capacity + 1, old_data, old_capacity + 1, alignof(u8)), *u8);
    if (s[0].data == null) {
        printf("I runtime: string8 grow allocation failure\n");
        exit(1);
    }
    if (new_capacity + 1 > old_capacity + 1) {
        memset(s[0].data + old_capacity + 1, 0, new_capacity - old_capacity);
    }
    s[0].capacity = new_capacity;
}

string8_from_cstr: proc(arena: *memops_arena, cstr: *const char)->string8 = {
    if (cstr == null) {
        return {};
    }
    length: u64 = cast(strlen(cstr), u64);
    s: string8 = string8_reserve(arena, length);
    if (length > 0) {
        memcpy(s.data, cstr, length);
    }
    s.length = length;
    s.data[s.length] = 0;
    return s;
}

string8_copy_from_slice: proc(arena: *memops_arena, data: *const u8, length: u64)->string8 = {
    s: string8 = string8_reserve(arena, length);
    if (length > 0 and data != null) {
        memcpy(s.data, data, length);
    }
    s.length = length;
    if (s.data != null) {
        s.data[s.length] = 0;
    }
    return s;
}

string8_to_cstr_temp: proc(arena: *memops_arena, s: string8)->*char = {
    out: *char = cast(memops_arena_push_zero(arena, s.length + 1, alignof(char)), *char);
    if (out == null) {
        printf("I runtime: string8 cstr allocation failure\n");
        exit(1);
    }
    if (s.length > 0 and s.data != null) {
        memcpy(out, s.data, s.length);
    }
    out[s.length] = 0;
    return out;
}

string8_append_byte: proc(arena: *memops_arena, s: *string8, byte: u8)->void = {
    string8_grow(arena, s, s[0].length + 1);
    s[0].data[s[0].length] = byte;
    s[0].length += 1;
    s[0].data[s[0].length] = 0;
}

string8_append_bytes: proc(arena: *memops_arena, s: *string8, src: *const u8, count: u64)->void = {
    if (count == 0) {
        return;
    }
    string8_grow(arena, s, s[0].length + count);
    memcpy(s[0].data + s[0].length, src, count);
    s[0].length += count;
    s[0].data[s[0].length] = 0;
}

string8_append_cstr: proc(arena: *memops_arena, s: *string8, cstr: *const char)->void = {
    if (cstr == null) {
        return;
    }
    count: u64 = cast(strlen(cstr), u64);
    string8_append_bytes(arena, s, cast(cstr, *const u8), count);
}

string8_clear: proc(s: *string8)->void = {
    if (s == null) {
        return;
    }
    s[0].length = 0;
    if (s[0].data != null) {
        s[0].data[0] = 0;
    }
}

string8_read_file: proc(arena: *memops_arena, filename: *const char)->string8 = {
    file: *FILE = fopen(filename, "rb");
    if (file == null) {
        return {};
    }

    fseek(file, 0, 2);
    size: i64 = cast(ftell(file), i64);
    fseek(file, 0, 0);
    if (size <= 0) {
        fclose(file);
        return {};
    }

    s: string8 = string8_reserve(arena, cast(size, u64));
    read_count: u64 = cast(fread(s.data, 1, cast(size, u64), file), u64);
    fclose(file);
    s.length = read_count;
    s.data[s.length] = 0;
    return s;
}

string8_equals: proc(a: *const string8, b: *const string8)->bool = {
    if (a == null or b == null) {
        return a == b;
    }
    if (a[0].length != b[0].length) {
        return 0;
    }
    if (a[0].length == 0) {
        return 1;
    }
    return memcmp(a[0].data, b[0].data, a[0].length) == 0;
}

string8_equals_cstr: proc(a: *const string8, cstr: *const char)->bool = {
    if (a == null or cstr == null) {
        return 0;
    }
    length: u64 = cast(strlen(cstr), u64);
    if (a[0].length != length) {
        return 0;
    }
    if (length == 0) {
        return 1;
    }
    return memcmp(a[0].data, cstr, length) == 0;
}

string8_print: proc(s: *const string8)->void = {
    if (s == null or s[0].data == null) {
        return;
    }
    printf("%.*s", cast(s[0].length, i32), cast(s[0].data, *const char));
}

string8slice_from_parts: proc(data: *u8, length: u64)->string8slice = {
    s: string8slice = {};
    s.data = data;
    s.length = length;
    return s;
}

string8slice_sub: proc(s: string8slice, start: u64, count: u64)->string8slice = {
    if (start > s.length) {
        start = s.length;
    }
    if (count > s.length - start) {
        count = s.length - start;
    }
    return string8slice_from_parts(s.data + start, count);
}

string8slice_from_string8: proc(s: string8)->string8slice = {
    return string8slice_from_parts(s.data, s.length);
}

string8slice_equals: proc(a: string8slice, b: string8slice)->bool = {
    if (a.length != b.length) {
        return 0;
    }
    if (a.length == 0) {
        return 1;
    }
    return memcmp(a.data, b.data, a.length) == 0;
}

string8slice_equals_cstr: proc(s: string8slice, cstr: *const char)->bool = {
    if (cstr == null) {
        return 0;
    }
    length: u64 = cast(strlen(cstr), u64);
    if (s.length != length) {
        return 0;
    }
    if (length == 0) {
        return 1;
    }
    return memcmp(s.data, cstr, length) == 0;
}

string8slice_to_cstr_temp: proc(arena: *memops_arena, s: string8slice)->*char = {
    out: *char = cast(memops_arena_push_zero(arena, s.length + 1, alignof(char)), *char);
    if (out == null) {
        printf("I runtime: string8slice cstr allocation failure\n");
        exit(1);
    }
    if (s.length > 0 and s.data != null) {
        memcpy(out, s.data, s.length);
    }
    out[s.length] = 0;
    return out;
}

string8slice_print: proc(s: string8slice)->void = {
    if (s.data == null) {
        return;
    }
    printf("%.*s", cast(s.length, i32), cast(s.data, *const char));
}

string8_split_byte: proc(arena: *memops_arena, src: string8, sep: u8)->Vec<string8> = {
    out: Vec<string8> = {};
    start: u64 = 0;
    i: u64 = 0;
    while (i <= src.length) {
        if (i == src.length or src.data[i] == sep) {
            piece: string8 = string8_copy_from_slice(arena, src.data + start, i - start);
            Vec<string8>append(arena, &out, piece);
            start = i + 1;
        }
        i += 1;
    }
    return out;
}

string8_split_char: proc(arena: *memops_arena, src: string8, sep: u8)->Vec<string8> = {
    return string8_split_byte(arena, src, sep);
}

string8slice_split: proc(arena: *memops_arena, src: string8slice, sep: u8)->Vec<string8slice> = {
    out: Vec<string8slice> = {};
    start: u64 = 0;
    i: u64 = 0;
    while (i <= src.length) {
        if (i == src.length or src.data[i] == sep) {
            piece: string8slice = string8slice_sub(src, start, i - start);
            Vec<string8slice>append(arena, &out, piece);
            start = i + 1;
        }
        i += 1;
    }
    return out;
}

string8slice_split_from_string8: proc(arena: *memops_arena, src: string8, sep: u8)->Vec<string8slice> = {
    return string8slice_split(arena, string8slice_from_string8(src), sep);
}
