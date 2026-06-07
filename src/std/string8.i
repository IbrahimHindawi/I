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

string8_ascii_lower: proc(c: u8)->u8 = {
    if (c >= 65 and c <= 90) {
        return c + 32;
    }
    return c;
}

string8_ascii_is_space: proc(c: u8)->bool = {
    return c == 32 or c == 9 or c == 10 or c == 13 or c == 11 or c == 12;
}

path_is_sep: proc(c: u8)->bool = {
    return c == 47 or c == 92;
}

string8_reserve: proc(arena: *memops_arena, capacity: u64)->string8 = {
    s: string8 = {};
    if (arena == null) {
        printf("I runtime: string8 reserve with null arena\n");
        exit(1);
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
    if (arena == null or s == null) {
        printf("I runtime: string8 grow invalid argument\n");
        exit(1);
    }
    if (s[0].capacity >= min_capacity) {
        if (s[0].data == null) {
            s[0].data = cast(memops_arena_push_zero(arena, s[0].capacity + 1, alignof(u8)), *u8);
            if (s[0].data == null) {
                printf("I runtime: string8 grow allocation failure\n");
                exit(1);
            }
        }
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
    if (old_data == null) {
        s[0].data = cast(memops_arena_push_zero(arena, new_capacity + 1, alignof(u8)), *u8);
    } else {
        s[0].data = cast(memops_arena_realloc_(arena, new_capacity + 1, old_data, old_capacity + 1, alignof(u8)), *u8);
    }
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
        return string8_reserve(arena, 0);
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
    if (data == null and length > 0) {
        return string8_reserve(arena, 0);
    }
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

string8slice_from_parts: proc(data: *u8, length: u64)->string8slice = {
    s: string8slice = {};
    s.data = data;
    s.length = length;
    return s;
}

string8slice_from_cstr: proc(cstr: *const char)->string8slice = {
    if (cstr == null) {
        return {};
    }
    return string8slice_from_parts(cast(cstr, *u8), cast(strlen(cstr), u64));
}

string8slice_from_string8: proc(s: string8)->string8slice = {
    return string8slice_from_parts(s.data, s.length);
}

string8_from_slice: proc(arena: *memops_arena, s: string8slice)->string8 = {
    return string8_copy_from_slice(arena, s.data, s.length);
}

string8_copy: proc(arena: *memops_arena, s: string8)->string8 = {
    return string8_copy_from_slice(arena, s.data, s.length);
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

path_to_cstr_temp: proc(arena: *memops_arena, s: string8slice)->*char = {
    return string8slice_to_cstr_temp(arena, s);
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

string8_append_slice: proc(arena: *memops_arena, s: *string8, src: string8slice)->void = {
    string8_append_bytes(arena, s, src.data, src.length);
}

string8_append_string8: proc(arena: *memops_arena, s: *string8, src: string8)->void = {
    string8_append_bytes(arena, s, src.data, src.length);
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
    file: *FILE = null;
    if (fopen_s(file.&, filename, "rb") != 0) {
        return string8_reserve(arena, 0);
    }
    if (file == null) {
        return string8_reserve(arena, 0);
    }

    fseek(file, 0, 2);
    size: i64 = cast(ftell(file), i64);
    fseek(file, 0, 0);
    if (size <= 0) {
        fclose(file);
        return string8_reserve(arena, 0);
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

string8slice_sub: proc(s: string8slice, start: u64, count: u64)->string8slice = {
    if (s.data == null) {
        return {};
    }
    if (start > s.length) {
        start = s.length;
    }
    if (count > s.length - start) {
        count = s.length - start;
    }
    return string8slice_from_parts(s.data + start, count);
}

string8_slice: proc(s: string8, start: u64, count: u64)->string8slice = {
    return string8slice_sub(string8slice_from_string8(s), start, count);
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

string8slice_print: proc(s: string8slice)->void = {
    if (s.data == null) {
        return;
    }
    printf("%.*s", cast(s.length, i32), cast(s.data, *const char));
}

print: proc<string8>(s: string8)->void = {
    string8_print(s.&);
}

print: proc<string8slice>(s: string8slice)->void = {
    string8slice_print(s);
}

string8slice_trim: proc(s: string8slice)->string8slice = {
    start: u64 = 0;
    end: u64 = s.length;
    if (s.data == null) {
        return {};
    }
    while (start < end and string8_ascii_is_space(s.data[start])) {
        start += 1;
    }
    while (end > start and string8_ascii_is_space(s.data[end - 1])) {
        end -= 1;
    }
    return string8slice_sub(s, start, end - start);
}

string8_trim: proc(s: string8)->string8slice = {
    return string8slice_trim(string8slice_from_string8(s));
}

string8slice_starts_with: proc(s: string8slice, prefix: string8slice)->bool = {
    if (prefix.length > s.length) {
        return 0;
    }
    if (prefix.length == 0) {
        return 1;
    }
    if (s.data == null or prefix.data == null) {
        return 0;
    }
    return memcmp(s.data, prefix.data, prefix.length) == 0;
}

string8_starts_with: proc(s: string8, prefix: string8slice)->bool = {
    return string8slice_starts_with(string8slice_from_string8(s), prefix);
}

string8slice_ends_with: proc(s: string8slice, suffix: string8slice)->bool = {
    if (suffix.length > s.length) {
        return 0;
    }
    if (suffix.length == 0) {
        return 1;
    }
    if (s.data == null or suffix.data == null) {
        return 0;
    }
    return memcmp(s.data + s.length - suffix.length, suffix.data, suffix.length) == 0;
}

string8_ends_with: proc(s: string8, suffix: string8slice)->bool = {
    return string8slice_ends_with(string8slice_from_string8(s), suffix);
}

string8slice_find: proc(s: string8slice, needle: string8slice)->i64 = {
    if (needle.length == 0) {
        return 0;
    }
    if (s.data == null or needle.data == null or needle.length > s.length) {
        return -1;
    }
    last: u64 = s.length - needle.length;
    for (i: u64 = 0; i <= last; i += 1) {
        if (s.data[i] == needle.data[0] and memcmp(s.data + i, needle.data, needle.length) == 0) {
            return cast(i, i64);
        }
    }
    return -1;
}

string8_find: proc(s: string8, needle: string8slice)->i64 = {
    return string8slice_find(string8slice_from_string8(s), needle);
}

string8slice_contains: proc(s: string8slice, needle: string8slice)->bool = {
    return string8slice_find(s, needle) >= 0;
}

string8_contains: proc(s: string8, needle: string8slice)->bool = {
    return string8_find(s, needle) >= 0;
}

string8slice_eq_ignore_case: proc(a: string8slice, b: string8slice)->bool = {
    if (a.length != b.length) {
        return 0;
    }
    if (a.length == 0) {
        return 1;
    }
    if (a.data == null or b.data == null) {
        return 0;
    }
    for (i: u64 = 0; i < a.length; i += 1) {
        if (string8_ascii_lower(a.data[i]) != string8_ascii_lower(b.data[i])) {
            return 0;
        }
    }
    return 1;
}

string8_eq_ignore_case: proc(a: string8, b: string8slice)->bool = {
    return string8slice_eq_ignore_case(string8slice_from_string8(a), b);
}

string8slice_lower_copy: proc(arena: *memops_arena, s: string8slice)->string8 = {
    out: string8 = string8_reserve(arena, s.length);
    if (s.data == null and s.length > 0) {
        return out;
    }
    out.length = s.length;
    for (i: u64 = 0; i < s.length; i += 1) {
        out.data[i] = string8_ascii_lower(s.data[i]);
    }
    out.data[out.length] = 0;
    return out;
}

string8_lower_copy: proc(arena: *memops_arena, s: string8)->string8 = {
    return string8slice_lower_copy(arena, string8slice_from_string8(s));
}

string8slice_hash: proc(s: string8slice)->u64 = {
    h: u64 = 5381;
    for (i: u64 = 0; i < s.length; i += 1) {
        h = ((h shl 5) + h) ^ cast(s.data[i], u64);
    }
    return h;
}

string8_hash: proc(s: string8)->u64 = {
    return string8slice_hash(string8slice_from_string8(s));
}

path_append_normalized_slice: proc(arena: *memops_arena, out: *string8, s: string8slice)->void = {
    for (i: u64 = 0; i < s.length; i += 1) {
        c: u8 = s.data[i];
        if (path_is_sep(c)) {
            c = 47;
        }
        string8_append_byte(arena, out, c);
    }
}

path_normalize_slashes: proc(arena: *memops_arena, path: string8slice)->string8 = {
    out: string8 = string8_reserve(arena, path.length);
    path_append_normalized_slice(arena, out.&, path);
    return out;
}

path_join: proc(arena: *memops_arena, a: string8slice, b: string8slice)->string8 = {
    out: string8 = string8_reserve(arena, a.length + b.length + 1);
    a_end: u64 = a.length;
    b_start: u64 = 0;
    while (a_end > 0 and path_is_sep(a.data[a_end - 1])) {
        a_end -= 1;
    }
    while (b_start < b.length and path_is_sep(b.data[b_start])) {
        b_start += 1;
    }
    path_append_normalized_slice(arena, out.&, string8slice_sub(a, 0, a_end));
    if (a_end > 0 and b_start < b.length) {
        string8_append_byte(arena, out.&, 47);
    }
    path_append_normalized_slice(arena, out.&, string8slice_sub(b, b_start, b.length - b_start));
    return out;
}

path_dirname: proc(path: string8slice)->string8slice = {
    end: u64 = path.length;
    if (path.data == null) {
        return {};
    }
    while (end > 0 and path_is_sep(path.data[end - 1])) {
        end -= 1;
    }
    i: u64 = end;
    while (i > 0) {
        if (path_is_sep(path.data[i - 1])) {
            if (i - 1 == 0) {
                return string8slice_sub(path, 0, 1);
            }
            return string8slice_sub(path, 0, i - 1);
        }
        i -= 1;
    }
    return {};
}

path_basename: proc(path: string8slice)->string8slice = {
    end: u64 = path.length;
    if (path.data == null) {
        return {};
    }
    while (end > 0 and path_is_sep(path.data[end - 1])) {
        end -= 1;
    }
    i: u64 = end;
    while (i > 0) {
        if (path_is_sep(path.data[i - 1])) {
            return string8slice_sub(path, i, end - i);
        }
        i -= 1;
    }
    return string8slice_sub(path, 0, end);
}

path_extension: proc(path: string8slice)->string8slice = {
    base: string8slice = path_basename(path);
    if (base.length == 0 or base.data == null) {
        return {};
    }
    i: u64 = base.length;
    while (i > 0) {
        if (base.data[i - 1] == 46) {
            if (i - 1 == 0) {
                return {};
            }
            return string8slice_sub(base, i - 1, base.length - (i - 1));
        }
        i -= 1;
    }
    return {};
}

path_strip_extension: proc(path: string8slice)->string8slice = {
    end: u64 = path.length;
    if (path.data == null) {
        return {};
    }
    while (end > 0 and !path_is_sep(path.data[end - 1])) {
        if (path.data[end - 1] == 46) {
            if (end == 1 or path_is_sep(path.data[end - 2])) {
                return path;
            }
            return string8slice_sub(path, 0, end - 1);
        }
        end -= 1;
    }
    return path;
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
