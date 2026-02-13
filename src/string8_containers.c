#include "string8_containers.h"

Array_string8 string8_split_char(Arena *arena, string8 src, char sep) {
    i32 count = 1;
    for (i32 i = 0; i < src.length; i++) {
        if (src.data[i] == (u8)sep) count++;
    }

    Array_string8 out = Array_string8_reserve(arena, count);

    u8 *start = src.data;
    u8 *end   = src.data + src.length;

    for (u8 *p = start; p < end; p++) {
        if (*p == (u8)sep) {
            i32 len = (i32)(p - start);
            Array_string8_append(arena, &out, (string8){
                .data = start,
                .length = len,
            });
            start = p + 1;
        }
    }

    if (start <= end) {
        i32 len = (i32)(end - start);
        Array_string8_append(arena, &out, (string8){
            .data = start,
            .length = len,
        });
    }

    return out;
}

