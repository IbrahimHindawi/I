#include <core.h>

#include "stdio.h"
#include "saha.i.h"

structdecl(vec2);
structdecl(array_i32);
structdecl(array_ptr_i32);
structdecl(ptr_i32);
structdecl(arrayfptr_i32);

structdef(vec2) {
    f32 x;
    f32 y;
};

structdef(array_i32) {
    u64 len;
    u64 cap;
    i32 * data;
};

structdef(array_ptr_i32) {
    u64 len;
    u64 cap;
    ptr_i32 * data;
};

structdef(ptr_i32) {
    i32 * data;
    u64 len;
};

structdef(arrayfptr_i32) {
    u64 len;
    u64 cap;
    ptr_i32 data;
};

i32 num = 7;
memops_arena arena = {};

i32 make(i32 x);
u64 hash_fnv1a(void * data, u64 len);
u64 hash_i32(i32 * x);
i32 main();
void array_i32_reserve(memops_arena * arena, array_i32 * ar, u64 count);
void array_ptr_i32_reserve(memops_arena * arena, array_ptr_i32 * ar, u64 count);
i32 makeg_i32(i32 x);
u64 prock_i32(i32 * value);
u64 makehash_i32(i32 x);

i32 make(i32 x) {
    return x;
}

u64 hash_fnv1a(void * data, u64 len) {
    u8 * p = ((u8 *)(data));
    u64 hash = ((u64)1469598103934665603);
    for (u64 i = 0; i < len; i += 1) {
        hash ^= p[i];
        hash *= ((u64)1099511628211);
    }
    return hash;
}

u64 hash_i32(i32 * x) {
    return hash_fnv1a(&x, sizeof(x[0]));
}

i32 main() {
    printf("Hello, World!\n");
    printf("num = %d\n", num);
    array_i32 a = {};
    array_i32_reserve(&arena, &a, 128);
    array_ptr_i32 b = {};
    array_ptr_i32_reserve(&arena, &b, 128);
    arrayfptr_i32 ff = {};
    i32 y = makeg_i32(3);
    u64 h = makehash_i32(num);
    i32 x = num + 8 + 1;
    y = x - 1;
    i32 z = 1;
    i32 w = x * y;
    i32 d = x / y;
    i32 t = prock_i32(&x);
    return 0;
}

void array_i32_reserve(memops_arena * arena, array_i32 * ar, u64 count) {
}

void array_ptr_i32_reserve(memops_arena * arena, array_ptr_i32 * ar, u64 count) {
}

i32 makeg_i32(i32 x) {
    return x;
}

u64 prock_i32(i32 * value) {
    return value[0];
}

u64 makehash_i32(i32 x) {
    return hash_i32(&x);
}

