#include <core.h>

#define SAHA_IMPLEMENTATION

#include "stdio.h"
#include "saha.i.h"

structdecl(vec2);
structdecl(array_ptr_i32);
structdecl(array_i32);
structdecl(ptr_i32);
structdecl(arrayfptr_i32);

structdef(vec2) {
    f32 x;
    f32 y;
};

structdef(array_ptr_i32) {
    u64 length;
    u64 border;
    ptr_i32 * data;
};

structdef(array_i32) {
    u64 length;
    u64 border;
    i32 * data;
};

structdef(ptr_i32) {
    i32 * data;
    u64 length;
};

structdef(arrayfptr_i32) {
    u64 length;
    u64 cap;
    ptr_i32 data;
};

i32 num = 7;

void memops_arena_initialize(memops_arena * arena);
void * memops_arena_push(memops_arena * arena, u64 alloc_size, u64 align);
i32 make(i32 x);
u64 hash_fnv1a(void * data, u64 length);
u64 hash_i32(i32 * x);
void test_(memops_arena * arena);
i32 main();
void * memops_arena_push_array_i_i32(memops_arena * arena, u64 count);
void * memops_arena_push_array_i_ptr_i32(memops_arena * arena, u64 count);
array_ptr_i32 array_ptr_i32_reserve(memops_arena * arena, u64 length);
array_i32 array_i32_reserve(memops_arena * arena, u64 length);
i32 makeg_i32(i32 x);
u64 prock_i32(i32 * value);
u64 makehash_i32(i32 x);

i32 make(i32 x) {
    return x;
}

u64 hash_fnv1a(void * data, u64 length) {
    u8 * p = ((u8 *)(data));
    u64 hash = ((u64)1469598103934665603);
    for (u64 i = 0; i < length; i += 1) {
        hash ^= p[i];
        hash *= ((u64)1099511628211);
    }
    return hash;
}

u64 hash_i32(i32 * x) {
    return hash_fnv1a(&x, sizeof(x[0]));
}

void test_(memops_arena * arena) {
    array_ptr_i32 b = {};
    array_ptr_i32_reserve(arena, 128);
    arrayfptr_i32 ff = {};
    i32 y = makeg_i32(3);
    u64 h = makehash_i32(num);
    i32 x = num + 8 + 1;
    y = x - 1;
    i32 z = 1;
    i32 w = x * y;
    i32 d = x / y;
    i32 t = prock_i32(&x);
}

i32 main() {
    printf("Hello, World!\n");
    printf("num = %d\n", num);
    memops_arena arena = {};
    memops_arena_initialize(&arena);
    array_i32 a = {};
    memops_arena_push_array_i_i32(&arena, 128);
    a = array_i32_reserve(&arena, 128);
    for (i32 i = 0; i < 128; i += 1) {
        a.data[i] = i;
    }
    for (i32 i = 0; i < 128; i += 1) {
        printf("i = %d, ", a.data[i]);
    }
    return 0;
}

void * memops_arena_push_array_i_i32(memops_arena * arena, u64 count) {
    u64 alloc_size = sizeof(i32) * count;
    u64 alignment = _Alignof(i32);
    void * alloc_ptr = memops_arena_push(arena, alloc_size, alignment);
    return alloc_ptr;
}

void * memops_arena_push_array_i_ptr_i32(memops_arena * arena, u64 count) {
    u64 alloc_size = sizeof(ptr_i32) * count;
    u64 alignment = _Alignof(ptr_i32);
    void * alloc_ptr = memops_arena_push(arena, alloc_size, alignment);
    return alloc_ptr;
}

array_ptr_i32 array_ptr_i32_reserve(memops_arena * arena, u64 length) {
    array_ptr_i32 arr = {};
    if (length == 0) {
        return arr;
    }
    arr.data = memops_arena_push_array_i_ptr_i32(arena, length);
    if (arr.data == 0) {
        printf("memops arena allocation failure!\n");
        arr.data = 0;
        return arr;
    }
    arr.border = length;
    return arr;
}

array_i32 array_i32_reserve(memops_arena * arena, u64 length) {
    array_i32 arr = {};
    if (length == 0) {
        return arr;
    }
    arr.data = memops_arena_push_array_i_i32(arena, length);
    if (arr.data == 0) {
        printf("memops arena allocation failure!\n");
        arr.data = 0;
        return arr;
    }
    arr.border = length;
    return arr;
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

