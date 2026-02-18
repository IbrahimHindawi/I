import "stdio.h"
define("SAHA_IMPLEMENTATION")
import "saha.i.h"

# external prototypes
memops_arena_initialize:proc(arena:*memops_arena)->void={external;}
memops_arena_push:proc(arena:*memops_arena, alloc_size:u64, align:u64)->*void={external;}

memops_arena_push_array_i:proc<T>(arena:*memops_arena, count:u64)->*void={
    alloc_size:u64=sizeof(T)*count;
    alignment:u64=alignof(T);
    alloc_ptr:*void=memops_arena_push(arena, alloc_size, alignment);
    return alloc_ptr;
}

# variable
num:i32=7;

# struct type
vec2:struct={x:f32; y:f32;}

# generic struct type
array:struct<T> = {
    length:u64; 
    border:u64; 
    data:*T;
}
array<T>reserve:proc<T>(arena: *memops_arena, length:u64)->array<T>={
    arr:array<T> = {};
    if (length == 0) {
        return arr;
    }
    arr.data = memops_arena_push_array_i<T>(arena, length);
    if (arr.data == null) {
        printf("memops arena allocation failure!\n");
        arr.data = 0;
        return arr;
    }
    arr.border = length;
    return arr;
}

ptr:struct<T>={data:*T;length:u64;}

# order of declaration matters =)
arrayfptr:struct<T>={length:u64; cap:u64; data:ptr<T>;}

# proc
make:proc(x:i32)->i32={return x;}

# generic proc
makeg:proc<T>(x:T)->T={return x;}

prock:proc<T>(value:*T)->u64{return value[0];}

hash_fnv1a:proc(data:*void, length:u64)->u64{
    p:*u8 = cast(data,*u8);
    hash:u64 = 1469598103934665603u64;
    for (i:u64=0; i<length; i+=1) {
        hash ^= p[i];
        hash *= 1099511628211u64;
    }
    return hash;
}

# requirement prototype:
# hash:proc<T>(x:*T)->u64{return hash_fnv1a(&x);}

# requirement instantiation:
hash:proc<i32>(x:*i32)->u64{return hash_fnv1a(&x, sizeof(x[0]));}

# constrained generic proc
makehash:proc<T:hash>(x:T)->u64={return hash<T>(&x);}

test_:proc(arena:*memops_arena)->void={
    b: array<ptr<i32>> = {};
    array<ptr<i32>>reserve(arena, 128);
    ff:arrayfptr<i32> = {};
    y:i32 = makeg<i32>(3);
    h:u64 = makehash<i32>(num);
    x:i32 = num + 8 + 1;
    y = x - 1;
    z: i32 = 1;
    w: i32 = x * y;
    d: i32 = x / y;
    t: i32 = prock<i32>(&x);
}

# entry point
main:proc()->i32={
    printf("Hello, World!\n");
    printf("num = {}\n", num);
    arena:memops_arena={};
    memops_arena_initialize(&arena);
    a: array<i32> = {};
    a = array<i32>reserve(&arena, 128);
    for (i:i32=0; i<128; i+=1) { 
        a.data[i] = i; 
    }
    for (i:i32=0; i<128; i+=1) {
        printf("i = {}, ", a.data[i]); 
    }
    return 0;
}
