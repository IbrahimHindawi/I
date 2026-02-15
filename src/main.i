import "stdio.h"


import "saha.i.h"

# external prototypes
# memops_arena_initialize:proc(arena:*memops_arena)->void={external;}

# variable
num:i32=7;

arena:memops_arena={};

# struct type
vec2:struct={x:f32; y:f32;}

# generic struct type
array:struct<T>={len:u64; cap:u64; data:*T;}
array<T>reserve:proc<T>(arena: *memops_arena, ar: *array<T>, count:u64)->void={
    # implementation...
}

ptr:struct<T>={data:*T;len:u64;}

# order of declaration matters =)
arrayfptr:struct<T>={len:u64; cap:u64; data:ptr<T>;}

# proc
make:proc(x:i32)->i32={ret x;}

# generic proc
makeg:proc<T>(x:T)->T={ret x;}

prock:proc<T>(value:*T)->u64{ret value[0];}

hash_fnv1a:proc(data:*void, len:u64)->u64{
    p:*u8 = cast(data,*u8);
    hash:u64 = 1469598103934665603u64;
    for (i:u64=0; i<len; i+=1) {
        hash ^= p[i];
        hash *= 1099511628211u64;
    }
    ret hash;
}

# requirement prototype:
# hash:proc<T>(x:*T)->u64{ret hash_fnv1a(&x);}

# requirement instantiation:
hash:proc<i32>(x:*i32)->u64{ret hash_fnv1a(&x, sizeof(x[0]));}

# constrained generic proc
makehash:proc<T:hash>(x:T)->u64={ret hash<T>(&x);}

# entry point
main:proc()->i32={
    printf("Hello, World!\n");
    printf("num = {}\n", num);
    a: array<i32> = {};
    array<i32>reserve(&arena, &a, 128);
    b: array<ptr<i32>> = {};
    array<ptr<i32>>reserve(&arena, &b, 128);
    # array_reserve<i32>(&arena, 128);
    # array@reserve<i32>(&arena, 128);
    # array::reserve<i32>(&arena, 128);
    ff:arrayfptr<i32> = {};
    y:i32 = makeg<i32>(3);
    h:u64 = makehash<i32>(num);
    x:i32 = num + 8 + 1;
    y = x - 1;
    z: i32 = 1;
    w: i32 = x * y;
    d: i32 = x / y;
    t: i32 = prock<i32>(&x);
    ret 0;
}
