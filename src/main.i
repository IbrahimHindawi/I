# variable
num:i32=7;

# struct type
vec2:struct={x:f32; y:f32;}

# generic struct type
arr:struct<T>={len:u64; cap:u64; data:ptr<T>;}

# ptr:struct<T>={data:*T;len:u64;}

# proc
make:proc(x:i32)->i32={ret x;}

# generic proc
makeg:proc<T>(x:T)->T={ret x;}

prock:proc<T>(value:*T)->u64{ret value[0];}
# hash:proc<T>(value:*T)->u64{ret hash_bytes(value, sizeof(value[0]));}

# constrained generic proc
# makehash:proc<T:hashable>(x:T)->u64={ret hash<T>(&x);}

# entry point
main:proc()->i32={
    a: arr<i32>;
    y:i32 = makeg<i32>(3);
    # h:u64 = makehash<i32>(num);
    x:i32 = num + 666 + 1;
    y = x - 1;
    z: i32 = 1;
    w: i32 = x * y;
    d: i32 = x / y;
    t: i32 = prock<i32>(&x);
    ret x;
}
