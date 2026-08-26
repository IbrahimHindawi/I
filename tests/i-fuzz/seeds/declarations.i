// Dense declaration surface: every top-level form the parser dispatches on, so a
// mutation lands somewhere interesting rather than in filler.
cinclude "stdio.h"
define("SEED_IMPLEMENTATION")

#define SEED_LIMIT 8

printf: proc[external](fmt: *const char, ...) -> i32 = {}

Color: enum = { Red = -1, Green = 0, Blue = 1 shl 2, Alias = Blue }

Pair: struct = {
    x: i32;
    y: f32;
}

Packed: struct = {
    lo: u32 : 3;
    hi: u32 : 29;
    union = {
        a: i32;
        b: f32;
    }
}

Bag: struct<T> = {
    items: *T;
    count: u64;
}

Handler: alias = *proc(value: i32) -> i32;

Blob: union = {
    as_i32: i32;
    as_f32: f32;
}

counter: static i32 = 0;
exported: i32 = 1;

hidden: static proc(x: i32) -> i32 = {
    local: static i32 = 0;
    local += x;
    return local;
}

Bag<T>first: proc<T>(bag: Bag<T>) -> T = {
    return bag.items[0];
}

main: proc(argc: i32, argv: **char) -> i32 = {
    p: Pair = { .x = 1, .y = 2.5 };
    k: Packed = {};
    k.lo = 7;
    b: Blob = {};
    b.as_i32 = 3;
    h: Handler = hidden;
    c: char = '\n';
    m: u32 = ~cast(0, u32);
    m shl= 1;
    m shr= 2;
    printf("%d %f %u %d %c\n", p.x, p.y, m, h(1), c);
    for (i: i32 = 0; i < SEED_LIMIT; i += 1) {
        if (i == 3) { goto done; }
        switch (i) {
            case 0: { break; }
            default: { break; }
        }
    }
done: label = {
    printf("done\n");
}
    return cast(Color_Red, i32) + argc;
}
