cinclude "stdio.h"

printf: proc[external](fmt: *const char, ...) -> i32 = {}

// InitList, CompoundInit and ZeroInit are three separate expression kinds, and
// `= {}` is the idiom every declaration in a real program uses. A lowering that
// forgets to zero the unmentioned tail leaves garbage that reads as plausible
// values, which is the worst way for this to fail.

P: struct = {
    x: i32;
    y: i32;
    z: i32;
}

Nest: struct = {
    p: P;
    arr: [3]i32;
    tag: i32;
}

// Naming one field must zero the other two rather than leaving them undefined.
partial: proc() -> i32 = {
    a: P = { 1 };
    return a.x * 100 + a.y * 10 + a.z;
}

// A designated initialiser lands in the named field and zeroes the rest.
designated: proc() -> i32 = {
    b: P = { .y = 7 };
    return b.x * 100 + b.y * 10 + b.z;
}

// `= {}` zeroes through a nested struct and the array inside it.
zeroed: proc() -> i32 = {
    n: Nest = {};
    return n.p.x + n.p.y + n.arr[0] + n.arr[1] + n.arr[2] + n.tag;
}

// Nested designators, with a partially filled inner array.
nested: proc() -> i32 = {
    m: Nest = { .p = { .x = 3 }, .arr = { 9, 8 }, .tag = 5 };
    return m.p.x * 10000 + m.p.y * 1000 + m.arr[0] * 100 + m.arr[1] * 10 + m.arr[2];
}

nested_tag: proc() -> i32 = {
    m: Nest = { .p = { .x = 3 }, .arr = { 9, 8 }, .tag = 5 };
    return m.tag;
}

// An array of structs where only the leading elements are given.
array_of_structs: proc() -> i32 = {
    arr: [3]P = { { .x = 1 }, { .x = 2 } };
    return arr[0].x * 100 + arr[1].x * 10 + arr[2].x;
}

// A zeroed array of structs is zero all the way down.
zeroed_array: proc() -> i32 = {
    arr: [3]P = {};
    total: i32 = 0;
    for (i: i32 = 0; i < 3; i += 1) {
        total += arr[i].x + arr[i].y + arr[i].z;
    }
    return total;
}

main: proc() -> i32 = {
    printf("%d\n", partial());
    printf("%d\n", designated());
    printf("%d\n", zeroed());
    printf("%d\n", nested());
    printf("%d\n", nested_tag());
    printf("%d\n", array_of_structs());
    printf("%d\n", zeroed_array());
    return 0;
}
