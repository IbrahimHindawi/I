cinclude "stdio.h"

printf: proc[external](fmt: *const char, ...) -> i32 = {}

// Aggregates are values. A lowering that passes or assigns them by reference
// produces the right answer for reads and the wrong answer the moment anything
// writes through the second name.

P: struct = {
    x: i32;
    y: i32;
}

Inner: struct = {
    a: [3]i32;
}

Outer: struct = {
    inner: Inner;
    tag: i32;
}

// Assignment copies: writing through b must not disturb a.
copy_independent: proc() -> i32 = {
    a: P = {};
    a.x = 1;
    b: P = a;
    b.x = 77;
    return a.x * 100 + b.x;
}

// The copy reaches through a nested struct and the array inside it.
nested_deep_copy: proc() -> i32 = {
    o1: Outer = {};
    o1.inner.a[1] = 42;
    o1.tag = 9;
    o2: Outer = o1;
    o2.inner.a[1] = 7;
    return o1.inner.a[1] * 100 + o2.inner.a[1];
}

// A parameter is a copy: the callee mutating it must not reach the caller.
mutate_param: proc(p: P) -> i32 = {
    p.x = 999;
    return p.x;
}

by_value: proc() -> i32 = {
    a: P = {};
    a.x = 1;
    got: i32 = mutate_param(a);
    return got * 10 + a.x;
}

// A returned struct is a value the caller owns.
make_p: proc() -> P = {
    r: P = {};
    r.x = 5;
    r.y = 6;
    return r;
}

by_return: proc() -> i32 = {
    g: P = make_p();
    return g.x * 10 + g.y;
}

// Struct elements of an array copy the same way.
struct_in_array: proc() -> i32 = {
    arr: [2]P = {};
    arr[0].x = 3;
    arr[1] = arr[0];
    arr[1].x = 8;
    return arr[0].x * 10 + arr[1].x;
}

// A pointer to an aggregate aliases it, which is the case that must still work.
alias_through_pointer: proc() -> i32 = {
    a: P = {};
    a.x = 4;
    p: *P = a.&;
    p[0].x = 12;
    return a.x;
}

main: proc() -> i32 = {
    printf("%d\n", copy_independent());
    printf("%d\n", nested_deep_copy());
    printf("%d\n", by_value());
    printf("%d\n", by_return());
    printf("%d\n", struct_in_array());
    printf("%d\n", alias_through_pointer());
    return 0;
}
