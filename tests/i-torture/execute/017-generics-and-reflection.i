cinclude "stdio.h"

printf: proc(fmt: *const char, ...)->i32 = { external; }

// Generics and reflection are I's distinctive features, which makes them the
// least battle-tested part of the compiler. Monomorphisation in particular has
// a failure mode where two instantiations mangle to the same C symbol and one
// silently wins.

Box: struct<T> = {
    v: T;
    tag: i32;
}

Box<T>make: proc<T>(v: T, tag: i32)->Box<T> = {
    b: Box<T> = {};
    b.v = v;
    b.tag = tag;
    return b;
}

Box<T>get: proc<T>(b: *Box<T>)->T = {
    return b[0].v;
}

// A generic whose field is itself an instantiation of a generic.
Pair: struct<T> = {
    a: Box<T>;
    b: Box<T>;
}

Color: enum = {
    Red = 1,
    Green = 2,
    Blue = 4,
}

Point: struct = {
    x: i32;
    y: i32;
    label: *const char;
}

// Two instantiations over different types are separate types with separate
// storage. If they collided, one of these reads would return the other's value.
distinct_instantiations: proc()->i32 = {
    bi: Box<i32> = Box<i32>make(7, 1);
    bf: Box<f32> = Box<f32>make(2.5f, 2);
    return Box<i32>get(bi.&) * 100 + bi.tag * 10 + bf.tag;
}

// The same instantiation used twice stays consistent.
repeated_instantiation: proc()->i32 = {
    one: Box<i32> = Box<i32>make(7, 0);
    two: Box<i32> = Box<i32>make(9, 0);
    return Box<i32>get(one.&) * 10 + Box<i32>get(two.&);
}

// A generic nested inside another generic instantiates both levels.
nested_generic: proc()->i32 = {
    p: Pair<i32> = {};
    p.a = Box<i32>make(4, 0);
    p.b = Box<i32>make(5, 0);
    return p.a.v * 10 + p.b.v;
}

// Reflection reports the enum's cardinality and its authored values, including
// the explicit non-sequential ones.
enum_reflection: proc()->i32 = {
    total: i32 = 0;
    for (i: u64 = 0; i < Color<>.value_count; i += 1) {
        total += Color<>.values[i].value;
    }
    return cast(Color<>.value_count, i32) * 100 + total;
}

// Field reflection walks the struct in declaration order.
field_reflection: proc()->i32 = {
    return cast(Point<>.field_count, i32);
}

// An enum's cardinality is usable as an array size, which is the thing that
// replaces a hand-maintained count constant and cannot drift from the enum.
enum_sized_array: proc()->i32 = {
    table: [Color<>.value_count]i32 = {};
    table[0] = 11;
    return cast(sizeof(table) / sizeof(i32), i32) * 100 + table[0];
}

main: proc()->i32 = {
    printf("%d\n", distinct_instantiations());
    printf("%d\n", repeated_instantiation());
    printf("%d\n", nested_generic());
    printf("%d\n", enum_reflection());
    printf("%d\n", field_reflection());
    printf("%d\n", enum_sized_array());
    printf("%s %s\n", Color<>.name, Point<>.name);
    printf("%s %s %s\n", Point<>.fields[0].name, Point<>.fields[1].name, Point<>.fields[2].name);
    printf("%s %s %s\n", Color<>.values[0].name, Color<>.values[1].name, Color<>.values[2].name);
    return 0;
}
