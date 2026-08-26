cinclude "stdio.h"

import "std/reflect.i"

printf: proc[external](fmt: *const char, ...) -> i32 = {}

// Generics and reflection are I's distinctive features, which makes them the
// least battle-tested part of the compiler. Monomorphisation in particular has
// a failure mode where two instantiations mangle to the same C symbol and one
// silently wins.

Box: struct<T> = {
    v: T;
    tag: i32;
}

Box<T>make: proc<T>(v: T, tag: i32) -> Box<T> = {
    b: Box<T> = {};
    b.v = v;
    b.tag = tag;
    return b;
}

Box<T>get: proc<T>(b: *Box<T>) -> T = {
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

// A negative member. Reflection stores values as i32 precisely so these survive
// the round trip; an unsigned field would report 4294967295 here and every
// lookup by value would then miss a member that plainly exists.
Slot: enum = {
    Empty = -1,
    Filled = 0,
}

Point: struct = {
    x: i32;
    y: i32;
    label: *const char;
}

// Union members overlap. Reflection reports a union as its own kind rather than
// as a struct, so a walker cannot treat these as three successive fields.
Word: union = {
    i: i32;
    f: f32;
    pair: [2]i16;
}

Inner: struct = {
    a: i32;
    b: i32;
}

// Fields whose types have their own tables, reached directly, through a pointer,
// and through an array -- plus one builtin, which has no table at all.
Nest: struct = {
    direct: Inner;
    through_ptr: *Inner;
    through_array: [3]Inner;
    plain: i32;
    hue: Color;
}

// A self-referential type must link to its own record without the emitter
// recursing forever or referring to a symbol it has not declared yet.
Node: struct = {
    value: i32;
    next: *Node;
}

// Two instantiations over different types are separate types with separate
// storage. If they collided, one of these reads would return the other's value.
distinct_instantiations: proc() -> i32 = {
    bi: Box<i32> = Box<i32>make(7, 1);
    bf: Box<f32> = Box<f32>make(2.5f, 2);
    return Box<i32>get(bi.&) * 100 + bi.tag * 10 + bf.tag;
}

// The same instantiation used twice stays consistent.
repeated_instantiation: proc() -> i32 = {
    one: Box<i32> = Box<i32>make(7, 0);
    two: Box<i32> = Box<i32>make(9, 0);
    return Box<i32>get(one.&) * 10 + Box<i32>get(two.&);
}

// A generic nested inside another generic instantiates both levels.
nested_generic: proc() -> i32 = {
    p: Pair<i32> = {};
    p.a = Box<i32>make(4, 0);
    p.b = Box<i32>make(5, 0);
    return p.a.v * 10 + p.b.v;
}

// Reflection reports the enum's cardinality and its authored values, including
// the explicit non-sequential ones.
enum_reflection: proc() -> i32 = {
    total: i32 = 0;
    for (i: u64 = 0; i < Color<>.count; i += 1) {
        total += Color<>.variant.values[i].value;
    }
    return cast(Color<>.count, i32) * 100 + total;
}

// Field reflection walks the struct in declaration order.
field_reflection: proc() -> i32 = {
    return cast(Point<>.count, i32);
}

// An enum's cardinality is usable as an array size, which is the thing that
// replaces a hand-maintained count constant and cannot drift from the enum.
enum_sized_array: proc() -> i32 = {
    table: [Color<>.count]i32 = {};
    table[0] = 11;
    return cast(sizeof(table) / sizeof(i32), i32) * 100 + table[0];
}

// One record now describes all three, and `kind` is what tells them apart. This
// is what replaced having to know in advance which of two unrelated record types
// a given `<>` would hand back.
kind_tags: proc() -> i32 = {
    return cast(Point<>.kind, i32) * 100 + cast(Word<>.kind, i32) * 10 + cast(Color<>.kind, i32);
}

// Every union member starts at offset zero. Summing the offsets is the cheapest
// discriminating check that the union's fields were not laid out consecutively.
union_offsets: proc() -> i32 = {
    total: u64 = 0;
    for (i: u64 = 0; i < Word<>.count; i += 1) {
        total += Word<>.variant.fields[i].offset;
    }
    return cast(Word<>.count, i32) * 10 + cast(total, i32);
}

// A real recursive walk: total the field counts of Nest's struct-typed fields by
// following `info`, without naming Inner's layout here. Before the nested link
// existed, a walk stopped at the mangled type-name string and this was
// impossible to write.
recursive_field_sum: proc() -> i32 = {
    total: u64 = 0;
    for (i: u64 = 0; i < Nest<>.count; i += 1) {
        info: *const reflect = Nest<>.variant.fields[i].info;
        if (info != null and info[0].kind == reflect_kind_struct) {
            total += info[0].count;
        }
    }
    return cast(total, i32);
}

// The checked accessors refuse the wrong variant arm rather than reinterpreting
// the pointer, which is what makes a plain union safe to hand around.
variant_guards: proc() -> i32 = {
    fields_on_enum: b32 = reflect_fields(Color<>.&) == null;
    values_on_struct: b32 = reflect_values(Point<>.&) == null;
    fields_on_union: b32 = reflect_fields(Word<>.&) != null;
    values_on_enum: b32 = reflect_values(Color<>.&) != null;
    return cast(fields_on_enum, i32) * 1000 +
        cast(values_on_struct, i32) * 100 +
        cast(fields_on_union, i32) * 10 +
        cast(values_on_enum, i32);
}

main: proc() -> i32 = {
    printf("%d\n", distinct_instantiations());
    printf("%d\n", repeated_instantiation());
    printf("%d\n", nested_generic());
    printf("%d\n", enum_reflection());
    printf("%d\n", field_reflection());
    printf("%d\n", enum_sized_array());
    printf("%d\n", kind_tags());
    printf("%d\n", union_offsets());
    printf("%d\n", recursive_field_sum());
    printf("%d\n", variant_guards());
    printf("%s %s\n", Color<>.name, Point<>.name);
    printf("%s %s %s\n",
        Point<>.variant.fields[0].name,
        Point<>.variant.fields[1].name,
        Point<>.variant.fields[2].name);
    printf("%s %s %s\n",
        Color<>.variant.values[0].name,
        Color<>.variant.values[1].name,
        Color<>.variant.values[2].name);
    printf("%d %d\n", Slot<>.variant.values[0].value, Slot<>.variant.values[1].value);
    printf("%s %s %s %s %s\n",
        Nest<>.variant.fields[0].info[0].name,
        Nest<>.variant.fields[1].info[0].name,
        Nest<>.variant.fields[2].info[0].name,
        Nest<>.variant.fields[3].info == null ? "null" : "linked",
        Nest<>.variant.fields[4].info[0].name);
    printf("%s\n", Node<>.variant.fields[1].info[0].name);
    printf("%s %s %s\n",
        reflect_kind_name(Point<>.kind),
        reflect_kind_name(Word<>.kind),
        reflect_kind_name(Color<>.kind));
    return 0;
}
