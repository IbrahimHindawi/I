cinclude "stdio.h"

printf: proc[external](fmt: *const char, ...) -> i32 = {}

// A generic proc can take several type parameters. Each instantiation
// monomorphises to one symbol whose suffix joins the arguments in order, so
// `pick_first<i32, f32>` becomes `pick_first_i32_f32`.

pick_first: proc<T, U>(a: T, b: U) -> T = {
    return a;
}

pick_second: proc<T, U>(a: T, b: U) -> U = {
    return b;
}

// The parameters are positional, so the same two types in the other order are a
// different instantiation with its own symbol.
middle: proc<A, B, C>(x: A, y: B, z: C) -> B = {
    return y;
}

// A parameter may appear more than once, and in a compound type.
sum_pair: proc<T, U>(a: T, b: T, tag: U) -> T = {
    unused: U = tag;
    return a + b;
}

count_through: proc<T, U>(value: T, out: *U, seen: U) -> T = {
    out[0] = seen;
    return value;
}

// One parameter still behaves exactly as it always did.
identity: proc<T>(v: T) -> T = {
    return v;
}

// Two instantiations over different type tuples must not collide: if they
// mangled to the same symbol one would silently win, which is the failure this
// whole scheme exists to avoid.
distinct_instantiations: proc() -> i32 = {
    a: i32 = pick_first<i32, f32>(7, 2.5f);
    b: i32 = pick_second<f32, i32>(2.5f, 9);
    return a * 10 + b;
}

// Order matters: <i32, f32> and <f32, i32> are separate types.
order_matters: proc() -> i32 = {
    from_left: i32 = pick_first<i32, f32>(3, 1.5f);
    from_right: i32 = pick_second<f32, i32>(1.5f, 4);
    return from_left * 10 + from_right;
}

// The return type follows whichever parameter the signature names.
returns_middle: proc() -> i32 = {
    return middle<f32, i32, char>(1.5f, 42, 65);
}

repeated_param: proc() -> i32 = {
    return sum_pair<i32, char>(20, 22, 65);
}

out_param: proc() -> i32 = {
    seen: i32 = 0;
    v: f32 = count_through<f32, i32>(2.5f, seen.&, 11);
    return seen + cast(v, i32);
}

single_param_unchanged: proc() -> i32 = {
    return identity<i32>(5) + cast(identity<f32>(0.5f), i32);
}

// Structs take several parameters too, and monomorphise the same way: the
// arguments join in order, so Pair<i32, f32> becomes Pair_i32_f32.
Pair: struct<K, V> = {
    key: K;
    value: V;
}

Pair<K, V>make: proc<K, V>(k: K, v: V) -> Pair<K, V> = {
    p: Pair<K, V> = {};
    p.key = k;
    p.value = v;
    return p;
}

// A generic struct nested inside another, so the substitution has to reach
// through a field whose own type is generic.
Boxed: struct<T> = {
    inner: T;
}

// Swapping the arguments is a different type with its own layout, which is the
// thing a joined name has to keep separate.
generic_struct_pair: proc() -> i32 = {
    a: Pair<i32, f32> = Pair<i32, f32>make(7, 2.5f);
    b: Pair<f32, i32> = Pair<f32, i32>make(1.5f, 9);
    return a.key * 10 + b.value;
}

// Reflection sees each instantiation as its own record.
generic_struct_reflection: proc() -> i32 = {
    return cast(Pair_i32_f32<>.count, i32) * 10 + cast(Pair_f32_i32<>.count, i32);
}

nested_generic_struct: proc() -> i32 = {
    b: Boxed<Pair<i32, f32>> = {};
    b.inner.key = 4;
    b.inner.value = 0.5f;
    return b.inner.key * 10 + cast(b.inner.value * 2.0f, i32);
}

main: proc() -> i32 = {
    printf("%d\n", distinct_instantiations());
    printf("%d\n", order_matters());
    printf("%d\n", returns_middle());
    printf("%d\n", repeated_param());
    printf("%d\n", out_param());
    printf("%d\n", single_param_unchanged());
    printf("%d\n", generic_struct_pair());
    printf("%d\n", generic_struct_reflection());
    printf("%d\n", nested_generic_struct());
    return 0;
}
