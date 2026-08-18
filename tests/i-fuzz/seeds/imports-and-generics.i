// Exercises the import graph, generic instantiation, requirements, reflection, and
// printfmt rewriting, which are the analysis paths with their own error handling.
import "std/memops.i"
import "std/Array.i"
import "std/List.i"
import "std/Print.i"

Payload: struct = {
    id: i32;
    weight: f32;
}

hash: proc<i32>(value: *i32)->u64 = {
    return cast(value[0], u64);
}

needs_hash: proc<T: hash>(value: T)->u64 = {
    return hash<T>(value.&);
}

add: proc<i32>(a: i32, b: i32)->i32 = {
    return a + b;
}

sum: proc<T>(items: *T, count: u64)->T = {
    total: T = {};
    for (i: u64 = 0; i < count; i += 1) {
        total = add<T>(total, items[i]);
    }
    return total;
}

main: proc()->i32 = {
    arena: memops_arena = {};
    memops_arena_initialize(arena.&);

    numbers: Array<i32> = Array<i32>reserve(arena.&, 4);
    for (i: i32 = 0; i < 4; i += 1) {
        numbers.data[i] = i;
    }
    printfmt("sum = {}\n", sum<i32>(numbers.data, numbers.length));
    printfmt("hash = {}\n", needs_hash<i32>(7));

    chain: *List<Payload> = List<Payload>create(arena.&);
    List<Payload>append(arena.&, chain, { .id = 1, .weight = 0.5 });

    printfmt("{} has {} fields\n", Payload<>.name, Payload<>.count);
    for (i: i32 = 0; i < Payload<>.count; i += 1) {
        printfmt("  {}\n", Payload<>.variant.fields[i].name);
    }
    return 0;
}
