cinclude "stdio.h"
import "std/memops.i"
import "std/Array.i"

printf: proc[external](fmt: *const char, ...)->i32 = {}

// Non-simple type arguments used to emit the mangled spelling as a C type name,
// so Array<*i32> generated 'ptr_i32 * data;' and would not compile.
main: proc()->i32 = {
    arena: memops_arena = {};
    memops_arena_initialize(arena.&);

    vals: Array<i32> = Array<i32>reserve(arena.&, 3);
    vals.data[0] = 10;
    vals.data[1] = 20;
    vals.data[2] = 30;

    ptrs: Array<*i32> = Array<*i32>reserve(arena.&, 3);
    for (i: i32 = 0; i < 3; i += 1) {
        ptrs.data[i] = vals.data[i].&;
    }

    names: Array<*const char> = Array<*const char>reserve(arena.&, 2);
    names.data[0] = "alpha";
    names.data[1] = "beta";

    consts: Array<const i32> = Array<const i32>reserve(arena.&, 1);
    vols: Array<volatile u32> = Array<volatile u32>reserve(arena.&, 1);
    nested: Array<Array<i32>> = Array<Array<i32>>reserve(arena.&, 1);
    nested.data[0] = vals;

    printf("%d %d %d\n", ptrs.data[0][0], ptrs.data[1][0], ptrs.data[2][0]);
    printf("%s %s\n", names.data[0], names.data[1]);
    printf("%d %llu\n", nested.data[0].data[1], nested.data[0].length);
    printf("%llu %llu %llu\n", consts.length, vols.length, ptrs.length);
    return 0;
}
