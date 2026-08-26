cinclude "stdio.h"

printf: proc[external](fmt: *const char, ...) -> i32 = {}

// Corners of the type surface that real code touches rarely enough to go
// untested: unions, aliases, sizeof against an array, and the empty for.

U: union = {
    i: i32;
    b: [4]u8;
}

Alias: alias = i32;

// Members of a union share storage. Writing the integer and reading the bytes
// back is the whole point of the construct; if the lowering gave each member
// its own storage the reads would come back zero.
union_aliases: proc() -> i32 = {
    u: U = {};
    u.i = 0x01020304;
    return cast(u.b[0], i32) * 100 + cast(u.b[3], i32);
}

// A union is as large as its largest member, not the sum of them.
union_size: proc() -> i32 = {
    return cast(sizeof(U), i32);
}

// An alias is the type it names, not a distinct one wrapping it.
alias_is_transparent: proc() -> i32 = {
    v: Alias = 9;
    w: i32 = v;
    return v + w;
}

// sizeof on an array is the array's size. If the array decayed to a pointer
// first -- the classic C trap -- this would report the pointer width instead,
// so compare against the element type rather than a hardcoded byte count.
sizeof_array_does_not_decay: proc() -> i32 = {
    arr: [4]i32 = {};
    return cast(sizeof(arr) / sizeof(i32), i32);
}

// A type's size and the size of a pointer to it are different questions.
// Returns 1 when they differ, which is the correct answer here: the union is
// four bytes and a pointer is the machine word. Stated as a comparison so the
// test does not bake in a word size.
sizeof_struct_differs_from_pointer: proc() -> i32 = {
    return sizeof(U) != sizeof(*U) ? 1 : 0;
}

// `for` with all three clauses omitted runs until something breaks out.
empty_for: proc() -> i32 = {
    n: i32 = 0;
    for (;;) {
        n += 1;
        if (n > 3) {
            break;
        }
    }
    return n;
}

// An else binds to the nearest if. I requires braces on every if body, so the
// dangling-else ambiguity cannot be written at all -- this records the binding
// that the braces already make unambiguous.
else_binds_to_nearest: proc() -> i32 = {
    a: i32 = 1;
    b: i32 = 0;
    r: i32 = -1;
    if (a != 0) {
        if (b != 0) {
            r = 10;
        } else {
            r = 20;
        }
    }
    return r;
}

main: proc() -> i32 = {
    printf("%d\n", union_aliases());
    printf("%d\n", union_size());
    printf("%d\n", alias_is_transparent());
    printf("%d\n", sizeof_array_does_not_decay());
    printf("%d\n", sizeof_struct_differs_from_pointer());
    printf("%d\n", empty_for());
    printf("%d\n", else_binds_to_nearest());
    return 0;
}
