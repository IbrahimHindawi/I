cinclude "stdio.h"

printf: proc[external](fmt: *const char, ...) -> i32 = {}

// Hazards that exist because I lowers to C rather than to machine code. The
// source is order-independent; the emitted C is not, and C resolves names by a
// different set of rules than I does. Each of these is a place where the two
// could disagree without either one looking wrong on its own.

// Mutual recursion: is_even calls is_odd before is_odd exists in the file, so
// the emitter has to produce a forward declaration. Without one the C is
// rejected, or worse, C infers a signature that disagrees with the real one.
is_even: proc(n: i32) -> i32 = {
    if (n == 0) {
        return 1;
    }
    return is_odd(n - 1);
}

is_odd: proc(n: i32) -> i32 = {
    if (n == 0) {
        return 0;
    }
    return is_even(n - 1);
}

// A struct whose field type is declared further down the file. The struct
// declarations have to be ordered or forward declared on the way out.
Node: struct = {
    payload: *Later;
    tag: i32;
}

Later: struct = {
    v: i32;
}

// A global that a local will shadow.
g_shadow: i32 = 100;

// The local wins inside the proc, and the global is untouched outside it. If
// the lowering dropped the shadow, both reads would return the same value.
shadows_global: proc() -> i32 = {
    g_shadow: i32 = 5;
    g_shadow += 1;
    return g_shadow;
}

// Sibling blocks may reuse a name, because their scopes do not overlap. Note
// that shadowing an *enclosing* local is currently rejected while shadowing a
// global is allowed; that asymmetry is an open question recorded in
// docs/compiler-hardening.md, so this test pins only the part that is settled.
sibling_scopes: proc() -> i32 = {
    total: i32 = 0;
    if (total == 0) {
        v: i32 = 1;
        total += v;
    }
    if (total == 1) {
        v: i32 = 20;
        total += v;
    }
    return total;
}

main: proc() -> i32 = {
    l: Later = {};
    l.v = 42;
    n: Node = {};
    n.payload = l.&;
    n.tag = 1;

    printf("%d %d\n", is_even(10), is_odd(10));
    printf("%d\n", n.payload[0].v);
    printf("%d\n", sibling_scopes());
    printf("%d %d\n", shadows_global(), g_shadow);
    return 0;
}
