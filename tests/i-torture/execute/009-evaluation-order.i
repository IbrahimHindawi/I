cinclude "stdio.h"

printf: proc[external](fmt: *const char, ...) -> i32 = {}

// Short-circuiting and how many times a subexpression runs are invisible in the
// result of most expressions, so they need side effects to be observable at all.
// A lowering that evaluates both operands, or that duplicates a subscript in a
// compound assignment, computes the right answer here and the wrong number of
// calls.

g_calls: i32 = 0;

bump: proc(v: i32) -> i32 = {
    g_calls += 1;
    return v;
}

reset: proc() -> void = {
    g_calls = 0;
}

// `and` must not evaluate its right operand when the left is false.
short_and: proc() -> i32 = {
    reset();
    if (0 != 0 and bump(1) != 0) {
        g_calls += 100;
    }
    return g_calls;
}

// `or` must not evaluate its right operand when the left is true.
short_or: proc() -> i32 = {
    reset();
    if (1 != 0 or bump(1) != 0) {
        g_calls += 100;
    }
    return g_calls;
}

// When the left operand does not decide the result, the right one must run.
full_and: proc() -> i32 = {
    reset();
    if (1 != 0 and bump(1) != 0) {
        g_calls += 100;
    }
    return g_calls;
}

// A conditional evaluates exactly one arm.
ternary_once: proc() -> i32 = {
    reset();
    v: i32 = 1 != 0 ? bump(5) : bump(9);
    return g_calls * 1000 + v;
}

// A compound assignment evaluates its target once. Lowering `a[f()] += x` as
// `a[f()] = a[f()] + x` calls f twice and still stores the right value.
compound_once: proc() -> i32 = {
    arr: [4]i32 = {};
    reset();
    arr[bump(2)] += 5;
    return g_calls * 1000 + arr[2];
}

// Arguments are evaluated once each, in some order; the count is what matters.
args_once: proc() -> i32 = {
    reset();
    v: i32 = bump(1) + bump(2) + bump(3);
    return g_calls * 1000 + v;
}

main: proc() -> i32 = {
    printf("%d\n", short_and());
    printf("%d\n", short_or());
    printf("%d\n", full_and());
    printf("%d\n", ternary_once());
    printf("%d\n", compound_once());
    printf("%d\n", args_once());
    return 0;
}
