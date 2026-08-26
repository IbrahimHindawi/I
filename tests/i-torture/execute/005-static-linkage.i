cinclude "stdio.h"

printf: proc[external](fmt: *const char, ...) -> i32 = {}

module_seed: static i32 = 41;
shared_total: i32 = 0;

bump: static proc() -> i32 = {
    calls: static i32 = 0;
    calls += 1;
    return calls;
}

accumulate: proc(n: i32) -> i32 = {
    for (i: i32 = 0; i < n; i += 1) {
        shared_total += bump();
    }
    return shared_total;
}

main: proc() -> i32 = {
    printf("%d\n", module_seed);
    // sequenced deliberately: C does not define argument evaluation order
    a: i32 = bump();
    b: i32 = bump();
    c: i32 = bump();
    printf("%d %d %d\n", a, b, c);
    printf("%d\n", accumulate(3));
    return 0;
}
