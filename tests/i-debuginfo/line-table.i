cinclude "stdio.h"

printf: proc[external](fmt: *const char, ...) -> i32 = {}

compute: proc(seed: i32) -> i32 = {
    alpha: i32 = seed + 10;
    beta: i32 = alpha * 2;
    gamma: i32 = beta - 5;
    printf("%d\n", gamma);
    return gamma;
}

branchy: proc(n: i32) -> i32 = {
    total: i32 = 0;
    for (i: i32 = 0; i < n; i += 1) {
        if (i == 2) {
            continue;
        }
        total += i;
    }
    while (total > 10) {
        total -= 3;
    }
    return total;
}

main: proc() -> i32 = {
    a: i32 = compute(1);
    b: i32 = branchy(6);
    return a - a + b - b;
}
