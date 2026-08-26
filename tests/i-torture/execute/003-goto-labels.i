cinclude "stdio.h"

printf: proc[external](fmt: *const char, ...) -> i32 = {}

// forward jump out of a nested loop, the usual reason C code reaches for goto
find_pair: proc(target: i32) -> i32 = {
    found: i32 = -1;
    for (i: i32 = 1; i < 10; i += 1) {
        for (j: i32 = 1; j < 10; j += 1) {
            if (i * j == target) {
                found = i * 100 + j;
                goto done;
            }
        }
    }
done: label = {
    printf("searched\n");
}
    return found;
}

// backward jump used as a retry loop
countdown: proc(from: i32) -> i32 = {
    n: i32 = from;
    steps: i32 = 0;
top: label = {
    if (n > 0) {
        n -= 1;
        steps += 1;
        goto top;
    }
}
    return steps;
}

main: proc() -> i32 = {
    printf("%d %d\n", find_pair(12), find_pair(97));
    printf("%d\n", countdown(7));
    return 0;
}
