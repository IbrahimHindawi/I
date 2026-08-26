cinclude "stdio.h"

printf: proc[external](fmt: *const char, ...) -> i32 = {}

// Loop lowering has a small number of plausible wrong answers, and each one
// changes an observable count rather than crashing.

// `continue` must still run the loop's increment. Lowering it as a jump to the
// top of the body instead of to the increment turns this into an infinite loop,
// which the harness catches as a timeout rather than a wrong answer.
continue_increments: proc() -> i32 = {
    n: i32 = 0;
    for (i: i32 = 0; i < 5; i += 1) {
        if (i == 2) {
            continue;
        }
        n += 1;
    }
    return n;
}

// `break` leaves only the innermost loop.
nested_break: proc() -> i32 = {
    n: i32 = 0;
    for (i: i32 = 0; i < 3; i += 1) {
        for (j: i32 = 0; j < 3; j += 1) {
            if (j == 1) {
                break;
            }
            n += 1;
        }
        n += 10;
    }
    return n;
}

// The loop variable belongs to its loop. If it leaked into the enclosing scope
// the second loop would collide with the first.
loop_var_scope: proc() -> i32 = {
    total: i32 = 0;
    for (i: i32 = 0; i < 3; i += 1) {
        total += i;
    }
    for (i: i32 = 10; i < 13; i += 1) {
        total += i;
    }
    return total;
}

// A do-while runs its body before testing, so it runs once even when the
// condition is false from the start.
do_runs_once: proc() -> i32 = {
    n: i32 = 100;
    do {
        n += 1;
    } while (n < 0);
    return n;
}

// A while tests first, so a false condition runs the body zero times.
while_runs_never: proc() -> i32 = {
    n: i32 = 0;
    while (n > 0) {
        n += 1;
    }
    return n;
}

// `continue` in a while must not skip the update the body is responsible for,
// which is the same hazard from the other side: here the body owns the step.
while_continue: proc() -> i32 = {
    i: i32 = 0;
    n: i32 = 0;
    while (i < 5) {
        i += 1;
        if (i == 3) {
            continue;
        }
        n += 1;
    }
    return n;
}

main: proc() -> i32 = {
    printf("%d\n", continue_increments());
    printf("%d\n", nested_break());
    printf("%d\n", loop_var_scope());
    printf("%d\n", do_runs_once());
    printf("%d\n", while_runs_never());
    printf("%d\n", while_continue());
    return 0;
}
