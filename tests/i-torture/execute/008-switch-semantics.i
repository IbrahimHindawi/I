cinclude "stdio.h"

printf: proc[external](fmt: *const char, ...) -> i32 = {}

// A case takes a block, so it does not fall through. This shipped falling
// through once: an enemy AI ran its approach case, fell into retreat, and
// negated its own movement vector. Every assertion below is chosen so that
// falling through changes the answer.

Mode: enum = {
    A,
    B,
    C,
}

// Assigning cases. Under fall-through every input collapses to the last value.
classify: proc(m: Mode) -> i32 = {
    v: i32 = 0;
    switch (m) {
        case Mode.A: { v = 1; }
        case Mode.B: { v = 2; }
        case Mode.C: { v = 4; }
        default: { v = 8; }
    }
    return v;
}

// Accumulating cases. Fall-through sums the tail instead of picking one arm.
accumulate: proc(m: Mode) -> i32 = {
    v: i32 = 0;
    switch (m) {
        case Mode.A: { v += 1; }
        case Mode.B: { v += 10; }
        case Mode.C: { v += 100; }
        default: { v += 1000; }
    }
    return v;
}

// break inside a case leaves the switch, not the enclosing loop. If it left the
// loop the total would stop accumulating after the first match.
loop_switch: proc() -> i32 = {
    total: i32 = 0;
    for (i: i32 = 0; i < 4; i += 1) {
        switch (i) {
            case 1: {
                total += 10;
                break;
            }
            case 2: {
                total += 100;
            }
            default: {
                total += 1;
            }
        }
        total += 1000;
    }
    return total;
}

// An empty case is still a case: it must match and do nothing, not fall into
// the next arm.
empty_case: proc(m: Mode) -> i32 = {
    v: i32 = 7;
    switch (m) {
        case Mode.A: {
        }
        case Mode.B: {
            v = 99;
        }
        default: {
            v = 55;
        }
    }
    return v;
}

// Nested switches: the inner one must not leak into the outer one's arms.
nested: proc(outer: i32, inner: i32) -> i32 = {
    v: i32 = 0;
    switch (outer) {
        case 0: {
            switch (inner) {
                case 0: { v = 1; }
                default: { v = 2; }
            }
        }
        default: {
            v = 3;
        }
    }
    return v;
}

// Switching on a plain integer, including sparse and negative labels.
sparse: proc(x: i32) -> i32 = {
    v: i32 = 0;
    switch (x) {
        case -5: { v = 11; }
        case 0: { v = 22; }
        case 1000: { v = 33; }
        default: { v = 44; }
    }
    return v;
}

main: proc() -> i32 = {
    printf("%d %d %d\n", classify(Mode.A), classify(Mode.B), classify(Mode.C));
    printf("%d %d %d\n", accumulate(Mode.A), accumulate(Mode.B), accumulate(Mode.C));
    printf("%d\n", loop_switch());
    printf("%d %d\n", empty_case(Mode.A), empty_case(Mode.B));
    printf("%d %d %d\n", nested(0, 0), nested(0, 1), nested(1, 0));
    printf("%d %d %d %d\n", sparse(-5), sparse(0), sparse(1000), sparse(7));
    return 0;
}
