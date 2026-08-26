cinclude "stdio.h"

printf: proc[external](fmt: *const char, ...) -> i32 = {}

// I lowers to C, so every operator has two precedences: I's, and the one the
// emitted C would get if the emitter dropped a parenthesis. The emitter
// currently parenthesises every subexpression, which makes the two agree by
// construction. These pin that: each expression below has a different value
// under the other grouping, so a regression in parenthesisation shows up as a
// wrong number rather than as C that happens to still compile.

main: proc() -> i32 = {
    // Explicit grouping in the source must survive lowering. These are the
    // unambiguous ones: they hold whatever the precedence table says.
    printf("%d %d\n", 1 + 2 * 3, (1 + 2) * 3);
    printf("%d %d\n", 8 - 3 - 2, 8 - (3 - 2));
    printf("%d %d\n", 16 / 4 / 2, 16 / (4 / 2));
    printf("%d %d\n", 7 % 4 + 1, 7 % (4 + 1));

    // Shifts bind looser than addition, as in C. Grouped the other way these
    // would be 7 and 5.
    printf("%d %d\n", 1 shl 2 + 3, 8 shr 1 + 1);

    // The bitwise group binds tighter than comparison, which is where I departs
    // from C. C binds & | ^ looser than == only because early C had no &&, and
    // by the time it did, changing it would have broken existing code; Ritchie
    // wrote it up as a known mistake. Rust, Zig, Go and Python all fixed it.
    //
    // `6 & 4 == 4` is (6 & 4) == 4, which is 1. Under C's table it would be
    // 6 & (4 == 4), which is 0. `1 ^ 3 == 3` is (1 ^ 3) == 3, which is 0.
    printf("%d %d %d\n", 1 | 2 & 3, 6 & 4 == 4, 1 ^ 3 == 3);

    // and/or stay looser than both comparison and the bitwise group, so this
    // reads ((6 & 4) == 4) and ((3 | 0) == 3). Under C's grouping the left arm
    // would be 6 & (4 == 4), which is 0, and the whole thing would take the
    // other branch.
    printf("%d\n", 6 & 4 == 4 and 3 | 0 == 3 ? 7 : 8);

    // Unary minus and bitwise not against arithmetic.
    printf("%d %d\n", -3 + 2, ~0 + 1);

    // Logical operators bind looser than comparison, so this is
    // (1 < 2) and (3 < 4), not 1 < (2 and 3) < 4.
    printf("%d\n", 1 < 2 and 3 < 4 ? 10 : 20);

    // A conditional nested in arithmetic keeps its own boundary.
    printf("%d\n", 1 + (1 != 0 ? 2 : 3) * 10);

    // Mixed shift and mask, the shape that packs and unpacks fields. Shift
    // binds tighter than mask, so the ungrouped form masks with a shifted
    // constant rather than shifting a masked value: 52, not 18. Wrong grouping
    // here silently corrupts every packed field in a program.
    v: u32 = 0xABCD1234;
    printf("%u %u\n", v & 0xFF00 shr 8, (v & 0xFF00) shr 8);

    return 0;
}
