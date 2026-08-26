cinclude "stdio.h"

printf: proc[external](fmt: *const char, ...) -> i32 = {}

// Conversions are where a lowering can be wrong without being obviously wrong:
// the program runs, the number is plausible, and it is off by a factor of two
// or by a sign. Everything below is well defined rather than
// implementation-defined, except the arithmetic shift noted at its use.

// A narrowing cast keeps the low bits.
truncate_cast: proc() -> u32 = {
    x: i32 = 300;
    b: u8 = cast(x, u8);
    return cast(b, u32);
}

// Signed to unsigned is modular, not saturating and not a reinterpretation
// error: -1 becomes the all-ones value of the target width.
neg_to_unsigned: proc() -> u32 = {
    i: i32 = -1;
    return cast(i, u32);
}

// Unsigned arithmetic wraps.
unsigned_wrap: proc() -> u32 = {
    b: u8 = 255;
    b += 1;
    return cast(b, u32);
}

// Right shift on an unsigned value is logical: the sign bit does not extend.
logical_shr: proc() -> u32 = {
    u: u32 = 0x80000000;
    return u shr 31;
}

// Right shift on a negative signed value is arithmetic here. C leaves this
// implementation-defined; this test records the choice so it cannot drift.
arith_shr: proc() -> i32 = {
    n: i32 = -16;
    return n shr 2;
}

// Widening a signed value preserves the sign rather than zero-filling.
widen_keeps_sign: proc() -> i32 = {
    s: i16 = -5;
    return cast(s, i32);
}

// Widening an unsigned value zero-fills.
widen_unsigned: proc() -> u32 = {
    s: u8 = 200;
    return cast(s, u32);
}

// A value too large for the destination truncates on the way in, so this is
// 300 mod 256 and not a clamp to 255.
narrow_assign: proc() -> u32 = {
    b: u8 = cast(300, u8);
    return cast(b, u32);
}

// Explicit widening before arithmetic keeps the full result.
widen_before_add: proc() -> i32 = {
    a: u8 = 200;
    b: u8 = 100;
    return cast(a, i32) + cast(b, i32);
}

main: proc() -> i32 = {
    printf("%u\n", truncate_cast());
    printf("%u\n", neg_to_unsigned());
    printf("%u\n", unsigned_wrap());
    printf("%u\n", logical_shr());
    printf("%d\n", arith_shr());
    printf("%d\n", widen_keeps_sign());
    printf("%u\n", widen_unsigned());
    printf("%u\n", narrow_assign());
    printf("%d\n", widen_before_add());
    return 0;
}
