cinclude "stdio.h"

printf: proc[external](fmt: *const char, ...) -> i32 = {}

main: proc() -> i32 = {
    x: u32 = 0b1010;
    printf("%u\n", ~x);

    y: u32 = 1;
    y shl= 4;
    printf("%u\n", y);
    y shr= 2;
    printf("%u\n", y);

    mask: u32 = ~cast(0, u32);
    printf("%u\n", mask shr 24);

    flags: i32 = 0;
    flags |= 1 shl 3;
    flags |= 1 shl 5;
    flags &= ~(1 shl 3);
    printf("%d\n", flags);

    n: i32 = -8;
    n shr= 1;
    printf("%d\n", n);
    return 0;
}
