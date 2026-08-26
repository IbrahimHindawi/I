cinclude "stdio.h"

printf: proc[external](fmt: *const char, ...) -> i32 = {}

// the shape C headers use for packed protocol headers
Packet: struct = {
    version: u32 : 4;
    kind:    u32 : 4;
    length:  u32 : 24;
    union = {
        as_u32: u32;
        as_f32: f32;
    }
    struct = {
        lo: u8;
        hi: u8;
    }
}

main: proc() -> i32 = {
    p: Packet = {};
    p.version = 3;
    p.kind = 9;
    p.length = 1000000;
    p.as_u32 = 0xDEADBEEF;
    p.lo = 1;
    p.hi = 2;

    printf("%u %u %u\n", p.version, p.kind, p.length);
    printf("%u %u %u\n", p.as_u32, p.lo, p.hi);

    // bitfields must actually truncate
    p.version = 31;
    printf("%u\n", p.version);

    // anonymous union members alias the same storage
    p.as_f32 = 1.5;
    printf("%u\n", p.as_u32);

    // reflection flattens anonymous members and skips bitfield offsets
    printf("%llu\n", Packet<>.count);
    printf("%s %s %s\n", Packet<>.variant.fields[0].name, Packet<>.variant.fields[3].name, Packet<>.variant.fields[5].name);
    return 0;
}
