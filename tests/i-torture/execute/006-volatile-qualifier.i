cinclude "stdio.h"

printf: proc[external](fmt: *const char, ...) -> i32 = {}

// the shape memory-mapped register access takes
Regs: struct = {
    status: volatile u32;
    data: *volatile u32;
}

flag: volatile i32 = 0;

poll: proc(reg: *volatile u32) -> u32 = {
    v: volatile u32 = reg[0];
    return v;
}

main: proc() -> i32 = {
    r: Regs = {};
    r.status = 7;
    cell: volatile u32 = 42;
    r.data = cell.&;

    local: volatile i32 = 5;
    local += 1;
    flag = 1;

    cv: *const volatile u32 = cell.&;

    printf("%u %u %u\n", r.status, poll(r.data), cv[0]);
    printf("%d %d\n", flag, local);
    printf("%llu\n", sizeof(Regs));
    return 0;
}
