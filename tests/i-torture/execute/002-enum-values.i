cinclude "stdio.h"

printf: proc[external](fmt: *const char, ...) -> i32 = {}

Status: enum = {
    Failed = -2,
    Cancelled = -1,
    Idle = 0,
    Busy,
}

Flags: enum = {
    None = 0,
    Read = 1 shl 0,
    Write = 1 shl 1,
    Exec = 1 shl 2,
    All = 7,
    Alias = All,
    Inverted = ~0,
}

describe: proc(s: Status) -> i32 = {
    switch (s) {
        case Status_Failed: { return 100; }
        case Status_Cancelled: { return 200; }
        case Status_Idle: { return 300; }
        default: { return 400; }
    }
}

main: proc() -> i32 = {
    printf("%d %d %d %d\n", Status_Failed, Status_Cancelled, Status_Idle, Status_Busy);
    printf("%d %d %d %d %d\n", Flags_Read, Flags_Write, Flags_Exec, Flags_All, Flags_Alias);
    printf("%d\n", Flags_Inverted);
    printf("%d %d\n", describe(Status_Failed), describe(Status_Busy));
    return 0;
}
