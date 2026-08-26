cinclude "stdio.h"

printf: proc[external](fmt: *const char, ...) -> i32 = {}

is_space: proc(c: u8) -> b32 = {
    return c == ' ' or c == '\t' or c == '\n' or c == '\r';
}

classify: proc(c: char) -> i32 = {
    if (c >= '0' and c <= '9') { return 1; }
    if (c >= 'a' and c <= 'z') { return 2; }
    if (c >= 'A' and c <= 'Z') { return 3; }
    return 0;
}

main: proc() -> i32 = {
    printf("%d %d %d %d\n", classify('7'), classify('q'), classify('Q'), classify('!'));
    printf("%d %d %d %d\n", is_space(' '), is_space('\t'), is_space('x'), is_space('\n'));
    // escapes must survive the trip through C unchanged
    printf("[%c%c%c%c]\n", '\\', '\'', '\x41', '\0' + 66);
    printf("%d %d\n", '\n', '\x7f');
    return 0;
}
