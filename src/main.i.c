#include <core.h>

structdef(vec2) {
    f32 x;
    f32 y;
};

structdef(arr_i32) {
    u64 len;
    u64 cap;
    i32 * data;
};

i32 num = 7;

i32 make(i32 x);
i32 main();
i32 makeg_i32(i32 x);
u64 prock_i32(i32 * value);

i32 make(i32 x) {
    return x;
}

i32 main() {
    arr_i32 a;
    i32 y = makeg_i32(3);
    i32 x = num + 666 + 1;
    y = x - 1;
    i32 z = 1;
    i32 w = x * y;
    i32 d = x / y;
    i32 t = prock_i32(&x);
    return x;
}

i32 makeg_i32(i32 x) {
    return x;
}

u64 prock_i32(i32 * value) {
    return value[0];
}

