cinclude "stdio.h"

print_cstr: proc(text: *const char)->void = {
    if (text == null) {
        printf("(null)");
        return;
    }
    printf("%s", text);
}

print: proc<i8>(x: i8)->void = {
    printf("%d", x);
}

print: proc<i16>(x: i16)->void = {
    printf("%d", x);
}

print: proc<i32>(x: i32)->void = {
    printf("%d", x);
}

print: proc<i64>(x: i64)->void = {
    printf("%lld", x);
}

print: proc<u8>(x: u8)->void = {
    printf("%u", x);
}

print: proc<u16>(x: u16)->void = {
    printf("%u", x);
}

print: proc<u32>(x: u32)->void = {
    printf("%u", x);
}

print: proc<u64>(x: u64)->void = {
    printf("%llu", x);
}

print: proc<usize>(x: usize)->void = {
    printf("%zu", x);
}

print: proc<f32>(x: f32)->void = {
    printf("%f", cast(x, f64));
}

print: proc<f64>(x: f64)->void = {
    printf("%f", x);
}

print: proc<char>(x: char)->void = {
    printf("%c", x);
}

print: proc<bool>(x: bool)->void = {
    if (x) {
        print_cstr("true");
    } else {
        print_cstr("false");
    }
}

print: proc<b32>(x: b32)->void = {
    if (x) {
        print_cstr("true");
    } else {
        print_cstr("false");
    }
}

print: proc<*char>(x: *char)->void = {
    print_cstr(x);
}

print: proc<*const char>(x: *const char)->void = {
    print_cstr(x);
}
