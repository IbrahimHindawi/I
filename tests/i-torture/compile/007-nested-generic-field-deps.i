TortureMaybe: struct<T> = {
    has_value: b32;
    value: T;
}

TortureBox: struct<T> = {
    value: T;
}

TortureWrap: struct<T> = {
    box: TortureBox<T>;
    maybe: TortureMaybe<TortureBox<T>>;
}

TorturePayload: struct = {
    x: i32;
}

main: proc() -> i32 = {
    wrap: TortureWrap<TorturePayload> = {};
    wrap.box.value.x = 31;
    wrap.maybe.has_value = 1;
    wrap.maybe.value = wrap.box;
    return wrap.maybe.value.value.x;
}
