TortureMaybe: struct<T> = {
    has_value: b32;
    value: T;
}

TortureMaybe<T>some: proc<T>(value: T) -> TortureMaybe<T> = {
    maybe: TortureMaybe<T> = {};
    maybe.has_value = 1;
    maybe.value = value;
    return maybe;
}

TortureBox: struct<T> = {
    value: T;
}

TortureBox<T>get: proc<T>(box: *TortureBox<T>) -> TortureMaybe<T> = {
    return TortureMaybe<T>some(box[0].value);
}

TorturePayload: struct = {
    x: i32;
}

main: proc() -> i32 = {
    box: TortureBox<TorturePayload> = {.value = {.x = 17}};
    return TortureBox<TorturePayload>get(&box).value.x;
}
