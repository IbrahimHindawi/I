TortureResult: struct<T> = {
    ok: b32;
    value: T;
}

TortureResult<T>ok: proc<T>(value: T) -> TortureResult<T> = {
    result: TortureResult<T> = {};
    result.ok = 1;
    result.value = value;
    return result;
}

TortureLocal: struct<T> = {
    value: T;
}

torture_local_unwrap: proc<T>(value: T) -> T = {
    local: TortureLocal<T> = {.value = value};
    result: TortureResult<T> = TortureResult<T>ok(local.value);
    return result.value;
}

TorturePayload: struct = {
    x: i32;
}

main: proc() -> i32 = {
    payload: TorturePayload = torture_local_unwrap<TorturePayload>({.x = 23});
    return payload.x;
}
