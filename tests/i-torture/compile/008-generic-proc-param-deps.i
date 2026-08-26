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

torture_make: proc<T>(value: T) -> TortureMaybe<T> = {
    return TortureMaybe<T>some(value);
}

torture_take: proc<T>(maybe: TortureMaybe<T>) -> T = {
    return maybe.value;
}

TorturePayload: struct = {
    x: i32;
}

main: proc() -> i32 = {
    payload: TorturePayload = torture_take<TorturePayload>(torture_make<TorturePayload>({.x = 41}));
    return payload.x;
}
