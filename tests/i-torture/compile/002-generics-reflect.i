TorturePayload: struct = {
    id: i32;
    weight: f32;
}

TortureBox: struct<T> = {
    value: T;
}

TortureBox<T>get: proc<T>(box: TortureBox<T>) -> T = {
    return box.value;
}

torture_payload_id: proc(payload: TorturePayload) -> i32 = {
    return payload.id;
}

main: proc() -> i32 = {
    payload: TorturePayload = {.id = 7, .weight = 2.0f};
    box: TortureBox<TorturePayload> = {.value = payload};
    unboxed: TorturePayload = TortureBox<TorturePayload>get(box);
    return torture_payload_id(unboxed) + cast(TorturePayload_reflect.count, i32);
}
