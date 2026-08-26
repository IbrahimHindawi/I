TortureBits: union = {
    i: i32;
    u: u32;
}

TortureBitsAlias: alias = TortureBits;
TortureVisitor: alias = *proc(bits: *TortureBitsAlias, add: i32) -> i32;

TortureHandler: struct = {
    visit: TortureVisitor;
    payload: TortureBitsAlias;
}

torture_bits_visit: proc(bits: *TortureBitsAlias, add: i32) -> i32 = {
    return bits[0].i + add;
}

torture_handler_run: proc(handler: *TortureHandler) -> i32 = {
    visit: TortureVisitor = handler[0].visit;
    return visit(handler[0].payload.&, 3);
}

main: proc() -> i32 = {
    handler: TortureHandler = {
        .visit = torture_bits_visit,
        .payload = {.i = 9},
    };
    return torture_handler_run(handler.&);
}
