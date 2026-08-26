TortureKind: enum = {
    A = 1,
    B = 2,
    C = 4,
}

TorturePair: struct = {
    x: i32;
    y: i32;
}

torture_sum: proc(pair: TorturePair, kind: TortureKind) -> i32 = {
    total: i32 = pair.x + pair.y;
    switch (kind) {
        case TortureKind_A: {
            total += 3;
            break;
        }
        case TortureKind_B: {
            total += 5;
            break;
        }
        default: {
            total += 7;
            break;
        }
    }
    for (i: i32 = 0; i < 4; i += 1) {
        if (i == 2) {
            continue;
        }
        total += i;
    }
    while (total < 20) {
        total += 1;
    }
    do {
        total -= 1;
    } while (total > 20);
    masked: i32 = (total shl 1) shr 1;
    return (masked == total and kind != TortureKind_C) ? total : 0;
}

main: proc() -> i32 = {
    pair: TorturePair = {.x = 1, .y = 2};
    return torture_sum(pair, TortureKind_B);
}
