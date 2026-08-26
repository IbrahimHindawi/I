TorturePairBox: struct<T> = {
    value: T;
}

TortureOuter: struct<T> = {
    pair: TorturePairBox<T>;
    values: [2]T;
}

TorturePairBox<T>make: proc<T>(value: T) -> TorturePairBox<T> = {
    box: TorturePairBox<T> = {.value = value};
    return box;
}

TortureOuter<T>first: proc<T>(outer: TortureOuter<T>) -> T = {
    return outer.pair.value;
}

main: proc() -> i32 = {
    outer: TortureOuter<i32> = {
        .pair = TorturePairBox<i32>make(4),
        .values = {5, 6},
    };
    return TortureOuter<i32>first(outer) + outer.values[0] + outer.values[1];
}
