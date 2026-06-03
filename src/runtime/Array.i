import "memops.i"

Array: struct<T> = {
    data: *T;
    length: u64;
    external;
}

Array<T>reserve: proc<T>(arena: *memops_arena, length: u64)->Array<T> = {
    arr: Array<T> = {};
    if (length == 0) {
        return arr;
    }
    arr.data = cast(memops_arena_push(arena, sizeof(T) * length, alignof(T)), *T);
    if (arr.data == null) {
        return arr;
    }
    arr.length = length;
    return arr;
}
