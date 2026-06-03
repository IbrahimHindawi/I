import "memops.i"

Vec: struct<T> = {
    data: *T;
    length: u64;
    border: u64;
    external;
}

Vec<T>reserve: proc<T>(arena: *memops_arena, length: u64)->Vec<T> = {
    arr: Vec<T> = {};
    if (length == 0) {
        return arr;
    }
    arr.data = cast(memops_arena_push(arena, sizeof(T) * length, alignof(T)), *T);
    if (arr.data == null) {
        return arr;
    }
    arr.border = length;
    return arr;
}

Vec<T>resize: proc<T>(arena: *memops_arena, array: *Vec<T>)->*T = {
    old_border: u64 = array[0].border;
    if (array[0].border == 0) {
        array[0].border = 1;
    } else {
        array[0].border *= 2;
    }
    array[0].data = cast(memops_arena_realloc_(arena, sizeof(T) * array[0].border, array[0].data, sizeof(T) * old_border, alignof(T)), *T);
    return array[0].data;
}

Vec<T>append: proc<T>(arena: *memops_arena, array: *Vec<T>, elem: T)->*T = {
    if (array[0].data == null) {
        array[0].border = 1;
        array[0].data = cast(memops_arena_push(arena, sizeof(T) * array[0].border, alignof(T)), *T);
    }
    if (array[0].length >= array[0].border) {
        Vec<T>resize(arena, array);
    }
    array[0].data[array[0].length] = elem;
    result: *T = array[0].data[array[0].length].&;
    array[0].length += 1;
    return result;
}
