import "memops.i"
import "Option.i"

Vec: struct<T> = {
    data: *T;
    length: u64;
    border: u64;
}

Vec<T>reserve: proc<T>(arena: *memops_arena, length: u64)->Vec<T> = {
    arr: Vec<T> = {};
    if (length == 0) {
        return arr;
    }
    arr.data = cast(memops_arena_push_zero(arena, sizeof(T) * length, alignof(T)), *T);
    if (arr.data == null) {
        printf("I runtime: Vec reserve allocation failure\n");
        exit(1);
        return arr;
    }
    arr.border = length;
    return arr;
}

Vec<T>destroy: proc<T>(arena: *memops_arena, array: *Vec<T>)->void = {
    array[0].data = null;
    array[0].length = 0;
    array[0].border = 0;
}

Vec<T>resize: proc<T>(arena: *memops_arena, array: *Vec<T>)->*T = {
    old_border: u64 = array[0].border;
    if (array[0].border == 0) {
        array[0].border = 1;
    } else {
        array[0].border *= 2;
    }
    array[0].data = cast(memops_arena_realloc_(arena, sizeof(T) * array[0].border, array[0].data, sizeof(T) * old_border, alignof(T)), *T);
    if (array[0].data == null) {
        printf("I runtime: Vec resize allocation failure\n");
        exit(1);
    }
    if (array[0].border > old_border) {
        memset(cast(array[0].data, *u8) + sizeof(T) * old_border, 0, sizeof(T) * (array[0].border - old_border));
    }
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

Vec<T>at: proc<T>(array: *Vec<T>, index: u64)->*T = {
    if (array == null or index >= array[0].length) {
        printf("I runtime: Vec index out of bounds\n");
        exit(1);
    }
    return array[0].data[index].&;
}

Vec<T>get: proc<T>(array: *Vec<T>, index: u64)->Option<T> = {
    if (array == null or index >= array[0].length) {
        return Option<T>none();
    }
    return Option<T>some(array[0].data[index]);
}

Vec<T>clear: proc<T>(array: *Vec<T>)->void = {
    if (array != null) {
        array[0].length = 0;
    }
}

Vec<T>is_empty: proc<T>(array: *Vec<T>)->b32 = {
    return array == null or array[0].length == 0;
}
