import "memops.i"
import "Option.i"

Array: struct<T> = {
    data: *T;
    length: u64;
}

Array<T>reserve: proc<T>(arena: *memops_arena, length: u64)->Array<T> = {
    arr: Array<T> = {};
    if (length == 0) {
        return arr;
    }
    arr.data = cast(memops_arena_push_zero(arena, sizeof(T) * length, alignof(T)), *T);
    if (arr.data == null) {
        printf("I runtime: Array reserve allocation failure\n");
        exit(1);
        return arr;
    }
    arr.length = length;
    return arr;
}

Array<T>destroy: proc<T>(arena: *memops_arena, array: *Array<T>)->void = {
    array[0].data = null;
    array[0].length = 0;
}

Array<T>at: proc<T>(array: *Array<T>, index: u64)->*T = {
    if (array == null or index >= array[0].length) {
        printf("I runtime: Array index out of bounds\n");
        exit(1);
    }
    return array[0].data[index].&;
}

Array<T>get: proc<T>(array: *Array<T>, index: u64)->Option<T> = {
    if (array == null or index >= array[0].length) {
        return Option<T>none();
    }
    return Option<T>some(array[0].data[index]);
}

Array<T>is_empty: proc<T>(array: *Array<T>)->bool = {
    return array == null or array[0].length == 0;
}
