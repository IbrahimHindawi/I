import "memops.i"
import "Option.i"

BiNode: struct<T> = {
    next: *BiNode<T>;
    prev: *BiNode<T>;
    data: T;
}

BiNode<T>create: proc<T>(arena: *memops_arena, data: T) -> *BiNode<T> = {
    node: *BiNode<T> = cast(memops_arena_push_zero(arena, sizeof(BiNode<T>), alignof(BiNode<T>)), *BiNode<T>);
    if (node == null) {
        printf("I runtime: BiNode allocation failure\n");
        exit(1);
    }
    node[0].next = null;
    node[0].prev = null;
    node[0].data = data;
    return node;
}

BiNode<T>destroy: proc<T>(arena: *memops_arena, node: **BiNode<T>) -> void = {
    node[0] = null;
}

BiNode<T>value: proc<T>(node: *BiNode<T>) -> Option<T> = {
    if (node == null) {
        return Option<T>none();
    }
    return Option<T>some(node[0].data);
}
