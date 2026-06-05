import "memops.i"
import "Option.i"

Node: struct<T> = {
    next: *Node<T>;
    data: T;
}

Node<T>create: proc<T>(arena: *memops_arena, data: T)->*Node<T> = {
    node: *Node<T> = cast(memops_arena_push_zero(arena, sizeof(Node<T>), alignof(Node<T>)), *Node<T>);
    if (node == null) {
        printf("I runtime: Node allocation failure\n");
        exit(1);
    }
    node[0].next = null;
    node[0].data = data;
    return node;
}

Node<T>destroy: proc<T>(arena: *memops_arena, node: **Node<T>)->void = {
    node[0] = null;
}

Node<T>value: proc<T>(node: *Node<T>)->Option<T> = {
    if (node == null) {
        return Option<T>none();
    }
    return Option<T>some(node[0].data);
}
